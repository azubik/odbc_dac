
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCQueryEx                                     }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  }
{                                                       }
{*******************************************************}

unit ODBCQueryEx;
{$I sv.inc}
interface
uses
  Classes,
  DB,
  odbcsqltypes,
  ODBCStmt,
  ODBCCustomDataSet;

type

  { TODBCStatementEx }

  TODBCStatementEx = class(TODBCCustomStatement)
  private
    FConnectionHandle: SQLHDBC;
    procedure SetConnectionHandle(Value: SQLHDBC);

  protected
    function GetConnectionHandle: SQLHDBC; override;

  public
    destructor Destroy; override;

    property ConnectionHandle: SQLHDBC read FConnectionHandle write SetConnectionHandle;

  published
    property ParamCheck;
    property Params;
    property SQL;

  end;


  { TODBCQueryEx }

  TODBCQueryEx = class(TODBCCustomDataSet)
  private
    FSQL: TStrings;
    FPrepared: Boolean;
    FParams: TODBCParams;
    FText: string;

    function GetRowsAffected: Integer;

    procedure QueryChanged(Sender: TObject);
    procedure SetPrepared(Value: Boolean);
    procedure SetQuery(const Value: TStrings);
    procedure SetParamsList(Value: TODBCParams);
    procedure SetParams;
    procedure SetParamsFromCursor;

    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);

    function GetConnectionHandle: SQLHDBC;
    procedure SetConnectionHandle(Value: SQLHDBC);

  protected
    function GetStatementClass: TODBCCustomStatementClass; override;

    procedure DefineProperties(Filer: TFiler); override;
    procedure InternalOpen; override;

    { IProviderSupport }
    procedure PSExecute; override;
    function PSGetTableName: string; override;
    procedure PSSetCommandText(const CommandText: string); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
    procedure ExecSQL;
    procedure Prepare;
    procedure UnPrepare;

    property ConnectionHandle: SQLHDBC read GetConnectionHandle write SetConnectionHandle;

    property Handle;
    property Prepared: Boolean read FPrepared write SetPrepared;
    property RowsAffected: Integer read GetRowsAffected;

    property Text: string read FText;

  published
    property BufferChunks;
    property CachedUpdates;
    property Constraints stored ConstraintsStored;
    property DataSource read GetDataSource write SetDataSource;
    property ParamCheck;
    property Params: TODBCParams read FParams write SetParamsList stored False;
    property SQL: TStrings read FSQL write SetQuery;
    property UniDirectional default False;
    property UpdateObject;

    property Active default False;
    property AutoCalcFields;
    property ObjectView default False;

    property AfterCancel;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterPost;
    property BeforeCancel;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforePost;

    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;

    property OnUpdateError;
    property OnUpdateRecord;
  end;


implementation
uses
  ODBCUtils,
  ODBCConsts;


{ TODBCStatementEx }

destructor TODBCStatementEx.Destroy;
begin
  Close;
  Unprepare;

  inherited;
end;

procedure TODBCStatementEx.SetConnectionHandle(Value: SQLHDBC);
begin
  Unprepare;
  FConnectionHandle := Value;
end;

function TODBCStatementEx.GetConnectionHandle: SQLHDBC;
begin
  if FConnectionHandle = nil then
    DatabaseError(SMissingConnectionHandle);

  Result := FConnectionHandle;
end;


{ TODBCQueryEx }

constructor TODBCQueryEx.Create(AOwner: TComponent);
begin
  inherited;
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := QueryChanged;
  FParams := TODBCParams.Create(Self);
  ParamCheck := True;
end;

destructor TODBCQueryEx.Destroy;
begin
  Close;
  UnPrepare;
  ConnectionHandle := nil;

  FSQL.Free;
  FParams.Free;
  inherited Destroy;
end;

function TODBCQueryEx.GetRowsAffected: Integer;
begin
  Result := QStmt.RowsAffected;
end;

procedure TODBCQueryEx.QueryChanged(Sender: TObject);
var
  List: TODBCParams;
begin
  if not (csReading in ComponentState) then
  begin
    Close;
    SetPrepared(False);

    if ParamCheck or (csDesigning in ComponentState) then
    begin
      List := TODBCParams.Create(Self);
      try
        FText := List.ParseSQL(SQL.Text, True);
        List.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(List);
      finally
        List.Free;
      end;
    end
    else
      FText := FSQL.Text;

    DataEvent(dePropertyChange, 0);
  end
  else
    FText := FParams.ParseSQL(FSQL.Text, False);

  StmtSQL.Assign(FSQL);
end;

procedure TODBCQueryEx.SetPrepared(Value: Boolean);
begin
  CheckInactive;

  if FPrepared <> Value then
  begin
    if Value then
      InternalPrepare
    else
      InternalUnPrepare;

    FPrepared := Value;
  end;
end;

procedure TODBCQueryEx.SetQuery(const Value: TStrings);
begin
  if FSQL.Text <> Value.Text then
  begin
    Close;
    FSQL.BeginUpdate;
    try
      FSQL.Assign(Value);
    finally
      FSQL.EndUpdate;
    end;
  end;
end;

procedure TODBCQueryEx.SetParamsList(Value: TODBCParams);
begin
  FParams.AssignValues(Value);
end;

procedure TODBCQueryEx.SetParams;
begin
  SQLParams.Assign(FParams);
end;

procedure TODBCQueryEx.SetParamsFromCursor;
var
  DataSet: TDataSet;
  i: Integer;

  procedure CheckRequiredParams;
  var
    i: Integer;
  begin
    for i := 0 to FParams.Count - 1 do
      with FParams[i] do
        if not Bound then
          DatabaseError(SRequiredParamNotSet);
  end;

begin
  if DataSource <> nil then
  begin
    DataSet := DataSource.DataSet;
    if DataSet <> nil then
    begin
      DataSet.FieldDefs.Update;
      for i := 0 to FParams.Count - 1 do
        with FParams[i] do
          if not Bound then
          begin
            Assign(DataSet.FieldByName(Name));
            Bound := False;
          end;
    end
    else
      CheckRequiredParams;
  end
  else
    CheckRequiredParams;
end;

procedure TODBCQueryEx.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(Params);
end;

procedure TODBCQueryEx.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

function TODBCQueryEx.GetStatementClass: TODBCCustomStatementClass;
begin
  Result := TODBCStatementEx;
end;

function TODBCQueryEx.GetConnectionHandle: SQLHDBC;
begin
  Result := (QStmt as TODBCStatementEx).ConnectionHandle;
end;

procedure TODBCQueryEx.SetConnectionHandle(Value: SQLHDBC);
begin
  CheckInactive;
  (QStmt as TODBCStatementEx).ConnectionHandle := Value;
end;

procedure TODBCQueryEx.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not Params.IsEqual(TODBCQueryEx(Filer.Ancestor).Params)
    else
      Result := Params.Count > 0;
  end;

begin
  inherited;
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData); {do not localize}
end;

procedure TODBCQueryEx.InternalOpen;
begin
  SetPrepared(True);

  if DataSource <> nil then
    SetParamsFromCursor;

  SetParams;
  inherited;
end;

procedure TODBCQueryEx.PSExecute;
begin
  ExecSQL;
end;

function TODBCQueryEx.PSGetTableName: string;
begin
  Result := inherited PSGetTableName;
end;

procedure TODBCQueryEx.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    SQL.Text := CommandText;
end;

procedure TODBCQueryEx.GetDetailLinkFields(MasterFields, DetailFields: TList);

  function AddFieldToList(const FieldName: string; DataSet: TDataSet; List: TList): Boolean;
  var
    Field: TField;
  begin
    Field := DataSet.FindField(FieldName);
    if (Field <> nil) then
      List.Add(Field);
    Result := Field <> nil;
  end;

var
  i: Integer;
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    for i := 0 to Params.Count - 1 do
      if AddFieldToList(Params[i].Name, DataSource.DataSet, MasterFields) then
        AddFieldToList(Params[i].Name, Self, DetailFields);
end;

procedure TODBCQueryEx.ExecSQL;
begin
  SetPrepared(True);

  if DataSource <> nil then
    SetParamsFromCursor;

  SetParams;

  QStmt.Execute;
end;

procedure TODBCQueryEx.Prepare;
begin
  SetPrepared(True);
end;

procedure TODBCQueryEx.UnPrepare;
begin
  SetPrepared(False);
end;


end.
