
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCQuery                                       }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  }
{                                                       }
{*******************************************************}

unit ODBCQuery;
{$I sv.inc}
interface
uses
  Classes,
  DB,

  odbcsqltypes,

  ODBCConnection,
  ODBCStmt,
  ODBCCustomDataset;


type

  { TODBCStatement }

  TODBCStatement = class(TODBCCustomStatement)
  private
    FConnection: TODBCConnection;

    procedure SetConnection(Value: TODBCConnection);

  protected
    procedure CheckConnection;
    procedure ActivateConnection;

    function GetConnectionHandle: SQLHDBC; override;

    procedure ConnectionStateChange(Sender: TObject; Connecting: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Connection: TODBCConnection read FConnection write SetConnection;

    property ParamCheck;
    property Params;
    property SQL;
  end;


  { TODBCQuery }

  TODBCQuery = class(TODBCCustomDataSetEx)
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

  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure InternalOpen; override;

    { IProviderSupport }
    procedure PSExecute; override;
    //function PSGetParams: TParams; override;
    function PSGetTableName: string; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
    procedure ExecSQL;
    procedure Prepare;
    procedure UnPrepare;

    property Handle;
    property Prepared: Boolean read FPrepared write SetPrepared;
    property RowsAffected: Integer read GetRowsAffected;

    property Text: string read FText;

  published
    property BufferChunks;
    property CachedUpdates;
    property Connection;
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


{ TODBCStatement }

constructor TODBCStatement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := nil;
end;

destructor TODBCStatement.Destroy;
begin
  Close;
  UnPrepare;
  SetConnection(nil);
  inherited;
end;

procedure TODBCStatement.SetConnection(Value: TODBCConnection);
begin
  CheckClosed;

  if FConnection <> Value then
  begin
    if Assigned(FConnection) then
    begin
      SetPrepared(False);
      FConnection.UnRegisterClient(Self);
    end;

    FConnection := Value;

    if Assigned(FConnection) then
      FConnection.RegisterClient(Self, ConnectionStateChange);
  end;
end;

procedure TODBCStatement.CheckConnection;
begin
  if FConnection = nil then
    DatabaseError(SMissingConnection);
end;

procedure TODBCStatement.ActivateConnection;
begin
  CheckConnection;

  if not FConnection.Connected then
    FConnection.Open;
end;

function TODBCStatement.GetConnectionHandle: SQLHDBC;
begin
  ActivateConnection;
  Result := FConnection.Handle;
end;

procedure TODBCStatement.ConnectionStateChange(Sender: TObject; Connecting: Boolean);
begin
  if not Connecting then
  begin
    Close;
    UnPrepare;
  end;
end;


{ TODBCQuery }

constructor TODBCQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := QueryChanged;
  FParams := TODBCParams.Create(Self);
  ParamCheck := True;
end;

destructor TODBCQuery.Destroy;
begin
  Close;
  UnPrepare;
  Connection := nil;

  FSQL.Free;
  FParams.Free;
  inherited Destroy;
end;

function TODBCQuery.GetRowsAffected: Integer;
begin
  Result := QStmt.RowsAffected;
end;

procedure TODBCQuery.QueryChanged(Sender: TObject);
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

procedure TODBCQuery.SetPrepared(Value: Boolean);
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

procedure TODBCQuery.SetQuery(const Value: TStrings);
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

procedure TODBCQuery.SetParamsList(Value: TODBCParams);
begin
  FParams.AssignValues(Value);
end;

procedure TODBCQuery.SetParams;
begin
  SQLParams.Assign(FParams);
end;

procedure TODBCQuery.SetParamsFromCursor;
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

procedure TODBCQuery.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(Params);
end;

procedure TODBCQuery.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

procedure TODBCQuery.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not Params.IsEqual(TODBCQuery(Filer.Ancestor).Params)
    else
      Result := Params.Count > 0;
  end;

begin
  inherited;
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData); {do not localize}
end;

procedure TODBCQuery.InternalOpen;
begin
  SetPrepared(True);

  if DataSource <> nil then
    SetParamsFromCursor;

  SetParams;
  inherited;
end;

procedure TODBCQuery.PSExecute;
begin
  ExecSQL;
end;

function TODBCQuery.PSGetTableName: string;
begin
  Result := inherited PSGetTableName;
end;

procedure TODBCQuery.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    SQL.Text := CommandText;
end;

procedure TODBCQuery.PSSetParams(AParams: TParams);
begin
  if AParams.Count > 0 then
  begin
    Params.Assign(AParams);
    Close;
  end;
  PSReset;
end;

procedure TODBCQuery.GetDetailLinkFields(MasterFields, DetailFields: TList);

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

procedure TODBCQuery.ExecSQL;
begin
  SetPrepared(True);

  if DataSource <> nil then
    SetParamsFromCursor;

  SetParams;

  QStmt.Execute;
end;

procedure TODBCQuery.Prepare;
begin
  SetPrepared(True);
end;

procedure TODBCQuery.UnPrepare;
begin
  SetPrepared(False);
end;


end.
