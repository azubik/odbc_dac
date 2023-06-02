
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCUpdateSQL                                   }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  }
{                                                       }
{*******************************************************}

unit ODBCUpdateSQL;
{$I sv.inc}
interface
uses
  SysUtils, Classes, DB,
  ODBCCustomDataset, ODBCQuery;


type

  { TODBCUpdateSQL }

  TODBCUpdateSQL = class(TODBCDataSetUpdateObject)
  private
    FCheckRowsAffected: Boolean;

    FDataSet: TODBCCustomDataSet;
    FQueries: array[TUpdateKind] of TODBCQuery;
    FSQLText: array[TUpdateKind] of TStrings;
    function GetQuery(UpdateKind: TUpdateKind): TODBCQuery;
    function GetSQLIndex(Index: Integer): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SetSQLIndex(Index: Integer; Value: TStrings);

  protected
    function GetSQL(UpdateKind: TUpdateKind): TStrings; override;
    function GetDataSet: TODBCCustomDataSet; override;
    procedure SetDataSet(ADataSet: TODBCCustomDataSet); override;
    procedure SQLChanged(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Apply(UpdateKind: TUpdateKind); override;
    procedure ExecSQL(UpdateKind: TUpdateKind);
    procedure SetParams(UpdateKind: TUpdateKind);

    property DataSet;
    property Query[UpdateKind: TUpdateKind]: TODBCQuery read GetQuery;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;

  published
    property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
    property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;

    property CheckRowsAffected: Boolean read FCheckRowsAffected write FCheckRowsAffected;
  end;


implementation
uses
{$IF RTLVersion >= 14.0} 
  Variants,
{$IFEND}
  ODBCStmt, ODBCUtils, ODBCConsts;


{ TODBCUpdateSQL }

constructor TODBCUpdateSQL.Create(AOwner: TComponent);
var
  UpdateKind: TUpdateKind;
begin
  inherited Create(AOwner);
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSQLText[UpdateKind] := TStringList.Create;
    TStringList(FSQLText[UpdateKind]).OnChange := SQLChanged;
  end;

  FCheckRowsAffected := True;
end;

destructor TODBCUpdateSQL.Destroy;
var
  UpdateKind: TUpdateKind;
begin
  if Assigned(FDataSet) and (FDataSet.UpdateObject = Self) then
    FDataSet.UpdateObject := nil;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKind].Free;
  inherited Destroy;
end;

function TODBCUpdateSQL.GetQuery(UpdateKind: TUpdateKind): TODBCQuery;
begin
  if not Assigned(FQueries[UpdateKind]) then
  begin
    FQueries[UpdateKind] := TODBCQuery.Create(Self);
    FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
    if (FDataSet is TODBCCustomDataSet) then
      FQueries[UpdateKind].Connection := TODBCCustomDataSetEx(FDataSet).Connection;
  end;
  Result := FQueries[UpdateKind];
end;

function TODBCUpdateSQL.GetSQLIndex(Index: Integer): TStrings;
begin
  Result := FSQLText[TUpdateKind(Index)];
end;

procedure TODBCUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

procedure TODBCUpdateSQL.SetSQLIndex(Index: Integer; Value: TStrings);
begin
  SetSQL(TUpdateKind(Index), Value);
end;

function TODBCUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSQLText[UpdateKind];
end;

function TODBCUpdateSQL.GetDataSet: TODBCCustomDataSet;
begin
  Result := FDataSet;
end;

procedure TODBCUpdateSQL.SetDataSet(ADataSet: TODBCCustomDataSet);
begin
  FDataSet := ADataSet;
end;

procedure TODBCUpdateSQL.SQLChanged(Sender: TObject);
var
  UpdateKind: TUpdateKind;
begin
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    if Sender = FSQLText[UpdateKind] then
    begin
      if Assigned(FQueries[UpdateKind]) then
      begin
        FQueries[UpdateKind].Params.Clear;
        FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
      end;
      Break;
    end;
end;

procedure TODBCUpdateSQL.Apply(UpdateKind: TUpdateKind);
begin
  SetParams(UpdateKind);
  ExecSQL(UpdateKind);
end;

procedure TODBCUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
begin
  with Query[UpdateKind] do
  begin
    Prepare;
    ExecSQL;
    if FCheckRowsAffected and (RowsAffected <> 1) then DatabaseError(SUpdateFailed);
  end;
end;

procedure TODBCUpdateSQL.SetParams(UpdateKind: TUpdateKind);

  procedure AssignParams(ADataSet: TDataSet; AParams: TODBCParams);
  var
    Param: TODBCParam;
    ParamName: string;
    Field: TField;
    Value: Variant;

    Old: Boolean;
    i: Integer;
  begin
    for i := 0 to AParams.Count - 1 do
    begin
      Param := AParams[i];
      Param.ParamType := ptInput;

      ParamName := Param.Name;
      Old := CompareText(Copy(ParamName, 1, 4), 'OLD_') = 0; {do not localize}
      if Old then System.Delete(ParamName, 1, 4);

      Field := ADataSet.FindField(ParamName);
      if not Assigned(Field) then Continue;

      if Old then
        Param.Assign(Field)
      else
      begin
        Value := Field.NewValue;
        if VarIsEmpty(Value) then Value := Field.OldValue;
        Param.Assign(Field);
      end;
    end;
  end;

begin
  if Assigned(FDataSet) then
    AssignParams(FDataSet, Query[UpdateKind].Params);
end;


end.
