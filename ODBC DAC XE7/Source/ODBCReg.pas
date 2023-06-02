
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       Registration unit                               }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  } 
{                                                       }
{*******************************************************}

unit ODBCReg;
{$I sv.inc}
interface
uses
  SysUtils, Classes,
  DB, DBReg, FldLinks, DsnDBCst,
  DesignIntf, DesignEditors,
  ODBCTable;


type

{ Property Editors }

  TSQLTypeProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;


  TODBCTableFieldLinkProperty = class(TFieldLinkProperty)
  private
    FTable: TODBCTable;
  protected
    procedure GetFieldNamesForIndex(List: TStrings); override;
    function GetIndexDefs: TIndexDefs; override;
    function GetIndexFieldNames: string; override;
    function GetIndexName: string; override;
    function GetMasterFields: string; override;
    procedure SetIndexFieldNames(const Value: string); override;
    procedure SetIndexName(const Value: string); override;
    procedure SetMasterFields(const Value: string); override;
  public
    property IndexDefs: TIndexDefs read GetIndexDefs;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
    property IndexName: string read GetIndexName write SetIndexName;
    property MasterFields: string read GetMasterFields write SetMasterFields;

    procedure Edit; override;
  end;


  TODBCTableNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TODBCStoredProcNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;


{ Component Editors }

  TODBCConnectionEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TODBCUpdateSQLEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


procedure Register;

implementation
{$R ODBCReg.res}
uses
  ODBCConnection,
  ODBCCustomDataset,
  ODBCStmt,
  ODBCQuery,
  ODBCStoredProc,
  ODBCUpdateSQL,
  ODBCCatalogFunc,
  ODBCQueryEx,
  ODBCCatalogFuncEx,
  ODBCConParamEd, ODBCUpdSQLEd, ODBCUtils;

const
  SPalODBCDAC = 'ODBC DAC Pro';
  SEditParams = 'Edit Connection Params...';
  SUpdateSQLEditor = '&ODBCUpdateSQL Editor...';


{ TSQLTypeProperty }

function TSQLTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TSQLTypeProperty.GetValue: string;
begin
  if not SQLTypeToIdent(GetOrdValue, Result) then
    FmtStr(Result, '%d', [GetOrdValue]);
end;

procedure TSQLTypeProperty.GetValues(Proc: TGetStrProc);
begin
  GetSQLTypeValues(Proc);
end;

procedure TSQLTypeProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToSQLType(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;


{ TODBCTableFieldLinkProperty }

procedure TODBCTableFieldLinkProperty.GetFieldNamesForIndex(List: TStrings);
var
  i: Integer;
begin
  for i := 0 to FTable.IndexFieldCount - 1 do
    List.Add(FTable.IndexFields[i].FieldName);
end;

function TODBCTableFieldLinkProperty.GetIndexDefs: TIndexDefs;
begin
  Result := FTable.IndexDefs;
end;

function TODBCTableFieldLinkProperty.GetIndexFieldNames: string;
begin
  Result := FTable.IndexFieldNames;
end;

function TODBCTableFieldLinkProperty.GetIndexName: string;
begin
  Result := FTable.IndexName;
end;

function TODBCTableFieldLinkProperty.GetMasterFields: string;
begin
  Result := FTable.MasterFields;
end;

procedure TODBCTableFieldLinkProperty.SetIndexFieldNames(const Value: string);
begin
  FTable.IndexFieldNames := Value;
end;

procedure TODBCTableFieldLinkProperty.SetIndexName(const Value: string);
begin
  if Value = SPrimary then
    FTable.IndexName := ''
  else
    FTable.IndexName := Value;
end;

procedure TODBCTableFieldLinkProperty.SetMasterFields(const Value: string);
begin
  FTable.MasterFields := Value;
end;

procedure TODBCTableFieldLinkProperty.Edit;
var
  Table: TODBCTable;
begin
  Table := DataSet as TODBCTable;
  FTable := TODBCTable.Create(nil);
  try
    FTable.Connection := Table.Connection;
    FTable.TableName := Table.TableName;
    if Table.IndexFieldNames <> '' then
      FTable.IndexFieldNames := Table.IndexFieldNames
    else
      FTable.IndexName := Table.IndexName;
    FTable.MasterFields := Table.MasterFields;
    FTable.Open;

    inherited Edit;

    if Changed then
    begin
      Table.MasterFields := FTable.MasterFields;

      if FTable.IndexFieldNames <> '' then
        Table.IndexFieldNames := FTable.IndexFieldNames
      else
        Table.IndexName := FTable.IndexName;
    end;
  finally
    FTable.Free;
  end;
end;


{ TODBCTableNameProperty }

procedure TODBCTableNameProperty.GetValueList(List: TStrings);
begin
  with GetComponent(0) as TODBCTable do
    if Assigned(Connection) and Connection.Connected then
      Connection.GetTableNames(List, TableType);
end;


{ TODBCStoredProcNameProperty }

procedure TODBCStoredProcNameProperty.GetValueList(List: TStrings);
begin
  with GetComponent(0) as TODBCStoredProc do
    if Assigned(Connection) and Connection.Connected then
      Connection.GetStoredProcNames(List);
end;


{ Component Editors }

procedure TODBCConnectionEditor.ExecuteVerb(Index: Integer);
begin
  if EditParams(Component) then Designer.Modified;
end;

function TODBCConnectionEditor.GetVerb(Index: Integer): string;
begin
  Result := SEditParams
end;

function TODBCConnectionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


{ TODBCUpdateSQLEditor }

procedure TODBCUpdateSQLEditor.ExecuteVerb(Index: Integer);
begin
  if EditUpdateSQL(TODBCUpdateSQL(Component)) then Designer.Modified;
end;

function TODBCUpdateSQLEditor.GetVerb(Index: Integer): string;
begin
  Result := SUpdateSQLEditor;
end;

function TODBCUpdateSQLEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


{ Registration }

procedure Register;
begin
  RegisterComponents(SPalODBCDAC, [
      TODBCConnection,
      TODBCTable,
      TODBCQuery,
      TODBCStoredProc,
      TODBCUpdateSQL,
      TODBCStatement,
      TODBCDataset,
      TODBCCatalogFunc,
      TODBCCatalogStatement,
      TODBCQueryEx,
      TODBCStatementEx,
      TODBCCatalogFuncEx,
      TODBCCatalogStatementEx,
      TODBCEnvironment]);

  RegisterPropertyEditor(TypeInfo(TSQLType), nil, 'SQLType', TSQLTypeProperty);

  RegisterPropertyEditor(TypeInfo(string), TODBCTable, 'TableName', TODBCTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TODBCTable, 'IndexName', TIndexNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TODBCTable, 'IndexFieldNames', TIndexFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TODBCTable, 'MasterFields', TODBCTableFieldLinkProperty);

  RegisterPropertyEditor(TypeInfo(string), TODBCStoredProc, 'StoredProcName', TODBCStoredProcNameProperty);

{$IFDEF WIN32}
  RegisterComponentEditor(TODBCConnection, TODBCConnectionEditor);
  RegisterComponentEditor(TODBCUpdateSQL, TODBCUpdateSQLEditor);
{$ENDIF}
end;


end.

