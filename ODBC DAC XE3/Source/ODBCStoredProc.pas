
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCStoredProc                                  }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  }
{                                                       }
{*******************************************************}

unit ODBCStoredProc;
{$I sv.inc}
interface
uses
  Classes, DB,
  ODBCConnection, ODBCStmt, ODBCCustomDataset;

type

  TODBCStoredProc = class(TODBCCustomDataSetEx)
  private
    FStoredProcName: string;
    FQuoteChar: Char;
    FParams: TODBCParams;

    FPrepared: Boolean;

    function GetParamsCount: Word;

    procedure SetPrepared(Value: Boolean);
    procedure SetParamsList(Value: TODBCParams);

    procedure SetParams;
    procedure GetParams;

    procedure SetStoredProcName(const Value: string);

    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);

    procedure GetParamsMetaData;

    function QuoteStoredProcName: string;
    function GenerateSQL: string;

  protected
    procedure DefineProperties(Filer: TFiler); override;

    { IProviderSupport }
    procedure PSExecute; override;
    function PSGetTableName: string; override;
    function PSGetParams: TParams; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;

    procedure InternalOpen; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CopyParams(Value: TODBCParams);
    function ParamByName(const Value: string): TODBCParam;

    procedure Prepare;
    procedure Unprepare;
    procedure ExecProc;

    property ParamCount: Word read GetParamsCount;
    property Prepared: Boolean read FPrepared write SetPrepared;
    property Handle;

  published
    property Params: TODBCParams read FParams write SetParamsList stored False;
    property StoredProcName: string read FStoredProcName write SetStoredProcName;

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
  SysUtils,

  odbcsqltypes,
  odbcsql,
{$IFDEF ODBC_DAC_UNICODE}
  odbcsqlucode,
{$ENDIF}
  odbcsqlext,

  ODBCIntf,
  ODBCException,
  ODBCUtils;


{ TODBCStoredProc }

constructor TODBCStoredProc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TODBCParams.Create(Self);

  ParamCheck := False;
end;

destructor TODBCStoredProc.Destroy;
begin
  Close;
  UnPrepare;
  Connection := nil;

  FParams.Free;
  inherited Destroy;
end;

function TODBCStoredProc.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

procedure TODBCStoredProc.SetPrepared(Value: Boolean);
begin
  CheckInactive;

  if FPrepared <> Value then
  begin
    if Value then
    begin
      if QStmt.SQL.Text = '' then
        QStmt.SQL.Text := GenerateSQL;

      InternalPrepare
    end
    else
    begin
      InternalUnprepare;
      QStmt.SQL.Text := '';

      FParams.Clear;
    end;

    FPrepared := Value;
  end;
end;

procedure TODBCStoredProc.SetParamsList(Value: TODBCParams);
begin
  CheckInactive;

  if Prepared then
  begin
    SetPrepared(False);
    FParams.Assign(Value);
    SetPrepared(True);
  end
  else
    FParams.Assign(Value);
end;

procedure TODBCStoredProc.SetParams;
begin
  SQLParams.Assign(FParams);
end;

procedure TODBCStoredProc.GetParams;
begin
  FParams.Assign(SQLParams);
end;

procedure TODBCStoredProc.SetStoredProcName(const Value: string);
begin
  if FStoredProcName <> Value then
  begin
    CheckInactive;

    FStoredProcName := Value;

    if (Value <> '') and (Connection <> nil) then
    begin
      ActivateConnection;

      FQuoteChar := Connection.QuoteChar;
      GetParamsMetaData;
      QStmt.SQL.Text := GenerateSQL;
    end
    else
      QStmt.SQL.Text := '';
  end;
end;

procedure TODBCStoredProc.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(Params);
end;

procedure TODBCStoredProc.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

procedure TODBCStoredProc.GetParamsMetaData;
var
{$IFNDEF ODBC_DAC_UNICODE}
  ColNameA: array[0..STR_LEN - 1] of SQLCHAR;
{$ELSE}
  ColNameW: array[0..STR_LEN - 1] of SQLWCHAR;
{$ENDIF}
  ColType: SQLSMALLINT;
  SqlDT: SQLSMALLINT;
  StrLen_or_Ind: SQLINTEGER;

  P: TParam;
  TmpHandle: SQLHANDLE;
  RetCode: SQLRETURN;
begin
  FParams.Clear;

  if SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_STMT, Connection.Handle, @TmpHandle)) then
  try
{$IFNDEF ODBC_DAC_UNICODE}
    RetCode := SQLProcedureColumnsA(TmpHandle, nil, 0, nil, 0, PSQLCHAR(AnsiString(FStoredProcName)), SQL_NTS, nil, 0);
{$ELSE}
    RetCode := SQLProcedureColumnsW(TmpHandle, nil, 0, nil, 0, PSQLWCHAR(FStoredProcName), SQL_NTS, nil, 0);
{$ENDIF}
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

{$IFNDEF ODBC_DAC_UNICODE}
    RetCode := SQLBindCol(TmpHandle, 4, SQL_C_CHAR, @ColNameA, STR_LEN, @StrLen_or_Ind);
{$ELSE}
    RetCode := SQLBindCol(TmpHandle, 4, SQL_C_WCHAR, @ColNameW, STR_LEN, @StrLen_or_Ind);
{$ENDIF}
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

    RetCode := SQLBindCol(TmpHandle, 5, SQL_C_SSHORT, @ColType, 0, @StrLen_or_Ind);
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

    RetCode := SQLBindCol(TmpHandle, 15, SQL_C_SSHORT, @SqlDT, 0, @StrLen_or_Ind);
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

    while SQL_SUCCEEDED(SQLFetch(TmpHandle)) do
      if ColType <> SQL_RESULT_COL then
      begin
        P := FParams.Add as TParam;

{$IFNDEF ODBC_DAC_UNICODE}
        P.Name := UnicodeString(AnsiString(PSQLCHAR(@ColNameA)));
{$ELSE}
        P.Name := UnicodeString(PSQLWCHAR(@ColNameW));
{$ENDIF}
        P.ParamType := ParamConstToParamType(ColType);
        P.DataType := SQLTypeToFieldType(SqlDT);
      end;
  finally
    SQLFreeHandle(SQL_HANDLE_STMT, TmpHandle);
  end;
end;

function TODBCStoredProc.QuoteStoredProcName: string;
var
  PosIdx: Integer;
  SchemaNamePart: string;
  StoredProcNamePart: string;
begin
  PosIdx := Pos(SCHEMA_SEPARATOR, FStoredProcName);

  if PosIdx = 0 then
    Result := QuoteStr(FStoredProcName, FQuoteChar)
  else
  begin
    SchemaNamePart := Copy(FStoredProcName, 1, PosIdx - 1);
    StoredProcNamePart := Copy(FStoredProcName, PosIdx + 1, Length(FStoredProcName));

    Result := QuoteStr(SchemaNamePart, FQuoteChar) + SCHEMA_SEPARATOR + QuoteStr(StoredProcNamePart, FQuoteChar);
  end;
end;

function TODBCStoredProc.GenerateSQL: string;

  function RetValExists: Boolean;
  begin
    Result := (FParams.Count > 0) and (FParams[0].ParamType = ptResult)
  end;

  function GetIOParamString: string;
  var
    DelimChar: string;
    i: Integer;
  begin
    DelimChar := '';

    for i := 0 to FParams.Count - 1 do
      if FParams[i].ParamType <> ptResult then
      begin
        Result := Result + DelimChar + '?';
        DelimChar := ','
      end;

    if (FParams.Count > 0) and (FParams[0].ParamType <> ptResult) then
      Result := '(' + Result + ')';
  end;

const
  SQL_PROC_CALL = '{%s call %s %s}';
var
  ProcName: string;
  RetVal: string;
  IOParamStr: string;
begin
  ProcName := QuoteStoredProcName;

  if RetValExists then
    RetVal := '?=';

  IOParamStr := GetIOParamString;

  // {[?=]call procedure-name[([parameter][,[parameter]]...)]}
  Result := Format(SQL_PROC_CALL, [RetVal, ProcName, IOParamStr]);
end;

procedure TODBCStoredProc.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TODBCStoredProc(Filer.Ancestor).FParams) else
      Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

procedure TODBCStoredProc.PSExecute;
begin
  ExecProc;
end;

function TODBCStoredProc.PSGetTableName: string;
begin
  Result := inherited PSGetTableName;
end;

function TODBCStoredProc.PSGetParams: TParams;
begin
  Result := nil;
end;

procedure TODBCStoredProc.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    StoredProcName := CommandText;
end;

procedure TODBCStoredProc.PSSetParams(AParams: TParams);
begin
  if AParams.Count > 0 then
    Params.Assign(AParams);
  Close;
end;

procedure TODBCStoredProc.InternalOpen;
begin
  if not Prepared then
    Prepare;

  SetParams;
  inherited;
end;

procedure TODBCStoredProc.CopyParams(Value: TODBCParams);
begin
  if not Prepared and (FParams.Count = 0) then
  try
    Prepare;
    Value.Assign(FParams);
  finally
    UnPrepare;
  end
  else
    Value.Assign(FParams);
end;

function TODBCStoredProc.ParamByName(const Value: string): TODBCParam;
begin
  if not Prepared and (FParams.Count = 0) then
    Prepare;
  Result := FParams.ParamByName(Value);
end;

procedure TODBCStoredProc.Prepare;
begin
  SetPrepared(True);
end;

procedure TODBCStoredProc.UnPrepare;
begin
  SetPrepared(False);
end;

procedure TODBCStoredProc.ExecProc;
begin
  CheckInActive;

  if not Prepared then
    Prepare;

  if FParams.Count > 0 then
    SetParams;

  QStmt.Execute;

  if FParams.Count > 0 then
    GetParams;
end;


end.
