
{*******************************************************}
{                                                       }
{       ODBC Data Access Components                     }
{                                                       }
{       ODBCException                                   }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  } 
{                                                       }
{*******************************************************}

unit ODBCException;
{$I sv.inc}
interface
uses
  Classes, DB,
  odbcsqltypes;

type
  { ODBC Error handling }

  { TDiagRec }

  TDiagRec = class
  private
    FSQLState: string;
    FNativeError: SQLINTEGER;
    FErrorMsg: string;
  public
    constructor CreateEx(const ASQLState: string; ANativeError: SQLINTEGER; const AErrorMsg: string);

    property SQLState: string read FSQLState;
    property NativeError: SQLINTEGER read FNativeError;
    property ErrorMsg: string read FErrorMsg;

  end;


  { EODBCError }

  EODBCError = class(EDatabaseError)
  private
    FDiagRecs: TList;
    FRetCode: SQLRETURN;

    function GetCount: Integer;
    function GetDiagRec(Index: Integer): TDiagRec;

  protected
    procedure Clear;

  public
    constructor CreateEx(AHandleType: SQLSMALLINT; AHandle: SQLHANDLE; ARetCode: SQLRETURN);
    destructor Destroy; override;

    property Count: Integer read GetCount;
    property DiagRecs[Index: Integer]: TDiagRec read GetDiagRec;
    property RetCode: SQLRETURN read FRetCode;
  end;

procedure ODBCError(HandleType: SQLSMALLINT; Handle: SQLHANDLE; RetCode: SQLRETURN);


implementation
uses
  SysUtils,
  odbcsql,
  ODBCIntf, ODBCConsts;

{ TDiagRec }

constructor TDiagRec.CreateEx(const ASQLState: string; ANativeError: SQLINTEGER; const AErrorMsg: string);
begin
  FSQLState := ASQLState;
  FNativeError := ANativeError;
  FErrorMsg := AErrorMsg;
end;


{ EODBCError }

constructor EODBCError.CreateEx(AHandleType: SQLSMALLINT; AHandle: SQLHANDLE; ARetCode: SQLRETURN);
const
  MAXBUFLEN = 256;
var
{$IFNDEF USE_UNICODE_DRIVER}
  ErrorMsgBuff, SQLStateBuff: array[0..MAXBUFLEN - 1] of SQLCHAR;
{$ELSE}
  ErrorMsgBuff, SQLStateBuff: array[0..MAXBUFLEN - 1] of SQLWCHAR;
{$ENDIF}
  NativeError: SQLINTEGER;

  RecNum: SQLSMALLINT;
  DiagRec: TDiagRec;

  ErrorMsg, SQLState: string;
begin
  FRetCode := ARetCode;
  FDiagRecs := TList.Create;

  case ARetCode of
    SQL_ERROR, SQL_SUCCESS_WITH_INFO:
      begin
        RecNum := 1;

        while
{$IFNDEF USE_UNICODE_DRIVER}
        SQLGetDiagRecA
{$ELSE}
        SQLGetDiagRecW
{$ENDIF}
          (AHandleType, AHandle, RecNum, @SQLStateBuff, @NativeError, @ErrorMsgBuff, MAXBUFLEN, nil) <> SQL_NO_DATA do
        begin
{$IFNDEF USE_UNICODE_DRIVER}
          ErrorMsg := UnicodeString(AnsiString(PAnsiChar(@ErrorMsgBuff)));
          SQLState := UnicodeString(AnsiString(PAnsiChar(@SQLStateBuff)));
{$ELSE}
          ErrorMsg := PWideChar(@ErrorMsgBuff);
          SQLState := PWideChar(@SQLStateBuff);
{$ENDIF}
          DiagRec := TDiagRec.CreateEx(SQLState, NativeError, ErrorMsg);
          FDiagRecs.Add(DiagRec);

          Message := Message
            + ErrorMsg + CRLF
            + SSQLState + SQLState + CRLF
            + SNativeErrCode + IntToStr(NativeError) + CRLF;

          Inc(RecNum);
        end;
      end;

    SQL_INVALID_HANDLE:
      Message := SSQL_INVALID_HANDLE;

    SQL_STILL_EXECUTING:
      Message := SSQL_STILL_EXECUTING;

    SQL_NEED_DATA:
      Message := SSQL_NEED_DATA;

    SQL_NO_DATA:
      Message := SSQL_NO_DATA;
  else
    Message := Format(SUNKNOWN_RETCODE, [ARetCode]);
  end;

  if Message = '' then
    Message := Format(SNoODBCInfo, [ARetCode]);

  inherited Create(Message);
end;

destructor EODBCError.Destroy;
begin
  Clear;
  FDiagRecs.Free;
  inherited;
end;

function EODBCError.GetCount: Integer;
begin
  Result := FDiagRecs.Count;
end;

function EODBCError.GetDiagRec(Index: Integer): TDiagRec;
begin
  Result := FDiagRecs[Index];
end;

procedure EODBCError.Clear;
begin
  while FDiagRecs.Count > 0 do
  begin
    TDiagRec(FDiagRecs[FDiagRecs.Count - 1]).Free;
    FDiagRecs.Delete(FDiagRecs.Count - 1);
  end;
end;

procedure ODBCError(HandleType: SQLSMALLINT; Handle: SQLHANDLE; RetCode: SQLRETURN);
begin
  raise EODBCError.CreateEx(HandleType, Handle, RetCode);
end;


end.
