
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCCustomDataset                               }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  } 
{                                                       }
{*******************************************************}

unit ODBCCustomDataset;
{$I sv.inc}
interface
uses
  Classes,
  SysUtils,
  DB,
  odbcsqltypes,
  ODBCException,
  ODBCConnection,
  ODBCStmt;

const
  BufferCacheSize = 32;                           { Allocate cache in this many record chunks}
  UniCache = 2;                                   { Uni-directional cache is 2 records big }

  FieldIsNull = Byte(0);                                // FieldIsNull flag
  FieldIsNotNull = Byte(1);                             // FieldIsNotNull flag
  SizeOfNullFlag = 1;                             // One byte before every field data.

type

  { Forward declaration }
  TODBCCustomDataSet = class;
  TODBCDataSet = class;


  { TODBCDataSetUpdateObject }

  TODBCDataSetUpdateObject = class(TComponent)
  protected
    function GetDataSet: TODBCCustomDataSet; virtual; abstract;
    procedure SetDataSet(ADataSet: TODBCCustomDataSet); virtual; abstract;

    procedure Apply(UpdateKind: TUpdateKind); virtual; abstract;
    function GetSQL(UpdateKind: TUpdateKind): TStrings; virtual; abstract;

    property DataSet: TODBCCustomDataSet read GetDataSet write SetDataSet;
  end;


  { TODBCDataLink }

  TODBCDataLink = class(TDetailDataLink)
  private
    FDataSet: TODBCCustomDataSet;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    function GetDetailDataSet: TDataSet; override;
    procedure CheckBrowseMode; override;
  public
    constructor Create(ADataSet: TODBCCustomDataSet);
  end;


  { TODBCCustomDataSet }

// Buffer structure:
// ----------------------------------------------------------
// |  PRecInfo  |  Record Data  | Calc Fields | Blob Fields |
// ----------------------------------------------------------
// |<----- FCalcFieldsOfs ----->|             |             |
// |<------------ FBlobCacheOfs ------------->|             |
// |<-------------------- FRecBufSize --------------------->|

  TBlobDataArray = array[0..0] of TBlobData;
  PBlobDataArray = ^TBlobDataArray;

  // Field description structure
  PFieldDesc = ^TFieldDesc;
  TFieldDesc = record
    DataSize: Cardinal;
    DataOfs: Cardinal;
    NullFlagOfs: Cardinal;
  end;

  TFieldDescArray = array[1..1] of TFieldDesc;
  PFieldDescArray = ^TFieldDescArray;

  // Record description structure
  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    UpdateStatus: TUpdateStatus;                  // CachedUpdate status
    PhyRecNum: Longint;                           // Physical record number in dataset (0-based).
    BookmarkFlag: TBookmarkFlag;                  // BOF, EOF, etc
    // Bookmark: string;
  end;


  TODBCCustomDataSet = class(TDataSet)
  private
    FDataLink: TDataLink;
    FQStmt: TODBCCustomStatement;

    FOpen: Boolean;
    FCachedUpdates: Boolean;
    FCacheBlobs: Boolean;
    FUniDirectional: Boolean;
    FUpdateMode: TUpdateMode;
    FUpdateObject: TODBCDataSetUpdateObject;
    FUpdatesPending: Boolean;
    FUpdateRecordTypes: TUpdateStatusSet;

    FRecordPos: Integer;
    FPhyRecCount: Integer;
    FDeletedRecordCount: Integer;                 // applied records only

    FRecBufSize: Integer;

    //Record's Offsets
    FCalcFieldsOfs: Integer;
    FBlobCacheOfs: Integer;
    FBlobBufferSize: Integer;

    // Record's fields info
    FMetaFields: PFieldDescArray;

    FBufferChunks: Integer;

    // Cache
    FChunks: TBufferList;
    FChunkSize: Integer;
    FChunkCount: Integer;
    FCurrentChunk: TRecordBuffer;

    FCacheList: TList;

    // Old cache
    FOldCache: TRecordBuffer;
    FOldCacheSize: Cardinal;
    FOldRecordCount: Integer;

    // CachedUpdates history
    FUpdateHistory: TList;

    // Other buffers
    FFilterBuffer: TRecordBuffer;
    FOldBuffer: TRecordBuffer;

    // Events
    FOnUpdateError: TUpdateErrorEvent;
    FOnUpdateRecord: TUpdateRecordEvent;

    // Get/Set methods
    function GetHandle: SQLHSTMT;
    function GetParamCheck: Boolean;
    function GetSQLParams: TODBCParams;
    function GetStmtSQL: TStrings;

    procedure SetBufferChunks(Value: Integer);
    procedure SetStmtSQL(Value: TStrings);
    procedure SetCacheBlobs(Value: Boolean);
    procedure SetCachedUpdates(Value: Boolean);
    procedure SetParamCheck(Value: Boolean);
    procedure SetUniDirectional(Value: Boolean);
    procedure SetUpdateMode(const Value: TUpdateMode);
    procedure SetUpdateObject(Value: TODBCDataSetUpdateObject);
    procedure SetUpdateRecordTypes(Value: TUpdateStatusSet);

    procedure FreeBlobFields(Buffer: TRecordBuffer);

    procedure FetchNext(Buffer: TRecordBuffer);
    function WriteBufferToCache(ARecNum: Integer; Buffer: TRecordBuffer): TRecordBuffer;

    procedure CopyBuffer(Source: TRecordBuffer; Dest: TRecordBuffer);
    procedure SaveToOldCache(Buffer: TRecordBuffer);

    function GetCacheBuffer(Buffer: TRecordBuffer): TRecordBuffer;
    function GetCacheBufferA(ARecNum: Integer): TRecordBuffer;
    function GetCacheOldBuffer(ARecNum: Integer): TRecordBuffer;

    function AdjustCurrentRecord(Buffer: TRecordBuffer; GetMode: TGetMode): TGetResult;
    function IsVisible(Buffer: TRecordBuffer): Boolean;

    // other methods
    procedure RefreshParams;

    procedure GetBlobData(Field: TField; Buffer: TRecordBuffer; out Data: Pointer; out DataLength: Integer);
    procedure SetBlobData(Field: TField; Buffer: TRecordBuffer; Data: Pointer; DataLength: Integer);

    procedure AddToUpdateHistory(Buffer: TRecordBuffer);
    procedure RemoveFromUpdateHistory(Buffer: TRecordBuffer);

  protected
    procedure CheckBiDirectional;
    procedure CheckEditState;
    procedure CheckCachedUpdateMode;

    procedure CheckDeleteOperation; virtual;
    procedure CheckInsertOperation; virtual;
    procedure CheckModifyOperation; virtual;

    procedure InitRecordMetaData;
    function GetRecordDataSize: Integer;

    procedure FreeCache;
    procedure FreeOldCache;

    function GetStatementClass: TODBCCustomStatementClass; virtual;
    function GetFieldDefSize(FT: TFieldType; ColSize: Integer; Len: Integer; DecDigits: SmallInt): Integer;

    function GetActiveBuffer: TRecordBuffer;
    function GetDataSource: TDataSource; override;
    procedure SetDataSource(Value: TDataSource);

    procedure ConnectionStateChange(Sender: TObject; Connecting: Boolean);
    function ConstraintsStored: Boolean;

    // Dataset editing routine
    procedure ApplyToDBMS(UpdateKind: TUpdateKind); // CachedUpdates = False
    procedure ApplyToCache(UpdateKind: TUpdateKind); // CachedUpdates = True
    procedure ApplyCachedToDBMS(UpdateKind: TUpdateKind; Buffer: Pointer);

    procedure ApplyCachedUpdate(Buffer: TRecordBuffer; var UpdateAction: TUpdateAction); virtual;
    procedure RevertCachedUpdate(Buffer: TRecordBuffer);

    procedure DoDelete; virtual;
    procedure DoInsert; virtual;
    procedure DoModify; virtual;

    procedure DoUpdateError(E: EODBCError; UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);

    procedure InternalPrepare; virtual;
    procedure InternalUnPrepare; virtual;
    procedure InternalExecute; virtual;

    function LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean): Boolean;
    procedure ReQuery;

    { IProviderSupport }
    function PSGetTableName: string; override;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSReset; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;

    { TDataSet methods }

    // Records buffering methods
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;

    function InternalGetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;

    function GetRecordSize: Word; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;

    // Bookmark methods
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;

    // Navigation methods
    procedure InternalFirst; override;
    procedure InternalLast; override;

    function FindRecord(Restart, GoForward: Boolean): Boolean; override;

    // Edit methods
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalCancel; override;
    procedure InternalEdit; override;
    procedure InternalDelete; override;
    procedure InternalInsert; override;
    procedure InternalPost; override;

    // Others methods
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalRefresh; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalHandleException; override;
    function IsCursorOpen: Boolean; override;

    procedure OpenCursor(InfoQuery: Boolean); override;

    // Additional methods
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;

    procedure ClearCalcFields(Buffer: TRecordBuffer); override;
    function GetCanModify: Boolean; override;

    property _Open: Boolean read FOpen write FOpen;

    property _RecordPos: Integer read FRecordPos write FRecordPos;
    property _PhyRecCount: Integer read FPhyRecCount write FPhyRecCount;
    property _DeletedRecordCount: Integer read FDeletedRecordCount write FDeletedRecordCount;

    property _RecBufSize: Integer read FRecBufSize write FRecBufSize;

    property _CalcFieldsOfs: Integer read FCalcFieldsOfs write FCalcFieldsOfs;
    property _BlobCacheOfs: Integer read FBlobCacheOfs write FBlobCacheOfs;
    property _BlobBufferSize: Integer read FBlobBufferSize write FBlobBufferSize;

    property _BufferChunks: Integer read FBufferChunks write FBufferChunks;

    property _ChunkSize: Integer read FChunkSize write FChunkSize;
    property _ChunkCount: Integer read FChunkCount write FChunkCount;
    property _CurrentChunk: TRecordBuffer read FCurrentChunk write FCurrentChunk;

    property _CacheList: TList read FCacheList write FCacheList;

    property _OldCache: TRecordBuffer read FOldCache write FOldCache;
    property _OldCacheSize: Cardinal read FOldCacheSize write FOldCacheSize;
    property _OldRecordCount: Integer read FOldRecordCount write FOldRecordCount;

    property _UpdateHistory: TList read FUpdateHistory write FUpdateHistory;

    property _FilterBuffer: TRecordBuffer read FFilterBuffer write FFilterBuffer;
    property _OldBuffer: TRecordBuffer read FOldBuffer write FOldBuffer;

    property Handle: SQLHSTMT read GetHandle;
    property MetaFields: PFieldDescArray read FMetaFields;
    property SQLParams: TODBCParams read GetSQLParams;
    property ParamCheck: Boolean read GetParamCheck write SetParamCheck;
    property QStmt: TODBCCustomStatement read FQStmt;

    property BufferChunks: Integer read FBufferChunks write SetBufferChunks;
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates;
    property StmtSQL: TStrings read GetStmtSQL write SetStmtSQL;
    property UniDirectional: Boolean read FUniDirectional write SetUniDirectional default False;
    property UpdateMode: TUpdateMode read FUpdateMode write SetUpdateMode default upWhereAll;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { TDataSet support methods }
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetCurrentRecord(Buffer: TRecordBuffer): Boolean; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override;
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
    function IsSequenced: Boolean; override;
    function UpdateStatus: TUpdateStatus; override;

    procedure ApplyUpdates;
    procedure CancelUpdates;
    procedure FetchAll;
    procedure RevertRecord;

    property CacheBlobs: Boolean read FCacheBlobs write SetCacheBlobs default True;
    property UpdateObject: TODBCDataSetUpdateObject read FUpdateObject write SetUpdateObject;
    property UpdatesPending: Boolean read FUpdatesPending;
    property UpdateRecordTypes: TUpdateStatusSet read FUpdateRecordTypes write SetUpdateRecordTypes;

    property OnUpdateError: TUpdateErrorEvent read FOnUpdateError write FOnUpdateError;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;

  published
    property AfterClose;
    property AfterOpen;
    property AfterRefresh;
    property AfterScroll;
    property BeforeClose;
    property BeforeOpen;
    property BeforeRefresh;
    property BeforeScroll;
  end;


  TODBCCustomDataSetEx = class(TODBCCustomDataSet)
  private
    function GetConnection: TODBCConnection;
    procedure SetConnection(Value: TODBCConnection);

  protected
    procedure InternalSetConnection(Value: TODBCConnection); virtual;
    procedure ActivateConnection; virtual;

    { IProviderSupport }
{$IFNDEF VCL50}
    function PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer = nil): Integer; override;
{$ENDIF}
    function PSGetQuoteChar: string; override;
    procedure PSStartTransaction; override;
    procedure PSEndTransaction(Commit: Boolean); override;
    function PSInTransaction: Boolean; override;

    procedure OpenCursor(InfoQuery: Boolean); override;

  public

  published
    property Connection: TODBCConnection read GetConnection write SetConnection;

  end;


  TODBCInternalDataSet = class(TODBCCustomDataSetEx)
  private
    FQDelete: TODBCCustomStatement;
    FQInsert: TODBCCustomStatement;
    FQModify: TODBCCustomStatement;

    function GetDeleteSQL: TStrings;
    function GetInsertSQL: TStrings;
    function GetModifySQL: TStrings;

    procedure SetDeleteSQL(Value: TStrings);
    procedure SetInsertSQL(Value: TStrings);
    procedure SetModifySQL(Value: TStrings);

    procedure SetInternalSQLParams(Stmt: TODBCCustomStatement; Buffer: TRecordBuffer);

  protected
    procedure CheckDeleteOperation; override;
    procedure CheckInsertOperation; override;
    procedure CheckModifyOperation; override;

    procedure ApplyCachedUpdate(Buffer: TRecordBuffer; var UpdateAction: TUpdateAction); override;

    procedure InternalSetConnection(Value: TODBCConnection); override;

    procedure DoDelete; override;
    procedure DoInsert; override;
    procedure DoModify; override;

    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;

    function GetCanModify: Boolean; override;
    procedure InternalSetParamsFromCursor;

    property QDelete: TODBCCustomStatement read FQDelete;
    property QInsert: TODBCCustomStatement read FQInsert;
    property QModify: TODBCCustomStatement read FQModify;

    property DeleteSQL: TStrings read GetDeleteSQL write SetDeleteSQL;
    property InsertSQL: TStrings read GetInsertSQL write SetInsertSQL;
    property ModifySQL: TStrings read GetModifySQL write SetModifySQL;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
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


  { TODBCDataSet }

  TODBCDataSet = class(TODBCInternalDataSet)
  private
    function GetPrepared: Boolean;
    function GetQSelect: TODBCCustomStatement;
    function GetSelectSQL: TStrings;

    procedure SetSelectSQL(Value: TStrings);

  protected
    procedure SetFiltered(Value: Boolean); override;
    procedure InternalOpen; override;

  public
    procedure Prepare;
    procedure UnPrepare;

    property SQLParams;
    property Prepared: Boolean read GetPrepared;

    property QDelete;
    property QInsert;
    property QSelect: TODBCCustomStatement read GetQSelect;
    property QModify;

    property UpdatesPending;

  published
    property DataSource read GetDataSource write SetDataSource;

    property BufferChunks;
    property CachedUpdates;

    property DeleteSQL;
    property InsertSQL;
    property SelectSQL: TStrings read GetSelectSQL write SetSelectSQL;
    property ModifySQL;

    property UniDirectional;
  end;


  { TsvBlobStream }
  TsvBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TODBCCustomDataSet;
    FBuffer: TRecordBuffer;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    procedure ReadBlobData;
    procedure WriteBlobData;
  public
    constructor Create(AField: TBlobField; AMode: TBlobStreamMode);
    destructor Destroy; override;

    function Write(const ABuffer; ACount: Longint): Longint; override;
    procedure Truncate;
  end;

implementation
uses
{$IFDEF CLX}
  QForms,
  QControls,
{$ELSE}
  Forms,
  Controls,
{$ENDIF}
  DBCommon,

  Variants,
  FmtBcd,

  odbcsql,
  odbcsqlext,
  odbcsqlucode,

  ODBCIntf,
  ODBCUtils,
  ODBCConsts,

  ODBCQuery;


{ TODBCDataLink }

constructor TODBCDataLink.Create(ADataSet: TODBCCustomDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
end;

procedure TODBCDataLink.ActiveChanged;
begin
  if FDataSet.Active then
    FDataSet.RefreshParams;
end;

procedure TODBCDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) and FDataSet.Active then
    FDataSet.RefreshParams;
end;

function TODBCDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TODBCDataLink.CheckBrowseMode;
begin
  if FDataSet.Active then
    FDataSet.CheckBrowseMode;
end;


{ TODBCCustomDataSet }

constructor TODBCCustomDataSet.Create(AOwner: TComponent);
begin
  inherited;

  FDataLink := TODBCDataLink.Create(Self);

  FQStmt := GetStatementClass.Create(Self);
  FQStmt.Cursor.GoToFirstRecord := False;

  FCacheList := TList.Create;
  FUpdateHistory := TList.Create;

  FUpdateRecordTypes := [usUnmodified, usModified, usInserted];

  BookmarkSize := SizeOf(Integer);
  FCacheBlobs := True;

  FBufferChunks := BufferCacheSize;
end;

destructor TODBCCustomDataSet.Destroy;
begin
  FDataLink.Free;
  FQStmt.Free;

  FCacheList.Free;
  FUpdateHistory.Free;

  inherited;
end;

function TODBCCustomDataSet.GetHandle: SQLHSTMT;
begin
  Result := FQStmt.Handle;
end;

function TODBCCustomDataSet.GetParamCheck: Boolean;
begin
  Result := FQStmt.ParamCheck;
end;

function TODBCCustomDataSet.GetSQLParams: TODBCParams;
begin
  Result := FQStmt.Params;
end;

function TODBCCustomDataSet.GetStmtSQL: TStrings;
begin
  Result := FQStmt.SQL;
end;

procedure TODBCCustomDataSet.SetBufferChunks(Value: Integer);
begin
  CheckInactive;

  if Value < 1 then
    FBufferChunks := BufferCacheSize
  else
    FBufferChunks := Value;
end;

procedure TODBCCustomDataSet.SetStmtSQL(Value: TStrings);
begin
  CheckInactive;
  FQStmt.SQL.Assign(Value);
end;

procedure TODBCCustomDataSet.SetCacheBlobs(Value: Boolean);
begin
  CheckInactive;
  if FCacheBlobs <> Value then
    FCacheBlobs := Value;
end;

procedure TODBCCustomDataSet.SetCachedUpdates(Value: Boolean);
begin
  if (State = dsInActive) or (csDesigning in ComponentState) then
    FCachedUpdates := Value
  else
    if FCachedUpdates <> Value then
    begin
      CheckBrowseMode;
      UpdateCursorPos;

      if not Value and FUpdatesPending then
        CancelUpdates;

      FCachedUpdates := Value;
      Resync([]);
    end;
end;

procedure TODBCCustomDataSet.SetParamCheck(Value: Boolean);
begin
  if Value <> FQStmt.ParamCheck then
    FQStmt.ParamCheck := Value;
end;

procedure TODBCCustomDataSet.SetUniDirectional(Value: Boolean);
begin
  if FUniDirectional <> Value then
  begin
    CheckInactive;
    FUniDirectional := Value;
  end;
end;

procedure TODBCCustomDataSet.SetUpdateMode(const Value: TUpdateMode);
begin
  if not CanModify then
    DatabaseError(SCannotUpdate)
  else
    FUpdateMode := Value;
end;

procedure TODBCCustomDataSet.SetUpdateObject(Value: TODBCDataSetUpdateObject);
begin
  if Value <> FUpdateObject then
  begin
    if Assigned(FUpdateObject) and (FUpdateObject.DataSet = Self) then
      FUpdateObject.DataSet := nil;
    FUpdateObject := Value;
    if Assigned(FUpdateObject) then
    begin
      if Assigned(FUpdateObject.DataSet) and
        (FUpdateObject.DataSet <> Self) then
        FUpdateObject.DataSet.UpdateObject := nil;
      FUpdateObject.DataSet := Self;
    end;
  end;
end;

procedure TODBCCustomDataSet.SetUpdateRecordTypes(Value: TUpdateStatusSet);
begin
  FUpdateRecordTypes := Value;

  if Active then
    First;
end;

procedure TODBCCustomDataSet.FreeBlobFields(Buffer: TRecordBuffer);
var
  i: Integer;
begin
  if FBlobBufferSize > 0 then
    for i := 0 to FQStmt.Cursor.BlobColumnCount - 1 do
      SetLength(PBlobDataArray(Buffer + FBlobCacheOfs)^[i], 0);
end;

procedure TODBCCustomDataSet.FetchNext(Buffer: TRecordBuffer);
var
  Fld: TField;
  FieldDesc: TFieldDesc;
  StmtField: TODBCSQLVAR;
  FieldOffset: Cardinal;
  i: Integer;
begin
  FieldOffset := 0;

  { Make sure blob cache is empty }
  if FBlobBufferSize > 0 then
    FillChar(TRecordBuffer(Buffer + FBlobCacheOfs)^, FBlobBufferSize, 0);

  { Init RecInfo }
  with PRecInfo(Buffer)^ do
  begin
    PhyRecNum := FPhyRecCount {ARecNum};
    UpdateStatus := usUnmodified;
    BookmarkFlag := bfCurrent;
    // Bookmark := Stmt.Cursor.Columns[0].AsString;
  end;

  for i := 0 to FieldCount - 1 do
  begin
    Fld := Fields[i];
    if Fld.FieldKind <> fkData then
      Continue;

    StmtField := FQStmt.Cursor.Columns[Fld.FieldNo];
    FieldDesc := FMetaFields^[Fld.FieldNo];

    if StmtField.IsNull then
    begin
      Buffer[FieldDesc.NullFlagOfs] := FieldIsNull;
      if StmtField.IsBlob and FCacheBlobs then
        Inc(FieldOffset);
    end
    else
    begin
      Buffer[FieldDesc.NullFlagOfs] := FieldIsNotNull;

      case StmtField.SQLType of
        SQL_CHAR, SQL_VARCHAR,
          SQL_INTERVAL, SQL_INTERVAL_YEAR..SQL_INTERVAL_MINUTE_TO_SECOND:
          begin
            Move(StmtField.Data^, Buffer[FieldDesc.DataOfs], StmtField.DataLength);
          end;

        SQL_WCHAR, SQL_WVARCHAR:
          Move(StmtField.Data^, Buffer[FieldDesc.DataOfs], StmtField.DataLength);

        SQL_SMALLINT, SQL_TINYINT,
          SQL_INTEGER,
          SQL_DOUBLE, SQL_FLOAT, SQL_REAL,
          SQL_BIGINT,
          SQL_BIT:
          Move(StmtField.Data^, Buffer[FieldDesc.DataOfs], StmtField.DataLength {FieldDesc^.DataSize});

        SQL_DECIMAL, SQL_NUMERIC:
          PBcd(@Buffer[FieldDesc.DataOfs])^ := StmtField.AsBcd;

        SQL_TYPE_DATE:
          PSQLINTEGER(@Buffer[FieldDesc.DataOfs])^ := DateTimeToTimeStamp(StmtField.AsDateTime).Date;

        SQL_TYPE_TIME:
          PSQLINTEGER(@Buffer[FieldDesc.DataOfs])^ := DateTimeToTimeStamp(StmtField.AsDateTime).Time;

        SQL_TYPE_TIMESTAMP:
          PSQLDOUBLE(@Buffer[FieldDesc.DataOfs])^ := TimeStampToMSecs(DateTimeToTimeStamp(StmtField.AsDateTime));

        SQL_BINARY, SQL_VARBINARY, SQL_LONGVARBINARY,
        SQL_LONGVARCHAR, SQL_WLONGVARCHAR:
          if FCacheBlobs then
          begin
            SetLength(PBlobDataArray(Buffer + FBlobCacheOfs)^[FieldOffset], StmtField.DataLength);
            Move(StmtField.Data^, PByte(PBlobDataArray(Buffer + FBlobCacheOfs)^[FieldOffset])^, StmtField.DataLength);
            Inc(FieldOffset);
          end;

        SQL_GUID:
          Move(PByte(StmtField.Data)^, Buffer[FieldDesc.DataOfs], StmtField.DataLength);
      else
        DatabaseErrorFmt(SUnsupportedDataType, [StmtField.Name]);
      end;
    end;
  end;

  FCacheList.Add(WriteBufferToCache(FPhyRecCount, Buffer));
  Inc(FPhyRecCount);
end;

function TODBCCustomDataSet.WriteBufferToCache(ARecNum: Integer; Buffer: TRecordBuffer): TRecordBuffer;
var
  Index: Integer;
begin
  if FUniDirectional then
    ARecNum := ARecNum mod UniCache;

  Index := ARecNum div FBufferChunks;
  if Index = FChunkCount then
  begin
    Inc(FChunkCount);
    SetLength(FChunks, FChunkCount);
    GetMem(FChunks[Index], FChunkSize);
    FCurrentChunk := FChunks[Index];
  end;

  Result := FCurrentChunk + (ARecNum mod FBufferChunks) * FRecBufSize;
  Move(Buffer^, Result^, FRecBufSize);
end;

procedure TODBCCustomDataSet.CopyBuffer(Source: TRecordBuffer; Dest: TRecordBuffer);
var
  i: Integer;
begin
  FreeBlobFields(Dest);

  Move(Source^, Dest^, FRecBufSize);
  FillChar(Dest[FBlobCacheOfs], FBlobBufferSize, 0);

  // copy blob fields
  for i := 0 to FQStmt.Cursor.BlobColumnCount - 1 do
    PBlobDataArray(Dest + FBlobCacheOfs)^[i] := PBlobDataArray(Source + FBlobCacheOfs)^[i];
end;

procedure TODBCCustomDataSet.SaveToOldCache(Buffer: TRecordBuffer);
var
  OldCacheOfs: Cardinal;
begin
  if PRecInfo(Buffer)^.UpdateStatus = usUnmodified then
  begin
    OldCacheOfs := FOldRecordCount * FRecBufSize;

    if OldCacheOfs = FOldCacheSize then
    begin
      Inc(FOldCacheSize, FChunkSize);
      ReallocMem(FOldCache, FOldCacheSize);
    end;

    Move(Buffer^, FOldCache[OldCacheOfs], FRecBufSize);
    Inc(FOldRecordCount);
  end;
end;

function TODBCCustomDataSet.GetCacheBuffer(Buffer: TRecordBuffer): TRecordBuffer;
begin
  Result := GetCacheBufferA(PRecInfo(Buffer)^.PhyRecNum);
end;

function TODBCCustomDataSet.GetCacheBufferA(ARecNum: Integer): TRecordBuffer;
var
  i: Integer;
begin
  i := 0;
  while (i < FCacheList.Count) and (PRecInfo(FCacheList[i])^.PhyRecNum <> ARecNum) do
    Inc(i);

  if i < FCacheList.Count then
    Result := FCacheList[i]
  else
    Result := nil;
end;

function TODBCCustomDataSet.GetCacheOldBuffer(ARecNum: Integer): TRecordBuffer;
var
  Size: Integer;
  Offset: Integer;
begin
  Size := FOldRecordCount * FRecBufSize;
  Offset := 0;

  while (Offset < Size) and (PRecInfo(FOldCache + Offset)^.PhyRecNum <> ARecNum) do
    Inc(Offset, FRecBufSize);

  if Offset < Size then
    Result := FOldCache + Offset
  else
    Result := nil;
end;

function TODBCCustomDataSet.AdjustCurrentRecord(Buffer: TRecordBuffer; GetMode: TGetMode): TGetResult;
begin
  Result := grOK;

  while not IsVisible(Buffer) do
    if GetMode = gmPrior then
    begin
      Dec(FRecordPos);
      if FRecordPos >= 0 then
        Move(FCacheList[FRecordPos]^, Buffer^, FRecBufSize)
      else
      begin
        Result := grBOF;
        Break;
      end;
    end
    else
    begin
      Inc(FRecordPos);
      if FRecordPos < FCacheList.Count then
        Move(FCacheList[FRecordPos]^, Buffer^, FRecBufSize)
      else
      begin
        FQStmt.Cursor.Next;
        if FQStmt.Cursor.EOF then
        begin
          Result := grEOF;
          Break;
        end
        else
          FetchNext(Buffer);
      end
    end;
end;

function TODBCCustomDataSet.IsVisible(Buffer: TRecordBuffer): Boolean;
begin
  Result := True;
  if not (State = dsOldValue) then
    Result := (PRecInfo(Buffer)^.UpdateStatus in FUpdateRecordTypes);
end;

procedure TODBCCustomDataSet.RefreshParams;
var
  DataSet: TDataSet;
begin
  DisableControls;
  try
    if FDataLink.DataSource <> nil then
    begin
      DataSet := FDataLink.DataSource.DataSet;
      if DataSet <> nil then
        if DataSet.Active and (DataSet.State <> dsSetKey) then
        begin
          Close;
          Open;
        end;
    end;
  finally
    EnableControls;
  end;
end;

procedure TODBCCustomDataSet.GetBlobData(Field: TField; Buffer: TRecordBuffer; out Data: Pointer; out DataLength: Integer);
begin
  Data := PBlobDataArray(Buffer + FBlobCacheOfs)^[Field.Offset];
  DataLength := Length(PBlobDataArray(Buffer + FBlobCacheOfs)^[Field.Offset]);
end;

procedure TODBCCustomDataSet.SetBlobData(Field: TField; Buffer: TRecordBuffer; Data: Pointer; DataLength: Integer);
var
  BlobData: TBytes;
begin
  if Buffer <> ActiveBuffer then Exit;

  BlobData := PBlobDataArray(Buffer + FBlobCacheOfs)^[Field.Offset];

  SetLength(BlobData, 0);               // clear old blobdata
  SetLength(BlobData, DataLength);      // get new memory
  Move(Data^, PByte(BlobData)^, DataLength);

  PBlobDataArray(Buffer + FBlobCacheOfs)^[Field.Offset] := BlobData;
end;

procedure TODBCCustomDataSet.AddToUpdateHistory(Buffer: TRecordBuffer);
begin
  if FUpdateHistory.IndexOf(Buffer) = -1 then
    FUpdateHistory.Add(Buffer);
end;

procedure TODBCCustomDataSet.RemoveFromUpdateHistory(Buffer: TRecordBuffer);
begin
  FUpdateHistory.Remove(Buffer);
end;

procedure TODBCCustomDataSet.CheckBiDirectional;
begin
  if FUniDirectional then
    DatabaseError(SDataSetUnidirectional);
end;

procedure TODBCCustomDataSet.CheckEditState;
begin
  case State of
    dsEdit: CheckModifyOperation;
    dsInsert: CheckInsertOperation;
  end;
end;

procedure TODBCCustomDataSet.CheckCachedUpdateMode;
begin
  if not FCachedUpdates then
    DatabaseError(SNoCachedUpdates);
end;

procedure TODBCCustomDataSet.CheckDeleteOperation;
begin
  if not (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukDelete).Text <> '')) then
    DatabaseError(SCannotDelete);
end;

procedure TODBCCustomDataSet.CheckInsertOperation;
begin
  if not (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukInsert).Text <> '')) then
    DatabaseError(SCannotInsert);
end;

procedure TODBCCustomDataSet.CheckModifyOperation;
begin
  if not (Assigned(FUpdateObject) and (FUpdateObject.GetSQL(ukModify).Text <> '')) then
    DatabaseError(SCannotUpdate);
end;

procedure TODBCCustomDataSet.InitRecordMetaData;
var
  Fld: TField;
  Ofs: Cardinal;
  i: Integer;
begin
  FMetaFields := AllocMem(FieldDefs.Count * SizeOf(TFieldDesc));
  Ofs := SizeOf(TRecInfo);

  for i := 0 to FieldCount - 1 do
  begin
    Fld := Fields[i];

    if Fld.FieldKind = fkData then
      with FMetaFields^[Fld.FieldNo] do
      begin
        DataSize := Fld.DataSize;

        NullFlagOfs := Ofs;
        DataOfs := Ofs + SizeOfNullFlag;

        Inc(Ofs, DataSize + SizeOfNullFlag);
      end;
  end;
end;

function TODBCCustomDataSet.GetRecordDataSize: Integer;
var
  Fld: TField;
  i: Integer;
begin
  Result := 0;

  for i := 0 to FieldCount - 1 do
  begin
    Fld := Fields[i];
    if Fld.FieldKind = fkData then
      Inc(Result, Fld.DataSize + SizeOfNullFlag);
  end;
end;

procedure TODBCCustomDataSet.FreeCache;
var
  i: Integer;
begin
  // free blob fields
  if FBlobBufferSize > 0 then
    for i := 0 to FCacheList.Count - 1 do
      FreeBlobFields(FCacheList[i]);

  // free cache chunks
  for i := 0 to Length(FChunks) - 1 do
    FreeMem(FChunks[i], FChunkSize);

  FChunks := nil;
  FChunkCount := 0;
end;

procedure TODBCCustomDataSet.FreeOldCache;
var
  i: Integer;
begin
  // free blob fields
  if FBlobBufferSize > 0 then
    for i := 0 to FOldRecordCount - 1 do
    begin
      FreeBlobFields(FOldCache);
      Inc(FOldCache, FRecBufSize);
    end;

  // free old cache page
  if FOldCacheSize > 0 then
    FreeMem(FOldCache, FOldCacheSize);

  FOldCache := nil;
  FOldCacheSize := 0;
  FOldRecordCount := 0;
end;

function TODBCCustomDataSet.GetStatementClass: TODBCCustomStatementClass;
begin
  Result := TODBCStatement;
end;

function TODBCCustomDataSet.GetFieldDefSize(FT: TFieldType; ColSize: Integer; Len: Integer; DecDigits: SmallInt): Integer;
begin
  case FT of
    ftString, ftFixedChar:
      Result := ColSize;

    ftWideString, ftFixedWideChar:
      Result := ColSize;

    ftBCD, ftFmtBCD:
      Result := DecDigits;

    ftBytes, ftVarBytes:
      Result := Len;

    ftBlob, ftGraphic, ftMemo, ftWideMemo:
      Result := Len;

    ftGuid:
      Result := 38;
  else
    Result := 0;
  end;
end;

function TODBCCustomDataSet.GetActiveBuffer: TRecordBuffer;
begin
  Result := nil;

  if FOpen then
    case State of
      dsBrowse:
        if IsEmpty then
          Result := nil
        else
          Result := ActiveBuffer;

      dsCalcFields:
        Result := CalcBuffer;

      dsFilter:
        Result := FFilterBuffer;

      dsOldValue:
        begin
          if PRecInfo(ActiveBuffer)^.PhyRecNum = PRecInfo(FOldBuffer)^.PhyRecNum then
            Result := FOldBuffer
          else
            Result := GetCacheOldBuffer(PRecInfo(ActiveBuffer)^.PhyRecNum);

          if Result = nil then
            Result := ActiveBuffer;
        end;
      dsInactive:
        Result := nil;
    else
      Result := ActiveBuffer;
    end;
end;

function TODBCCustomDataSet.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TODBCCustomDataSet.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink);

  if FDataLink.DataSource <> Value then
    FDataLink.DataSource := Value;
end;

procedure TODBCCustomDataSet.ConnectionStateChange(Sender: TObject; Connecting: Boolean);
begin
  if not Connecting and Active then
    Close;
end;

function TODBCCustomDataSet.ConstraintsStored: Boolean;
begin
  Result := Constraints.Count > 0;
end;

procedure TODBCCustomDataSet.ApplyToDBMS(UpdateKind: TUpdateKind);
var
  TmpBuff: TRecordBuffer;
begin
  TmpBuff := GetActiveBuffer;

  with PRecInfo(TmpBuff)^ do
    case UpdateKind of
      ukDelete:
        begin
          DoDelete;

          FreeBlobFields(FCacheList[FRecordPos]);
          FCacheList.Delete(FRecordPos);
        end;

      ukInsert:
        begin
          DoInsert;

          UpdateStatus := usUnmodified;
          PhyRecNum := FPhyRecCount;
          BookmarkFlag := bfCurrent;

          FCacheList.Insert(FRecordPos, WriteBufferToCache(FPhyRecCount, TmpBuff));

          SetModified(False);
        end;

      ukModify:
        begin
          DoModify;

          UpdateStatus := usUnmodified;

          Move(TmpBuff^, FCacheList[FRecordPos]^, FRecBufSize);
          CopyBuffer(TmpBuff, FOldBuffer);

          SetModified(False);
        end;
    end;
end;

procedure TODBCCustomDataSet.ApplyToCache(UpdateKind: TUpdateKind);
var
  Buff: TRecordBuffer;
begin
  Buff := GetActiveBuffer;

  with PRecInfo(Buff)^ do
    case UpdateKind of
      ukDelete:
        if UpdateStatus = usInserted then
        begin
          RemoveFromUpdateHistory(FCacheList[FRecordPos]);

          FreeBlobFields(FCacheList[FRecordPos]);
          FCacheList.Delete(FRecordPos);
        end
        else
          if UpdateStatus <> usDeleted then
          begin
            if UpdateStatus = usUnmodified then
              AddToUpdateHistory(FCacheList[FRecordPos]);

            UpdateStatus := usDeleted;
            Move(Buff^, FCacheList[FRecordPos]^, FRecBufSize);
            Inc(FDeletedRecordCount);
          end;

      ukInsert:
        begin
          UpdateStatus := usInserted;
          PhyRecNum := FPhyRecCount;
          BookmarkFlag := bfInserted;

          FCacheList.Insert(FRecordPos, WriteBufferToCache(PhyRecNum, Buff));
          AddToUpdateHistory(FCacheList[FRecordPos]);
        end;

      ukModify:
        begin
          case UpdateStatus of
            usDeleted:
              DatabaseError(SCannotModifyDelRec);

            usUnmodified:
              begin
                UpdateStatus := usModified;
                AddToUpdateHistory(FCacheList[FRecordPos]);
              end;
          end;
          Move(Buff^, FCacheList[FRecordPos]^, FRecBufSize);
        end;
    end;
  FUpdatesPending := True;
end;

procedure TODBCCustomDataSet.ApplyCachedToDBMS(UpdateKind: TUpdateKind; Buffer: Pointer);
begin
  with PRecInfo(Buffer)^ do
    case UpdateKind of
      ukDelete:
        begin
          DoDelete;

          FreeBlobFields(Buffer);
          FCacheList.Remove(Buffer);
        end;

      ukInsert:
        begin
          DoInsert;

          UpdateStatus := usUnmodified;
          BookmarkFlag := bfCurrent;
          SetModified(False);
        end;

      ukModify:
        begin
          DoModify;

          UpdateStatus := usUnmodified;
          SetModified(False);
        end;
    end;
end;

procedure TODBCCustomDataSet.ApplyCachedUpdate(Buffer: TRecordBuffer; var UpdateAction: TUpdateAction);
var
  UpdateKind: TUpdateKind;
begin
  // defining of UpdateKind
  case PRecInfo(Buffer)^.UpdateStatus of
    usDeleted: UpdateKind := ukDelete;
    usInserted: UpdateKind := ukInsert;
  else
    UpdateKind := ukModify;
  end;

  Move(Buffer^, ActiveBuffer^, FRecBufSize);

  if Assigned(FOnUpdateRecord) then
  try
    FOnUpdateRecord(Self, UpdateKind, UpdateAction);
  except
    on E: Exception do
      if E is EODBCError then
      begin
        DoUpdateError(EODBCError(E), UpdateKind, UpdateAction);

        if UpdateAction = uaFail then
          raise;
      end;
  end
  else
    if Assigned(FUpdateObject) then
    try
      FUpdateObject.Apply(UpdateKind);
      UpdateAction := uaApplied;
    except
      on E: EODBCError do
        DoUpdateError(EODBCError(E), UpdateKind, UpdateAction);
    end;
end;

procedure TODBCCustomDataSet.RevertCachedUpdate(Buffer: TRecordBuffer);
var
  OldBuff: TRecordBuffer;
begin
  RemoveFromUpdateHistory(Buffer);

  with PRecInfo(Buffer)^ do
  begin
    OldBuff := GetCacheOldBuffer(PhyRecNum);

    case UpdateStatus of
      usDeleted:
        if OldBuff = nil then
        begin
          UpdateStatus := usUnmodified;
          Dec(FDeletedRecordCount);
        end
        else
          CopyBuffer(OldBuff, Buffer);

      usInserted:
        begin
          FreeBlobFields(Buffer);
          FCacheList.Remove(Buffer);
        end;

      usModified:
        CopyBuffer(OldBuff, Buffer);
    end;
  end;
end;

procedure TODBCCustomDataSet.DoDelete;
begin
  FUpdateObject.Apply(ukDelete);
end;

procedure TODBCCustomDataSet.DoInsert;
begin
  FUpdateObject.Apply(ukInsert);
end;

procedure TODBCCustomDataSet.DoModify;
begin
  FUpdateObject.Apply(ukModify);
end;

procedure TODBCCustomDataSet.DoUpdateError(E: EODBCError; UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  if Assigned(FOnUpdateError) then
    FOnUpdateError(Self, E, UpdateKind, UpdateAction);
end;

procedure TODBCCustomDataSet.InternalPrepare;
begin
  if not FQStmt.Prepared then
  begin
    FQStmt.ParamCheck := ParamCheck;
    FQStmt.Prepare;
  end;
end;

procedure TODBCCustomDataSet.InternalUnPrepare;
begin
  CheckInactive;

  if Handle <> nil then
    FQStmt.UnPrepare;

  FieldDefs.Clear;
end;

procedure TODBCCustomDataSet.InternalExecute;
begin
  FQStmt.Execute;
end;

function TODBCCustomDataSet.LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean): Boolean;
var
  FieldList: TList;
  FieldListCount: Integer;
  FieldValue: Variant;
  TmpValue: Variant;
  i: Integer;

  Save_Bookmark: TBookmark;
begin
  CheckBrowseMode;
  CursorPosChanged;

  Result := False;
  Save_Bookmark := Bookmark;

  FieldList := TList.Create;
  DisableControls;
  try
    GetFieldList(FieldList, KeyFields);
    FieldListCount := FieldList.Count;

    First;
    while not EOF do
    begin
      i := 0;
      Result := True;

      while Result and (i < FieldListCount) do
      begin
        // get current field value
        FieldValue := TField(FieldList[i]).Value;

        // get search value
        if FieldListCount > 1 then
          TmpValue := KeyValues[i]
        else
          if VarIsArray(KeyValues) then
            TmpValue := KeyValues[0]
          else
            TmpValue := KeyValues;

        // value is not null
        Result := not (VarIsNull(TmpValue) or VarIsNull(FieldValue));

        // convert FieldValue to specified type
        if Result then
        try
          FieldValue := VarAsType(FieldValue, VarType(TmpValue));
        except
          on E: EVariantError do
            Result := False;
        end;

        // compare values
        if Result then
        begin
          if TField(FieldList[i]).DataType in [ftString, ftFixedChar, ftWideString] then
          begin
            FieldValue := TrimRight(FieldValue);
            TmpValue := TrimRight(TmpValue);

            if loCaseInsensitive in Options then
            begin
              FieldValue := AnsiUpperCase(FieldValue);
              TmpValue := AnsiUpperCase(TmpValue);
            end;

            if loPartialKey in Options then
              Result := AnsiPos(TmpValue, FieldValue) = 1
            else
              Result := TmpValue = FieldValue;
          end
          else
            Result := TmpValue = FieldValue;
        end;

        Inc(i);
      end;

      // if record was found then break cycle loop
      if Result then Break;

      // go to next record
      Next;
    end;
    UpdateCursorPos;
  finally
    EnableControls;
    FieldList.Free;
  end;

  if (not Result) and SyncCursor then
    Bookmark := Save_Bookmark;
end;

procedure TODBCCustomDataSet.ReQuery;
begin
  FUpdateHistory.Clear;

  FreeCache;
  FreeOldCache;

  FCacheList.Clear;

  FRecordPos := -1;
  FPhyRecCount := 0;
  FDeletedRecordCount := 0;

  // close
  FQStmt.Close;
  FOpen := False;

  // open
  FQStmt.Execute;
  FOpen := FQStmt.IsCursorOpen;

  First;
end;


{ IProviderSupport }

function TODBCCustomDataSet.PSGetTableName: string;
begin
  Result := GetTableNameFromSQL(QStmt.SQL.Text);
end;

function TODBCCustomDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
begin
  if not Assigned(E) then
    E := EDatabaseError.Create(SErrorMappingError);

  Result := inherited PSGetUpdateException(E, Prev);
end;

function TODBCCustomDataSet.PSIsSQLBased: Boolean;
begin
  Result := True;
end;

function TODBCCustomDataSet.PSIsSQLSupported: Boolean;
begin
  Result := True;
end;

procedure TODBCCustomDataSet.PSReset;
begin
  inherited PSReset;
  if Active then
  begin
    Close;
    Open;
  end;
end;

function TODBCCustomDataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
var
  UpdateAction: TUpdateAction;
begin
  Result := False;

  if Assigned(FOnUpdateRecord) then
  begin
    UpdateAction := uaFail;
    FOnUpdateRecord(Delta, UpdateKind, UpdateAction);
    Result := UpdateAction = uaApplied;
  end;
end;

function TODBCCustomDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := AllocMem(FRecBufSize);
end;

procedure TODBCCustomDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
end;

procedure TODBCCustomDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillChar(Buffer^, FRecBufSize, 0);
  with PRecInfo(Buffer)^ do
  begin
    UpdateStatus := usInserted;
    PhyRecNum := -1;
    BookmarkFlag := bfInserted;
  end;
end;

function TODBCCustomDataSet.InternalGetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grOK;

  case GetMode of
    gmCurrent:
      if FRecordPos >= 0 then
      begin
        if FRecordPos < FCacheList.Count then
          Move(FCacheList[FRecordPos]^, Buffer^, FRecBufSize)
        else
        begin
          while (not FQStmt.Cursor.EOF) and (FRecordPos >= FCacheList.Count) do
          begin
            FQStmt.Cursor.Next;

            if FQStmt.Cursor.EOF then
            begin
              Result := grEOF;
              Break;
            end
            else
              FetchNext(Buffer);
          end;

          if FQStmt.Cursor.EOF then
            Result := grEOF;

          if Result <> grEOF then
          begin
            FRecordPos := FCacheList.Count - 1;
            Move(FCacheList[FRecordPos]^, Buffer^, FRecBufSize)
          end;
        end;
      end
      else
        Result := grBOF;

    gmNext:
      if FRecordPos = FCacheList.Count - 1 then
      begin
        if not FQStmt.Cursor.EOF then
        begin
          FQStmt.Cursor.Next;
          Inc(FRecordPos);
        end;
        if FQStmt.Cursor.EOF then
          Result := grEOF
        else
          FetchNext(Buffer);
      end
      else
        if FRecordPos < FCacheList.Count then
        begin
          Inc(FRecordPos);
          Move(FCacheList[FRecordPos]^, Buffer^, FRecBufSize);
        end
        else
          Result := grEOF;

    gmPrior:
      begin
        CheckBiDirectional;

        if FRecordPos > 0 then
        begin
          Dec(FRecordPos);
          Move(FCacheList[FRecordPos]^, Buffer^, FRecBufSize);
        end
        else
        begin
          FRecordPos := -1;
          Result := grBOF;
        end;
      end;
  end;

  if Result = grOk then
    Result := AdjustCurrentRecord(Buffer, GetMode);

  with PRecInfo(Buffer)^ do
    case Result of
      grOk:
        begin
          BookmarkFlag := bfCurrent;
          GetCalcFields(Buffer);
        end;
      grEOF:
        BookmarkFlag := bfEOF;

      grBOF:
        BookmarkFlag := bfBOF;
    end;
end;

function TODBCCustomDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
  SaveState: TDataSetState;
begin
  if Filtered and Assigned(OnFilterRecord) then
  begin
    FFilterBuffer := Buffer;
    SaveState := SetTempState(dsFilter);

    Accept := True;
    repeat
      Result := InternalGetRecord(Buffer, GetMode, DoCheck);
      if Result = grOK then
      begin
        OnFilterRecord(Self, Accept);
        if not Accept and (GetMode = gmCurrent) then
          Result := grError;
      end;
    until Accept or (Result <> grOK);

    RestoreState(SaveState);
  end
  else
    Result := InternalGetRecord(Buffer, GetMode, DoCheck);
end;

function TODBCCustomDataSet.GetRecordSize: Word;
begin
  Result := FRecBufSize;
end;

procedure TODBCCustomDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  Buff, TmpBuff: TRecordBuffer;
begin
  Buff := GetActiveBuffer;

  if Field.FieldKind = fkData then
  begin
    CheckEditState;

    Field.Validate(Buffer);

    if Buffer = nil then
      Buff[FMetaFields^[Field.FieldNo].NullFlagOfs] := FieldIsNull
    else
    begin
      Buff[FMetaFields^[Field.FieldNo].NullFlagOfs] := FieldIsNotNull;
      Move(Buffer^, Buff[FMetaFields^[Field.FieldNo].DataOfs], FMetaFields^[Field.FieldNo].DataSize);
    end;
    SetModified(True);
  end
  else
  begin
    // calculated fields
    TmpBuff := Buff + FCalcFieldsOfs + Field.Offset;
    Boolean(TmpBuff[0]) := LongBool(Buffer);

    if Boolean(TmpBuff[0]) then
      Move(Buffer^, TmpBuff[1], Field.DataSize);
  end;

  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, {$IF RTLVersion >= 16.0}NativeInt{$ELSE}Longint{$IFEND}(Field));
end;

function TODBCCustomDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer)^.BookmarkFlag;
end;

procedure TODBCCustomDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TODBCCustomDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PSQLINTEGER(Data)^ := PRecInfo(Buffer)^.PhyRecNum;
end;

procedure TODBCCustomDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  // empty implementation
end;

procedure TODBCCustomDataSet.InternalGotoBookmark(Bookmark: Pointer);
var
  i: Integer;
begin
  if LongInt(Bookmark^) < 0 then Exit;

  i := 0;
  while i < FCacheList.Count do
  begin
    if PRecInfo(FCacheList[i])^.PhyRecNum = LongInt(Bookmark^) then
    begin
      FRecordPos := i;
      Break;
    end;
    Inc(i);
  end;

  if i = FCacheList.Count then
    DatabaseError(SBookmarkNotFound);
end;

procedure TODBCCustomDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  InternalGotoBookmark(@(PRecInfo(Buffer)^.PhyRecNum));
end;

procedure TODBCCustomDataSet.InternalFirst;
begin
  FRecordPos := -1;
end;

procedure TODBCCustomDataSet.InternalLast;
var
  Buffer: TRecordBuffer;
begin
  if FQStmt.Cursor.EOF then
    FRecordPos := FCacheList.Count
  else
  begin
    Buffer := AllocRecordBuffer;
    try
      FQStmt.Cursor.Next;
      while not FQStmt.Cursor.EOF do
      begin
        FetchNext(Buffer);
        FQStmt.Cursor.Next;
      end;
      FRecordPos := FCacheList.Count;
    finally
      FreeRecordBuffer(Buffer);
    end;
  end;
end;

function TODBCCustomDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;
begin
  raise Exception.Create(SUnsupportedFeature);
end;

procedure TODBCCustomDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  CheckEditState;
  CursorPosChanged;

  if Append then InternalLast;

  Move(Buffer^, ActiveBuffer^, FRecBufSize);
  InternalPost;
end;

procedure TODBCCustomDataSet.InternalCancel;
var
  Buff: TRecordBuffer;
begin
  Buff := GetActiveBuffer;

  if (Buff <> nil) and (State = dsEdit) then
  begin
    CopyBuffer(FOldBuffer, Buff);
    WriteBufferToCache(PRecInfo(Buff)^.PhyRecNum, Buff);
  end;
end;

procedure TODBCCustomDataSet.InternalEdit;
begin
  CopyBuffer(GetActiveBuffer, FOldBuffer);
end;

procedure TODBCCustomDataSet.InternalDelete;
begin
  CheckDeleteOperation;

  if FCachedUpdates then
    ApplyToCache(ukDelete)
  else
    ApplyToDBMS(ukDelete);
end;

procedure TODBCCustomDataSet.InternalInsert;
begin
  CursorPosChanged;
end;

procedure TODBCCustomDataSet.InternalPost;
begin
  CheckEditState;

  case State of
    dsInsert:
      begin
        if FRecordPos < 0 then
          FRecordPos := 0;

        if FCachedUpdates then
          ApplyToCache(ukInsert)
        else
          ApplyToDBMS(ukInsert);

        Inc(FPhyRecCount);
      end;

    dsEdit:
      begin
        if FCachedUpdates then
        begin
          SaveToOldCache(FOldBuffer);
          ApplyToCache(ukModify);
        end
        else
          ApplyToDBMS(ukModify);
      end;
  end;
end;

procedure TODBCCustomDataSet.InternalOpen;
begin
  // initial settings
  FRecordPos := -1;
  FDeletedRecordCount := 0;
  FPhyRecCount := 0;
  FOldRecordCount := 0;

  FChunkCount := 0;
  FOldCacheSize := 0;

  // stmt execute
  InternalExecute;

  FOpen := FQStmt.IsCursorOpen;
  if not FOpen then
    Abort;

  // update FieldDefs
  FieldDefs.Updated := False;
  FieldDefs.Update;

  // create and bind Fields
  if DefaultFields then
    CreateFields;
  BindFields(True);

  // init Field's offsets
  InitRecordMetaData;

  // Set up the FCalcFieldsOfs, FBlobCacheOfs and FRecBufSize.
  FCalcFieldsOfs := SizeOf(TRecInfo) + GetRecordDataSize;
  FBlobCacheOfs := FCalcFieldsOfs + CalcFieldsSize;
  if FCacheBlobs then
    FBlobBufferSize := {BlobFieldCount} FQStmt.Cursor.BlobColumnCount * SizeOf(TBlobData)
  else
    FBlobBufferSize := 0;

  FRecBufSize := FBlobCacheOfs + FBlobBufferSize;

  // allocate memory for chunks of records
  if UniDirectional then
    FChunkSize := FRecBufSize * UniCache
  else
  begin
    FChunkSize := FRecBufSize * FBufferChunks;
    FCacheList.Capacity := FBufferChunks;
  end;

  // allocate memory for old buffer
  FOldBuffer := AllocRecordBuffer;
  InitRecord(FOldBuffer);
end;

procedure TODBCCustomDataSet.InternalClose;
begin
  BindFields(False);
  if DefaultFields then
    DestroyFields;

  FreeCache;
  FreeOldCache;

  FUpdateHistory.Clear;
  FCacheList.Clear;

  FreeRecordBuffer(FOldBuffer);
  FOldBuffer := nil;

  FRecordPos := -1;
  FPhyRecCount := 0;
  FDeletedRecordCount := 0;

  FreeMem(FMetaFields);
  FMetaFields := nil;

  FQStmt.Close;
  FOpen := False;
end;

procedure TODBCCustomDataSet.InternalRefresh;
var
  KeyFields: string;
  KeyValues: Variant;

  Save_Bookmark: TBookmark;
begin
  DisableControls;
  try
    KeyFields := PSGetKeyFields;
    if KeyFields <> '' then
    begin
      KeyValues := FieldValues[KeyFields];
      ReQuery;
      LocateRecord(KeyFields, KeyValues, [], True);
    end
    else
    begin
      Save_Bookmark := Bookmark;
      ReQuery;
      try
        Bookmark := Save_Bookmark;
      except
      end;
    end;

    UpdateCursorPos;
  finally
    EnableControls;
  end;
end;

procedure TODBCCustomDataSet.InternalInitFieldDefs;
var
  FieldDef: TFieldDef;
  StmtField: TODBCColumn;
  i: Integer;

  function ValidateFieldName(const FieldName: string): string;
  var
    i: Integer;
  begin
    Result := FieldName;
    i := 0;
    while (Result = '') or (FieldDefs.IndexOf(Result) > -1) do
    begin
      Inc(i);
      if FieldName = '' then
        Result := Format('%s%d', [S_COLUMN, i])
      else
        Result := Format('%s_%d', [FieldName, i]);
    end;
  end;

begin
  FieldDefs.BeginUpdate;
  try
    FieldDefs.Clear;

    for i := 1 to FQStmt.Cursor.Columns.Count - 1 do // ignore Column[0] - reserved for ODBC bookmarks
    begin
      StmtField := FQStmt.Cursor.Columns[i];

      FieldDef := FieldDefs.AddFieldDef;
      FieldDef.Name := ValidateFieldName(StmtField.Name);

      if StmtField.IsAutoInc then
        FieldDef.DataType := ftAutoInc
      else
        FieldDef.DataType := SQLTypeToFieldType(StmtField.SQLType);

      FieldDef.Size := GetFieldDefSize(FieldDef.DataType, StmtField.ColumnSize, StmtField.DataLength, StmtField.DecimalDigits);
      FieldDef.Precision := StmtField.ColumnSize;
      FieldDef.Required := not StmtField.Nullable;
    end;
  finally
    FieldDefs.EndUpdate;
  end;
end;

procedure TODBCCustomDataSet.InternalHandleException;
begin
  Application.HandleException(Self);
end;

function TODBCCustomDataSet.IsCursorOpen: Boolean;
begin
  Result := FOpen;
end;

procedure TODBCCustomDataSet.OpenCursor(InfoQuery: Boolean);
begin
  try
    inherited OpenCursor(False);
  except
    on EAbort do
    begin
      // do nothing
    end;
  end;
end;

function TODBCCustomDataSet.GetRecordCount: Integer;
begin
  CheckActive;
  Result := FCacheList.Count - FDeletedRecordCount;
end;

function TODBCCustomDataSet.GetRecNo: Integer;
var
  Buff: TRecordBuffer;
begin
  Result := inherited GetRecNo;

  Buff := GetCacheBuffer(GetActiveBuffer);
  if Buff <> nil then
    Result := FCacheList.IndexOf(Buff);
end;

procedure TODBCCustomDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;

  if Value < 1 then
    Value := 1
  else
    if Value > FCacheList.Count then
    begin
      InternalLast;
      Value := Min(FCacheList.Count, Value);
    end;

  if Value <> RecNo then
  begin
    DoBeforeScroll;
    FRecordPos := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TODBCCustomDataSet.ClearCalcFields(Buffer: TRecordBuffer);
begin
  FillChar(Buffer[FCalcFieldsOfs], CalcFieldsSize, 0);
end;

function TODBCCustomDataSet.GetCanModify: Boolean;
begin
  Result := Assigned(FUpdateObject);

  if FCachedUpdates then
    Result := Result or Assigned(FOnUpdateRecord);
end;

function TODBCCustomDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2, -1), (1, 0));
begin
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];

  if Result = 2 then
  begin
    if PSQLINTEGER(Bookmark1)^ < PSQLINTEGER(Bookmark2)^ then
      Result := -1
    else
      if PSQLINTEGER(Bookmark1)^ > PSQLINTEGER(Bookmark2)^ then
        Result := 1
      else
        Result := 0;
  end;
end;

function TODBCCustomDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TsvBlobStream.Create(Field as TBlobField, Mode);
end;

function TODBCCustomDataSet.GetCurrentRecord(Buffer: TRecordBuffer): Boolean;
begin
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then
  begin
    UpdateCursorPos;
    CopyBuffer(GetCacheBuffer(ActiveBuffer), Buffer);
    Result := True;
  end
  else
    Result := False;
end;

function TODBCCustomDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  Buff, Data: TRecordBuffer;
begin
  Result := False;
  Buff := GetActiveBuffer;

  if Buff <> nil then
  begin
    if Field.FieldKind = fkData then
    begin
      Result := Buff[FMetaFields^[Field.FieldNo].NullFlagOfs] <> FieldIsNull;
      if Result and (Buffer <> nil) then
      begin
        Data := Buff + FMetaFields^[Field.FieldNo].DataOfs;
        Move(Data^, Buffer^, Field.DataSize);
      end;
    end
    else
    begin
      // calculated fields
      Inc(Buff, FCalcFieldsOfs + Field.Offset);
      Result := Boolean(Buff[0]);
      if Result and (Buffer <> nil) then
        Move(Buff[1], Buffer^, Field.DataSize);
    end;
  end;
end;

function TODBCCustomDataSet.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
begin
  Result := GetFieldData(FieldByNumber(FieldNo), Buffer);
end;

function TODBCCustomDataSet.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options, True);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TODBCCustomDataSet.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
begin
  Result := Null;

  if LocateRecord(KeyFields, KeyValues, [], False) then
  begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

function TODBCCustomDataSet.IsSequenced: Boolean;
begin
  Result := False;                                // Result := FQStmt.Cursor.EOF;
end;

function TODBCCustomDataSet.UpdateStatus: TUpdateStatus;
begin
  if FCachedUpdates then
    Result := PRecInfo(GetActiveBuffer)^.UpdateStatus
  else
    Result := usUnModified;
end;

procedure TODBCCustomDataSet.ApplyUpdates;
var
  Buff: TRecordBuffer;
  UpdateAction: TUpdateAction;
  i: Integer;
begin
  CheckCachedUpdateMode;

  if State in dsEditModes then Post;

  if FUpdatesPending then
  begin
    DisableControls;
    try
      UpdateCursorPos;

      i := 0;
      while i < FUpdateHistory.Count do
      begin
        Buff := FUpdateHistory[i];
        UpdateAction := uaFail;

        ApplyCachedUpdate(Buff, UpdateAction);

        case UpdateAction of
          uaFail:
            DatabaseError(SUserAbort);

          uaAbort:
            SysUtils.Abort;

          uaApplied:
            begin
              with PRecInfo(Buff)^ do
                if UpdateStatus = usDeleted then
                  FCacheList.Remove(Buff)
                else
                  UpdateStatus := usUnmodified;

              FUpdateHistory[i] := nil;
            end;

          uaRetry:
            Continue;
        end;
        Inc(i);
      end;

    finally
      FUpdateHistory.Pack;
      FUpdatesPending := FUpdateHistory.Count > 0;

      EnableControls;
    end;
    if FRecordPos >= FCacheList.Count then
      FRecordPos := FCacheList.Count - 1;

    Resync([]);
  end;
end;

procedure TODBCCustomDataSet.CancelUpdates;
begin
  CheckCachedUpdateMode;

  if State in dsEditModes then Cancel;

  if FUpdatesPending then
  begin
    CursorPosChanged;

    DisableControls;
    try
      while FUpdateHistory.Count > 0 do
        RevertCachedUpdate(FUpdateHistory[FUpdateHistory.Count - 1]);

      FUpdatesPending := False;
      FOldRecordCount := 0;
    finally
      EnableControls;
    end;
    if FRecordPos >= FCacheList.Count then
      FRecordPos := FCacheList.Count - 1;

    UpdateCursorPos;
    Resync([]);
  end;
end;

procedure TODBCCustomDataSet.FetchAll;
var
  Save_Bookmark: TBookmark;
begin
  if not EOF then
  begin
    DisableControls;
    try
      CheckBrowseMode;

      Save_Bookmark := Bookmark;
      Last;
      Bookmark := Save_Bookmark;
    finally
      EnableControls;
    end;
  end;
end;

procedure TODBCCustomDataSet.RevertRecord;
begin
  CheckCachedUpdateMode;

  if State in dsEditModes then Cancel;

  if FUpdatesPending then
  begin
    UpdateCursorPos;
    RevertCachedUpdate(FCacheList[FRecordPos]);
    Resync([]);

    FUpdatesPending := FUpdateHistory.Count > 0;
  end;
end;


{ TODBCCustomDataSetEx }

function TODBCCustomDataSetEx.GetConnection: TODBCConnection;
begin
  Result := (QStmt as TODBCStatement).Connection;
end;

procedure TODBCCustomDataSetEx.SetConnection(Value: TODBCConnection);
begin
  InternalSetConnection(Value);
end;

procedure TODBCCustomDataSetEx.InternalSetConnection(Value: TODBCConnection);
begin
  CheckInactive;

  if Connection <> Value then
  begin
    if Assigned(Connection) then
      Connection.UnRegisterClient(Self);

    (QStmt as TODBCStatement).Connection := Value;

    if Assigned(Connection) then
      Connection.RegisterClient(Self, ConnectionStateChange);
  end;
end;

procedure TODBCCustomDataSetEx.ActivateConnection;
begin
  if GetConnection = nil then
    DatabaseError(SMissingConnection);

  if not Connection.Connected then
    Connection.Open;
end;


{ IProviderSupport }

{$IFNDEF VCL50}

function TODBCCustomDataSetEx.PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer = nil): Integer;

  procedure ParamTypeUpdate(AParams: TParams);
  var
    Param: TParam;
    i: Integer;
  begin
    for i := 0 to AParams.Count - 1 do
    begin
      Param := AParams[i];
      if Param.ParamType = ptUnknown then
        Param.ParamType := ptInput;
    end;
  end;

begin
  // if Param.ParamType = ptUnknown
  // Param.ParamType := ptInput;
  ParamTypeUpdate(AParams);

  if Assigned(ResultSet) then
  begin
    TODBCQuery(ResultSet^) := TODBCQuery.Create(nil);
    with TODBCQuery(ResultSet^) do
    begin
      Connection := Self.Connection;
      SQL.Text := ASQL;
      Params.Assign(AParams);
      Open;
      Result := RowsAffected;
    end;
  end
  else
    with TODBCQuery.Create(nil) do
    try
      Connection := Self.Connection;
      SQL.Text := ASQL;
      Params.Assign(AParams);
      ExecSQL;
      Result := RowsAffected;
    finally
      Free;
    end;
end;
{$ENDIF}

function TODBCCustomDataSetEx.PSGetQuoteChar: string;
begin
  ActivateConnection;
  Result := Connection.QuoteChar;
end;

procedure TODBCCustomDataSetEx.PSStartTransaction;
begin
  ActivateConnection;
  Connection.StartTransaction;
end;

procedure TODBCCustomDataSetEx.PSEndTransaction(Commit: Boolean);
begin
  if Commit then
    Connection.Commit
  else
    Connection.Rollback;
end;

function TODBCCustomDataSetEx.PSInTransaction: Boolean;
begin
  Result := Connection.InTransaction;
end;

procedure TODBCCustomDataSetEx.OpenCursor(InfoQuery: Boolean);
begin
  ActivateConnection;
  inherited OpenCursor(InfoQuery);
end;


{ TODBCInternalDataSet }

constructor TODBCInternalDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FQDelete := TODBCStatement.Create(Self);
  FQInsert := TODBCStatement.Create(Self);
  FQModify := TODBCStatement.Create(Self);
end;

destructor TODBCInternalDataSet.Destroy;
begin
  Close;

  Connection := nil;

  FQDelete.Free;
  FQInsert.Free;
  FQModify.Free;

  inherited Destroy;
end;

function TODBCInternalDataSet.GetDeleteSQL: TStrings;
begin
  Result := FQDelete.SQL;
end;

function TODBCInternalDataSet.GetInsertSQL: TStrings;
begin
  Result := FQInsert.SQL;
end;

function TODBCInternalDataSet.GetModifySQL: TStrings;
begin
  Result := FQModify.SQL;
end;

procedure TODBCInternalDataSet.SetDeleteSQL(Value: TStrings);
begin
  CheckInactive;
  FQDelete.SQL.Assign(Value);
end;

procedure TODBCInternalDataSet.SetInsertSQL(Value: TStrings);
begin
  CheckInactive;
  FQInsert.SQL.Assign(Value);
end;

procedure TODBCInternalDataSet.SetModifySQL(Value: TStrings);
begin
  CheckInactive;
  FQModify.SQL.Assign(Value);
end;

procedure TODBCInternalDataSet.SetInternalSQLParams(Stmt: TODBCCustomStatement; Buffer: TRecordBuffer);
var
  Param: TODBCParam;
  ParamName: string;
  TmpField: TField;
  Old: Boolean;
  i: Integer;
begin
  for i := 0 to Stmt.Params.Count - 1 do
  begin
    Param := Stmt.Params[i];
    Param.ParamType := ptInput;
    ParamName := Param.Name;

    Old := CompareText(Copy(ParamName, 1, 4), 'OLD_') = 0; {do not localize}
    if Old then System.Delete(ParamName, 1, 4);

    TmpField := FindField(ParamName);
    Param.Assign(TmpField);
  end;
end;

procedure TODBCInternalDataSet.CheckDeleteOperation;
begin
  if DeleteSQL.Text = '' then inherited;
end;

procedure TODBCInternalDataSet.CheckInsertOperation;
begin
  if InsertSQL.Text = '' then inherited;
end;

procedure TODBCInternalDataSet.CheckModifyOperation;
begin
  if ModifySQL.Text = '' then inherited;
end;

procedure TODBCInternalDataSet.ApplyCachedUpdate(Buffer: TRecordBuffer; var UpdateAction: TUpdateAction);
var
  UpdateKind: TUpdateKind;
begin
  if Assigned(UpdateObject) or Assigned(OnUpdateRecord) then
    inherited ApplyCachedUpdate(Buffer, UpdateAction)
  else
  begin
    case PRecInfo(Buffer)^.UpdateStatus of
      usDeleted:
        UpdateKind := ukDelete;

      usInserted:
        UpdateKind := ukInsert;
    else
      UpdateKind := ukModify;
    end;

    Move(Buffer^, ActiveBuffer^, FRecBufSize);
    try
      ApplyCachedToDBMS(UpdateKind, Buffer);
      UpdateAction := uaApplied;
    except
      on E: EODBCError do
        DoUpdateError(E, UpdateKind, UpdateAction);
    end;
  end;
end;


procedure TODBCInternalDataSet.InternalSetConnection(Value: TODBCConnection);
begin
  inherited;
  (FQDelete as TODBCStatement).Connection := Connection;
  (FQInsert as TODBCStatement).Connection := Connection;
  (FQModify as TODBCStatement).Connection := Connection;
end;

procedure TODBCInternalDataSet.DoDelete;
begin
  if Assigned(UpdateObject) then
    UpdateObject.Apply(ukDelete)
  else
  begin
    if FCachedUpdates then
      SetInternalSQLParams(FQDelete, ActiveBuffer)
    else
      SetInternalSQLParams(FQDelete, GetActiveBuffer);

    FQDelete.Execute;
  end;
end;

procedure TODBCInternalDataSet.DoInsert;
begin
  if Assigned(UpdateObject) then
    UpdateObject.Apply(ukInsert)
  else
  begin
    if FCachedUpdates then
      SetInternalSQLParams(FQInsert, ActiveBuffer)
    else
      SetInternalSQLParams(FQInsert, GetActiveBuffer);

    FQInsert.Execute;
  end;
end;

procedure TODBCInternalDataSet.DoModify;
begin
  if Assigned(UpdateObject) then
    UpdateObject.Apply(ukModify)
  else
  begin
    if FCachedUpdates then
      SetInternalSQLParams(FQModify, ActiveBuffer)
    else
      SetInternalSQLParams(FQModify, GetActiveBuffer);

    FQModify.Execute;
  end;
end;

procedure TODBCInternalDataSet.InternalPrepare;
begin
  inherited;
  if Trim(FQDelete.SQL.Text) <> '' then
    FQDelete.Prepare;
  if Trim(FQInsert.SQL.Text) <> '' then
    FQInsert.Prepare;
  if Trim(FQModify.SQL.Text) <> '' then
    FQModify.Prepare;
end;

procedure TODBCInternalDataSet.InternalUnPrepare;
begin
  CheckInactive;

  FQDelete.UnPrepare;
  FQInsert.UnPrepare;
  FQModify.UnPrepare;

  inherited;
end;

function TODBCInternalDataSet.GetCanModify: Boolean;
begin
  Result := inherited GetCanModify or
    (FQDelete.SQL.Text <> '') or
    (FQInsert.SQL.Text <> '') or
    (FQModify.SQL.Text <> '');
end;

procedure TODBCInternalDataSet.InternalSetParamsFromCursor;
var
  Param: TODBCParam;
  TmpField: TField;
  i: Integer;
begin
  if Handle = nil then
    InternalPrepare;

  if not QStmt.Prepared then
    QStmt.Prepare;

  if (SQLParams.Count > 0) and (DataSource <> nil) and (DataSource.DataSet <> nil) then
    for i := 0 to SQLParams.Count - 1 do
    begin
      Param := SQLParams[i];
      Param.ParamType := ptInput;

      TmpField := DataSource.DataSet.FindField(Param.Name);
      Param.Assign(TmpField);
    end;
end;


{ TODBCDataset }

function TODBCDataset.GetPrepared: Boolean;
begin
  Result := QStmt.Prepared;
end;

function TODBCDataset.GetQSelect: TODBCCustomStatement;
begin
  Result := QStmt;
end;

function TODBCDataset.GetSelectSQL: TStrings;
begin
  Result := QStmt.SQL;
end;

procedure TODBCDataset.SetSelectSQL(Value: TStrings);
begin
  CheckInactive;
  QStmt.SQL.Assign(Value);
end;

procedure TODBCDataset.SetFiltered(Value: Boolean);
begin
  if Filtered <> Value then
  begin
    if Value then
      CheckBiDirectional;

    inherited SetFiltered(Value);

    if FOpen then
    begin
      DisableControls;
      try
        Close;
        Open;
      finally
        EnableControls;
      end;
    end;
  end;
end;

procedure TODBCDataset.InternalOpen;
begin
  InternalSetParamsFromCursor;
  inherited InternalOpen;
end;

procedure TODBCDataset.Prepare;
begin
  InternalPrepare;
end;

procedure TODBCDataset.UnPrepare;
begin
  InternalUnPrepare;
end;


{ TsvBlobStream }

constructor TsvBlobStream.Create(AField: TBlobField; AMode: TBlobStreamMode);
begin
  inherited Create;
  FField := AField;
  FMode := AMode;
  FDataSet := FField.DataSet as TODBCCustomDataSet;
  FBuffer := FDataSet.GetActiveBuffer;

  if FBuffer <> nil then
  begin
    if AMode <> bmRead then
    begin
      if FField.ReadOnly then
        DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName], FDataSet);

      if not (FDataSet.State in [dsEdit, dsInsert]) then
        DatabaseError(SNotEditing, FDataSet);
    end;

    if AMode = bmWrite then
      Truncate
    else
      ReadBlobData;
  end;
end;

destructor TsvBlobStream.Destroy;
begin
  if FModified then
  try
    WriteBlobData;
    FField.Modified := True;
    FDataSet.DataEvent(deFieldChange, Longint(FField));
  except
    Application.HandleException(Self);
  end;
  inherited Destroy;
end;

procedure TsvBlobStream.ReadBlobData;
var
  Data: Pointer;
  Len: Integer;
begin
  FDataSet.GetBlobData(FField, FBuffer, Data, Len);

  SetSize(Len);
  Move(Data^, Memory^, Len);
end;

procedure TsvBlobStream.WriteBlobData;
begin
  FDataSet.SetBlobdata(FField, FBuffer, Memory, Size);
end;

function TsvBlobStream.Write(const ABuffer; ACount: Integer): Longint;
begin
  Result := inherited Write(ABuffer, ACount);
  FModified := True;
end;

procedure TsvBlobStream.Truncate;
begin
  Clear;
  FModified := True;
end;


end.

