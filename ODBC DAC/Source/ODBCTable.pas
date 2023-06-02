
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCTable                                       }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  } 
{                                                       }
{*******************************************************}

unit ODBCTable;
{$I sv.inc}
interface
uses
  Classes,
  SysUtils,
  DB,
  ODBCConnection,
  ODBCStmt,
  ODBCCustomDataset;


type

  DBSearchCond = (                                { Search condition for keys }
    keySEARCHEQ,                                  { = }
    keySEARCHGT,                                  { > }
    keySEARCHGEQ                                  { >= }
    );

  // Key description structure
  TKeyIndex = (kiLookup, kiRangeStart, kiRangeEnd, kiCurRangeStart, kiCurRangeEnd, kiSave);

  PKeyBuffer = ^TKeyBuffer;
  TKeyBuffer = packed record
    Modified: Boolean;
    Exclusive: Boolean;
    FieldCount: Integer;
  end;


  { TODBCTable }

  TODBCTable = class(TODBCInternalDataSet)
  private
    FTableName: string;
    FTableType: TODBCTableType;
    FQuoteChar: Char;

    FDefaultIndex: Boolean;
    FFieldsIndex: Boolean;
    FIndexDefs: TIndexDefs;
    FIndexName: string;

    FCurrentIdxFlds: TList;                       // Current Index Fields

    FKeyBuffers: array[TKeyIndex] of PKeyBuffer;
    FKeyBuffer: PKeyBuffer;

    FMasterLink: TMasterDataLink;
    FStoreDefs: Boolean;
    FReadOnly: Boolean;

    FRegenerateSQL: Boolean;

    // intended to implement "Range" methods
    FTableRecordSize: Cardinal;
    FRangeSQL: string;
    FRangeParams: TODBCParams;

    function GetExists: Boolean;
    function GetIndexField(Index: Integer): TField;
    function GetIndexFieldCount: Integer;
    function GetIndexFieldNames: string;
    function GetIndexName: string;
    function GetMasterFields: string;

    procedure SetDataSource(const Value: TDataSource);
    procedure SetIndex(const Value: string; FieldsIndex: Boolean);
    procedure SetIndexDefs(const Value: TIndexDefs);
    procedure SetIndexField(Index: Integer; const Value: TField);
    procedure SetIndexFieldNames(const Value: string);
    procedure SetIndexName(const Value: string);
    procedure SetMasterFields(const Value: string);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetTableName(const Value: string);

    function GetCurrentIdxDef: TIndexDef;
    procedure UpdateCurrentIdxFlds;

    function GetTableRecBufSize: Integer;

    procedure CheckSetKeyMode;
    procedure CheckMasterRange;

    procedure AllocKeyBuffers;
    procedure FreeKeyBuffers;
    function InitKeyBuffer(Buffer: PKeyBuffer): PKeyBuffer;

    function GetKeyExclusive: Boolean;
    function GetKeyFieldCount: Integer;

    function _GetRecordForKey(iFields: Word; iPartLen: Word; pKey: Pointer; pRecBuf: Pointer): Boolean;
    function _SetToKey(SearchCond: DBSearchCond; iFields: Word; iPartLen: Word; pRecBuf: Pointer): Boolean;
    procedure _SetRange(iFields: Word; pKey1: Pointer; bKey1Incl: Boolean; pKey2: Pointer; bKey2Incl: Boolean);
    procedure _DropRange;

    procedure SetKeyExclusive(Value: Boolean);
    procedure SetKeyFieldCount(Value: Integer);
    procedure SetKeyBuffer(KeyIndex: TKeyIndex; Clear: Boolean);
    procedure SetKeyFields(KeyIndex: TKeyIndex; const Values: array of const);
    procedure SetLinkRanges(MasterFields: TList);

    procedure PostKeyBuffer(Commit: Boolean);

    procedure ReopenTable;

    function FieldDefsStored: Boolean;
    function IndexDefsStored: Boolean;

    function FormatFieldList(const Value: string): string;

    function QuoteTableName: string;
    function GenerateSQL: string;
    procedure GenerateUpdateSQLs;

    procedure CheckTableName;
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);

  protected
    procedure ActivateConnection; override;

    { IProviderSupport }
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetKeyFields: string; override;
    function PSGetTableName: string; override;
    function PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    { }

    procedure DoOnNewRecord; override;
    function GetCanModify: Boolean; override;
    function GetDataSource: TDataSource; override;

    function GetRecordSize: Word; override;

    procedure UpdateIndexDefs; override;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    procedure DefChanged(Sender: TObject); override;
    procedure InitFieldDefs; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;

    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;

    function SetCursorRange: Boolean;
    function ResetCursorRange: Boolean;

    function GetIsIndexField(Field: TField): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure DeleteIndex(const Name: string);
    procedure EmptyTable;
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
    procedure GetIndexNames(List: TStrings);

    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override;
    procedure Post; override;
    procedure Cancel; override;

    { Range methods }
    procedure EditKey;
    procedure EditRangeEnd;
    procedure EditRangeStart;

    procedure SetKey;
    procedure SetRange(const StartValues, EndValues: array of const);
    procedure SetRangeEnd;
    procedure SetRangeStart;

    procedure ApplyRange;
    procedure CancelRange;

    function FindKey(const KeyValues: array of const): Boolean;
    procedure FindNearest(const KeyValues: array of const);
    function GotoKey: Boolean;
    procedure GotoNearest;

    property Exists: Boolean read GetExists;
    property IndexFieldCount: Integer read GetIndexFieldCount;
    property IndexFields[Index: Integer]: TField read GetIndexField write SetIndexField;

    property KeyExclusive: Boolean read GetKeyExclusive write SetKeyExclusive;
    property KeyFieldCount: Integer read GetKeyFieldCount write SetKeyFieldCount;

  published
    property BufferChunks;
    property CachedUpdates;
    property Connection;
    property Constraints stored ConstraintsStored;
    property DefaultIndex: Boolean read FDefaultIndex write FDefaultIndex default True;
    property FieldDefs stored FieldDefsStored;
    property Filter;
    property Filtered;
    property FilterOptions;
    property IndexDefs: TIndexDefs read FIndexDefs write SetIndexDefs stored IndexDefsStored;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
    property IndexName: string read GetIndexName write SetIndexName;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property StoreDefs: Boolean read FStoreDefs write FStoreDefs default False;
    property TableName: string read FTableName write SetTableName;
    property TableType: TODBCTableType read FTableType write FTableType;
    property UpdateObject;
  end;


implementation
uses
{$IFDEF VER140_OR_ABOVE}
  Variants,
  FMTBcd,
  SQLTimSt,
{$ENDIF}

  odbcsqltypes,
  odbcsql,
  odbcsqlext,

  ODBCIntf,
  ODBCException,
  ODBCUtils,
  ODBCConsts,
  ODBCQuery;

const
  BOR_QCHAR = '"';                                // Borland quote char - used to replace parameters with "?"


  { TODBCTable }

constructor TODBCTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIndexDefs := TIndexDefs.Create(Self);

  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange := MasterChanged;
  FMasterLink.OnMasterDisable := MasterDisabled;

  FCurrentIdxFlds := TList.Create;

  FDefaultIndex := True;
  FRegenerateSQL := True;

  FRangeParams := TODBCParams.Create(Self);
end;

destructor TODBCTable.Destroy;
begin
  Close;

  FRangeParams.Free;
  FCurrentIdxFlds.Free;

  FMasterLink.Free;
  FIndexDefs.Free;
  inherited Destroy;
end;

function TODBCTable.GetExists: Boolean;
var
  TableList: TStrings;
begin
  Result := Active;
  if Result or (FTableName = '') then Exit;

  TableList := TStringList.Create;
  try
    Connection.GetTableNames(TableList, FTableType);
    Result := TableList.IndexOf(FTableName) > -1;
  finally
    TableList.Free;
  end;
end;

function TODBCTable.GetIndexField(Index: Integer): TField;
begin
  if (Index < 0) or (Index >= FCurrentIdxFlds.Count) then
    DatabaseError(SFieldIndexError, Self);

  Result := FCurrentIdxFlds[Index];
end;

function TODBCTable.GetIndexFieldCount: Integer;
begin
  Result := FCurrentIdxFlds.Count;
end;

function TODBCTable.GetIndexFieldNames: string;
begin
  if FFieldsIndex then
    Result := FIndexName
  else
    Result := '';
end;

function TODBCTable.GetIndexName: string;
begin
  if FFieldsIndex then
    Result := ''
  else
    Result := FIndexName;
end;

function TODBCTable.GetMasterFields: string;
begin
  Result := FMasterLink.FieldNames;
end;

procedure TODBCTable.SetDataSource(const Value: TDataSource);
begin
  if IsLinkedTo(Value) then DatabaseError(SCircularDataLink);
  FMasterLink.DataSource := Value;
end;

procedure TODBCTable.SetIndex(const Value: string; FieldsIndex: Boolean);
begin
  if Active then CheckBrowseMode;

  if (FIndexName <> Value) or (FFieldsIndex <> FieldsIndex) then
  begin
    FIndexName := Value;
    FFieldsIndex := FieldsIndex;

    if Active then
    begin
      UpdateCurrentIdxFlds;
      ReopenTable;
    end;
  end;
end;

procedure TODBCTable.SetIndexDefs(const Value: TIndexDefs);
begin
  FIndexDefs := Value;
end;

procedure TODBCTable.SetIndexField(Index: Integer; const Value: TField);
begin
  GetIndexField(Index).Assign(Value);
end;

procedure TODBCTable.SetIndexFieldNames(const Value: string);
begin
  SetIndex(Value, Value <> '');
end;

procedure TODBCTable.SetIndexName(const Value: string);
begin
  SetIndex(Value, False);
end;

procedure TODBCTable.SetMasterFields(const Value: string);
begin
  FMasterLink.FieldNames := Value;
end;

procedure TODBCTable.SetReadOnly(const Value: Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

procedure TODBCTable.SetTableName(const Value: string);
begin
  if FTableName <> Value then
    if csReading in ComponentState then
      FTableName := Value
    else
    begin
      CheckInactive;

      FTableName := Value;

      DeleteSQL.Text := '';
      InsertSQL.Text := '';
      StmtSQL.Text := '';
      ModifySQL.Text := '';

      IndexName := '';
      IndexFieldNames := '';

      FRegenerateSQL := True;

      DataEvent(dePropertyChange, 0);
    end;
end;

function TODBCTable.GetCurrentIdxDef: TIndexDef;

  function GetIdx(IdxType: TIndexOption): TIndexDef;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to FIndexDefs.Count - 1 do
      if IdxType in FIndexDefs[i].Options then
      try
        Result := FIndexDefs[i];
        GetFieldList(nil, Result.Fields);
        Break;
      except
        Result := nil;
      end;
  end;

var
  IdxDef: TIndexDef;
begin
  FIndexDefs.Update;

  if IndexName <> '' then
    IdxDef := FIndexDefs.Find(IndexName)
  else
    if IndexFieldNames <> '' then
      IdxDef := FIndexDefs.FindIndexForFields(IndexFieldNames)
    else
      IdxDef := nil;

  if Assigned(IdxDef) then
  try
    GetFieldList(nil, IdxDef.Fields);
  except
    IdxDef := nil;
  end;

  if not Assigned(IdxDef) then
    IdxDef := GetIdx(ixPrimary);

  if not Assigned(IdxDef) then
    IdxDef := GetIdx(ixUnique);

  if Assigned(IdxDef) then
  begin
    Result := TIndexDef.Create(nil);
    Result.Assign(IdxDef);
  end
  else
    Result := nil;
end;

procedure TODBCTable.UpdateCurrentIdxFlds;
var
  IdxDef: TIndexDef;
begin
  IdxDef := GetCurrentIdxDef;
  try
    FCurrentIdxFlds.Clear;

    if IdxDef <> nil then
      GetFieldList(FCurrentIdxFlds, IdxDef.Fields);
  finally
    IdxDef.Free;
  end;
end;

function TODBCTable.GetTableRecBufSize: Integer;
var
  BlobBufferSize: Integer;
begin
  if CacheBlobs then
    BlobBufferSize := BlobFieldCount {//!QStmt.Cursor.BlobColumnCount} * SizeOf(Pointer)
  else
    BlobBufferSize := 0;

  Result := SizeOf(TRecInfo) + GetRecordDataSize + CalcFieldsSize + BlobBufferSize;
end;

procedure TODBCTable.CheckSetKeyMode;
begin
  if State <> dsSetKey then DatabaseError(SNotEditing, Self);
end;

procedure TODBCTable.CheckMasterRange;
begin
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
  begin
    SetLinkRanges(FMasterLink.Fields);
    SetCursorRange;
  end;
end;

procedure TODBCTable.AllocKeyBuffers;
var
  KeyIndex: TKeyIndex;
begin
  for KeyIndex := Low(TKeyIndex) to High(TKeyIndex) do
    FKeyBuffers[KeyIndex] := InitKeyBuffer(AllocMem(SizeOf(TKeyBuffer) + FTableRecordSize));
end;

procedure TODBCTable.FreeKeyBuffers;
var
  KeyIndex: TKeyIndex;
begin
  for KeyIndex := Low(TKeyIndex) to High(TKeyIndex) do
    DisposeMem(FKeyBuffers[KeyIndex], SizeOf(TKeyBuffer) + FTableRecordSize);
end;

function TODBCTable.InitKeyBuffer(Buffer: PKeyBuffer): PKeyBuffer;
begin
  FillChar(Buffer^, SizeOf(TKeyBuffer) + RecordSize, 0);

  { Double source code part to speed data processing }
  //InternalInitRecord(PChar(Buffer) + SizeOf(TKeyBuffer));
  with PRecInfo(PChar(Buffer) + SizeOf(TKeyBuffer))^ do
  begin
    UpdateStatus := usInserted;
    PhyRecNum := -1;
    BookmarkFlag := bfInserted;
  end;

  Result := Buffer;
end;

function TODBCTable.GetKeyExclusive: Boolean;
begin
  CheckSetKeyMode;
  Result := FKeyBuffer.Exclusive;
end;

function TODBCTable.GetKeyFieldCount: Integer;
begin
  CheckSetKeyMode;
  Result := FKeyBuffer.FieldCount;
end;

function TODBCTable._GetRecordForKey(iFields: Word; iPartLen: Word; pKey: Pointer; pRecBuf: Pointer): Boolean;
var
  FldCount: Integer;

  KeyFields: string;
  KeyValues: Variant;

  SaveState: TDataSetState;
  i: Integer;
begin
  if iFields > 0 then
    FldCount := iFields
  else
    FldCount := FCurrentIdxFlds.Count;

  KeyFields := '';
  KeyValues := VarArrayCreate([0, FldCount - 1], varVariant);

  SaveState := SetTempState(dsSetKey);
  try
    for i := 0 to FldCount - 1 do
    begin
      if KeyFields <> '' then
        {+}
                // old: KeyFields := KeyFields + ',';
        KeyFields := KeyFields + ';';
      {-}
      KeyFields := KeyFields + GetIndexField(i).FieldName;

      KeyValues[i] := GetIndexField(i).Value;
    end;
  finally
    RestoreState(SaveState);
  end;
  {+}// A.Zubik 09.02.2006
  // old: Locate(KeyFields, KeyValues, []);
  Result := Locate(KeyFields, KeyValues, []);
  {-}
end;

function TODBCTable._SetToKey(SearchCond: DBSearchCond; iFields: Word; iPartLen: Word; pRecBuf: Pointer): Boolean;
var
  FldCount: Integer;
  KeyValues: Variant;

  FldValue: Variant;
  KeyValue: Variant;

  SaveState: TDataSetState;
  i: Integer;
begin
  CheckBrowseMode;
  CursorPosChanged;

  Result := False;

  if iFields > 0 then
    FldCount := iFields
  else
    FldCount := FCurrentIdxFlds.Count;

  KeyValues := VarArrayCreate([0, FldCount - 1], varVariant);
  SaveState := SetTempState(dsSetKey);
  try
    for i := 0 to FldCount - 1 do
      KeyValues[i] := GetIndexField(i).Value;
  finally
    RestoreState(SaveState);
  end;

  // Search ////////////////////////////////////////////////////////////////////
  DisableControls;
  try
    First;
    while not EOF do
    begin
      i := 0;
      Result := True;

      while Result and (i < FldCount) do
      begin
        FldValue := GetIndexField(i).Value;
        KeyValue := KeyValues[i];

        // value is not null
        Result := not (VarIsNull(KeyValue) or VarIsNull(FldValue));

        // convert FieldValue to specified type
        if Result then
        try
          FldValue := VarAsType(FldValue, VarType(KeyValue));
        except
          on E: EVariantError do
            Result := False;
        end;

        // compare values
        if Result then
        begin
          if GetIndexField(i).DataType in [ftString, ftFixedChar, ftWideString] then
          begin
            FldValue := TrimRight(FldValue);
            KeyValue := TrimRight(KeyValue);

            Result := CompareText(Copy(FldValue, 1, iPartLen), KeyValue) = 0;
          end
          else
            case SearchCond of
              keySEARCHEQ: Result := KeyValue = FldValue;
              keySEARCHGT: Result := KeyValue < FldValue;
              keySEARCHGEQ: Result := KeyValue <= FldValue;
            end;
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
  end;
end;

procedure TODBCTable._SetRange(iFields: Word; pKey1: Pointer; bKey1Incl: Boolean; pKey2: Pointer; bKey2Incl: Boolean);
const
  LOWER_BOUND = 0;
  UPPER_BOUND = 1;

  function GenLowerBoundSQL(FldCount: Word; pKey: Pointer; bKeyIncl: Boolean; Params: TODBCParams): string;
  const
    compare_str = '>';
    equal_str = '=';

    // field value = null stub strings
    compare_stub_str = ' IS NOT NULL ';
    equal_stub_str = '(1 = 1)';

    // parameters prefix
    param_pfx = 'L_';

  var
    SubSQL: string;
    CondStr: string;

    ColName: string;
    ParamName: string;

    IdxField: TField;
    Param: TODBCParam;

    SaveState: TDatasetState;
    i, j: Integer;
  begin
    Result := '';

    // Init KeyBuffer
    FKeyBuffer := FKeyBuffers[kiRangeStart];

    SaveState := SetTempState(dsSetKey);
    try
      for i := 0 to FldCount - 1 do
      begin
        // Get SubSQL string
        SubSQL := '';
        for j := 0 to FldCount - i - 1 do
        begin
          IdxField := FCurrentIdxFlds[j];

          ColName := QuoteStr(IdxField.FieldName, FQuoteChar);
          ParamName := QuoteStr(param_pfx + IdxField.FieldName, BOR_QCHAR);

          // Determine condition string
          if j = FldCount - i - 1 then
            if i = 0 then
            begin
              CondStr := compare_str;
              if bKeyIncl then
                CondStr := CondStr + equal_str;
            end
            else
              CondStr := compare_str
          else
            CondStr := equal_str;

          // Generate SQL sub-string
          if SubSQL <> '' then
            SubSQL := SubSQL + ' AND ';

          if IdxField.IsNull then
            if CondStr = equal_str then
              SubSQL := SubSQL + equal_stub_str
            else
              SubSQL := SubSQL + ColName + compare_stub_str
          else
          begin
            // Generate Param instance
            Param := TODBCParam(Params.Add);
            Param.Assign(IdxField);
            Param.Name := ParamName;
            Param.ParamType := ptInput;

            SubSQL := SubSQL + '(' + ColName + CondStr + ':' + ParamName + ')';
          end;
        end;

        // Add SubSQL string
        if Result <> '' then
          Result := Result + ' OR ';
        Result := Result + SubSQL;
      end;
    finally
      RestoreState(SaveState);
    end;
  end;

  function GenUpperBoundSQL(FldCount: Word; pKey: Pointer; bKeyIncl: Boolean; Params: TODBCParams): string;
  const
    compare_str = '<';
    equal_str = '=';

    // field value = null stub strings
    compare_stub_str = '(0 = 1)';
    equal_stub_str = ' IS NULL ';

    // parameters prefix
    param_pfx = 'U_';

  var
    SubSQL: string;
    CondStr: string;

    ColName: string;
    ParamName: string;

    IdxField: TField;
    Param: TODBCParam;

    SaveState: TDatasetState;
    i, j: Integer;
  begin
    Result := '';

    // Init KeyBuffer by kiRangeEnd
    FKeyBuffer := FKeyBuffers[kiRangeEnd];

    SaveState := SetTempState(dsSetKey);
    try
      for i := 0 to FldCount - 1 do
      begin
        // Get SubSQL string
        SubSQL := '';
        for j := 0 to FldCount - i - 1 do
        begin
          IdxField := FCurrentIdxFlds[j];

          ColName := QuoteStr(IdxField.FieldName, FQuoteChar);
          ParamName := QuoteStr(param_pfx + IdxField.FieldName, BOR_QCHAR);

          // Determine condition string
          if j = FldCount - i - 1 then
            if i = 0 then
            begin
              CondStr := compare_str;
              if bKeyIncl then
                CondStr := CondStr + equal_str;
            end
            else
              CondStr := compare_str
          else
            CondStr := equal_str;

          // Generate SQL sub-string
          if SubSQL <> '' then
            SubSQL := SubSQL + ' AND ';

          if IdxField.IsNull then
            if j < FldCount - i - 1 then
              SubSQL := SubSQL + equal_stub_str
            else
              SubSQL := SubSQL + ColName + compare_stub_str
          else
          begin
            // Generate Param instance
            Param := TODBCParam(Params.Add);
            Param.Assign(IdxField);
            Param.Name := ParamName;
            Param.ParamType := ptInput;

            if j < FldCount - i - 1 then
              SubSQL := SubSQL + '(' + ColName + CondStr + ':' + ParamName + ')'
            else
              SubSQL := SubSQL + '(' + ColName + ' IS NULL OR ' + ColName + CondStr + ':' + ParamName + ')';
          end;
        end;

        // Add SubSQL string
        if Result <> '' then
          Result := Result + ' OR ';
        Result := Result + SubSQL;
      end;
    finally
      RestoreState(SaveState);
    end;
  end;

  function GenEqualBoundsSQL(FldCount: Word; pKey: Pointer; Params: TODBCParams): string;
  var
    SubSQL: string;

    ColName: string;
    ParamName: string;

    IdxField: TField;
    Param: TODBCParam;

    SaveState: TDatasetState;
    i: Integer;
  begin
    Result := '';

    // Init KeyBuffer
    // RangeStart and RangeEnd buffers are equal
    FKeyBuffer := FKeyBuffers[kiRangeEnd];

    SaveState := SetTempState(dsSetKey);
    try
      for i := 0 to FldCount - 1 do
      begin
        // Get SubSQL string
        SubSQL := '';
        IdxField := FCurrentIdxFlds[i];

        ColName := QuoteStr(IdxField.FieldName, FQuoteChar);
        ParamName := QuoteStr(IdxField.FieldName, BOR_QCHAR);

        if TField(IdxField).IsNull then
          SubSQL := SubSQL + ColName + ' IS NULL '
        else
        begin
          // Generate Param instance
          Param := TODBCParam(Params.Add);
          Param.Assign(IdxField);
          Param.Name := ParamName;
          Param.ParamType := ptInput;

          SubSQL := SubSQL + '(' + ColName + '=' + ':' + ParamName + ')';
        end;

        // Add SubSQL string
        if Result <> '' then
          Result := Result + ' AND ';
        Result := Result + SubSQL;
      end;
    finally
      RestoreState(SaveState);
    end;
  end;

var
  FldCount: Integer;

  LowerBoundSQLPart: string;
  UpperBoundSQLPart: string;
begin
  FRangeSQL := '';
  FRangeParams.Clear;

  // Field Count
  if iFields > 0 then
    FldCount := iFields
  else
    FldCount := FCurrentIdxFlds.Count;

  if FldCount = 0 then
    DatabaseError(SNoFieldIndexes, Self);

  if BuffersEqual(pKey1, pKey2, FTableRecordSize) then
    FRangeSQL := GenEqualBoundsSQL(FldCount, pKey1, FRangeParams)
  else
  begin
    LowerBoundSQLPart := GenLowerBoundSQL(FldCount, pKey1, bKey1Incl, FRangeParams);
    UpperBoundSQLPart := GenUpperBoundSQL(FldCount, pKey2, bKey2Incl, FRangeParams);

    // Build range SQL string
    FRangeSQL := LowerBoundSQLPart;

    if FRangeSQL <> '' then
      FRangeSQL := FRangeSQL + ' AND ';

    FRangeSQL := FRangeSQL + UpperBoundSQLPart;
  end;

  // Table reopen
  if Active then
    ReopenTable;
end;

procedure TODBCTable._DropRange;
begin
  FRangeSQL := '';
  FRangeParams.Clear;

  ReopenTable;
end;

procedure TODBCTable.SetKeyExclusive(Value: Boolean);
begin
  CheckSetKeyMode;
  FKeyBuffer.Exclusive := Value;
end;

procedure TODBCTable.SetKeyFieldCount(Value: Integer);
begin
  CheckSetKeyMode;
  FKeyBuffer.FieldCount := Value;
end;

procedure TODBCTable.SetKeyBuffer(KeyIndex: TKeyIndex; Clear: Boolean);
begin
  CheckBrowseMode;
  FKeyBuffer := FKeyBuffers[KeyIndex];
  Move(FKeyBuffer^, FKeyBuffers[kiSave]^, SizeOf(TKeyBuffer) + FTableRecordSize);

  if Clear then
    InitKeyBuffer(FKeyBuffer);

  SetState(dsSetKey);
  SetModified(FKeyBuffer.Modified);
  DataEvent(deDataSetChange, 0);
end;

procedure TODBCTable.SetKeyFields(KeyIndex: TKeyIndex; const Values: array of const);
var
  SaveState: TDataSetState;
  i: Integer;
begin
  if IndexFieldCount = 0 then
    DatabaseError(SNoFieldIndexes, Self);

  SaveState := SetTempState(dsSetKey);
  try
    FKeyBuffer := InitKeyBuffer(FKeyBuffers[KeyIndex]);

    for i := 0 to High(Values) do
      GetIndexField(i).AssignValue(Values[i]);

    FKeyBuffer^.FieldCount := High(Values) + 1;
    FKeyBuffer^.Modified := Modified;
  finally
    RestoreState(SaveState);
  end;
end;

procedure TODBCTable.SetLinkRanges(MasterFields: TList);
var
  SaveState: TDataSetState;
  i: Integer;
begin
  SaveState := SetTempState(dsSetKey);
  try
    FKeyBuffer := InitKeyBuffer(FKeyBuffers[kiRangeStart]);
    FKeyBuffer^.Modified := True;

    for i := 0 to MasterFields.Count - 1 do
      GetIndexField(i).Assign(TField(MasterFields[i]));

    FKeyBuffer^.FieldCount := MasterFields.Count;
  finally
    RestoreState(SaveState);
  end;
  Move(FKeyBuffers[kiRangeStart]^, FKeyBuffers[kiRangeEnd]^, SizeOf(TKeyBuffer) + FTableRecordSize);
end;

procedure TODBCTable.PostKeyBuffer(Commit: Boolean);
begin
  DataEvent(deCheckBrowseMode, 0);
  if Commit then
    FKeyBuffer.Modified := Modified
  else
    Move(FKeyBuffers[kiSave]^, FKeyBuffer^, SizeOf(TKeyBuffer) + FTableRecordSize);

  SetState(dsBrowse);
  DataEvent(deDataSetChange, 0);
end;

procedure TODBCTable.ReopenTable;
begin
  CheckActive;

  DisableControls;
  try
    // close
    QStmt.Close;
    _Open := False;

    // clear cache
    _UpdateHistory.Clear;

    FreeCache;
    FreeOldCache;

    _CacheList.Clear;

    _RecordPos := -1;
    _PhyRecCount := 0;
    _DeletedRecordCount := 0;

    // update SQL
    QStmt.SQL.Text := GenerateSQL;
    // update Params
    QStmt.Params.Assign(FRangeParams);

    // reopen table
    QStmt.Execute;
    _Open := QStmt.IsCursorOpen;

    Resync([]);
  finally
    EnableControls;
  end;
end;

function TODBCTable.FieldDefsStored: Boolean;
begin
  Result := StoreDefs and (FieldDefs.Count > 0);
end;

function TODBCTable.IndexDefsStored: Boolean;
begin
  Result := StoreDefs and (FIndexDefs.Count > 0);
end;

function TODBCTable.FormatFieldList(const Value: string): string;
begin
  Result := StringReplace(Value, ';', ',', [rfReplaceAll]); {do not localize}
end;

function TODBCTable.QuoteTableName: string;
var
  PosIdx: Integer;
  SchemaNamePart: string;
  TableNamePart: string;
begin
  PosIdx := Pos(SCHEMA_SEPARATOR, FTableName);

  if PosIdx = 0 then
    Result := QuoteStr(FTableName, FQuoteChar)
  else
  begin
    SchemaNamePart := Copy(FTableName, 1, PosIdx - 1);
    TableNamePart := Copy(FTableName, PosIdx + 1, Length(FTableName));

    Result := QuoteStr(SchemaNamePart, FQuoteChar) + SCHEMA_SEPARATOR + QuoteStr(TableNamePart, FQuoteChar);
  end;
end;

function TODBCTable.GenerateSQL: string;

  function Get_WHERE_Text(IdxDef: TIndexDef): string;
  begin
    // Range section
    Result := FRangeSQL;

    // Filter section
    if Filtered and (Filter <> '') then
    begin
      if Result <> '' then
        Result := Result + ' AND ';               {do not localize}

      Result := Result + '(' + Filter + ')';
    end;

    // Total WHERE
    if Result <> '' then
      Result := ' WHERE ' + Result;               {do not localize}
  end;

  function Get_ORDER_BY_Text(IdxDef: TIndexDef): string;
  var
    IFNames, IFName: string;
    Pos: Integer;
  begin
    Result := '';
    if not DefaultIndex or (IdxDef = nil) then Exit;

    IFNames := IdxDef.Fields;
    Pos := 1;
    while Pos <= Length(IFNames) do
    begin
      if Result <> '' then
        Result := Result + ', ';

      IFName := ExtractFieldName(IFNames, Pos);
      Result := Result + QuoteStr(IFName, FQuoteChar);
    end;

    if Result <> '' then
      Result := ' ORDER BY ' + Result;            {do not localize}
  end;

const
  SLCT_SQL = 'SELECT * FROM %s';                  {do not localize}
var
  CurrentIdxDef: TIndexDef;
begin
  CurrentIdxDef := GetCurrentIdxDef;
  try
    Result :=
      Format(SLCT_SQL, [QuoteTableName]) +
      Get_WHERE_Text(CurrentIdxDef) +
      Get_ORDER_BY_Text(CurrentIdxDef);
  finally
    CurrentIdxDef.Free;
  end;
end;

procedure TODBCTable.GenerateUpdateSQLs;

  function GenerateWhereClause: string;
  var
    Condition: string;

    KeyFields: string;
    KFName: string;
    Pos: Integer;
  begin
    KeyFields := PSGetKeyFields;

    Pos := 1;
    while Pos <= Length(KeyFields) do
    begin
      KFName := ExtractFieldName(KeyFields, Pos);

      if Condition <> '' then
        Condition := Condition + ' AND ';

      Condition := Condition + QuoteStr(KFName, FQuoteChar) + ' = :' + QuoteStr('OLD_' + KFName, BOR_QCHAR);
    end;

    if Condition <> '' then
      Result := ' WHERE ' + Condition
    else
      Result := '';
  end;

  function GenerateDelSQL: string;
  const
    DEL_SQL = 'DELETE FROM %s';
  var
    WhereClause: string;
  begin
    WhereClause := GenerateWhereClause;

    if WhereClause <> '' then
      Result := Format(DEL_SQL, [QuoteTableName]) + WhereClause
    else
      Result := SQL_NO_UNIQUE_KEY;
  end;

  function GenerateInsSQL: string;
  const
    INS_SQL = 'INSERT INTO %s (%s) VALUES (%s)';
  var
    InsFields: string;
    InsParams: string;
    i: Integer;
  begin
    for i := 0 to Fields.Count - 1 do
      with Fields[i] do
      begin
        if (FieldKind = fkData) and not (ReadOnly or (DataType = ftAutoInc)) then
        begin
          if InsFields <> '' then InsFields := InsFields + ', ';
          InsFields := InsFields + QuoteStr(FieldName, FQuoteChar);

          if InsParams <> '' then InsParams := InsParams + ', ';
          InsParams := InsParams + ':' + QuoteStr(FieldName, BOR_QCHAR);
        end;
      end;

    Result := Format(INS_SQL, [QuoteTableName, InsFields, InsParams]);
  end;

  function GenerateUpdSQL: string;
  const
    UPD_SQL = 'UPDATE %s SET %s';
  var
    UpdFields: string;
    WhereClause: string;

    i: Integer;
  begin
    WhereClause := GenerateWhereClause;

    for i := 0 to Fields.Count - 1 do
      with Fields[i] do
        if (FieldKind = fkData) and not (ReadOnly or (DataType = ftAutoInc)) then
        begin
          if UpdFields <> '' then
            UpdFields := UpdFields + ', ';

          UpdFields := UpdFields + QuoteStr(FieldName, FQuoteChar) + ' = :' + QuoteStr(FieldName, BOR_QCHAR);
        end;

    if WhereClause <> '' then
      Result := Format(UPD_SQL, [QuoteTableName, UpdFields]) + WhereClause
    else
      Result := SQL_NO_UNIQUE_KEY;
  end;

begin
  DeleteSQL.Text := GenerateDelSQL;
  InsertSQL.Text := GenerateInsSQL;
  ModifySQL.Text := GenerateUpdSQL;
end;

procedure TODBCTable.CheckTableName;
begin
  if Trim(FTableName) = '' then DatabaseError(SNoTableName);
end;

procedure TODBCTable.MasterChanged(Sender: TObject);
begin
  CheckBrowseMode;
  SetLinkRanges(FMasterLink.Fields);
  ApplyRange;
end;

procedure TODBCTable.MasterDisabled(Sender: TObject);
begin
  CancelRange;
end;


{ protected methods }

procedure TODBCTable.ActivateConnection;
begin
  inherited;
  FQuoteChar := Connection.QuoteChar;
end;

function TODBCTable.PSGetDefaultOrder: TIndexDef;
begin
  Result := GetCurrentIdxDef;
end;

function TODBCTable.PSGetKeyFields: string;
var
  IndexFound: Boolean;
  i, Pos: Integer;
begin
  Result := inherited PSGetKeyFields;
  if Result = '' then
  begin
    IndexFound := False;
    FIndexDefs.Update;

    for i := 0 to FIndexDefs.Count - 1 do
      if ixUnique in FIndexDefs[i].Options then
      begin
        Result := FIndexDefs[i].Fields;
        IndexFound := FieldCount = 0;
        if not IndexFound then
        begin
          Pos := 1;
          while Pos <= Length(Result) do
          begin
            IndexFound := FindField(ExtractFieldName(Result, Pos)) <> nil;
            if not IndexFound then Break;
          end;
        end;
        if IndexFound then Break;
      end;
    if not IndexFound then
      Result := '';
  end;
end;

function TODBCTable.PSGetTableName: string;
begin
  Result := FTableName;
end;

function TODBCTable.PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs;
begin
  Result := GetIndexDefs(FIndexDefs, IndexTypes);
end;

procedure TODBCTable.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    TableName := CommandText;
end;

procedure TODBCTable.PSSetParams(AParams: TParams);
begin
  if AParams.Count > 0 then
    Open;
  PSReset;
end;

procedure TODBCTable.DoOnNewRecord;
var
  i: Integer;
begin
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
    for i := 0 to FMasterLink.Fields.Count - 1 do
      IndexFields[i] := TField(FMasterLink.Fields[i]);

  inherited DoOnNewRecord;
end;

function TODBCTable.GetCanModify: Boolean;
begin
  Result := inherited GetCanModify and not ReadOnly;
end;

function TODBCTable.GetDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;

function TODBCTable.GetRecordSize: Word;
begin
  Result := FTableRecordSize;
end;

procedure TODBCTable.UpdateIndexDefs;
var
  IdxName, ColName: array[0..STR_LEN - 1] of SQLCHAR;
  non_unique: SmallInt;
  info_type: SmallInt;
  asc_or_desc: array[0..1] of SQLCHAR;
  SL_Ind: SQLINTEGER;

  StatTypes: TStrings;
  IdxDef: TIndexDef;
  IdxPos: Integer;
  IdxFields: string;

  PosIdx: Integer;
  SchemaNamePart: string;
  TableNamePart: string;

  TmpHandle: SQLHANDLE;
  RetCode: SQLRETURN;
  i: Integer;
begin
  if not Active then
    FieldDefs.Update;
  FIndexDefs.Clear;

  StatTypes := TStringList.Create;
  try
    // registering of statistic type
    StatTypes.Add('SQL_TABLE_STAT');              // do not localize
    StatTypes.Add('SQL_INDEX_CLUSTERED');         // do not localize
    StatTypes.Add('SQL_INDEX_HASHED');            // do not localize
    StatTypes.Add('SQL_INDEX_OTHER');             // do not localize

    // get Schema and Table name parts
    PosIdx := Pos(SCHEMA_SEPARATOR, FTableName);
    if PosIdx = 0 then
    begin
      SchemaNamePart := '';
      TableNamePart := FTableName;
    end
    else
    begin
      SchemaNamePart := Copy(FTableName, 1, PosIdx - 1);
      TableNamePart := Copy(FTableName, PosIdx + 1, Length(FTableName));
    end;

    // get Indexes
    RetCode := SQLAllocHandle(SQL_HANDLE_STMT, Connection.Handle, @TmpHandle);
    if not SQL_SUCCEEDED(RetCode) then
      ODBCError(SQL_HANDLE_DBC, Connection.Handle, RetCode);

    try
      RetCode := SQLStatistics(TmpHandle, nil, 0, PSQLCHAR(SchemaNamePart), SQL_NTS {0}, PSQLCHAR(TableNamePart), SQL_NTS, SQL_INDEX_ALL, SQL_QUICK);
      if not SQL_SUCCEEDED(RetCode) then
        ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

      // non_unique
      RetCode := SQLBindCol(TmpHandle, 4, SQL_C_SSHORT, @NON_UNIQUE, 0, @SL_Ind);
      if not SQL_SUCCEEDED(RetCode) then
        ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

      // index name
      RetCode := SQLBindCol(TmpHandle, 6, SQL_C_CHAR, @IdxName, STR_LEN, @SL_Ind);
      if not SQL_SUCCEEDED(RetCode) then
        ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

      // info_type
      RetCode := SQLBindCol(TmpHandle, 7, SQL_C_SSHORT, @info_type, 0, @SL_Ind);
      if not SQL_SUCCEEDED(RetCode) then
        ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

      // column name
      RetCode := SQLBindCol(TmpHandle, 9, SQL_C_CHAR, @ColName, STR_LEN, @SL_Ind);
      if not SQL_SUCCEEDED(RetCode) then
        ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

      // asc or desc
      RetCode := SQLBindCol(TmpHandle, 10, SQL_C_CHAR, @asc_or_desc, SizeOf(asc_or_desc), @SL_Ind);
      if not SQL_SUCCEEDED(RetCode) then
        ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

      // initialize the FieldDefs
      while SQL_SUCCEEDED(SQLFetch(TmpHandle)) do
      begin
        case info_type of
          SQL_INDEX_CLUSTERED, SQL_INDEX_HASHED, SQL_INDEX_OTHER:
            begin
              IdxPos := FIndexDefs.IndexOf(IdxName);
              if IdxPos < 0 then
              begin
                IdxDef := FIndexDefs.AddIndexDef;
                IdxDef.Name := IdxName;
                IdxDef.Fields := ColName;

                if asc_or_desc[0] = 'D' then      // do not localize
                  IdxDef.DescFields := ColName;

                if non_unique = SQL_FALSE then
                  IdxDef.Options := IdxDef.Options + [ixUnique];

                IdxDef.Source := StatTypes[info_type];
              end
              else
              begin
                IdxDef := FIndexDefs[IdxPos];
                IdxDef.Fields := IdxDef.Fields + '; ' + ColName; // do not localize
                if asc_or_desc[0] = 'D' then      // do not localize
                  IdxDef.DescFields := IdxDef.DescFields + '; ' + ColName; // do not localize
              end;
            end;
        else
          // do nothing (SQL_TABLE_STAT, SQL_INDEX_BTREE)
        end;
      end;

      // Close the cursor (the TmpHandle is still allocated).
      SQLFreeStmt(TmpHandle, SQL_CLOSE);


      // get primary key
      RetCode := SQLPrimaryKeys(TmpHandle, nil, 0, PSQLCHAR(SchemaNamePart) {nil}, SQL_NTS {0}, PSQLCHAR(TableNamePart), SQL_NTS);

      // some drivers does not support SQLPrimaryKeys function
      if SQL_SUCCEEDED(RetCode) then
      begin
        // column name
        RetCode := SQLBindCol(TmpHandle, 4, SQL_C_CHAR, @ColName, STR_LEN, @SL_Ind);
        if not SQL_SUCCEEDED(RetCode) then
          ODBCError(SQL_HANDLE_STMT, TmpHandle, RetCode);

        // initialize the FieldDefs
        while SQL_SUCCEEDED(SQLFetch(TmpHandle)) do
          if IdxFields = '' then
            IdxFields := ColName
          else
            IdxFields := IdxFields + '; ' + ColName; // do not localize

        for i := 0 to FIndexDefs.Count - 1 do
          if FIndexDefs[i].Fields = IdxFields then
            FIndexDefs[i].Options := FIndexDefs[i].Options + [ixPrimary];
      end;
    finally
      SQLFreeHandle(SQL_HANDLE_STMT, TmpHandle);
    end;
  finally
    StatTypes.Free;
  end;
end;

procedure TODBCTable.DataEvent(Event: TDataEvent; Info: Longint);
begin
  if (Event = dePropertyChange) and Assigned(FIndexDefs) then
  begin
    FIndexDefs.Updated := False;
    FRegenerateSQL := True;
  end;
  inherited DataEvent(Event, Info);
end;

procedure TODBCTable.DefChanged(Sender: TObject);
begin
  StoreDefs := True;
end;

procedure TODBCTable.InitFieldDefs;
var
  TmpQuery: TODBCStatement;
  TmpCol: TODBCColumn;
  i: Integer;
begin
  CheckTableName;

  if Handle <> nil then
    InternalInitFieldDefs
  else
  begin
    ActivateConnection;

    TmpQuery := TODBCStatement.Create(Self);
    try
      TmpQuery.Connection := Self.Connection;
      TmpQuery.SQL.Text := Format('SELECT * FROM %s WHERE 0=1', [QuoteTableName]); // do not localize
      TmpQuery.Cursor.GoToFirstRecord := False;
      TmpQuery.Execute;

      FieldDefs.BeginUpdate;
      try
        FieldDefs.Clear;

        for i := 1 to TmpQuery.Cursor.Columns.Count - 1 do // ignore Column[0] - reserved for ODBC bookmarks
          with FieldDefs.AddFieldDef do
          begin
            TmpCol := TmpQuery.Cursor.Columns[i];

            Name := TmpCol.Name;

            if TmpCol.IsAutoInc then
              DataType := ftAutoInc
            else
              DataType := SQLTypeToFieldType(TmpCol.SQLType);

            FieldNo := TmpCol.Index;
            Size := GetFieldDefSize(DataType, TmpCol.DataLength, TmpCol.DecimalDigits);
            Precision := TmpCol.ColumnSize;
            Required := not TmpCol.Nullable;
          end;
      finally
        FieldDefs.EndUpdate;
      end;
    finally
      TmpQuery.Free;
    end;
  end;
end;

procedure TODBCTable.InternalOpen;

  procedure OpenDS;
  begin
    InternalExecute;

    _Open := QStmt.IsCursorOpen;
    if not _Open then
      Abort;

    _RecordPos := -1;
    _DeletedRecordCount := 0;
    _PhyRecCount := 0;
    _OldRecordCount := 0;

    _ChunkCount := 0;
    _OldCacheSize := 0;

    // Set up the FCalcFieldsOfs, FBlobCacheOfs and FRecBufSize.
    _CalcFieldsOfs := SizeOf(TRecInfo) + GetRecordDataSize;
    _BlobCacheOfs := _CalcFieldsOfs + CalcFieldsSize;
    if CacheBlobs then
      _BlobBufferSize := {BlobFieldCount} QStmt.Cursor.BlobColumnCount * SizeOf(Pointer)
    else
      _BlobBufferSize := 0;

    _RecBufSize := _BlobCacheOfs + _BlobBufferSize;

    // allocate memory for chunks of records
    if UniDirectional then
      _ChunkSize := _RecBufSize * UniCache
    else
    begin
      _ChunkSize := _RecBufSize * _BufferChunks;
      _CacheList.Capacity := _BufferChunks;
    end;

    // allocate memory for old buffer
    _OldBuffer := AllocRecordBuffer;
    InitRecord(_OldBuffer);
  end;

begin
  CheckTableName;
  ActivateConnection;

  // update FieldDefs
  FieldDefs.Updated := False;
  FieldDefs.Update;

  // create and bind Fields
  if DefaultFields then
    CreateFields;
  BindFields(True);

  // init Field's offsets
  InitRecordMetaData;

  FTableRecordSize := GetTableRecBufSize;
  AllocKeyBuffers;

  UpdateCurrentIdxFlds;
  CheckMasterRange;

  // init Stmt
  QStmt.SQL.Text := GenerateSQL;
  QStmt.Params.Assign(FRangeParams);

  // Open Dataset
  OpenDS;

  // Generate update SQLs
  if FRegenerateSQL and not FReadOnly then
  begin
    GenerateUpdateSQLs;
    FRegenerateSQL := False;
  end;
end;

procedure TODBCTable.InternalClose;
begin
  inherited InternalClose;
  InternalUnPrepare;
  {+}
  FRegenerateSQL := True;
  {-}

  FreeKeyBuffers;

  FRangeSQL := '';
  FRangeParams.Clear;

  FTableRecordSize := 0;
end;

procedure TODBCTable.SetFiltered(Value: Boolean);
begin
  if Filtered <> Value then
  begin
    inherited SetFiltered(Value);
    if Active then
      ReopenTable;
  end;
end;

procedure TODBCTable.SetFilterText(const Value: string);
begin
  if Filter <> Value then
  begin
    inherited SetFilterText(value);
    if Active and Filtered then
      ReopenTable;
  end;
end;

procedure TODBCTable.SetFilterOptions(Value: TFilterOptions);
begin
  if Value <> [] then
    DatabaseError(SNotSupported);
end;

function TODBCTable.SetCursorRange: Boolean;
var
  RangeStart, RangeEnd: PKeyBuffer;
  StartKey, EndKey: PChar;
begin
  Result := False;
  if not (
    BuffersEqual(FKeyBuffers[kiRangeStart], FKeyBuffers[kiCurRangeStart], SizeOf(TKeyBuffer) + FTableRecordSize) and
    BuffersEqual(FKeyBuffers[kiRangeEnd], FKeyBuffers[kiCurRangeEnd], SizeOf(TKeyBuffer) + FTableRecordSize)) then
  begin
    RangeStart := FKeyBuffers[kiRangeStart];
    RangeEnd := FKeyBuffers[kiRangeEnd];

    StartKey := PChar(RangeStart) + SizeOf(TKeyBuffer);
    EndKey := PChar(RangeEnd) + SizeOf(TKeyBuffer);

    _SetRange(RangeStart.FieldCount, StartKey, not RangeStart.Exclusive, EndKey, not RangeEnd.Exclusive);

    Move(FKeyBuffers[kiRangeStart]^, FKeyBuffers[kiCurRangeStart]^, SizeOf(TKeyBuffer) + FTableRecordSize);
    Move(FKeyBuffers[kiRangeEnd]^, FKeyBuffers[kiCurRangeEnd]^, SizeOf(TKeyBuffer) + FTableRecordSize);

    Result := True;
  end;
end;

function TODBCTable.ResetCursorRange: Boolean;
begin
  Result := False;
  if FKeyBuffers[kiCurRangeStart].Modified or FKeyBuffers[kiCurRangeEnd].Modified then
  begin
    _DropRange;

    InitKeyBuffer(FKeyBuffers[kiCurRangeStart]);
    InitKeyBuffer(FKeyBuffers[kiCurRangeEnd]);

    Result := True;
  end;
end;

function TODBCTable.GetIsIndexField(Field: TField): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to IndexFieldCount - 1 do
    if GetIndexField(i) = Field then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TODBCTable.SetFieldData(Field: TField; Buffer: Pointer);
var
  Buff: PChar;
begin
  if not (State in dsWriteModes) then
    DatabaseError(SNotEditing, Self);

  if State = dsSetKey then
  begin
    if not GetIsIndexField(Field) then
      DatabaseErrorFmt(SNotIndexField, [Field.DisplayName], Self);

    Buff := PChar(FKeyBuffer) + SizeOf(TKeyBuffer);

    Field.Validate(Buffer);

    if Buffer = nil then
      Buff[MetaFields^[Field.FieldNo].NullFlagOfs] := FieldIsNull
    else
    begin
      Buff[MetaFields^[Field.FieldNo].NullFlagOfs] := FieldIsNotNull;
      Move(Buffer^, Buff[MetaFields^[Field.FieldNo].DataOfs], MetaFields^[Field.FieldNo].DataSize);
    end;
    SetModified(True);
  end
  else
  begin
    if Field.DataType = ftAutoInc then
      DatabaseErrorFmt(SFieldIsAutoInc, [Field.DisplayName], Self);

    inherited SetFieldData(Field, Buffer);
  end;
end;


{ public methods }

procedure TODBCTable.AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string);
const
  PKEY_SQL = 'ALTER TABLE %s ADD CONSTRAINT %s PRIMARY KEY (%s)'; {do not localize}
  UDKEY_SQL = 'CREATE UNIQUE INDEX %s ON %s (%s DESC)'; {do not localize}
  UKEY_SQL = 'CREATE UNIQUE INDEX %s ON %s (%s)'; {do not localize}
  DKEY_SQL = 'CREATE INDEX %s ON %s (%s DESC)';   {do not localize}
  KEY_SQL = 'CREATE INDEX %s ON %s (%s)';         {do not localize}

var
  FieldList: string;
begin
  FieldDefs.Update;

  if Active then
  begin
    CheckBrowseMode;
    CursorPosChanged;
  end;

  FieldList := FormatFieldList(Fields);

  with TODBCStatement.Create(Self) do
  try
    Connection := Self.Connection;

    if (ixPrimary in Options) then
      SQL.Text := Format(PKEY_SQL, [QuoteTableName, Name, FieldList])
    else
      if ([ixUnique, ixDescending] * Options = [ixUnique, ixDescending]) then
        SQL.Text := Format(UDKEY_SQL, [Name, QuoteTableName, FieldList])
      else
        if (ixUnique in Options) then
          SQL.Text := Format(UKEY_SQL, [Name, QuoteTableName, FieldList])
        else
          if (ixDescending in Options) then
            SQL.Text := Format(DKEY_SQL, [Name, QuoteTableName, FieldList])
          else
            SQL.Text := Format(KEY_SQL, [Name, QuoteTableName, FieldList]);

    Execute;

    FIndexDefs.Updated := False;
  finally
    Free;
  end;
end;

procedure TODBCTable.DeleteIndex(const Name: string);
const
  DRPIDX_SQL = 'DROP INDEX %s';                   {do not localize}
begin
  if Active then
    CheckBrowseMode;

  FIndexDefs.Update;

  with TODBCStatement.Create(Self) do
  try
    Connection := Self.Connection;
    SQL.Text := Format(DRPIDX_SQL, [Name]);
    Execute;

    FIndexDefs.Updated := False;
  finally
    Free;
  end;
end;

procedure TODBCTable.EmptyTable;
const
  EMPTLB_SQL = 'DELETE FROM %s';                  {do not localize}
begin
  CheckTableName;
  if Active then CheckBrowseMode;

  with TODBCStatement.Create(Self) do
  try
    Connection := Self.Connection;
    SQL.Text := Format(EMPTLB_SQL, [QuoteTableName]);
    Execute;

    if Active then
    begin
      ClearBuffers;
      DataEvent(deDataSetChange, 0);
    end;
  finally
    Free;
  end;
end;

procedure TODBCTable.GetDetailLinkFields(MasterFields, DetailFields: TList);
var
  Idx: TIndexDef;
  i: Integer;
begin
  MasterFields.Clear;
  DetailFields.Clear;

  if (MasterSource <> nil) and (MasterSource.DataSet <> nil) and (Self.MasterFields <> '') then
  begin
    Idx := nil;
    MasterSource.DataSet.GetFieldList(MasterFields, Self.MasterFields);
    UpdateIndexDefs;

    if IndexName <> '' then
      Idx := FIndexDefs.Find(IndexName)
    else
      if IndexFieldNames <> '' then
        Idx := FIndexDefs.GetIndexForFields(IndexFieldNames, False)
      else
        for i := 0 to FIndexDefs.Count - 1 do
          if ixPrimary in FIndexDefs[i].Options then
          begin
            Idx := FIndexDefs[i];
            Break;
          end;

    if Idx <> nil then
      GetFieldList(DetailFields, Idx.Fields);
  end;
end;

procedure TODBCTable.GetIndexNames(List: TStrings);
begin
  FIndexDefs.Update;
  FIndexDefs.GetItemNames(List);
end;

function TODBCTable.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  Buff, Data: PChar;
begin
  if State = dsSetKey then
  begin
    Buff := PChar(FKeyBuffer) + SizeOf(TKeyBuffer);

    Result := Buff[MetaFields^[Field.FieldNo].NullFlagOfs] <> FieldIsNull;
    if Result and (Buffer <> nil) then
    begin
      Data := PChar(Buff + MetaFields^[Field.FieldNo].DataOfs);
      Move(Data^, Buffer^, Field.DataSize);
    end;
  end
  else
    Result := inherited GetFieldData(Field, Buffer);
end;

procedure TODBCTable.Post;
begin
  inherited Post;
  if State = dsSetKey then
    PostKeyBuffer(True);
end;

procedure TODBCTable.Cancel;
begin
  inherited Cancel;
  if State = dsSetKey then
    PostKeyBuffer(False);
end;


{ Range methods }

procedure TODBCTable.EditKey;
begin
  SetKeyBuffer(kiLookup, False);
end;

procedure TODBCTable.EditRangeEnd;
begin
  SetKeyBuffer(kiRangeEnd, False);
end;

procedure TODBCTable.EditRangeStart;
begin
  SetKeyBuffer(kiRangeStart, False);
end;

procedure TODBCTable.SetKey;
begin
  SetKeyBuffer(kiLookup, True);
end;

procedure TODBCTable.SetRange(const StartValues, EndValues: array of const);
begin
  CheckBrowseMode;
  SetKeyFields(kiRangeStart, StartValues);
  SetKeyFields(kiRangeEnd, EndValues);
  ApplyRange;
end;

procedure TODBCTable.SetRangeEnd;
begin
  SetKeyBuffer(kiRangeEnd, True);
end;

procedure TODBCTable.SetRangeStart;
begin
  SetKeyBuffer(kiRangeStart, True);
end;

procedure TODBCTable.ApplyRange;
begin
  CheckBrowseMode;
  if SetCursorRange then First;
end;

procedure TODBCTable.CancelRange;
var
  FldCount: Integer;

  KeyFields: string;
  KeyValues: Variant;

  i: Integer;
begin
  CheckBrowseMode;

  FldCount := FCurrentIdxFlds.Count;
  KeyFields := '';

  // Save field values
  KeyValues := VarArrayCreate([0, FldCount - 1], varVariant);
  for i := 0 to FldCount - 1 do
  begin
    if KeyFields <> '' then
      KeyFields := KeyFields + ',';               // do not localize
    KeyFields := KeyFields + GetIndexField(i).FieldName;

    KeyValues[i] := GetIndexField(i).Value;
  end;

  // reset range and specify record position
  if ResetCursorRange then
    Locate(KeyFields, KeyValues, []);
end;

function TODBCTable.FindKey(const KeyValues: array of const): Boolean;
begin
  CheckBrowseMode;
  SetKeyFields(kiLookup, KeyValues);
  Result := GotoKey;
end;

procedure TODBCTable.FindNearest(const KeyValues: array of const);
begin
  CheckBrowseMode;
  SetKeyFields(kiLookup, KeyValues);
  GotoNearest;
end;

function TODBCTable.GotoKey: Boolean;
var
  KeyBuffer: PKeyBuffer;
  RecBuffer: PChar;
begin
  CheckBrowseMode;
  CursorPosChanged;

  KeyBuffer := FKeyBuffers[kiLookup];
  RecBuffer := PChar(KeyBuffer) + SizeOf(TKeyBuffer);

  Result := _GetRecordForKey(KeyBuffer.FieldCount, 0, RecBuffer, nil);
  if Result then
    Resync([rmExact, rmCenter]);
end;

procedure TODBCTable.GotoNearest;
var
  SearchCond: DBSearchCond;
  KeyBuffer: PKeyBuffer;
  RecBuffer: PChar;
begin
  CheckBrowseMode;

  KeyBuffer := FKeyBuffers[kiLookup];
  RecBuffer := PChar(KeyBuffer) + SizeOf(TKeyBuffer);

  if KeyBuffer^.Exclusive then
    SearchCond := keySEARCHGT
  else
    SearchCond := keySEARCHGEQ;

  _SetToKey(SearchCond, KeyBuffer.FieldCount, 0, RecBuffer);
  Resync([rmCenter]);
end;

end.
