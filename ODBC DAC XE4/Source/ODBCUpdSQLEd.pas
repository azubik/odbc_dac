
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCUpdateSQL Component Editor                  }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  }
{                                                       }
{*******************************************************}

unit ODBCUpdSQLEd;
{$I sv.inc}
interface

uses
  SysUtils, Classes,
{$IFDEF WIN32}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  Forms, DB, ExtCtrls, StdCtrls, Controls, ComCtrls, Menus,
  ODBCConnection, ODBCCustomDataset, ODBCQuery, ODBCTable, ODBCUpdateSQL;


type

  TWaitMethod = procedure of object;

  TfrmODBCUpdateSQLEdit = class(TForm)
    OkButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    GenerateButton: TButton;
    PrimaryKeyButton: TButton;
    DefaultButton: TButton;
    UpdateTableName: TComboBox;
    FieldsPage: TTabSheet;
    SQLPage: TTabSheet;
    PageControl: TPageControl;
    KeyFieldList: TListBox;
    UpdateFieldList: TListBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    SQLMemo: TMemo;
    StatementType: TRadioGroup;
    QuoteFields: TCheckBox;
    GetTableFieldsButton: TButton;
    FieldListPopup: TPopupMenu;
    miSelectAll: TMenuItem;
    miClearAll: TMenuItem;
    FTempTable: TODBCTable;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure StatementTypeClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure DefaultButtonClick(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure PrimaryKeyButtonClick(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure GetTableFieldsButtonClick(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure UpdateTableNameChange(Sender: TObject);
    procedure UpdateTableNameClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure ClearAllClick(Sender: TObject);
    procedure SQLMemoKeyPress(Sender: TObject; var Key: Char);
  private
    FStmtIndex: Integer;
    FDataSet: TODBCCustomDataSet;
    FUpdateSQL: TODBCUpdateSQL;
    FSettingsChanged: Boolean;
    FDatasetDefaults: Boolean;
    SQLText: array[TUpdateKind] of TStrings;

    function GetTableRef(const TabName, QuoteChar: string): string;
    function Edit: Boolean;
    procedure GenWhereClause(const TabAlias, QuoteChar: string; KeyFields, SQL: TStrings);
    procedure GenDeleteSQL(const TableName, QuoteChar: string; KeyFields, SQL: TStrings);
    procedure GenInsertSQL(const TableName, QuoteChar: string; UpdateFields, SQL: TStrings);
    procedure GenModifySQL(const TableName, QuoteChar: string; KeyFields, UpdateFields, SQL: TStrings);
    procedure GenerateSQL;
    procedure GetDataSetFieldNames;
    procedure GetTableFieldNames;
    procedure InitGenerateOptions;
    procedure InitUpdateTableNames;
    procedure SetButtonStates;
    procedure SelectPrimaryKeyFields;
    procedure SetDefaultSelections;
    procedure ShowWait(WaitMethod: TWaitMethod);
    function TempTable: TODBCTable;
  end;

{ TSQLParser }

  TSQLToken = (stSymbol, stAlias, stNumber, stComma, stEQ, stOther, stLParen,
    stRParen, stEnd);

  TSQLParser = class
  private
    FText: string;
    FSourcePtr: PWideChar;
    FTokenPtr: PWideChar;
    FTokenString: string;
    FToken: TSQLToken;
    FSymbolQuoted: Boolean;
    function NextToken: TSQLToken;
    function TokenSymbolIs(const S: string): Boolean;
    procedure Reset;
  public
    constructor Create(const Text: string);
    procedure GetSelectTableNames(List: TStrings);
    procedure GetUpdateTableName(var TableName: string);
    procedure GetUpdateFields(List: TStrings);
    procedure GetWhereFields(List: TStrings);
  end;

function EditUpdateSQL(AUpdateSQL: TODBCUpdateSQL): Boolean;

implementation

{$R *.DFM}

uses ODBCConsts, LibHelp, TypInfo, Dialogs;

{ Global Interface functions }

function EditUpdateSQL(AUpdateSQL: TODBCUpdateSQL): Boolean;
begin
  with TfrmODBCUpdateSQLEdit.Create(Application) do
  try
    FUpdateSQL := AUpdateSQL;
    Result := Edit;
  finally
    Free;
  end;
end;

{ Utility Routines }

procedure GetSelectedItems(ListBox: TListBox; List: TStrings);
var
  i: Integer;
begin
  List.Clear;
  for i := 0 to ListBox.Items.Count - 1 do
    if ListBox.Selected[i] then
      List.Add(ListBox.Items[i]);
end;

function SetSelectedItems(ListBox: TListBox; List: TStrings): Integer;
var
  i: Integer;
begin
  Result := 0;
  ListBox.Items.BeginUpdate;
  try
    for i := 0 to ListBox.Items.Count - 1 do
      if List.IndexOf(ListBox.Items[i]) > -1 then
      begin
        ListBox.Selected[i] := True;
        Inc(Result);
      end
      else
        ListBox.Selected[i] := False;
    if ListBox.Items.Count > 0 then
    begin
      ListBox.ItemIndex := 0;
      ListBox.TopIndex := 0;
    end;
  finally
    ListBox.Items.EndUpdate;
  end;
end;

procedure SelectAll(ListBox: TListBox);
var
  i: Integer;
begin
  ListBox.Items.BeginUpdate;
  try
    with ListBox do
      for i := 0 to Items.Count - 1 do
        Selected[i] := True;
    if ListBox.Items.Count > 0 then
    begin
      ListBox.ItemIndex := 0;
      ListBox.TopIndex := 0;
    end;
  finally
    ListBox.Items.EndUpdate;
  end;
end;

procedure GetDataFieldNames(Dataset: TDataset; ErrorName: string; List: TStrings);
var
  i: Integer;
begin
  with Dataset do
  try
    FieldDefs.Update;
    List.BeginUpdate;
    try
      List.Clear;
      for i := 0 to FieldDefs.Count - 1 do
        List.Add(FieldDefs[i].Name);
    finally
      List.EndUpdate;
    end;
  except
    if ErrorName <> '' then
      MessageDlg(Format(SSQLDataSetOpen, [ErrorName]), mtError, [mbOK], 0);
  end;
end;

procedure GetSQLTableNames(const SQL: string; List: TStrings);
begin
  with TSQLParser.Create(SQL) do
  try
    GetSelectTableNames(List);
  finally
    Free;
  end;
end;

procedure ParseUpdateSQL(const SQL: string; var TableName: string; UpdateFields: TStrings; WhereFields: TStrings);
begin
  with TSQLParser.Create(SQL) do
  try
    GetUpdateTableName(TableName);
    if Assigned(UpdateFields) then
    begin
      Reset;
      GetUpdateFields(UpdateFields);
    end;
    if Assigned(WhereFields) then
    begin
      Reset;
      GetWhereFields(WhereFields);
    end;
  finally
    Free;
  end;
end;


{ TSQLParser }

constructor TSQLParser.Create(const Text: string);
begin
  FText := Text;
  FSourcePtr := PWideChar(Text);
  NextToken;
end;

function TSQLParser.NextToken: TSQLToken;
var
  P, TokenStart: PWideChar;
  QuoteChar: Char;
  IsParam: Boolean;

  function IsKatakana(const Chr: Byte): Boolean;
  begin
    Result := (SysLocale.PriLangID = LANG_JAPANESE) and (Chr in [$A1..$DF]);
  end;

begin
  if FToken = stEnd then SysUtils.Abort;
  FTokenString := '';
  FSymbolQuoted := False;
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do
    Inc(P);
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_', '$', #127..#255:
      begin
        TokenStart := P;
        if not SysLocale.FarEast then
        begin
          Inc(P);
          while CharInSet(P^, ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '"', '$', #127..#255]) do
            Inc(P);
        end
        else
        begin
          while TRUE do
          begin
            if CharInSet(P^, ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '"', '$']) or
              IsKatakana(Byte(P^)) then
              Inc(P)
            else
              if CharInSet(P^, LeadBytes) then
                Inc(P, 2)
              else
                Break;
          end;
        end;
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := stSymbol;
      end;
    '''', '"':
      begin
        QuoteChar := P^;
        Inc(P);
        IsParam := P^ = ':';
        if IsParam then Inc(P);
        TokenStart := P;
        while not CharInSet(P^, [QuoteChar, #0]) do
          Inc(P);
        SetString(FTokenString, TokenStart, P - TokenStart);
        Inc(P);
        Trim(FTokenString);
        FToken := stSymbol;
        FSymbolQuoted := True;
      end;
    '-', '0'..'9':
      begin
        TokenStart := P;
        Inc(P);
        while CharInSet(P^, ['0'..'9', '.', 'e', 'E', '+', '-']) do
          Inc(P);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := stNumber;
      end;
    ',':
      begin
        Inc(P);
        FToken := stComma;
      end;
    '=':
      begin
        Inc(P);
        FToken := stEQ;
      end;
    '(':
      begin
        Inc(P);
        FToken := stLParen;
      end;
    ')':
      begin
        Inc(P);
        FToken := stRParen;
      end;
    #0:
      FToken := stEnd;
  else
    begin
      FToken := stOther;
      Inc(P);
    end;
  end;
  FSourcePtr := P;
  if (FToken = stSymbol) and
    (FTokenString[Length(FTokenString)] = '.') then FToken := stAlias;
  Result := FToken;
end;

procedure TSQLParser.Reset;
begin
  FSourcePtr := PWideChar(FText);
  FToken := stSymbol;
  NextToken;
end;

function TSQLParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (FToken = stSymbol) and (CompareText(FTokenString, S) = 0);
end;

procedure TSQLParser.GetSelectTableNames(List: TStrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    if TokenSymbolIs('SELECT') then     { Do not localize }
    try
      while not TokenSymbolIs('FROM') do { Do not localize }
        NextToken;
      NextToken;
      while FToken = stSymbol do
      begin
        List.AddObject(FTokenString, Pointer(Integer(FSymbolQuoted)));
        if NextToken = stSymbol then NextToken;
        if FToken = stComma then
          NextToken
        else
          break;
      end;
    except
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TSQLParser.GetUpdateTableName(var TableName: string);
begin
  if TokenSymbolIs('UPDATE') and (NextToken = stSymbol) then { Do not localize }
    TableName := FTokenString
  else
    TableName := '';
end;

procedure TSQLParser.GetUpdateFields(List: TStrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    if TokenSymbolIs('UPDATE') then     { Do not localize }
    try
      while not TokenSymbolIs('SET') do
        NextToken;                      { Do not localize }
      NextToken;
      while True do
      begin
        if FToken = stAlias then NextToken;
        if FToken <> stSymbol then Break;
        List.Add(FTokenString);
        if NextToken <> stEQ then Break;
        while NextToken <> stComma do
          if TokenSymbolIs('WHERE') then Exit; { Do not localize }
        NextToken;
      end;
    except
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TSQLParser.GetWhereFields(List: TStrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    if TokenSymbolIs('UPDATE') then     { Do not localize }
    try
      while not TokenSymbolIs('WHERE') do
        NextToken;                      { Do not localize }
      NextToken;
      while True do
      begin
        while FToken in [stLParen, stAlias] do
          NextToken;
        if FToken <> stSymbol then Break;
        List.Add(FTokenString);
        if NextToken <> stEQ then Break;
        while True do
        begin
          NextToken;
          if FToken = stEnd then Exit;
          if TokenSymbolIs('AND') then Break; { Do not localize }
        end;
        NextToken;
      end;
    except
    end;
  finally
    List.EndUpdate;
  end;
end;


{ TUpdateSQLEditor }

function TfrmODBCUpdateSQLEdit.Edit: Boolean;
var
  Index: TUpdateKind;
  DataSetName: string;
begin
  Result := False;
  if Assigned(FUpdateSQL.DataSet) and (FUpdateSQL.DataSet is TODBCCustomDataSet) then
  begin
    FDataSet := FUpdateSQL.DataSet;
    FTempTable.Connection := TODBCCustomDataSetEx(FDataSet).Connection;
    DataSetName := Format('%s%s%s', [FDataSet.Owner.Name, DotSep, FDataSet.Name]);
  end
  else
    DataSetName := SNoDataSet;
  Caption := Format('%s%s%s (%s)', [FUpdateSQL.Owner.Name, DotSep, FUpdateSQL.Name, DataSetName]);
  try
    for Index := Low(TUpdateKind) to High(TUpdateKind) do
    begin
      SQLText[Index] := TStringList.Create;
      SQLText[Index].Assign(FUpdateSQL.SQL[Index]);
    end;
    StatementTypeClick(Self);
    InitUpdateTableNames;
    ShowWait(InitGenerateOptions);
    PageControl.ActivePage := PageControl.Pages[0];
    if ShowModal = mrOk then
    begin
      for Index := Low(TUpdateKind) to High(TUpdateKind) do
        FUpdateSQL.SQL[Index] := SQLText[Index];
      Result := True;
    end;
  finally
    for Index := Low(TUpdateKind) to High(TUpdateKind) do
      SQLText[Index].Free;
  end;
end;

procedure TfrmODBCUpdateSQLEdit.GenWhereClause(const TabAlias, QuoteChar: string; KeyFields, SQL: TStrings);
var
  i: Integer;
  BindText: string;
  FieldName: string;
begin
  SQL.Add('WHERE');                     { Do not localize }
  for i := 0 to KeyFields.Count - 1 do
  begin
    FieldName := KeyFields[i];
    BindText := Format('  %s%s%s%1:s = :%1:sOLD_%2:s%1:s', { Do not localize }
      [TabAlias, QuoteChar, FieldName]);
    if i < KeyFields.Count - 1 then
      BindText := Format('%s AND', [BindText]); { Do not localize }
    SQL.Add(BindText);
  end;
end;

procedure TfrmODBCUpdateSQLEdit.GenDeleteSQL(const TableName, QuoteChar: string; KeyFields, SQL: TStrings);
begin
  SQL.Clear;
  SQL.Add(Format('DELETE FROM %s', [TableName])); { Do not localize }
  GenWhereClause(GetTableRef(TableName, QuoteChar), QuoteChar, KeyFields, SQL);
end;

procedure TfrmODBCUpdateSQLEdit.GenInsertSQL(const TableName, QuoteChar: string; UpdateFields, SQL: TStrings);

  procedure GenFieldList(const TabName, ParamChar, QuoteChar: string);
  var
    L: string;
    i: integer;
    Comma: string;
  begin
    L := '  (';
    Comma := ', ';
    for i := 0 to UpdateFields.Count - 1 do
    begin
      if i = UpdateFields.Count - 1 then Comma := '';
      L := Format('%s%s%s%s%s%3:s%5:s',
        [L, TabName, ParamChar, QuoteChar, UpdateFields[i], Comma]);
      if (Length(L) > 70) and (i <> UpdateFields.Count - 1) then
      begin
        SQL.Add(L);
        L := '   ';
      end;
    end;
    SQL.Add(L + ')');
  end;

begin
  SQL.Clear;
  SQL.Add(Format('INSERT INTO %s', [TableName])); { Do not localize }
  GenFieldList(GetTableRef(TableName, QuoteChar), '', QuoteChar);
  SQL.Add('VALUES');                    { Do not localize }
  GenFieldList('', ':', QuoteChar);
end;

procedure TfrmODBCUpdateSQLEdit.GenModifySQL(const TableName, QuoteChar: string; KeyFields, UpdateFields, SQL: TStrings);
var
  Comma: string;
  TableRef: string;
  i: Integer;
begin
  SQL.Clear;
  SQL.Add(Format('UPDATE %s', [TableName])); { Do not localize }
  SQL.Add('SET');                       { Do not localize }
  Comma := ',';
  TableRef := GetTableRef(TableName, QuoteChar);
  for i := 0 to UpdateFields.Count - 1 do
  begin
    if i = UpdateFields.Count - 1 then Comma := '';
    SQL.Add(Format('  %s%s%s%1:s = :%1:s%2:s%1:s%3:s',
      [TableRef, QuoteChar, UpdateFields[i], Comma]));
  end;
  GenWhereClause(TableRef, QuoteChar, KeyFields, SQL);
end;

procedure TfrmODBCUpdateSQLEdit.GenerateSQL;
var
  KeyFields: TStringList;
  UpdateFields: TStringList;
  QuoteChar, TableName: string;
begin
  if (KeyFieldList.SelCount = 0) or (UpdateFieldList.SelCount = 0) then
    raise Exception.CreateRes(@SSQLGenSelect);

  KeyFields := TStringList.Create;
  try
    GetSelectedItems(KeyFieldList, KeyFields);
    UpdateFields := TStringList.Create;
    try
      GetSelectedItems(UpdateFieldList, UpdateFields);
      TableName := UpdateTableName.Text;

      if QuoteFields.Checked then
        QuoteChar := '"'
      else
        QuoteChar := '';

      GenDeleteSQL(TableName, QuoteChar, KeyFields, SQLText[ukDelete]);
      GenInsertSQL(TableName, QuoteChar, UpdateFields, SQLText[ukInsert]);
      GenModifySQL(TableName, QuoteChar, KeyFields, UpdateFields, SQLText[ukModify]);

      SQLMemo.Modified := False;
      StatementTypeClick(Self);
      PageControl.SelectNextPage(True);
    finally
      UpdateFields.Free;
    end;
  finally
    KeyFields.Free;
  end;
end;

procedure TfrmODBCUpdateSQLEdit.GetDataSetFieldNames;
begin
  if Assigned(FDataSet) then
  begin
    GetDataFieldNames(FDataSet, FDataSet.Name, KeyFieldList.Items);
    UpdateFieldList.Items.Assign(KeyFieldList.Items);
  end;
end;

procedure TfrmODBCUpdateSQLEdit.GetTableFieldNames;
begin
  GetDataFieldNames(TempTable, TempTable.TableName, KeyFieldList.Items);
  UpdateFieldList.Items.Assign(KeyFieldList.Items);
  FDatasetDefaults := False;
end;

function TfrmODBCUpdateSQLEdit.GetTableRef(const TabName, QuoteChar: string): string;
begin
  if QuoteChar <> '' then
    Result := TabName + '.'
  else
    Result := '';
end;

procedure TfrmODBCUpdateSQLEdit.InitGenerateOptions;
var
  UpdTabName: string;

  procedure InitFromDataSet;
  begin
    // If this is a Query with more than 1 table in the "from" clause then
    //  initialize the list of fields from the table rather than the dataset.
    if UpdateTableName.Items.Count > 1 then
      GetTableFieldNames
    else
    begin
      GetDataSetFieldNames;
      FDatasetDefaults := True;
    end;
    SetDefaultSelections;
  end;

  procedure InitFromUpdateSQL;
  var
    UpdFields,
      WhFields: TStrings;
  begin
    UpdFields := TStringList.Create;
    try
      WhFields := TStringList.Create;
      try
        ParseUpdateSQL(SQLText[ukModify].Text, UpdTabName, UpdFields, WhFields);
        GetDataSetFieldNames;
        if SetSelectedItems(UpdateFieldList, UpdFields) < 1 then
          SelectAll(UpdateFieldList);
        if SetSelectedItems(KeyFieldList, WhFields) < 1 then
          SelectAll(KeyFieldList);
      finally
        WhFields.Free;
      end;
    finally
      UpdFields.Free;
    end;
  end;

begin
  // If there are existing update SQL statements, try to initialize the
  // dialog with the fields that correspond to them.
  if SQLText[ukModify].Count > 0 then
  begin
    ParseUpdateSQL(SQLText[ukModify].Text, UpdTabName, nil, nil);
    // If the table name from the update statement is not part of the
    // dataset, then initialize from the dataset instead.
    if (UpdateTableName.Items.Count > 0) and
      (UpdateTableName.Items.IndexOf(UpdTabName) > -1) then
    begin
      UpdateTableName.Text := UpdTabName;
      InitFromUpdateSQL;
    end
    else
    begin
      InitFromDataSet;
      UpdateTableName.Items.Add(UpdTabName);
    end;
  end
  else
    InitFromDataSet;
  SetButtonStates;
end;

procedure TfrmODBCUpdateSQLEdit.InitUpdateTableNames;
begin
  UpdateTableName.Items.Clear;
  if Assigned(FDataSet) then
  begin
    if FDataSet is TODBCQuery then
      GetSQLTableNames(TODBCQuery(FDataSet).SQL.Text, UpdateTableName.Items)
    else
      if (FDataSet is TODBCTable) and (TODBCTable(FDataSet).TableName <> '') then
        UpdateTableName.Items.Add(TODBCTable(FDataSet).TableName);
  end;
  if UpdateTableName.Items.Count > 0 then
    UpdateTableName.ItemIndex := 0;
end;

procedure TfrmODBCUpdateSQLEdit.SetButtonStates;
begin
  GetTableFieldsButton.Enabled := UpdateTableName.Text <> '';
  PrimaryKeyButton.Enabled := GetTableFieldsButton.Enabled and (KeyFieldList.Items.Count > 0);
  GenerateButton.Enabled := GetTableFieldsButton.Enabled and (UpdateFieldList.Items.Count > 0) and (KeyFieldList.Items.Count > 0);
  DefaultButton.Enabled := Assigned(FDataSet) and not FDatasetDefaults;
end;

procedure TfrmODBCUpdateSQLEdit.SelectPrimaryKeyFields;
var
  FName, FieldNames: string;
  SepPos, Index: Integer;
  i: Integer;
begin
  if KeyFieldList.Items.Count < 1 then Exit;
  with TempTable do
  begin
    IndexDefs.Update;
    for i := 0 to KeyFieldList.Items.Count - 1 do
      KeyFieldList.Selected[i] := False;
    for i := 0 to IndexDefs.Count - 1 do
      if ixPrimary in IndexDefs[i].Options then
      begin
        FieldNames := IndexDefs[i].Fields + ';';
        while Length(FieldNames) > 0 do
        begin
          SepPos := Pos(';', FieldNames);
          if SepPos < 1 then Break;
          FName := Copy(FieldNames, 1, SepPos - 1);
          System.Delete(FieldNames, 1, SepPos);
          Index := KeyFieldList.Items.IndexOf(FName);
          if Index > -1 then KeyFieldList.Selected[Index] := True;
        end;
        Break;
      end;
  end;
end;

procedure TfrmODBCUpdateSQLEdit.SetDefaultSelections;
var
  DSFields: TStringList;
begin
  if FDatasetDefaults or not Assigned(FDataSet) then
  begin
    SelectAll(UpdateFieldList);
    SelectAll(KeyFieldList);
  end
  else
    if FDataSet.FieldDefs.Count > 0 then
    begin
      DSFields := TStringList.Create;
      try
        GetDataFieldNames(FDataSet, '', DSFields);
        SetSelectedItems(KeyFieldList, DSFields);
        SetSelectedItems(UpdateFieldList, DSFields);
      finally
        DSFields.Free;
      end;
    end;
end;

procedure TfrmODBCUpdateSQLEdit.ShowWait(WaitMethod: TWaitMethod);
begin
  Screen.Cursor := crHourGlass;
  try
    WaitMethod;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TfrmODBCUpdateSQLEdit.TempTable: TODBCTable;
begin
  if FTempTable.TableName <> UpdateTableName.Text then
  begin
    FTempTable.Close;
    FTempTable.TableName := UpdateTableName.Text;
  end;
  Result := FTempTable;
end;

{ Event Handlers }

procedure TfrmODBCUpdateSQLEdit.FormCreate(Sender: TObject);
begin
  HelpContext := hcDUpdateSQL;
end;

procedure TfrmODBCUpdateSQLEdit.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TfrmODBCUpdateSQLEdit.StatementTypeClick(Sender: TObject);
begin
  if SQLMemo.Modified then
    SQLText[TUpdateKind(FStmtIndex)].Assign(SQLMemo.Lines);

  FStmtIndex := StatementType.ItemIndex;
  SQLMemo.Lines.Assign(SQLText[TUpdateKind(FStmtIndex)]);
end;

procedure TfrmODBCUpdateSQLEdit.OkButtonClick(Sender: TObject);
begin
  if SQLMemo.Modified then
    SQLText[TUpdateKind(FStmtIndex)].Assign(SQLMemo.Lines);
end;

procedure TfrmODBCUpdateSQLEdit.DefaultButtonClick(Sender: TObject);
begin
  with UpdateTableName do
    if Items.Count > 0 then ItemIndex := 0;

  ShowWait(GetDataSetFieldNames);
  FDatasetDefaults := True;
  SetDefaultSelections;
  KeyfieldList.SetFocus;
  SetButtonStates;
end;

procedure TfrmODBCUpdateSQLEdit.GenerateButtonClick(Sender: TObject);
begin
  GenerateSQL;
  FSettingsChanged := False;
end;

procedure TfrmODBCUpdateSQLEdit.PrimaryKeyButtonClick(Sender: TObject);
begin
  ShowWait(SelectPrimaryKeyFields);
  SettingsChanged(Sender);
end;

procedure TfrmODBCUpdateSQLEdit.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if (PageControl.ActivePage = PageControl.Pages[0]) and not SQLPage.Enabled then
    AllowChange := False;
end;

procedure TfrmODBCUpdateSQLEdit.GetTableFieldsButtonClick(Sender: TObject);
begin
  ShowWait(GetTableFieldNames);
  SetDefaultSelections;
  SettingsChanged(Sender);
end;

procedure TfrmODBCUpdateSQLEdit.SettingsChanged(Sender: TObject);
begin
  FSettingsChanged := True;
  FDatasetDefaults := False;
  SetButtonStates;
end;

procedure TfrmODBCUpdateSQLEdit.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrOK) and FSettingsChanged then
    CanClose := MessageDlg(SSQLNotGenerated, mtConfirmation, mbYesNoCancel, 0) = mrYes;
end;

procedure TfrmODBCUpdateSQLEdit.UpdateTableNameChange(Sender: TObject);
begin
  SettingsChanged(Sender);
end;

procedure TfrmODBCUpdateSQLEdit.UpdateTableNameClick(Sender: TObject);
begin
  if not Visible then Exit;
  GetTableFieldsButtonClick(Sender);
end;

procedure TfrmODBCUpdateSQLEdit.SelectAllClick(Sender: TObject);
begin
  SelectAll(FieldListPopup.PopupComponent as TListBox);
end;

procedure TfrmODBCUpdateSQLEdit.ClearAllClick(Sender: TObject);
var
  i: Integer;
begin
  with FieldListPopup.PopupComponent as TListBox do
  begin
    Items.BeginUpdate;
    try
      for i := 0 to Items.Count - 1 do
        Selected[i] := False;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TfrmODBCUpdateSQLEdit.SQLMemoKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then Close;
end;

end.

