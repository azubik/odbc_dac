unit CachedUp;

{ This demo uses the Informix demo-database "stores_demo".
  See the file ABOUT.TXT for a description of this demo. }

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, DB, StdCtrls, Menus,
  DBCtrls, Grids, DBGrids, ExtCtrls, Buttons;

type


  TCacheDemoForm = class(TForm)
    DBGrid: TDBGrid;
    MainMenu: TMainMenu;
    mmiAbout: TMenuItem;
    DBNavigator: TDBNavigator;
    GroupBox1: TGroupBox;
    UnmodifiedCB: TCheckBox;
    ModifiedCB: TCheckBox;
    InsertedCB: TCheckBox;
    DeletedCB: TCheckBox;
    Panel1: TPanel;
    CachedUpdates: TCheckBox;
    UseUpdateSQL: TCheckBox;
    Panel2: TPanel;
    ApplyUpdatesBtn: TButton;
    CancelUpdatesBtn: TButton;
    RevertRecordBtn: TButton;
    ReExecuteButton: TButton;
    btnConnect: TButton;

    procedure FormCreate(Sender: TObject);
    procedure ApplyUpdatesBtnClick(Sender: TObject);
    procedure ToggleUpdateMode(Sender: TObject);
    procedure CancelUpdatesBtnClick(Sender: TObject);
    procedure RevertRecordBtnClick(Sender: TObject);
    procedure UpdateRecordsToShow(Sender: TObject);
    procedure ReExecuteButtonClick(Sender: TObject);
    procedure UseUpdateSQLClick(Sender: TObject);
    procedure mmiAboutClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);

  private
    { Private declarations }
    procedure SetControlStates(Enabled: Boolean);

  public
    { Public declarations }
  end;

var
  CacheDemoForm: TCacheDemoForm;

implementation

{$R *.DFM}

uses
  About, ErrForm, DataMod,
  ODBCConnection;


procedure TCacheDemoForm.FormCreate(Sender: TObject);
begin
  CacheData.ODBCQuery.CachedUpdates := CachedUpdates.Checked;
  SetControlStates(CacheData.ODBCQuery.CachedUpdates);
end;

procedure TCacheDemoForm.btnConnectClick(Sender: TObject);
begin
  CacheData.ODBCConnection.DriverCompletion := dcPrompt;
  CacheData.ODBCConnection.Open;
  
  if CacheData.ODBCConnection.Connected then
    CacheData.ODBCQuery.Open;
end;


{ This method enables and disables controls when cached updates are
  turned on and off }

procedure TCacheDemoForm.SetControlStates(Enabled: Boolean);
begin
  ApplyUpdatesBtn.Enabled := Enabled;
  CancelUpdatesBtn.Enabled := Enabled;
  RevertRecordBtn.Enabled := Enabled;

  UnmodifiedCB.Enabled := Enabled;
  ModifiedCB.Enabled := Enabled;
  InsertedCB.Enabled := Enabled;
  DeletedCB.Enabled := Enabled;
end;

procedure TCacheDemoForm.ToggleUpdateMode(Sender: TObject);
begin
  { Toggle the state of the CachedUpdates property }
  CacheData.ODBCQuery.CachedUpdates := not CacheData.ODBCQuery.CachedUpdates;
  { Enable/Disable Controls }
  SetControlStates(CacheData.ODBCQuery.CachedUpdates);
end;

procedure TCacheDemoForm.ApplyUpdatesBtnClick(Sender: TObject);
begin
  CacheData.ODBCQuery.ApplyUpdates;
end;

procedure TCacheDemoForm.CancelUpdatesBtnClick(Sender: TObject);
begin
  CacheData.ODBCQuery.CancelUpdates;
end;

procedure TCacheDemoForm.RevertRecordBtnClick(Sender: TObject);
begin
  CacheData.ODBCQuery.RevertRecord;
end;

{ This event is triggered when the user checks or unchecks one
  of the "Show Records" check boxes.  It translates the states
  of the checkboxes into a set value which is required by the
  UpdateRecordTypes property of TDataSet.  The UpdateRecordTypes
  property controls what types of records are included in the
  dataset.  The default is to show only unmodified modified
  and inserted records.  To "undelete" a record, you would
  check the Deleted checkbox, then position the grid to the
  row you want to undelete and finally click the Revert Record
  Button }

procedure TCacheDemoForm.UpdateRecordsToShow(Sender: TObject);
var
  UpdRecTypes: TUpdateStatusSet;
begin
  UpdRecTypes := [];
  if UnModifiedCB.Checked then
    Include(UpdRecTypes, usUnModified);
  if ModifiedCB.Checked then
    Include(UpdRecTypes, usModified);
  if InsertedCB.Checked then
    Include(UpdRecTypes, usInserted);
  if DeletedCB.Checked then
    Include(UpdRecTypes, usDeleted);

  CacheData.ODBCQuery.UpdateRecordTypes := UpdRecTypes;
end;

procedure TCacheDemoForm.ReExecuteButtonClick(Sender: TObject);
begin
  CacheData.ODBCQuery.Close;
  CacheData.ODBCQuery.Open;
end;

procedure TCacheDemoForm.UseUpdateSQLClick(Sender: TObject);
begin
  CacheData.ODBCQuery.Close;
  if UseUpdateSQL.Checked then
    CacheData.ODBCQuery.UpdateObject := CacheData.ODBCUpdateSQL
  else
    CacheData.ODBCQuery.UpdateObject := nil;
  CacheData.ODBCQuery.Open;
end;

procedure TCacheDemoForm.mmiAboutClick(Sender: TObject);
begin
  with TfrmAboutBox.Create(Application) do
  try
    AboutMemo.Lines.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'ABOUT.TXT');
    ShowModal;
  finally
    Free;
  end;
end;

end.

