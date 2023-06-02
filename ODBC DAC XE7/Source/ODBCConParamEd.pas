
{*******************************************************}
{                                                       }
{       ODBC DAC Pro                                    }
{                                                       }
{       ODBCConnection Component Editor                 }
{                                                       }
{       GNU GENERAL PUBLIC LICENSE                      }
{       Version 3                                       }
{                                                       }
{       e-mail: andrey.zubik@gmail.com                  } 
{                                                       }
{*******************************************************}

unit ODBCConParamEd;
{$I sv.inc}
interface

uses SysUtils, Classes,
  Windows,
  Forms, Controls, StdCtrls, Buttons, ComCtrls, ExtCtrls, Dialogs, Mask,
  ODBCConnection;


type
  TfrmConParamEd = class(TForm)
    plBase: TPanel;
    plBottom: TPanel;
    PageControl: TPageControl;
    tabParams: TTabSheet;
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnHelp: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    tabAbout: TTabSheet;
    lbProdName: TLabel;
    lbVersion: TLabel;
    lbCopyRight: TLabel;
    lb_httpRef: TLabel;
    plButtons: TPanel;
    btnLoad: TButton;
    btnSave: TButton;
    btnGenerate: TButton;
    Memo: TMemo;
    btnClear: TButton;
    procedure btnClearClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);

    procedure FormShow(Sender: TObject);

  private
    FConnection: TODBCConnection;

    function Edit: Boolean;
  end;

function EditParams(AComponent: TComponent): Boolean;

implementation
uses
  TypInfo, ODBCConsts;

{$R *.DFM}


function EditParams(AComponent: TComponent): Boolean;
begin
  with TfrmConParamEd.Create(Application) do
  try
    FConnection := AComponent as TODBCConnection;
    Caption := Format('%s%s%s %s', [AComponent.Owner.Name, DotSep, AComponent.Name, 'Params']);
    Memo.Lines.Assign(FConnection.Params);

    Result := Edit;
  finally
    Free;
  end;
end;



{ TfrmCP }

function TfrmConParamEd.Edit: Boolean;
begin
  Result := False;
  if ShowModal = mrOk then
  begin
    FConnection.Params := Memo.Lines;
    Result := True;
  end;
end;

procedure TfrmConParamEd.btnClearClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

procedure TfrmConParamEd.btnGenerateClick(Sender: TObject);
var
  TmpConnection: TODBCConnection;
begin
  TmpConnection := TODBCConnection.Create(nil);
  try
    TmpConnection.DriverCompletion := dcPrompt;
    TmpConnection.LoginPrompt := False;

    TmpConnection.Open;
    TmpConnection.Close;

    if TmpConnection.Params.Count > 0 then
      Memo.Lines.Assign(TmpConnection.Params);
  finally
    TmpConnection.Free;
  end;
end;

procedure TfrmConParamEd.btnLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    Memo.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TfrmConParamEd.btnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    Memo.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TfrmConParamEd.FormShow(Sender: TObject);
begin
  Memo.Lines.Assign(FConnection.Params);
  lbVersion.Caption := lbVersion.Caption + SODBC_DAC_Version; 
end;

end.

