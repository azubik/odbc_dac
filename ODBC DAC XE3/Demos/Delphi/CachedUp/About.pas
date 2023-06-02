unit About;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TfrmAboutBox = class(TForm)
    OKButton: TButton;
    Panel: TPanel;
    lbODBCDAC: TLabel;
    lbCachedUp: TLabel;
    plBevel: TPanel;
    AboutMemo: TMemo;
    BottomBevel: TPanel;
    procedure lb_httpRefClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation
uses
  ShellAPI;
{$R *.DFM}

procedure TfrmAboutBox.lb_httpRefClick(Sender: TObject);
begin
  ShellExecute(0,'open', 'www.softvector.com', nil, nil, SW_SHOWNORMAL);
end;

end.

