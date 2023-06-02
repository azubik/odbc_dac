unit CustRpt;

interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, Quickrpt, QRCtrls;

type
  TCustomerByInvoiceReport = class(TQuickRep)
    DetailBand1: TQRBand;
    QRExpr1: TQRExpr;
    QRDBText2: TQRDBText;
    QRDBText3: TQRDBText;
    QRDBText4: TQRDBText;
    QRDBText5: TQRDBText;
    ColumnHeaderBand1: TQRBand;
    QRLabel4: TQRLabel;
    QRLabel5: TQRLabel;
    QRLabel6: TQRLabel;
    QRLabel7: TQRLabel;
    QRLabel8: TQRLabel;
    QRLabel9: TQRLabel;
    TitleBand1: TQRBand;
    QRLabel1: TQRLabel;
    QRLabel2: TQRLabel;
    QRLabel3: TQRLabel;
    QRDBText1: TQRDBText;
    procedure QRDBText1Print(sender: TObject; var Value: string);
  private

  public

  end;

var
  CustomerByInvoiceReport: TCustomerByInvoiceReport;

implementation

{$R *.DFM}

uses DataMod;

procedure TCustomerByInvoiceReport.QRDBText1Print(Sender: TObject; var Value: string);
begin
  with MastData do
    Value := DateToStr(CustByLastInvQueryLastInvoiceDate.Value);
end;

end.

