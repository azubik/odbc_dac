//---------------------------------------------------------------------------
// Borland C++Builder
// Copyright (c) 1987, 1998 Borland International Inc.  All Rights Reserved.
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFILE("readme.txt");
USEDATAMODULE("Datamod.pas", MastData);
USEFORM("about.pas", AboutBox);
USEFORM("brcstord.pas", BrCustOrdForm);
USEFORM("brparts.pas", BrPartsForm);
USEFORM("custrpt.pas", CustomerByInvoiceReport);
USEFORM("edcust.pas", EdCustForm);
USEFORM("edorders.pas", EdOrderForm);
USEFORM("edparts.pas", EdPartsForm);
USEFORM("invcrpt.pas", InvoiceByOrderNoReport);
USEFORM("main.pas", MainForm);
USEFORM("orderrpt.pas", OrdersByDateReport);
USEFORM("pickdate.pas", BrDateForm);
USEFORM("pickinvc.pas", PickOrderNoDlg);
USEFORM("pickrep.pas", PickRpt);
USEFORM("qrycust.pas", QueryCustDlg);
USEFORM("splash.pas", SplashForm);
USEFORM("srchdlg.pas", SearchDlg);
//---------------------------------------------------------------------------
#include "splash.h"

WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
     try
     {
       SplashForm = new TSplashForm(Application);
       SplashForm->Show();
       SplashForm->Update();

       Application->Initialize();
       Application->Title = "Marine Adventures Order Entry";
       Application->HelpFile = "Mastapp.hlp";
       Application->CreateForm(__classid(TMainForm), &MainForm);
       Application->CreateForm(__classid(TMastData), &MastData);
       Application->CreateForm(__classid(TAboutBox), &AboutBox);
       Application->CreateForm(__classid(TQueryCustDlg), &QueryCustDlg);
       Application->CreateForm(__classid(TBrPartsForm), &BrPartsForm);
       Application->CreateForm(__classid(TBrCustOrdForm), &BrCustOrdForm);
       Application->CreateForm(__classid(TEdCustForm), &EdCustForm);
       Application->CreateForm(__classid(TBrDateForm), &BrDateForm);
       Application->CreateForm(__classid(TSearchDlg), &SearchDlg);
       Application->CreateForm(__classid(TEdPartsForm), &EdPartsForm);
       Application->CreateForm(__classid(TEdOrderForm), &EdOrderForm);
       Application->CreateForm(__classid(TPickRpt), &PickRpt);
       Application->CreateForm(__classid(TPickOrderNoDlg), &PickOrderNoDlg);
       Application->CreateForm(__classid(TInvoiceByOrderNoReport), &InvoiceByOrderNoReport);
       Application->CreateForm(__classid(TCustomerByInvoiceReport), &CustomerByInvoiceReport);
       Application->CreateForm(__classid(TOrdersByDateReport), &OrdersByDateReport);
       SplashForm->Hide();
       SplashForm->Close();
       Application->Run();
   }
   catch (Exception &exception)
   {
       Application->ShowException(&exception);
   }
       return 0;
}
//---------------------------------------------------------------------------
