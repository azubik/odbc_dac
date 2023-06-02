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
USEFORMNS("about.pas", About, AboutBox);
USEFORMNS("brcstord.pas", brcstord, BrCustOrdForm);
USEFORMNS("brparts.pas", brparts, BrPartsForm);
USEFORMNS("custrpt.pas", custrpt, CustomerByInvoiceReport);
USEFORMNS("edcust.pas", edcust, EdCustForm);
USEFORMNS("edorders.pas", edorders, EdOrderForm);
USEFORMNS("edparts.pas", edparts, EdPartsForm);
USEFORMNS("invcrpt.pas", invcrpt, InvoiceByOrderNoReport);
USEFORMNS("main.pas", Main, MainForm);
USEFORMNS("orderrpt.pas", orderrpt, OrdersByDateReport);
USEFORMNS("pickdate.pas", pickdate, BrDateForm);
USEFORMNS("pickinvc.pas", pickinvc, PickOrderNoDlg);
USEFORMNS("pickrep.pas", pickrep, PickRpt);
USEFORMNS("qrycust.pas", qrycust, QueryCustDlg);
USEFORMNS("splash.pas", splash, SplashForm);
USEFORMNS("srchdlg.pas", srchdlg, SearchDlg);
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
