//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ODBCConnection"
#pragma link "ODBCCustomDataset"
#pragma link "ODBCTable"
#pragma link "ODBCQuery"
#pragma resource "*.dfm"
TForm1 *Form1;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner): TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  ODBCConnection->DriverCompletion = dcPrompt;
  ODBCConnection->LoginPrompt = false;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnConnectClick(TObject *Sender)
{
  // open connection
  ODBCConnection->Open();

  // init buttons
  btnConnect->Enabled = false;
  btnDisconnect->Enabled = true;

  // init treeview
  InitTreeView();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnDisconnectClick(TObject *Sender)
{
  // clear controls
  TreeView->Selected = NULL;
  TreeView->Items->Clear();
  StatusBar->SimpleText = "";

  // close connection
  ODBCConnection->Close();

  // init buttons
  btnDisconnect->Enabled = false;
  btnConnect->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TreeViewChange(TObject *Sender, TTreeNode *Node)
{
  // close table
  ODBCTable->Close();

  // init table
  if (Node != NULL)
  {
    ODBCTable->TableName = Node->Text;
    ODBCTable->Open();
    
    StatusBar->SimpleText = Node->Text;
  }
}
//---------------------------------------------------------------------------
void TForm1::InitTreeView()
{
  // clear
  TreeView->Items->Clear();

  TStringList *Tables = new TStringList;
  ODBCConnection->GetTableNames(Tables, ttTable);

  for (int i=0;i<=Tables->Count - 1;i++)  {
    TreeView->Items->Add(NULL, Tables->Strings[i]);
  }
  Tables->Free();
}
