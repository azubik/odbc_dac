//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <DBCtrls.hpp>
#include <DBGrids.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include "ODBCConnection.hpp"
#include "ODBCCustomDataset.hpp"
#include "ODBCTable.hpp"
#include "ODBCQuery.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TPanel *plTop;
        TSplitter *Splitter;
        TPageControl *PageControl;
        TTabSheet *TabSheet;
        TDataSource *DataSource;
        TDBGrid *dbgData;
        TDBNavigator *DBNavigator;
        TButton *btnConnect;
        TButton *btnDisconnect;
        TODBCConnection *ODBCConnection;
        TODBCTable *ODBCTable;
        TPanel *plLeft;
        TTreeView *TreeView;
        TLabel *Label1;
        TStatusBar *StatusBar;
        void __fastcall btnConnectClick(TObject *Sender);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall btnDisconnectClick(TObject *Sender);
        void __fastcall TreeViewChange(TObject *Sender, TTreeNode *Node);
private:
        void InitTreeView();
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
