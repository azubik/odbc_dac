//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("odbcdac_pro50c.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\ODBCCatalogFunc.pas");
USEUNIT("..\ODBCConnection.pas");
USEUNIT("..\ODBCConsts.pas");
USEUNIT("..\ODBCCustomDataset.pas");
USEUNIT("..\ODBCDrvSpec.pas");
USEUNIT("..\ODBCException.pas");
USEUNIT("..\ODBCIntf.pas");
USEUNIT("..\ODBCQuery.pas");
USEUNIT("..\ODBCStmt.pas");
USEUNIT("..\ODBCStoredProc.pas");
USEUNIT("..\ODBCTable.pas");
USEUNIT("..\ODBCUpdateSQL.pas");
USEUNIT("..\ODBCUtils.pas");
USEUNIT("..\ODBCQueryEx.pas");
USEUNIT("..\ODBCCatalogFuncEx.pas");
USEUNIT("..\ODBC\odbcsql.pas");
USEUNIT("..\ODBC\odbcsqlext.pas");
USEUNIT("..\ODBC\odbcsqltypes.pas");
USEUNIT("..\ODBC\odbcsqlucode.pas");
USEPACKAGE("vcldb50.bpi");

//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
