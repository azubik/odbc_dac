//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("dclodbcdac_pro50c.res");
USEUNIT("..\ODBCReg.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("odbcdac_pro50c.bpi");
USEPACKAGE("dcldb50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclx50.bpi");
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
