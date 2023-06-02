//----------------------------------------------------------------------------
//Borland C++Builder
//Copyright (c) 1987, 1998 Borland International Inc. All Rights Reserved.
//----------------------------------------------------------------------------
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFILE("readme.txt");
USEFORM("cachedup.pas", CacheDemoForm);
USERES("cache.res");
USEDATAMODULE("datamod.pas", CacheData);
USEFORM("errform.pas", UpdateErrorForm);
USEFORM("about.pas", AboutDialog);
//---------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{

   try
   {
        Application->Initialize();
        Application->CreateForm(__classid(TCacheDemoForm), &CacheDemoForm);
        Application->CreateForm(__classid(TCacheData), &CacheData);
        Application->CreateForm(__classid(TUpdateErrorForm), &UpdateErrorForm);
        Application->Run();
   }
   catch (Exception &exception)
   {
        Application->ShowException(&exception);
   }
        return 0;
}
//---------------------------------------------------------------------------
