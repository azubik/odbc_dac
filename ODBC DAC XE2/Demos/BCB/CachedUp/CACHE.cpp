//----------------------------------------------------------------------------
//Borland C++Builder
//Copyright (c) 1987, 1998 Borland International Inc. All Rights Reserved.
//----------------------------------------------------------------------------
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USERES("cache.res");
USEFORMNS("cachedup.pas", CachedUp, CacheDemoForm);
USEFORMNS("datamod.pas", DataMod, CacheData);
USEFORMNS("errform.pas", ErrForm, UpdateErrorForm);
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
