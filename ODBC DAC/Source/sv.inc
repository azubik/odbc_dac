
{--------------- User section ----------------------------}



{--------------- Softvector.com section ------------------}

{$DEFINE USE_DRV_SPEC} 
// by default on, 
// when applications use unique driver specific)


{$O+,A+,W-,R-,Q-,B-,X+,T-,P+,H+,J+,C-}


// Delphi 5 & CBuilder 5
{$IFDEF VER130}
  {$IFDEF BCB}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER5}

    {$ObjExportAll On}  { <- needed for compatibility with run-time packages in C++Builder 3+ }
  {$ELSE}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI5}
  {$ENDIF}

  {$DEFINE VCL50}
  {$DEFINE VCL50_OR_ABOVE}

  {$DEFINE VER130_OR_ABOVE}
{$ENDIF}

// Delphi 6
{$IFDEF VER140}
  {$IFDEF BCB}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER6}

    {$ObjExportAll On}  { <- needed for compatibility with run-time packages in C++Builder 3+ }
  {$ELSE}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI6}
  {$ENDIF}

  {$DEFINE VCL60}

  {$DEFINE VCL50_OR_ABOVE}
  {$DEFINE VCL60_OR_ABOVE}

  {$DEFINE VER130_OR_ABOVE}
  {$DEFINE VER140_OR_ABOVE}
{$ENDIF}

// Delphi 7
{$IFDEF VER150}
  {$IFDEF BCB}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER7}
  {$ELSE}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI7}
  {$ENDIF}

  {$DEFINE VCL70}

  {$DEFINE VCL50_OR_ABOVE}
  {$DEFINE VCL60_OR_ABOVE}
  {$DEFINE VCL70_OR_ABOVE}

  {$DEFINE VER130_OR_ABOVE}
  {$DEFINE VER140_OR_ABOVE}
  {$DEFINE VER150_OR_ABOVE}
{$ENDIF}


//Delphi 8
{$IFDEF VER160}
  {$IFDEF BCB}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER8}
  {$ELSE}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI8}
  {$ENDIF}

  {$DEFINE VCL80}

  {$DEFINE VCL50_OR_ABOVE}
  {$DEFINE VCL60_OR_ABOVE}
  {$DEFINE VCL70_OR_ABOVE}
  {$DEFINE VCL80_OR_ABOVE}

  {$DEFINE VER130_OR_ABOVE}
  {$DEFINE VER140_OR_ABOVE}
  {$DEFINE VER150_OR_ABOVE}
  {$DEFINE VER160_OR_ABOVE}
{$ENDIF}

//Delphi 9
{$IFDEF VER170}
  {$IFDEF BCB}
    {$DEFINE CBUILDER}
    {$DEFINE CBUILDER9}
  {$ELSE}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI9}
  {$ENDIF}

  {$DEFINE VCL90}

  {$DEFINE VCL50_OR_ABOVE}
  {$DEFINE VCL60_OR_ABOVE}
  {$DEFINE VCL70_OR_ABOVE}
  {$DEFINE VCL80_OR_ABOVE}
  {$DEFINE VCL90_OR_ABOVE}

  {$DEFINE VER130_OR_ABOVE}
  {$DEFINE VER140_OR_ABOVE}
  {$DEFINE VER150_OR_ABOVE}
  {$DEFINE VER160_OR_ABOVE}
  {$DEFINE VER170_OR_ABOVE}
{$ENDIF}


// Linux
{$IFDEF LINUX}
  {$UNDEF DELPHI6}
  {$DEFINE KYLIX}
  {$IFDEF VER140}
    {$DEFINE KYLIX2}
  {$ENDIF}
  {$IFDEF VER145}
    {$DEFINE KYLIX3}
    {$IFDEF BCB}
      {$DEFINE BCBKYLIX3}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF VER150}
    {$DEFINE KYLIX4}
  {$ENDIF}
{$ENDIF}

