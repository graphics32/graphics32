{$IFDEF VER150}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$IFNDEF BCB}
    {$DEFINE DELPHI7}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE COMPILER6}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$IFNDEF BCB}
    {$DEFINE DELPHI6}
  {$ELSE}
    {$DEFINE BCB6}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$IFNDEF BCB}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI4}
  {$ELSE}
    {$ObjExportAll On}
    {$DEFINE BCB5}
    {$DEFINE BCB4}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE COMPILER4}
  {$DEFINE BCB4}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE COMPILER4}
  {$DEFINE DELPHI4}
{$ENDIF}

{$IFNDEF COMPILER4}
//////////////////////////////////////////////////////////////////////
'  Graphics32 may only be installed with Delphi 4, Delphi 5,         '
'  Delphi6, Delphi7, C++ Builder 4, C++ Builder 5, or C++Builder 6 ! '
//////////////////////////////////////////////////////////////////////
{$ENDIF}

{$IFDEF DELPHI5}
  {$DEFINE EXT_PROP_EDIT}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE CLX}
{$ENDIF}

//{$DEFINE CLX}  // uncomment this if you want CLX for Windows

{$R-}{$Q-}  // switch off overflow and range checking

{$DEFINE XPTHEMES} // enable support for windows xp themes