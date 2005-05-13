{$IFDEF VER170}
  {$DEFINE INLININGSUPPORTED}
  {$DEFINE COMPILER2005}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2005}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

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

{$IFDEF INLININGSUPPORTED}
  // comment the following define out if you want to avoid function inlining
  // which might increase executable size but will give better performance in
  // most cases.
  {$DEFINE USEINLINING}
{$ENDIF}

// uncomment the following define if you want CLX for Windows:
//{$DEFINE CLX}

// uncomment the following define if you want GR32 to behave like version
// 1.7.x and down
// Please note: support for this will be dropped at some time and is only
// provided for smooth transition
//{$DEFINE DEPRECATEDMODE}

// uncomment the following define if you want to have an OnChange / OnAreaChange
// event with safe pixel setters (PixelS, PixelTS, PixelFS etc.)
// Please note: while enabling this feature is generally better for the repaint
// and layer optimization in "lazy code" it's also much slower.
//{$DEFINE CHANGED_IN_PIXELS}

{$R-}{$Q-}  // switch off overflow and range checking

{$DEFINE XPTHEMES} // enable support for windows xp themes
