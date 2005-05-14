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

(* Symbol CLX :
    Defines whether CLX/QT rather than VCL/WinAPI should be used as toolkit on Windows.
    NOTE:
    For Kylix/Linux support this symbol is set automatically. *)

  {-$DEFINE CLX}


(* Symbol DEPRECATEDMODE :
    Defines if GR32 should behave like version 1.7.x and down.
    NOTE:
    Support for this will be dropped at some time and is only provided for
    transition *)

  {-$DEFINE DEPRECATEDMODE}


(* Symbol CHANGED_IN_PIXELS :
    Defines if the OnChange / OnAreaChange event should be fired with
    safe pixel setters (PixelS, PixelTS, PixelFS etc.)
    NOTE:
    While enabling this feature is generally better for the repaint and layer
    optimization, in "lazy code" it is also much slower. *)

  {-$DEFINE CHANGED_IN_PIXELS}


{$IFDEF INLININGSUPPORTED}
(* Symbol USEINLINING :
    Defines whether to use function inlining.
    NOTE:
    Enabling this symbol increases executable size but will result in better
    performance in most cases. *)

  {$DEFINE USEINLINING}
{$ENDIF}

(* Symbol USEFILLCHAR :
    Defines whether to use FillChar instead of FillLongword. *)

  {-$DEFINE USEFILLCHAR}


(* Symbol USEMOVE :
    Defines whether to use Move instead of MoveLongword. *)

  {-$DEFINE USEMOVE}


(* FastCode specific symbols:
    Adding these symbols to your project's define list will force use of the
    particular routine over the standard GR32 routine.

   GR32_FASTCODEMOVE     - Uses the patched Move routine rather than MoveLongword.
   GR32_FASTCODEFILLCHAR - Uses the patched FillChar routine rather than FillLongword.

   For more information on the FastCode project take a look at this URL:
     http://dennishomepage.gugs-cats.dk/FastCodeProject.htm

   Libraries can be downloaded here:
     http://dennishomepage.gugs-cats.dk/Libraries.htm
*)

{$IFDEF GR32_FASTCODEMOVE}
  {$DEFINE USEMOVE}
{$ENDIF}

{$IFDEF GR32_FASTCODEFILLCHAR}
  {$DEFINE USEFILLCHAR}
{$ENDIF}

{$R-}{$Q-}  // switch off overflow and range checking

{$DEFINE XPTHEMES} // enable support for windows xp themes
