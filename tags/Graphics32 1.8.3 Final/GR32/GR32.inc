(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Andre Beckedorf <Andre@metaException.de>
 *
 * ***** END LICENSE BLOCK ***** *)

{$IFDEF VER180}
  {$DEFINE INLININGSUPPORTED}
  {$DEFINE COMPILER2006}
  {$DEFINE COMPILER2005}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2006}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

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


(* Symbol USEMOVE :
    Defines whether to use Move instead of MoveLongword. *)

  {-$DEFINE USEMOVE}


(* Symbol USEMULTITHREADING :
    Use multithreading by default if possible. *)

  {-$DEFINE USEMULTITHREADING}

(* FastCode specific symbols:
    Adding these symbols to your project's define list will force use of the
    particular routine over the standard GR32 routine.

   GR32_FASTCODEMOVE     - Uses the patched Move routine rather than MoveLongword.

   For more information on the FastCode project take a look at this URL:
     http://dennishomepage.gugs-cats.dk/FastCodeProject.htm

   Libraries can be downloaded here:
     http://dennishomepage.gugs-cats.dk/Libraries.htm

   For FastMove make sure to download the CPU id based function, ie. runtime
   RTL patching.
*)

{$IFDEF GR32_FASTCODEMOVE}
  {$DEFINE USEMOVE}
{$ENDIF}

{$R-}{$Q-}  // switch off overflow and range checking

{$IFDEF COMPILER5}
  {$IFNDEF CLX}
    {$DEFINE EXT_PROP_EDIT}
  {$ENDIF}
{$ENDIF}

{$DEFINE XPTHEMES} // enable support for windows xp themes
