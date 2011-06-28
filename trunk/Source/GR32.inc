(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
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
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Andre Beckedorf <Andre@metaException.de>
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Christian Budde <Christian@aixcoustic.com>
 *
 * ***** END LICENSE BLOCK ***** *)

 {$IFDEF VER220}
  {$DEFINE INLININGSUPPORTED}
  {$DEFINE COMPILERXE1}
  {$DEFINE COMPILER2010}
  {$DEFINE COMPILER2009}
  {$DEFINE COMPILER2007}
  {$DEFINE COMPILER2006}
  {$DEFINE COMPILER2005}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE1}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER210}
  {$DEFINE INLININGSUPPORTED}
  {$DEFINE COMPILER2010}
  {$DEFINE COMPILER2009}
  {$DEFINE COMPILER2007}
  {$DEFINE COMPILER2006}
  {$DEFINE COMPILER2005}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2010}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE INLININGSUPPORTED}
  {$DEFINE COMPILER2009}
  {$DEFINE COMPILER2007}
  {$DEFINE COMPILER2006}
  {$DEFINE COMPILER2005}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2009}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE INLININGSUPPORTED}
  {$DEFINE COMPILER2007}
  {$DEFINE COMPILER2006}
  {$DEFINE COMPILER2005}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2007}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER180}
  {-$DEFINE INLININGSUPPORTED} // disabled due to a compiler bug (http://qc.embarcadero.com/wc/qcmain.aspx?d=41166)
  {$DEFINE COMPILER2006}
  {$DEFINE COMPILER2005}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2006}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER170}
  {-$DEFINE INLININGSUPPORTED} // disabled due to a compiler bug (http://qc.embarcadero.com/wc/qcmain.aspx?d=41166)
  {$DEFINE COMPILER2005}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2005}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$IFNDEF BCB}
    {$DEFINE DELPHI7}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE COMPILER6}
  {$IFNDEF BCB}
    {$DEFINE DELPHI6}
  {$ELSE}
    {$DEFINE BCB6}
  {$ENDIF}
{$ENDIF}

(* Symbol TARGET_* :
    Defines the processor platform (x86, x64 or PowerPC)
*)

{$IFDEF FPC}
  {$DEFINE PLATFORM_INDEPENDENT}
  {$MODE Delphi}

  // Set up internal CPU target directives according to FPC directives
  {$IFDEF CPU386}
    {$IFDEF CPUI386}
      // target is an Intel 80386 or later.
      {$DEFINE TARGET_x86}
    {$ENDIF}

    {$IFDEF CPUX86_64}
      // target is a 64-bit processor (AMD or INTEL).
      {$DEFINE TARGET_x64}
    {$ENDIF}

    {$IFDEF CPUPOWERPC}
      // target is a 32-bit PowerPC or compatible.
      // currently only indirect support, added here as example on how to add
      // future specific targets
      {$DEFINE TARGET_POWERPC}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CPUX86_64}
    // target is a 64-bit processor (AMD or INTEL).
    {$DEFINE TARGET_x64}
    {$ASMMODE INTEL}
  {$ENDIF}

  // Widgetsets other then Windows will want to implement BITS_GETTER
  {$IFNDEF LCLWin32}
    {$DEFINE BITS_GETTER}
  {$ENDIF}
{$ELSE}
  // define default (delphi etc) target (32 bit Intel 80386 or later)
  {$DEFINE TARGET_x86}
{$ENDIF}


(* Symbol PUREPASCAL :
    Forces GR32 into pure pascal mode. *)
// FPC generates very efficient code, there is no need for assembler,
// which just causes trouble
{$IFDEF FPC}
//{$IFDEF TARGET_x86}
{-$IFNDEF TARGET_x64}
  {$DEFINE PUREPASCAL}
{-$ENDIF}
{$ENDIF}

// The DARWIN target of Free Pascal generates PIC code by default
// Which isn't compatible with the current Assembler
// So force PUREPASCAL
{$IFDEF DARWIN}
  {$DEFINE PUREPASCAL}
{$ENDIF}

{$IFNDEF COMPILER6}
{$IFNDEF FPC}
+--------------------------------------------------------------------+
|  Graphics32 may only be installed with Delphi/BCB 6 (or higher)    |
|   or Free Pascal / Lazarus                                         |
+--------------------------------------------------------------------+
{$ENDIF}
{$ENDIF}

{$IFDEF LINUX}
  {$IFNDEF FPC}
    {$DEFINE PLATFORM_INDEPENDENT}
    {$DEFINE BITS_GETTER}
  {$ENDIF}
  {$DEFINE UNIX}
{$ENDIF}

{$IFNDEF FPC}
  {$IFNDEF LINUX}
    {$DEFINE Windows}
  {$ENDIF}
{$ENDIF}

(* Symbol PUREPASCAL :
    Forces GR32 into pure pascal mode. Should be used only for testing and
    debugging purposes. *)
    {-$DEFINE PUREPASCAL}


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


(* Symbol USE_GUIDS_IN_MMF :
    Defines whether to use GUIDs for temporary MMFs filename instead of
    using the GetTempFilename in WinAPI. *)

  {-$DEFINE USE_GUIDS_IN_MMF}


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
     http://fastcode.sourceforge.net/

   For FastMove make sure to download the CPU id based function, ie. runtime
   RTL patching.
*)

{$IFDEF GR32_FASTCODEMOVE}
  {$DEFINE USEMOVE}
{$ENDIF}

{$R-}{$Q-}  // switch off overflow and range checking

{$IFDEF COMPILER5}
  {$DEFINE EXT_PROP_EDIT}
{$ENDIF}

{$IFNDEF FPC}
  {$DEFINE XPTHEMES} // enable support for windows xp themes
{$ENDIF}
