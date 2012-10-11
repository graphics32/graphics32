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

 {$I GR32_Compiler.inc}


(* Symbol PUREPASCAL:
   ------------------

      Forces GR32 into pure pascal mode. Should be used only for testing and
      debugging purposes.

      (Defined by default for FPC and DARWIN targets in GR32_Compilers.INC.)

*)

  {-$DEFINE PUREPASCAL}



(* Symbol DEPRECATEDMODE :
   -----------------------

      Defines if GR32 should behave like version 1.7.x and down.

   NOTE: Support for this will be dropped at some time and is only provided
   for transition *)

  {-$DEFINE DEPRECATEDMODE}



(* Symbol CHANGED_IN_PIXELS :
   --------------------------

      Defines if the OnChange / OnAreaChange event should be fired with
      safe pixel setters (PixelS, PixelTS, PixelFS etc.)

   NOTE: While enabling this feature is generally better for the repaint and
   layer optimization, in "lazy code" it is also much slower. *)

  {-$DEFINE CHANGED_IN_PIXELS}



(* Symbol USEINLINING :
   --------------------

      Defines whether to use function inlining.

   NOTE: Enabling this symbol increases executable size but will probably
   result in better performance in most cases. *)

  {$IFDEF INLININGSUPPORTED}
    {$DEFINE USEINLINING}
  {$ENDIF}



(* Symbol USE_GUIDS_IN_MMF :
   -------------------------

    Defines whether to use GUIDs for temporary MMFs filename instead of
    using the GetTempFilename in WinAPI. *)

{-$DEFINE USE_GUIDS_IN_MMF}



(* Symbol USEMOVE :
   ----------------

    Defines whether to use Move instead of MoveLongword. *)

{-$DEFINE USEMOVE}



(* Symbol XPTHEMES :
   -----------------

    Enable support for windows xp themes. Eventually undefine if GR32 should
    be used within a DLL *)

  {$DEFINE XPTHEMES}



(* Symbol USEMULTITHREADING :
   --------------------------

    Use multithreading by default if possible. *)

  {-$DEFINE USEMULTITHREADING}



(* FastCode specific symbols:
    Adding these symbols to your project's define list will force use of the
    particular routine over the standard GR32 routine.

   GR32_FASTCODEMOVE - Uses the patched Move routine rather than MoveLongword.

   For more information on the FastCode project take a look at this URL:
     http://fastcode.sourceforge.net/

   For FastMove make sure to download the CPU id based function, ie. runtime
   RTL patching.
*)

  {-$DEFINE GR32_FASTCODEMOVE}
  {$IFDEF GR32_FASTCODEMOVE}
    {$DEFINE USEMOVE}
  {$ENDIF}



(*
  Symbol OMIT_MMX:
  ----------------

     If defined MMX optimizations are not used (omitted)

   For faster pixel/color processing, MMX can be used which results in a huge
   performance boost over PUREPASCAL code or native assembler code.
   However, there's a penalty (a call to EMMS) when switching between FPU and
   MMX registers. This call is not necessary when SSE2 is available. Though,
   for backward compatibility it is necessary to call EMMS even if SSE2 is used.

   NOTE: On every x64 system SSE2 is available and thus MMX support is not
   necessary. In fact it is problematic in case the Delphi XE2 compiler is used.
*)

  {-$DEFINE OMIT_MMX}
  {$IFDEF TARGET_x64}
    {$DEFINE OMIT_MMX}
  {$ENDIF}


(*
  Symbol OMIT_SSE2:
  ----------------

     If defined SSE2 optimizations are not used (omitted)

   For faster pixel/color processing, SSE2 can be used which results in a huge
   performance boost over PUREPASCAL code or native assembler code.

*)

  {-$DEFINE OMIT_SSE2}
