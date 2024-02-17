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


(*
  Available Compilers:
  --------------------

    COMPILERFPC  - FPC (since version 2.6.0)
    COMPILER6    - Delphi 6
    COMPILER7    - Delphi 7
    COMPILER2005 - Delphi 2005
    COMPILER2006 - Delphi 2006 / BDS 2006
    COMPILER2007 - Delphi / RAD Studio 2007
    COMPILER2009 - Delphi 2009
    COMPILER2010 - Delphi 2010
    COMPILERXE1  - Delphi XE
    COMPILERXE2  - Delphi XE2
    COMPILERXE3  - Delphi XE3


  Available Targets:
  ------------------

    TARGET_x86     - x86 (32-Bit)
    TARGET_x64     - x86 (64-Bit)
    TARGET_POWERPC - Power PC

*)

{$DEFINE VER_LATEST}

{$IFDEF FPC}
  {$DEFINE COMPILERFPC}
{$ENDIF}

{$IFDEF VER_LATEST} // adjust for newer version (always use latest version)
  {$DEFINE COMPILERXE3}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE3}
  {$ELSE}
    {$DEFINE BCB16}
  {$ENDIF}
  {$UNDEF VER_LATEST}
{$ENDIF}

{$IFDEF VER280}
  {$DEFINE COMPILERXE7}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE7}
  {$ELSE}
    {$DEFINE BCB20}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER270}
  {$DEFINE COMPILERXE6}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE6}
  {$ELSE}
    {$DEFINE BCB19}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER260}
  {$DEFINE COMPILERXE5}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE5}
  {$ELSE}
    {$DEFINE BCB18}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER250}
  {$DEFINE COMPILERXE4}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE4}
  {$ELSE}
    {$DEFINE BCB17}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER240}
  {$DEFINE COMPILERXE3}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE3}
  {$ELSE}
    {$DEFINE BCB16}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER230}
  {$DEFINE COMPILERXE2}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE2}
  {$ELSE}
    {$DEFINE BCB15}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER220}
  {$DEFINE COMPILERXE1}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE1}
  {$ELSE}
    {$DEFINE BCB14}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER210}
  {$DEFINE COMPILER2010}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2010}
  {$ELSE}
    {$DEFINE BCB12}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE COMPILER2009}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2009}
  {$ELSE}
    {$DEFINE BCB11}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE COMPILER2007}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2007}
  {$ELSE}
    {$DEFINE BCB10}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE COMPILER2006}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2006}
  {$ELSE}
    {$DEFINE BCB9}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE COMPILER2005}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2005}
  {$ELSE}
    {$DEFINE BCB8}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE COMPILER7}
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

{$IFDEF COMPILERXE7}
  {$DEFINE COMPILERXE7_UP}
  {$DEFINE COMPILERXE6_UP}
  {$DEFINE COMPILERXE5_UP}
  {$DEFINE COMPILERXE4_UP}
  {$DEFINE COMPILERXE3_UP}
  {$DEFINE COMPILERXE2_UP}
  {$DEFINE COMPILERXE1_UP}
  {$DEFINE COMPILER2010_UP}
  {$DEFINE COMPILER2009_UP}
  {$DEFINE COMPILER2007_UP}
  {$DEFINE COMPILER2006_UP}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILERXE6}
  {$DEFINE COMPILERXE6_UP}
  {$DEFINE COMPILERXE5_UP}
  {$DEFINE COMPILERXE4_UP}
  {$DEFINE COMPILERXE3_UP}
  {$DEFINE COMPILERXE2_UP}
  {$DEFINE COMPILERXE1_UP}
  {$DEFINE COMPILER2010_UP}
  {$DEFINE COMPILER2009_UP}
  {$DEFINE COMPILER2007_UP}
  {$DEFINE COMPILER2006_UP}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILERXE5}
  {$DEFINE COMPILERXE5_UP}
  {$DEFINE COMPILERXE4_UP}
  {$DEFINE COMPILERXE3_UP}
  {$DEFINE COMPILERXE2_UP}
  {$DEFINE COMPILERXE1_UP}
  {$DEFINE COMPILER2010_UP}
  {$DEFINE COMPILER2009_UP}
  {$DEFINE COMPILER2007_UP}
  {$DEFINE COMPILER2006_UP}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILERXE4}
  {$DEFINE COMPILERXE4_UP}
  {$DEFINE COMPILERXE3_UP}
  {$DEFINE COMPILERXE2_UP}
  {$DEFINE COMPILERXE1_UP}
  {$DEFINE COMPILER2010_UP}
  {$DEFINE COMPILER2009_UP}
  {$DEFINE COMPILER2007_UP}
  {$DEFINE COMPILER2006_UP}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILERXE3}
  {$DEFINE COMPILERXE3_UP}
  {$DEFINE COMPILERXE2_UP}
  {$DEFINE COMPILERXE1_UP}
  {$DEFINE COMPILER2010_UP}
  {$DEFINE COMPILER2009_UP}
  {$DEFINE COMPILER2007_UP}
  {$DEFINE COMPILER2006_UP}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILERXE2}
  {$DEFINE COMPILERXE2_UP}
  {$DEFINE COMPILERXE1_UP}
  {$DEFINE COMPILER2010_UP}
  {$DEFINE COMPILER2009_UP}
  {$DEFINE COMPILER2007_UP}
  {$DEFINE COMPILER2006_UP}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILERXE1}
  {$DEFINE COMPILERXE1_UP}
  {$DEFINE COMPILER2010_UP}
  {$DEFINE COMPILER2009_UP}
  {$DEFINE COMPILER2007_UP}
  {$DEFINE COMPILER2006_UP}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILER2010}
  {$DEFINE COMPILER2010_UP}
  {$DEFINE COMPILER2009_UP}
  {$DEFINE COMPILER2007_UP}
  {$DEFINE COMPILER2006_UP}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILER2009}
  {$DEFINE COMPILER2009_UP}
  {$DEFINE COMPILER2007_UP}
  {$DEFINE COMPILER2006_UP}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILER2007}
  {$DEFINE COMPILER2007_UP}
  {$DEFINE COMPILER2006_UP}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILER2006}
  {$DEFINE COMPILER2006_UP}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILER2005}
  {$DEFINE COMPILER2005_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILER7}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILER6}
  {$DEFINE COMPILER6_UP}
{$ENDIF}

{$IFDEF COMPILERFPC}
  {$DEFINE PLATFORM_INDEPENDENT}
  {$MODE Delphi}
  {$ASMMODE INTEL}
{$ENDIF}

{$IFDEF COMPILERXE2_UP}
  {$IFDEF TARGET_x64}
    {$EXCESSPRECISION OFF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF COMPILER6_UP}
{$IFNDEF COMPILERFPC}
+--------------------------------------------------------------------+
|  DocProcessor may only be installed with Delphi/BCB 6 (or higher)  |
|   or Free Pascal / Lazarus                                         |
+--------------------------------------------------------------------+
{$ENDIF}
{$ENDIF}


(*
  Symbol INLININGSUPPORTED:
  -------------------------

    In later Delphi versions procedures and functions that do not contain
    assembler code can be inlined. This can be extremely useful for calls
    to a small portion of code in a loop. However, depending on the instruction
    cache size, this may or may not result in a performance boost compared to a
    call of assembler optimized code.
*)

{$IFDEF COMPILER2007_UP}
  // disabled prior Delphi versions due to a compiler bug
  // see (http://qc.embarcadero.com/wc/qcmain.aspx?d=41166)
  {$DEFINE INLININGSUPPORTED}
{$ENDIF}

{$IFDEF COMPILERFPC}
  {$DEFINE INLININGSUPPORTED}
{$ENDIF}


(*
  Symbol TARGET_* :
    Defines the processor platform (x86, x64 or PowerPC)
*)

{$IFDEF COMPILERFPC}
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
  {$ENDIF}
{$ELSE}
  // check for XE2 64-Bit compiler define
  {$IFDEF CPUX64}
    {$DEFINE TARGET_x64}
  {$ELSE}
    // define default (delphi etc) target (32 bit Intel 80386 or later)
    {$DEFINE TARGET_x86}
  {$ENDIF}
{$ENDIF}



(*
  Miscellaneous Defines:
  ---------------------- *)


{$IFNDEF COMPILER2010_UP}
  {$DEFINE USETHREADRESUME}
{$ENDIF}
{$IFDEF COMPILERFPC}
  {$UNDEF USETHREADRESUME}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE UNIX}
{$ENDIF}

{$IFNDEF FPC}
  {$IFNDEF LINUX}
    {$DEFINE Windows}
  {$ENDIF}
{$ENDIF}

{$R-}{$Q-}  // switch off overflow and range checking

{$IFDEF COMPILER2010_UP}
  {$DEFINE SUPPORTS_UNICODE}
{$ENDIF COMPILER2010_UP}

{$IFDEF COMPILER2010_UP}
  {$DEFINE SUPPORT_ENHANCED_RECORDS}
{$ENDIF}

{$IFNDEF COMPILERFPC}
  {$DEFINE SUPPORT_XPTHEMES} // enable support for windows xp themes
{$ENDIF}
