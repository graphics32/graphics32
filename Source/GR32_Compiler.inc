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
    COMPILERXE4  - Delphi XE4
    COMPILERXE5  - Delphi XE5
    COMPILERXE6  - Delphi XE6
    COMPILERXE7  - Delphi XE7
    COMPILERXE8  - Delphi XE8
    COMPILERRX   - Delphi RX
    COMPILERRX1  - Delphi RX1
	  COMPILERRX2  - Delphi RX2 (10.2 Tokyo)
    COMPILERRX3  - Delphi RX3 (10.3 Rio)


  Available Targets:
  ------------------

    TARGET_x86     - x86 (32-Bit)
    TARGET_x64     - x86 (64-Bit)
    TARGET_POWERPC - Power PC

*)



{$IFDEF FPC}
  {$DEFINE COMPILERFPC}
{$ENDIF}

{$IFDEF VER_LATEST} // adjust for newer version (always use latest version)
  {$DEFINE COMPILERRX3}
  {$IFNDEF BCB}
    {$DEFINE DELPHIRX3}
  {$ELSE}
    {$DEFINE BCBRX3}
  {$ENDIF}
  {$UNDEF VER_LATEST}
{$ENDIF}

{$IFDEF VER330}
  {$DEFINE COMPILERRX3}
  {$IFNDEF BCB}
    {$DEFINE DELPHIRX3}
  {$ELSE}
    {$DEFINE BCBRX3}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER320}
  {$DEFINE COMPILERRX2}
  {$IFNDEF BCB}
    {$DEFINE DELPHIRX2}
  {$ELSE}
    {$DEFINE BCBRX2}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER310}
  {$DEFINE COMPILERRX1}
  {$IFNDEF BCB}
    {$DEFINE DELPHIRX1}
  {$ELSE}
    {$DEFINE BCBRX1}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER300}
  {$DEFINE COMPILERRX}
  {$IFNDEF BCB}
    {$DEFINE DELPHIRX}
  {$ELSE}
    {$DEFINE BCBRX}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER290}
  {$DEFINE COMPILERXE8}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE8}
  {$ELSE}
    {$DEFINE BCBXE8}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER280}
  {$DEFINE COMPILERXE7}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE7}
  {$ELSE}
    {$DEFINE BCBXE7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER270}
  {$DEFINE COMPILERXE6}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE6}
  {$ELSE}
    {$DEFINE BCBXE6}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER260}
  {$DEFINE COMPILERXE5}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE5}
  {$ELSE}
    {$DEFINE BCBXE5}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER250}
  {$DEFINE COMPILERXE4}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE4}
  {$ELSE}
    {$DEFINE BCBXE4}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER240}
  {$DEFINE COMPILERXE3}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE3}
  {$ELSE}
    {$DEFINE BCBXE3}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER230}
  {$DEFINE COMPILERXE2}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE2}
  {$ELSE}
    {$DEFINE BCBXE2}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER220}
  {$DEFINE COMPILERXE1}
  {$IFNDEF BCB}
    {$DEFINE DELPHIXE1}
  {$ELSE}
    {$DEFINE BCBXE1}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER210}
  {$DEFINE COMPILER2010}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2010}
  {$ELSE}
    {$DEFINE BCB14}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE COMPILER2009}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2009}
  {$ELSE}
    {$DEFINE BCB12}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE COMPILER2007}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2007}
  {$ELSE}
    {$DEFINE BCB11}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE COMPILER2006}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2006}
  {$ELSE}
    {$DEFINE BCB10}
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

{$IFDEF COMPILERRX3}
  {$DEFINE COMPILERRX3_UP}
  {$DEFINE COMPILERRX2_UP}
  {$DEFINE COMPILERRX1_UP}
  {$DEFINE COMPILERRX_UP}
  {$DEFINE COMPILERXE8_UP}
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

{$IFDEF COMPILERRX2}
  {$DEFINE COMPILERRX2_UP}
  {$DEFINE COMPILERRX1_UP}
  {$DEFINE COMPILERRX_UP}
  {$DEFINE COMPILERXE8_UP}
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

{$IFDEF COMPILERRX1}
  {$DEFINE COMPILERRX1_UP}
  {$DEFINE COMPILERRX_UP}
  {$DEFINE COMPILERXE8_UP}
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

{$IFDEF COMPILERRX}
  {$DEFINE COMPILERRX_UP}
  {$DEFINE COMPILERXE8_UP}
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

{$IFDEF COMPILERXE8}
  {$DEFINE COMPILERXE8_UP}
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
{$ENDIF}

{$IFDEF COMPILERXE2_UP}
  {$IFDEF TARGET_x64}
    {$EXCESSPRECISION OFF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF COMPILER6_UP}
{$IFNDEF COMPILERFPC}
+--------------------------------------------------------------------+
|  Graphics32 may only be installed with Delphi/BCB 6 (or higher)    |
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
      {$ASMMODE INTEL}
    {$ENDIF}

    {$IFDEF CPUX86_64}
      // target is a 64-bit processor (AMD or INTEL).
      {$DEFINE TARGET_x64}
      {$ASMMODE INTEL}
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

  {$IFDEF CPUARM}
    // target is an ARM processor.
    {$DEFINE TARGET_ARM}
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
  Symbol BITS_GETTER:
  -------------------

*)

{$IFDEF COMPILERFPC}
  // Widgetsets other then Windows will want to implement BITS_GETTER
  {$IFNDEF LCLWin32}
    {$DEFINE BITS_GETTER}
  {$ENDIF}
{$ENDIF}


(*
  Symbol PUREPASCAL:
  ------------------

     Forces GR32 into pure pascal mode.


   NOTE: Further work needed to make assembly routines FPC compatible.

   NOTE: The DARWIN target of Free Pascal generates PIC code by default
   Which isn't compatible with the current assembler, so force PUREPASCAL

   NOTE: Due to several issues with the Delphi X64 compiler, PUREPASCAL is
   defined. However, most of the assembler optimized code does already work.
   (for issues please see http://qc.embarcadero.com/wc/qcmain.aspx?d=98616
   and http://qc.embarcadero.com/wc/qcmain.aspx?d=98613)
*)

{$IFDEF COMPILERFPC}
  {-$DEFINE PUREPASCAL}
{$ENDIF}

{$IFDEF TARGET_x64}
  {-$DEFINE PUREPASCAL}
{$ENDIF}

{$IFDEF TARGET_ARM}
  {$DEFINE PUREPASCAL}
  {$DEFINE OMIT_MMX}
  {$DEFINE OMIT_SSE2}
{$ENDIF}

{$IFDEF DARWIN}
  {$DEFINE PUREPASCAL}
{$ENDIF}

(*
  Symbol BITS_GETTER:
  -------------------

*)

{$IFDEF COMPILERFPC}
  // Widgetsets other then Windows will want to implement BITS_GETTER
  {$IFNDEF LCLWin32}
    {$DEFINE BITS_GETTER}
  {$ENDIF}
{$ENDIF}


(*
  Symbol NATIVE_SINCOS:
  ---------------------

     Use native FPU function to retrieve SIN/COS values for a given argument

   NOTE: On older systems a native call is typically much faster than calling
   Sin() and Cos() from a higher level library (such as the math/system unit)
   separately. However, on 64-bit systems a call can be slower, due to
   additional conversion between XMM registers and the FPU
*)

{$IFDEF PUREPASCAL}
  {$DEFINE NATIVE_SINCOS}
{$ENDIF}
{$IFDEF TARGET_x64}
  {$DEFINE NATIVE_SINCOS}
{$ENDIF}


(*
  Symbol HAS_NATIVEINT:
  ---------------------

     With the latest compilers (Delphi XE1+ and FPC 2.6.0+) the type NativeInt
     is available which has the native size of the OS (32-Bit or 64-Bit). Thus,
     it can be used to perform pointer arithmetrics.

   NOTE: Please do not use simple Cardinal() or Integer() casts on pointers only
*)

{$IFDEF COMPILERXE1_UP}
  {$DEFINE HAS_NATIVEINT}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE HAS_NATIVEINT}
{$ENDIF}


(*
  Miscellaneous Defines:
  ---------------------- *)


{$IFDEF COMPILER6}
  {$DEFINE EXT_PROP_EDIT}
{$ENDIF}

{$IFNDEF COMPILER2010_UP}
  {$DEFINE USETHREADRESUME}
{$ENDIF}
{$IFDEF COMPILERFPC}
  {$UNDEF USETHREADRESUME}
{$ENDIF}

{$IFDEF LINUX}
  {$IFNDEF FPC}
    {$DEFINE BITS_GETTER}
  {$ENDIF}
  {$DEFINE UNIX}
{$ENDIF}

{$IFNDEF FPC}
  {$IFNDEF LINUX}
    {$DEFINE Windows}
  {$ENDIF}
{$ENDIF}

{$R-}{$Q-}  // switch off overflow and range checking

{$IFDEF COMPILER6_UP}
  {$DEFINE EXT_PROP_EDIT}
{$ENDIF}

{$IFDEF COMPILER2005_UP}
  {$DEFINE HasParentBackground}
{$ENDIF}

{$IFDEF COMPILER2010_UP}
  {$DEFINE SUPPORT_ENHANCED_RECORDS}
{$ENDIF}

{$IFNDEF COMPILERFPC}
  {$DEFINE SUPPORT_XPTHEMES} // enable support for windows xp themes
{$ENDIF}



(*
  Compatibility Symbols
  ---------------------

  These symbols are deprecated and will be removed in future releases.
  *)


{$IFDEF COMPILER6_UP}
  {$DEFINE COMPILER6}
{$ENDIF}

{$IFDEF COMPILER7_UP}
  {$DEFINE COMPILER7}
{$ENDIF}

{$IFDEF COMPILER2005_UP}
  {$DEFINE COMPILER2005}
{$ENDIF}

{$IFDEF COMPILER2006}
  {$DEFINE COMPILER2006}
{$ENDIF}

{$IFDEF COMPILER2007_UP}
  {$DEFINE COMPILER2007}
{$ENDIF}

{$IFDEF COMPILER2009_UP}
  {$DEFINE COMPILER2009}
{$ENDIF}

{$IFDEF COMPILER2010_UP}
  {$DEFINE COMPILER2010}
{$ENDIF}

{$IFDEF COMPILERXE1_UP}
  {$DEFINE COMPILERXE1}
{$ENDIF}

{$IFDEF COMPILERXE2_UP}
  {$DEFINE COMPILERXE2}
{$ENDIF}

{$IFDEF COMPILERXE3_UP}
  {$DEFINE COMPILERXE3}
{$ENDIF}

{$IFDEF COMPILERXE4_UP}
  {$DEFINE COMPILERXE4}
{$ENDIF}

{$IFDEF COMPILERXE5_UP}
  {$DEFINE COMPILERXE5}
{$ENDIF}

{$IFDEF COMPILERXE6_UP}
  {$DEFINE COMPILERXE6}
{$ENDIF}

{$IFDEF COMPILERXE7_UP}
  {$DEFINE COMPILERXE7}
{$ENDIF}

{$IFDEF COMPILERXE8_UP}
  {$DEFINE COMPILERXE8}
{$ENDIF}

{$IFDEF COMPILERRX_UP}
  {$DEFINE COMPILERRX8}
{$ENDIF}

{$IFDEF COMPILERRX1_UP}
  {$DEFINE COMPILERRX1}
{$ENDIF}

{$IFDEF COMPILERRX2_UP}
  {$DEFINE COMPILERRX2}
{$ENDIF}
