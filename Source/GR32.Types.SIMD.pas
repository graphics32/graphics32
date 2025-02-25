unit GR32.Types.SIMD;

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
 * The Original Code is SIMD for Graphics32
 *
 * The Initial Developer of the Original Code is Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2025
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{$if not defined(PUREPASCAL)}

//------------------------------------------------------------------------------
//
//      SSE MXCSR rounding modes
//      For use with the SSE2 CVTSS2SI instruction - and friends.
//
//------------------------------------------------------------------------------
type
  MXCSR = record
    const
      MASK              = $FFFF9FFF;
      NEAREST           = $00000000;        // Round
      DOWN              = $00002000;        // Floor
      UP                = $00004000;        // Ceil
      TRUNC             = $00006000;        // Trunc
  end;


//------------------------------------------------------------------------------
//
//      Rounding control values.
//      For use with the SSE4.1 ROUND[S/P][S/D] instruction
//
//------------------------------------------------------------------------------
type
  SSE_ROUND = record
    const
      TO_NEAREST_INT    = $00; // Round
      TO_NEG_INF        = $01; // Floor
      TO_POS_INF        = $02; // Ceil
      TO_ZERO           = $03; // Trunc
      CUR_DIRECTION     = $04; // Rounds using default from MXCSR register

      RAISE_EXC         = $00; // Raise exceptions
      NO_EXC            = $08; // Suppress exceptions
  end;


//------------------------------------------------------------------------------
//
//      SIMD constants
//
//------------------------------------------------------------------------------
// All SIMD values are arrays of 4 elements.
// Element size is 32-bits so the type is either Single, Cardinal or Integer.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Unaligned constants
//------------------------------------------------------------------------------
const
  SSE_FloatOne          : array[0..3] of Single   = (1, 1, 1, 1);
  SSE_Float256x256      : array[0..3] of Single   = ($00010000, $00010000, $00010000, $00010000); // 256*256
  SSE_IntAbsMask        : array[0..3] of Cardinal = ($7FFFFFFF, $7FFFFFFF, $7FFFFFFF, $7FFFFFFF);


//------------------------------------------------------------------------------
// Aligned constants. Implemented as no-code assembly routines.
//------------------------------------------------------------------------------
// 8 x $FF00
procedure SSE_FF00FF00_ALIGNED;

// x/255 bias table ($7F * $8101)
procedure SSE_003FFF7F_ALIGNED;

// Aligned pack table for PSHUFB: Picks low byte of 4 dwords
procedure SSE_0C080400_ALIGNED;


{$ifend}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

procedure SSE_FF00FF00_ALIGNED; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$ifdef FPC}
  ALIGN 16
{$else}
  .ALIGN 16
{$endif}
  dw $FF00, $FF00, $FF00, $FF00
  dw $FF00, $FF00, $FF00, $FF00
end;

// Aligned bias table
procedure SSE_003FFF7F_ALIGNED; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$ifdef FPC}
  ALIGN 16
{$else}
  .ALIGN 16
{$endif}
  db $7F, $FF, $3F, $0
  db $7F, $FF, $3F, $0
  db $7F, $FF, $3F, $0
  db $7F, $FF, $3F, $0
end;

// Aligned pack table for PSHUFB: Picks low byte of 4 dwords
procedure SSE_0C080400_ALIGNED; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$ifdef FPC}
  ALIGN 16
{$else}
  .ALIGN 16
{$endif}
  db $00, $04, $08, $0C
  db $00, $04, $08, $0C
  db $00, $04, $08, $0C
  db $00, $04, $08, $0C
end;

end.

