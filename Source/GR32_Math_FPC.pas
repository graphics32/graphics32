unit GR32_Math_FPC;

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
 * The Original Code is Additional Math Routines for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 * (parts of this unit were moved here from GR32_System.pas and GR32.pas by Alex A. Denisov)
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *  Michael Hansen <dyster_tid@hotmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$IFDEF FPC}
{$IFDEF TARGET_X64}
{$mode objfpc}

(*
  FPC has no similar {$EXCESSPRECISION OFF} directive,
  but we can easily emulate that by overriding some internal math functions
*)
function PI: Single; [internproc: fpc_in_pi_real];
//function Abs(D: Single): Single; [internproc: fpc_in_abs_real];
//function Sqr(D: Single): Single; [internproc: fpc_in_sqr_real];
function Sqrt(D: Single): Single; [internproc: fpc_in_sqrt_real];
function ArcTan(D: Single): Single; [internproc: fpc_in_arctan_real];
function Ln(D: Single): Single; [internproc: fpc_in_ln_real];
function Sin(D: Single): Single; [internproc: fpc_in_sin_real];
function Cos(D: Single): Single; [internproc: fpc_in_cos_real];
function Exp(D: Single): Single; [internproc: fpc_in_exp_real];
function Round(D: Single): Int64; [internproc: fpc_in_round_real];
function Frac(D: Single): Single; [internproc: fpc_in_frac_real];
function Int(D: Single): Single; [internproc: fpc_in_int_real];
function Trunc(D: Single): Int64; [internproc: fpc_in_trunc_real];

function Ceil(X: Single): Integer; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
function Floor(X: Single): Integer; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
{$ENDIF}
{$ENDIF}

implementation

end.
