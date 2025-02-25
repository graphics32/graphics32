unit GR32.Text.Types;

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
 * The Original Code is Delphi/Windows text vectorization utilities for Graphics32
 *
 * The Initial Developer of the Original Code is Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  GR32;

//------------------------------------------------------------------------------
//
//      Text layout bit flags
//
//------------------------------------------------------------------------------
// Used by
// - ITextSupport.Textout
// - ITextToPathSupport.TextToPath
//------------------------------------------------------------------------------
const
  // See also Window's DrawText() flags ...
  // http://msdn.microsoft.com/en-us/library/ms901121.aspx
  DT_LEFT               = $0000;
  DT_CENTER             = $0001;
  DT_RIGHT              = $0002;
  DT_VCENTER            = $0004;
  DT_BOTTOM             = $0008;
  DT_WORDBREAK          = $0010;
  DT_SINGLELINE         = $0020;
  DT_NOCLIP             = $0100;

  {$NODEFINE DT_LEFT}
  {$NODEFINE DT_CENTER}
  {$NODEFINE DT_RIGHT}
  {$NODEFINE DT_VCENTER}
  {$NODEFINE DT_BOTTOM}
  {$NODEFINE DT_WORDBREAK}
  {$NODEFINE DT_SINGLELINE}
  {$NODEFINE DT_NOCLIP}

  // Graphics32 additions ...
  DT_JUSTIFY            = $0003;
  DT_HORZ_ALIGN_MASK    = $0003;


//------------------------------------------------------------------------------
//
//      Font hinting
//
//------------------------------------------------------------------------------
// Used by IFontHintingSupport
//------------------------------------------------------------------------------
// Will likely be deprecated at some point as hinting isn't really used that
// much anymore.
//------------------------------------------------------------------------------
type
  TTextHinting = (thNone, thNoHorz, thHinting);


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

end.
