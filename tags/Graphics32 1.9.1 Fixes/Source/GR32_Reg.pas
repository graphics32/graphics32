unit GR32_Reg;

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
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Classes, TypInfo,
{$IFDEF FPC}
  LCLIntf, LResources, LazIDEIntf, PropEdits, ComponentEditors
{$ELSE}
  DesignIntf
{$ENDIF};

procedure Register;

implementation

uses
  GR32,
  GR32_Dsgn_Color,
  GR32_Dsgn_Bitmap,
  GR32_Dsgn_Misc,
  GR32_Image,
  {$IFDEF Windows}
  GR32_ExtImage,
  {$ENDIF}
  GR32_Layers,
  GR32_RangeBars,
  GR32_Resamplers;

{ Registration }
procedure Register;
begin
  RegisterComponents('Graphics32', [TPaintBox32, TImage32, TBitmap32List,
    TRangeBar, TGaugeBar, TImgView32{$IFDEF Windows}, TSyntheticImage32{$ENDIF}]);
  RegisterPropertyEditor(TypeInfo(TColor32), nil, '', TColor32Property);
  RegisterPropertyEditor(TypeInfo(TBitmap32), nil, '', TBitmap32Property);
  RegisterComponentEditor(TCustomImage32, TImage32Editor);

  RegisterPropertyEditor(TypeInfo(string), TBitmap32, 'ResamplerClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomResampler), TBitmap32, 'Resampler', TResamplerClassProperty);
  RegisterPropertyEditor(TypeInfo(string), TKernelResampler, 'KernelClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomKernel), TKernelResampler, 'Kernel', TKernelClassProperty);
end;

initialization
  {$IFDEF FPC}
  {$i GR32_reg.lrs}
  {$ENDIF}

end.
