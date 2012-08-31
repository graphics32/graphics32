unit MainUnit;

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
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Andre Beckedorf <andre@metaexception.de>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GR32, GR32_Image, GR32_Resamplers, GR32_RangeBars;

type
  TFormImage32Example = class(TForm)
    Image: TImage32;
    PnlImage: TPanel;
    PnlControl: TPanel;
    RgpScaleMode: TRadioGroup;
    RgpKernel: TRadioGroup;
    RgpBitmapAlign: TRadioGroup;
    LblScale: TStaticText;
    SbrScale: TGaugeBar;
    procedure FormCreate(Sender: TObject);
    procedure RgpBitmapAlignClick(Sender: TObject);
    procedure SbrScaleChange(Sender: TObject);
    procedure RgpScaleModeClick(Sender: TObject);
    procedure RgpKernelClick(Sender: TObject);
  public
    Time: Single;
  end;

var
  FormImage32Example: TFormImage32Example;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  GR32_MediaPathLocator,
{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG;
{$ELSE}
  LazJPG;
{$ENDIF}

procedure TFormImage32Example.FormCreate(Sender: TObject);
var
  MediaPath: TFileName;
begin
  MediaPath := ExpandFileName(GetMediaPath);

  // load example image
  Assert(FileExists(MediaPath + 'delphi.jpg'));
  Image.Bitmap.LoadFromFile(MediaPath + 'delphi.jpg');

  with TKernelResampler.Create(Image.Bitmap) do
  begin
    KernelMode := kmTableNearest;
    TableSize := 16;
  end;
end;

procedure TFormImage32Example.RgpBitmapAlignClick(Sender: TObject);
const
  BA_CONSTS: array [0..2] of TBitmapAlign = (baTopLeft, baCenter, baTile);
begin
  Image.BitmapAlign := BA_CONSTS[RgpBitmapAlign.ItemIndex];
end;

procedure TFormImage32Example.SbrScaleChange(Sender: TObject);
begin
  SbrScale.Update;
  Image.Scale := SbrScale.Position * 0.01;
end;

procedure TFormImage32Example.RgpScaleModeClick(Sender: TObject);
const
  SM_CONSTS: array [0..5] of TScaleMode = (smNormal, smStretch, smScale, smResize, smOptimal, smOptimalScaled);
var
  ScaleEnabled: Boolean;
begin
  Image.ScaleMode := SM_CONSTS[RgpScaleMode.ItemIndex];
  ScaleEnabled := (RgpScaleMode.ItemIndex = 2) or (RgpScaleMode.ItemIndex = 5);
  SbrScale.Enabled := ScaleEnabled;
  SbrScale.Enabled := ScaleEnabled;
end;

procedure TFormImage32Example.RgpKernelClick(Sender: TObject);
const
  K_CONSTS: array [0..4] of TCustomKernelClass =
    (TBoxKernel, TLinearKernel, TSplineKernel, TLanczosKernel, TMitchellKernel);
begin
  TKernelResampler(Image.Bitmap.Resampler).Kernel := K_CONSTS[RgpKernel.ItemIndex].Create;
end;

end.
