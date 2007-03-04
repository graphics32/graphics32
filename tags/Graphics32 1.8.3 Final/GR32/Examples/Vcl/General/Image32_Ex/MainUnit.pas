unit MainUnit;

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
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Andre Beckedorf <andre@metaexception.de>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  {$IFNDEF FPC} {$ELSE}LResources, {$ENDIF}
  GR32, GR32_Image, GR32_Resamplers, GR32_RangeBars;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image: TImage32;
    rgScaleMode: TRadioGroup;
    rgKernel: TRadioGroup;
    rgBitmapAlign: TRadioGroup;
    StaticText1: TStaticText;
    sbScale: TGaugeBar;
    procedure rgBitmapAlignClick(Sender: TObject);
    procedure sbScaleChange(Sender: TObject);
    procedure rgScaleModeClick(Sender: TObject);
    procedure rgKernelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    Time: Single;
  end;

var
  Form1: TForm1;

implementation

{$IFNDEF FPC}
{$R *.DFM}

uses JPEG;
{$ELSE}
uses LazJPEG;
{$ENDIF}

procedure TForm1.rgBitmapAlignClick(Sender: TObject);
const
  BA_CONSTS: array [0..2] of TBitmapAlign = (baTopLeft, baCenter, baTile);
begin
  Image.BitmapAlign := BA_CONSTS[rgBitmapAlign.ItemIndex];
end;

procedure TForm1.sbScaleChange(Sender: TObject);
begin
  sbScale.Update;
  Image.Scale := sbScale.Position / 100;
end;

procedure TForm1.rgScaleModeClick(Sender: TObject);
const
  SM_CONSTS: array [0..5] of TScaleMode = (smNormal, smStretch, smScale, smResize, smOptimal, smOptimalScaled);
var
  ScaleEnabled: Boolean;
begin
  Image.ScaleMode := SM_CONSTS[rgScaleMode.ItemIndex];
  ScaleEnabled := (rgScaleMode.ItemIndex = 2) or (rgScaleMode.ItemIndex = 5);
  sbScale.Enabled := ScaleEnabled;
  StaticText1.Enabled := ScaleEnabled;
end;

procedure TForm1.rgKernelClick(Sender: TObject);
const
  K_CONSTS: array [0..4] of TCustomKernelClass =
    (TBoxKernel, TLinearKernel, TSplineKernel, TLanczosKernel, TMitchellKernel);
begin
  TKernelResampler(Image.Bitmap.Resampler).Kernel := K_CONSTS[rgKernel.ItemIndex].Create;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image.Bitmap.LoadFromFile('..\..\..\Media\delphi.jpg');
  with TKernelResampler.Create(Image.Bitmap) do
  begin
    KernelMode := kmTableNearest;
    TableSize := 16;
  end;
end;

{$IFDEF FPC}
initialization
  {$I MainUnit.lrs}
{$ENDIF}

end.
