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
 * The Initial Developers of the Original Code is:
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  Math, StdCtrls, ExtCtrls, Jpeg,
  GR32, GR32_Image, GR32_Blend, GR32_RangeBars, GR32_Resamplers;
  
type
  TMainForm = class(TForm)
    MasterAlphaBar: TGaugeBar;
    Label5: TLabel;
    BlendBox: TComboBox;
    CombImg: TImage32;
    Label4: TLabel;
    WeightmapImg: TImage32;
    Label3: TLabel;
    TexAImg: TImage32;
    Label1: TLabel;
    TexBImg: TImage32;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    GenerateButton: TButton;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure MasterAlphaBarChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GenerateWeightmap(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

var
  ColorAlgebraReg: TBlendReg;

function ColorAlgebraEx(F, B, M: TColor32): TColor32;
begin
  // Call the coloralgebra routine in action, restore foreground alpha and blend
  Result := BlendRegEx(ColorAlgebraReg(F, B) and $FFFFFF or F and $FF000000, B, M);
end;

function SoftMaskedEx(F, B, M: TColor32): TColor32;
var
   X: Integer;
begin
  // Some sort of masking with MasterAlpha (as threshold) included
  X := F shr 24 - (255 - M);
  if X > 0 then
    Result := F
  else
  if X = 0 then
    Result := ColorAverage(F, B) // Create soft edges
  else
    Result := B;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
// Load the textures (note size 256x256 is implicity expected!)
  TexAImg.Bitmap.LoadFromFile('texture_a.jpg');
  TexBImg.Bitmap.LoadFromFile('texture_b.jpg');
  CombImg.Bitmap.SetSizeFrom(TexBImg.Bitmap);

// Set up Weightmap and trigger generate
  WeightmapImg.Bitmap.SetSize(256, 256);
  GenerateButton.OnClick := GenerateWeightmap;

//we don't want the same series of weightmaps repeat every time the app is run
  Randomize;

  GenerateWeightmap(Self);
end;

procedure TMainForm.GenerateWeightmap(Sender: TObject);
// Below code is very much based on experimentation, feel free to play around..
const
  nS = 1 / 255;
var
  I, J: Integer;
  W : TColor32;
  D, WImg: PColor32;
  s, a, b, c, x, y: Single;
begin
  // Setup some random factors:
  a := random;
  c := random - random;
  b := random * random(100) - 50;

  D := @TexBImg.Bitmap.Bits[0]; //We use the weightmap as TexB alpha
  WImg := @WeightmapImg.Bitmap.Bits[0];
  for J := 0 to 255 do
    for I := 0 to 255 do
    begin
      x := Cos(I * nS + (PI * a));
      y := Sin(J * nS * (PI * c));
      if a > 0.5 then
        s := Min(Cos(x * PI * a * 2 + b), Sqr(0.1 + c + x*y) - y) * Min(Sin(y * b * a) - c,  ArcTan2(x + Cos((x - y)*b), y + a))
      else
        s := Cos(x * PI * a * 2 + c) * Sin(y * b * a) +  ArcTan2(x + Cos((x - y) * b), y * c * Sin(x - a));
      W := Round(EnsureRange(Abs(S - a) * 200, 0, 255));
      WImg^ := $FF000000 + W shl 16 + W shl 8 + W;
      D^ := D^ and $00FFFFFF or W shl 24;
      Inc(D);
      Inc(WImg);
    end;
  EMMS;
  WeightmapImg.Invalidate;
  MasterAlphaBarChange(Self);
end;

procedure TMainForm.MasterAlphaBarChange(Sender: TObject);
var
  ABlendRegEx: TBlendRegEx;
begin
  //Setup blendmode
  case BlendBox.ItemIndex of
    0: ABlendRegEx := BlendRegEx;
    1: ABlendRegEx := SoftMaskedEx;
  else
    begin
       ABlendRegEx := ColorAlgebraEx;
       case BlendBox.ItemIndex of
         2: ColorAlgebraReg := ColorAdd;
         3: ColorAlgebraReg := ColorSub;
         4: ColorAlgebraReg := ColorDiv;
         5: ColorAlgebraReg := ColorModulate;
         6: ColorAlgebraReg := ColorMax;
         7: ColorAlgebraReg := ColorMin;
         8: ColorAlgebraReg := ColorDifference;
         9: ColorAlgebraReg := ColorAverage;
         10: ColorAlgebraReg := ColorExclusion;
       end;
    end;
 end;

// Combine Texture A with B
  BlendTransfer(CombImg.Bitmap, 0, 0, CombImg.Bitmap.BoundsRect, TexBImg.Bitmap,
    TexBImg.Bitmap.BoundsRect, TexAImg.Bitmap, TexAImg.Bitmap.BoundsRect,
    ABlendRegEx, MasterAlphaBar.Position);

//This is needed because we may use MMX in the custom pixelcombiners
  EMMS;
end;

end.
