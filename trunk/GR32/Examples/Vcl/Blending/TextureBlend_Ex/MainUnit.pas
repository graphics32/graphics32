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
 * The Original Code is Texture Blend Example
 *
 * The Initial Developer(s) of the Original Code is:
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$IFNDEF FPC}
  {$DEFINE Windows}
{$ENDIF}

uses
  {$IFDEF FPC}LCLIntf, LResources, Buttons, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Math, StdCtrls, ExtCtrls,
  GR32_Image, GR32_RangeBars;

type
TMainForm = class(TForm)
    MasterAlphaBar: TGaugeBar;
    CombImg: TImage32;
    WeightmapImg: TImage32;
    TexAImg: TImage32;
    TexBImg: TImage32;
    Label5: TLabel;
    BlendBox: TComboBox;
    Label4: TLabel;
    Label3: TLabel;
    Label1: TLabel;
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

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
{$IFDEF Darwin}
  FPCMacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG,
{$ELSE}
  LazJPEG,
{$ENDIF}
  GR32, GR32_Resamplers, GR32_LowLevel, GR32_Blend;

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
var
{$IFDEF Darwin}
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
  pathMedia: string;
begin
  // Under Mac OS X we need to get the location of the bundle
{$IFDEF Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
{$ENDIF}

  // On Lazarus we don't use design-time packages because they consume time to be installed
{$IFDEF FPC}
  MasterAlphaBar := TGaugeBar.Create(Self);
  MasterAlphaBar.Parent := Self;
  MasterAlphaBar.Left := 352;
  MasterAlphaBar.Height := 16;
  MasterAlphaBar.Top := 32;
  MasterAlphaBar.Width := 177;
  MasterAlphaBar.Color := clScrollBar;
  MasterAlphaBar.Max := 255;
  MasterAlphaBar.ShowArrows := False;
  MasterAlphaBar.ShowHandleGrip := True;
  MasterAlphaBar.Style := rbsMac;
  MasterAlphaBar.Position := 200;
  MasterAlphaBar.OnChange := MasterAlphaBarChange;

  CombImg := TImage32.Create(Self);
  CombImg.Parent := Self;
  CombImg.Left := 272;
  CombImg.Height := 256;
  CombImg.Top := 112;
  CombImg.Width := 256;
  CombImg.Bitmap.ResamplerClassName := 'TNearestResampler';
  CombImg.Bitmap.OnChange := nil;
  CombImg.Bitmap.OnResize := nil;
  CombImg.BitmapAlign := baCenter;
  CombImg.Color := clBlack;
  CombImg.ParentColor := False;
  CombImg.Scale := 1;
  CombImg.TabOrder := 1;

  WeightmapImg := TImage32.Create(Self);
  WeightmapImg.Parent := Self;
  WeightmapImg.Left := 8;
  WeightmapImg.Height := 256;
  WeightmapImg.Top := 112;
  WeightmapImg.Width := 257;
  WeightmapImg.Bitmap.ResamplerClassName := 'TNearestResampler';
  WeightmapImg.Bitmap.OnChange := nil;
  WeightmapImg.Bitmap.OnResize := nil;
  WeightmapImg.BitmapAlign := baCenter;
  WeightmapImg.Color := clBlack;
  WeightmapImg.ParentColor := False;
  WeightmapImg.Scale := 1;
  WeightmapImg.TabOrder := 2;

  TexAImg := TImage32.Create(Self);
  TexAImg.Parent := Self;
  TexAImg.Left := 8;
  TexAImg.Height := 256;
  TexAImg.Top := 400;
  TexAImg.Width := 256;
  TexAImg.Bitmap.ResamplerClassName := 'TNearestResampler';
  TexAImg.Bitmap.OnChange := nil;
  TexAImg.Bitmap.OnResize := nil;
  TexAImg.BitmapAlign := baCenter;
  TexAImg.Color := clBlack;
  TexAImg.ParentColor := False;
  TexAImg.Scale := 1;
  TexAImg.TabOrder := 3;

  TexBImg := TImage32.Create(Self);
  TexBImg.Parent := Self;
  TexBImg.Left := 272;
  TexBImg.Height := 256;
  TexBImg.Top := 400;
  TexBImg.Width := 256;
  TexBImg.Bitmap.ResamplerClassName := 'TNearestResampler';
  TexBImg.Bitmap.OnChange := nil;
  TexBImg.Bitmap.OnResize := nil;
  TexBImg.BitmapAlign := baCenter;
  TexBImg.Color := clBlack;
  TexBImg.ParentColor := False;
  TexBImg.Scale := 1;
  TexBImg.TabOrder := 4;
{$ENDIF}

  // Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia := '..\..\..\Media\';
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF Darwin}
    pathMedia := pathStr + '/Contents/Resources/Media/';
  {$ELSE}
    pathMedia := '../../../Media/';
  {$ENDIF}
{$ENDIF}

  BlendBox.ItemIndex := 0;
  
  // Load the textures (note size 256x256 is implicity expected!)
  TexAImg.Bitmap.LoadFromFile(pathMedia + 'texture_a.jpg');
  TexBImg.Bitmap.LoadFromFile(pathMedia + 'texture_b.jpg');
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
var
  a, b, c: Single;

  function GenerateSomething(x,y : Single): Single;
  begin
    if a < 0.6 then
      Result := Max(Cos(x * PI * a * 2 + b), Sqr(0.1 + c + x*y - y)) *
        (Sin(y * b * a) - c + ArcTan2(x + Cos((x - y) * b), y + a))
    else
      Result := Cos(x * PI * a * 2 + c) * Sin(y * b * a) +
        Sin(ArcTan2(x + Cos((x - y) * b), y * c * Sin(x - a)));
  end;

const
  nS = 1 / 255;

var
  I, J: Integer;
  W : TColor32;
  D, WImg: PColor32;
  x,y: Single;
begin
  // Setup some random factors:
  a := 0.01 + random;
  b := random * random * (PI * (20 * a)) - PI * (10 * a);
  c := random - random;

  //We use the weightmap as TexB alpha, so we write that on the loop too
  D := @TexBImg.Bitmap.Bits[0];
  WImg := @WeightmapImg.Bitmap.Bits[0];
  for J := 0 to 255 do
    for I := 0 to 255 do
    begin
      x := Cos(I * nS + (PI * a));
      y := Sin(J * nS * (PI * c));
      W := Round(Constrain(Abs(Min(GenerateSomething(x * c, y),
        GenerateSomething(y + c , x * a))) * 200, 0, 255));
      if c > 0 then
        WImg^ := ColorDifference(WImg^, $FF000000 + W shl 16 + W shl 8 + W)
      else
        WImg^ := $FF000000 + W shl 16 + W shl 8 + W;
      EMMS;
      D^ := D^ and $00FFFFFF or W shl 24;
      Inc(D);
      Inc(WImg);
    end;
  EMMS;
  WeightmapImg.Invalidate;
  MasterAlphaBarChange(Self);
  
  // Needed under Mac OS X
  CombImg.Invalidate;
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

{$IFDEF FPC}
initialization
  {$I MainUnit.lrs}
{$ENDIF}

end.
