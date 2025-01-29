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
 * The Original Code is PixelCombine Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  {$IFNDEF FPC} Windows, {$ELSE} LCLIntf, LCLType, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GR32,
  GR32_Image,
  GR32_Layers;

type
  TFormPixelCombine = class(TForm)
    ImgView: TImgView32;
    RadioGroup: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure RadioGroupClick(Sender: TObject);
  protected
    procedure PC_Add(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_Sub(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_Modulate(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_Min(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_Max(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_Screen(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_ColorBurn(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_ColorDodge(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_Difference(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_Exclusion(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_Pattern(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_Blend(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_BlendAdd(F: TColor32; var B: TColor32; M: Cardinal);
    procedure PC_BlendModulate(F: TColor32; var B: TColor32; M: Cardinal);
  private
    FPatCount: Integer;
    FLayer1: TBitmapLayer;
    FLayer2: TBitmapLayer;
  end;

var
  FormPixelCombine: TFormPixelCombine;

implementation

{$R *.dfm}

uses
  Types,
  GR32_Blend,
  GR32_RangeBars,
  GR32.ImageFormats.JPG;

{ TFormPixelCombine }

procedure TFormPixelCombine.FormCreate(Sender: TObject);

  procedure GenerateBitmap(Bitmap: TBitmap32);
  var
    X, Y: Integer;
    SinY, SinX: Double;
    Color: TColor32;
  begin
    // Just a pattern with some variation
    for Y := 0 to Bitmap.Height-1 do
    begin
      SinY := Sin(Y * 0.1);

      for X := 0 to Bitmap.Width-1 do
      begin
        SinX := Sin(X * 0.1);

        Color :=  Gray32(Round(((SinX + SinY) * 0.25 + 0.5) * 255));
        // Alpha gradient
        Color := SetAlpha(Color, MulDiv(255, Y, Bitmap.Height-1));
        
        Bitmap[X, Y] := Color;    
      end;
    end;
  end;

var
  RubberbandLayer: TRubberbandLayer;
  r: TRect;
  Viewport: TRect;
  Location: TFloatRect;
const
  BitmapSize = 200;
  BitmapOffset = 20;
begin
  // Load background picture 'Runner'
  ImgView.Bitmap.LoadFromResourceName(HInstance, 'Runner', RT_RCDATA);

  // Create foreground bitmap layers
  
  // First layer is unscaled
  FLayer1 := TBitmapLayer.Create(ImgView.Layers);
  FLayer1.Visible := False;
  FLayer1.Bitmap.SetSize(BitmapSize, BitmapSize);
  FLayer1.Bitmap.DrawMode := dmCustom;
  GenerateBitmap(FLayer1.Bitmap);
  FLayer1.Scaled := False;
  // Position top-left
  r := FLayer1.Bitmap.BoundsRect;
  r.Offset(BitmapOffset, BitmapOffset);
  if (FLayer1.Scaled) then
    // Location is relative to bitmap
    Location := ImgView.ControlToBitmap(r)
  else
    // Location is relative to viewport
    Location := FloatRect(r);
  FLayer1.Location := Location;

  // Second layer is scaled
  FLayer2 := TBitmapLayer.Create(ImgView.Layers);
  FLayer2.Visible := False;
  FLayer2.Bitmap.Assign(FLayer1.Bitmap);
  FLayer2.Scaled := True;
  // Position bottom-right
  r := FLayer1.Bitmap.BoundsRect;
  Viewport := ImgView.GetViewportRect;
  r.Offset(Viewport.Width-r.Width-BitmapOffset, Viewport.Height-r.Height-BitmapOffset);
  if (FLayer2.Scaled) then
    // Location is relative to bitmap
    Location := ImgView.ControlToBitmap(r)
  else
    // Location is relative to viewport
    Location := FloatRect(r);
  FLayer2.Location := Location;


  // Create rubberband layers so we can move the foreground layers around
  RubberbandLayer := TRubberbandLayer.Create(ImgView.Layers);
  RubberbandLayer.Visible := False;
  RubberbandLayer.ChildLayer := FLayer1;
  RubberbandLayer.Handles := [rhCenter, rhFrame, rhCorners];
  RubberbandLayer.ChildLayer.Visible := True;
  RubberbandLayer.Visible := True;

  RubberbandLayer := TRubberbandLayer.Create(ImgView.Layers);
  RubberbandLayer.Visible := False;
  RubberbandLayer.ChildLayer := FLayer2;
  RubberbandLayer.Handles := [rhCenter, rhFrame, rhCorners];
  RubberbandLayer.ChildLayer.Visible := True;
  RubberbandLayer.Visible := True;
end;

procedure TFormPixelCombine.PC_Add(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := ColorAdd(F, B);
end;

procedure TFormPixelCombine.PC_Max(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := ColorMax(F, B);
end;

procedure TFormPixelCombine.PC_Min(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := ColorMin(F, B);
end;

procedure TFormPixelCombine.PC_Modulate(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := ColorModulate(F, B);
end;

procedure TFormPixelCombine.PC_Pattern(F: TColor32; var B: TColor32; M: Cardinal);
begin
  FPatCount := 1 - FPatCount;
  if FPatCount = 0 then
    B := F;
end;

procedure TFormPixelCombine.PC_Sub(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := ColorSub(F, B);
end;

procedure TFormPixelCombine.PC_Screen(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := ColorScreen(F, B);
end;

procedure TFormPixelCombine.PC_ColorDodge(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := ColorDodge(F, B);
end;

procedure TFormPixelCombine.PC_ColorBurn(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := ColorBurn(F, B);
end;

procedure TFormPixelCombine.PC_Difference(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := ColorDifference(F, B);
end;

procedure TFormPixelCombine.PC_Exclusion(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := ColorExclusion(F, B);
end;

procedure TFormPixelCombine.PC_Blend(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := BlendReg(F, B);
end;

procedure TFormPixelCombine.PC_BlendAdd(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := BlendColorAdd(F, B);
end;

procedure TFormPixelCombine.PC_BlendModulate(F: TColor32; var B: TColor32; M: Cardinal);
begin
  B := BlendColorModulate(F, B);
end;

procedure TFormPixelCombine.RadioGroupClick(Sender: TObject);
begin
  case RadioGroup.ItemIndex of
    0:
      FLayer1.Bitmap.OnPixelCombine := nil;
    1:
      FLayer1.Bitmap.OnPixelCombine := PC_Add;
    2:
      FLayer1.Bitmap.OnPixelCombine := PC_Sub;
    3:
      FLayer1.Bitmap.OnPixelCombine := PC_Modulate;
    4:
      FLayer1.Bitmap.OnPixelCombine := PC_Min;
    5:
      FLayer1.Bitmap.OnPixelCombine := PC_Max;
    6:
      FLayer1.Bitmap.OnPixelCombine := PC_Screen;
    7:
      FLayer1.Bitmap.OnPixelCombine := PC_ColorDodge;
    8:
      FLayer1.Bitmap.OnPixelCombine := PC_ColorBurn;
    9:
      FLayer1.Bitmap.OnPixelCombine := PC_Difference;
    10:
      FLayer1.Bitmap.OnPixelCombine := PC_Exclusion;
    11:
      FLayer1.Bitmap.OnPixelCombine := PC_Pattern;
    12:
      FLayer1.Bitmap.OnPixelCombine := PC_Blend;
    13:
      FLayer1.Bitmap.OnPixelCombine := PC_BlendAdd;
    14:
      FLayer1.Bitmap.OnPixelCombine := PC_BlendModulate;
  end;
  FLayer2.Bitmap.OnPixelCombine := FLayer1.Bitmap.OnPixelCombine;
  
  FLayer1.Changed;
  FLayer2.Changed;
end;

end.
