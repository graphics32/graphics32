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

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GR32, GR32_Image, GR32_Layers, GR32_Blend, GR32_RangeBars;

type
  TFormPixelCombine = class(TForm)
    ImgView: TImgView32;
    RadioGroup: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure RadioGroupClick(Sender: TObject);
  protected
    procedure PC_Add(F: TColor32; var B: TColor32; M: TColor32);
    procedure PC_Sub(F: TColor32; var B: TColor32; M: TColor32);
    procedure PC_Modulate(F: TColor32; var B: TColor32; M: TColor32);
    procedure PC_Min(F: TColor32; var B: TColor32; M: TColor32);
    procedure PC_Max(F: TColor32; var B: TColor32; M: TColor32);
    procedure PC_Difference(F: TColor32; var B: TColor32; M: TColor32);
    procedure PC_Exclusion(F: TColor32; var B: TColor32; M: TColor32);
    procedure PC_Pattern(F: TColor32; var B: TColor32; M: TColor32);
  public
    PatCount: Integer;
    L: TBitmapLayer;
  end;

var
  FormPixelCombine: TFormPixelCombine;

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

{ TFormPixelCombine }

procedure TFormPixelCombine.FormCreate(Sender: TObject);
var
  I, J: Integer;
{$IFDEF Darwin}
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
  pathMedia: TFileName;
begin
  pathMedia := ExpandFileName(GetMediaPath);

  // On Lazarus we don't use design-time packages because they consume time to be installed
{$IFDEF FPC}
  ImgView := TImgView32.Create(Self);
  ImgView.Parent := Self;
  ImgView.Left := 16;
  ImgView.Top := 20;
  ImgView.Width := 367;
  ImgView.Height := 309;
  ImgView.Anchors := [akLeft, akTop, akRight, akBottom];
  ImgView.Bitmap.ResamplerClassName := 'TNearestResampler';
  ImgView.BitmapAlign := baCustom;
  ImgView.Color := clBtnShadow;
  ImgView.ParentColor := False;
  ImgView.Scale := 1.000000000000000000;
  ImgView.ScaleMode := smScale;
  ImgView.ScrollBars.ShowHandleGrip := True;
  ImgView.ScrollBars.Style := rbsDefault;
  ImgView.OverSize := 0;
  ImgView.TabOrder := 0;
{$ENDIF}

  // load example image
  Assert(FileExists(pathMedia + 'runner.jpg'));
  ImgView.Bitmap.LoadFromFile(pathMedia + 'runner.jpg');
  
  L := TBitmapLayer.Create(ImgView.Layers);
  L.Bitmap.SetSize(200, 200);
  L.Bitmap.DrawMode := dmCustom;
  L.Location := FloatRect(20, 20, 220, 220);
  for J := 0 to 199 do
    for I := 0 to 199 do
      L.Bitmap[I, J] := Gray32(Round(((Sin(I * 0.1) + Sin(J * 0.1)) * 0.25 + 0.5) * 255));
  L.Bitmap.OnPixelCombine := nil; // none by default
end;

procedure TFormPixelCombine.PC_Add(F: TColor32; var B: TColor32; M: TColor32);
begin
  B := ColorAdd(F, B);
end;

procedure TFormPixelCombine.PC_Max(F: TColor32; var B: TColor32; M: TColor32);
begin
  B := ColorMax(F, B);
end;

procedure TFormPixelCombine.PC_Min(F: TColor32; var B: TColor32; M: TColor32);
begin
  B := ColorMin(F, B);
end;

procedure TFormPixelCombine.PC_Modulate(F: TColor32; var B: TColor32; M: TColor32);
begin
  B := ColorModulate(F, B);
end;

procedure TFormPixelCombine.PC_Pattern(F: TColor32; var B: TColor32; M: TColor32);
begin
  PatCount := 1 - PatCount;
  if PatCount = 0 then B := F;
end;

procedure TFormPixelCombine.PC_Sub(F: TColor32; var B: TColor32; M: TColor32);
begin
  B := ColorSub(F, B);
end;

procedure TFormPixelCombine.PC_Difference(F: TColor32; var B: TColor32; M: TColor32);
begin
  B := ColorDifference(F, B);
end;

procedure TFormPixelCombine.PC_Exclusion(F: TColor32; var B: TColor32; M: TColor32);
begin
  B := ColorExclusion(F, B);
end;

procedure TFormPixelCombine.RadioGroupClick(Sender: TObject);
begin
  case RadioGroup.ItemIndex of
    0: L.Bitmap.OnPixelCombine := nil;
    1: L.Bitmap.OnPixelCombine := PC_Add;
    2: L.Bitmap.OnPixelCombine := PC_Sub;
    3: L.Bitmap.OnPixelCombine := PC_Modulate;
    4: L.Bitmap.OnPixelCombine := PC_Min;
    5: L.Bitmap.OnPixelCombine := PC_Max;
    6: L.Bitmap.OnPixelCombine := PC_Difference;
    7: L.Bitmap.OnPixelCombine := PC_Exclusion;
    8: L.Bitmap.OnPixelCombine := PC_Pattern;
  end;
  L.Bitmap.Changed;
end;

end.
