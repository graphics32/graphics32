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
 * The Original Code is Texture Blend Example
 *
 * The Initial Developer(s) of the Original Code is:
 * Christian-W. Budde <Christian@pcjv.de>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2010
 * the Initial Developer. All Rights Reserved.
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, Buttons, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Math, StdCtrls, ExtCtrls,
  GR32, GR32_Blend, GR32_Image;

type
  TMainForm = class(TForm)
    CheckBoxBackground: TCheckBox;
    CheckBoxForeground: TCheckBox;
    CheckBoxTransparent: TCheckBox;
    DstImg: TImage32;
    LabelBlendHint: TLabel;
    LabelBlendSettings: TLabel;
    LabelMergeHint: TLabel;
    LabelOverlay: TLabel;
    LabelVisible: TLabel;
    RadioButtonBlend: TRadioButton;
    RadioButtonMerge: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxImageClick(Sender: TObject);
    procedure DstImgPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure RadioButtonBlendClick(Sender: TObject);
    procedure RadioButtonMergeClick(Sender: TObject);
    procedure RadioButtonNoneClick(Sender: TObject);
  private
    FForeground: TBitmap32;
    FBackground: TBitmap32;
    FBackgroundOpaque: TBitmap32;
    FBlendFunc: TBlendReg;
    procedure ModifyAlphaValues;
    procedure UpdateBlendModeEnabled;
    procedure DrawBitmap;
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG,
{$ELSE}
  LazJPG,
{$ENDIF}
  GR32_Resamplers, GR32_LowLevel;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  ResStream: TResourceStream;
  JPEG: TJPEGImage;
begin
  // setup custom checker board paint stage
  with DstImg do
  begin
    with PaintStages[0]^ do //Set up custom paintstage to draw checkerboard
    begin
      Stage := PST_CUSTOM;
      Parameter := 1; // use parameter to tag the stage, we inspect this in OnPaintStage
    end;
  end;

  // Load the textures (note size 256x256 is implicity expected!)
  JPEG := TJPEGImage.Create;
  try
    ResStream := TResourceStream.Create(HInstance, 'TextureA', 'JPG');
    try
      JPEG.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
    FForeground := TBitmap32.Create;
    FForeground.Assign(JPEG);

    ResStream := TResourceStream.Create(HInstance, 'TextureB', 'JPG');
    try
      JPEG.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
    FBackground := TBitmap32.Create;
    FBackground.Assign(JPEG);
  finally
    JPEG.Free;
  end;

  // clone background (= store original background without transparency)
  FBackgroundOpaque := TBitmap32.Create;
  FBackgroundOpaque.Assign(FBackground);

  // apply transparency to both background and foreground
  ModifyAlphaValues;

  DstImg.Bitmap.SetSize(FForeground.Width, FForeground.Height);
  FBlendFunc := MergeReg;
  DrawBitmap;
end;

procedure TMainForm.ModifyAlphaValues;
var
  X, Y: Integer;
  Line: PColor32EntryArray;
begin
  // apply a linear alpha gradient from left (transparent) to right (opaque)
  for Y := 0 to FForeground.Height - 1 do
  begin
    Line := PColor32EntryArray(FForeground.ScanLine[Y]);
    for X := 0 to FForeground.Width - 1 do
      Line^[X].A := X;
  end;

  // apply a linear alpha gradient from top (transparent) to bottom (opaque)
  for Y := 0 to FBackground.Height - 1 do
  begin
    Line := PColor32EntryArray(FBackground.ScanLine[Y]);
    for X := 0 to FBackground.Width - 1 do
      Line^[X].A := Y;
  end;
end;

procedure TMainForm.DstImgPaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
const
  Colors: array [Boolean] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  R: TRect;
  I, J: Integer;
  OddY: Integer;
  TilesHorz, TilesVert: Integer;
  TileX, TileY: Integer;
  TileHeight, TileWidth: Integer;
begin
  // draw checker board
  with TImgView32(Sender) do
  begin
    BeginUpdate;
    R := GetViewportRect;
    TileHeight := 8;
    TileWidth := 8;
    TilesHorz := (R.Right - R.Left) div TileWidth;
    TilesVert := (R.Bottom - R.Top) div TileHeight;
    TileY := 0;
    for J := 0 to TilesVert do
    begin
      TileX := 0;
      OddY := J and $1;
      for I := 0 to TilesHorz do
      begin
        Buffer.FillRectS(TileX, TileY, TileX + TileWidth, TileY +
          TileHeight, Colors[I and $1 = OddY]);
        Inc(TileX, TileWidth);
      end;
      Inc(TileY, TileHeight);
    end;
    EndUpdate;
  end;
end;

procedure TMainForm.RadioButtonNoneClick(Sender: TObject);
begin
  DstImg.Bitmap.Clear(0);

  // Needed under Mac OS X
  DstImg.Invalidate;
end;

procedure TMainForm.RadioButtonBlendClick(Sender: TObject);
begin
  FBlendFunc := BlendReg;

  DrawBitmap;
end;

procedure TMainForm.RadioButtonMergeClick(Sender: TObject);
begin
  FBlendFunc := MergeReg;

  DrawBitmap;
end;

procedure TMainForm.CheckBoxImageClick(Sender: TObject);
begin
  DrawBitmap;
  UpdateBlendModeEnabled;
end;

procedure TMainForm.UpdateBlendModeEnabled;
var
  Value: Boolean;
begin
  Value := CheckBoxForeground.Checked and CheckBoxBackground.Checked;
  RadioButtonBlend.Enabled := Value;
  RadioButtonMerge.Enabled := Value;
end;

procedure TMainForm.DrawBitmap;
var
  X, Y: Integer;
  PSrcF, PSrcB, PDst: PColor32Array;
  Background: TBitmap32;
begin
  // select whether the opaque or transparent image shall be used
  if CheckBoxTransparent.Checked then
    Background := FBackground
  else
    Background := FBackgroundOpaque;

  if CheckBoxForeground.Checked then
  begin
    if CheckBoxBackground.Checked then
      for Y := 0 to FForeground.Height - 1 do
      begin
        // blend lines according to the blend function (blend or merge)
        PSrcF := PColor32Array(FForeground.ScanLine[Y]);
        PSrcB := PColor32Array(Background.ScanLine[Y]);
        PDst := PColor32Array(DstImg.Bitmap.ScanLine[Y]);
        for X := 0 to FForeground.Width - 1 do
          PDst[X] := FBlendFunc(PSrcF[X], PSrcB[X]);
      end
    else
      for Y := 0 to FForeground.Height - 1 do
      begin
        // copy lines from the foreground image
        PSrcF := PColor32Array(FForeground.ScanLine[Y]);
        PDst := PColor32Array(DstImg.Bitmap.ScanLine[Y]);
        MoveLongword(PSrcF^, PDst^, FForeground.Width);
      end
  end
  else
  begin
    if CheckBoxBackground.Checked then
      for Y := 0 to FForeground.Height - 1 do
      begin
        // copy lines from the background image
        PSrcB := PColor32Array(Background.ScanLine[Y]);
        PDst := PColor32Array(DstImg.Bitmap.ScanLine[Y]);
        MoveLongword(PSrcB^, PDst^, FForeground.Width);
      end
    else
      DstImg.Bitmap.Clear(0);
  end;

  //This is needed because we may use MMX
  EMMS;

  // Needed under Mac OS X
  DstImg.Invalidate;
end;

end.
