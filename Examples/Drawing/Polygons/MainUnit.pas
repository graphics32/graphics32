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
 * The Original Code is Polygons Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Christian-W. Budde
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFNDEF FPC} Windows, {$ELSE} LCLIntf, LResources, LCLType, Buttons, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Vcl.ComCtrls,
  GR32, GR32_Image, GR32_Layers, GR32_Polygons, GR32_Paths, GR32_Brushes;

type
  TFormPolygons = class(TForm)
    BitmapList: TBitmap32List;
    BtnNewLine: TButton;
    CbxPattern: TCheckBox;
    CbxThickOutline: TCheckBox;
    FillAlpha: TTrackBar;
    Image: TImage32;
    LblFillOpacity: TLabel;
    LblLineOpacity: TLabel;
    LblMiterLimit: TLabel;
    LblOutlineThickness: TLabel;
    LblOutlineThicknessValue: TLabel;
    LineAlpha: TTrackBar;
    LineThickness: TTrackBar;
    MemoHint: TMemo;
    MiterLimit: TTrackBar;
    PanelControl: TPanel;
    RgpFillMode: TRadioGroup;
    RgpJointMode: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure BtnNewLineClick(Sender: TObject);
    procedure FillAlphaChange(Sender: TObject);
    procedure FillModeChange(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure JointModeChange(Sender: TObject);
    procedure LineAlphaChange(Sender: TObject);
    procedure MiterLimitChange(Sender: TObject);
    procedure ParamsChanged(Sender: TObject);
    procedure PatternFillingChange(Sender: TObject);
    procedure ThicknessChanged(Sender: TObject);
    procedure ThickOutlineChange(Sender: TObject);
  private
    FCanvas: TCanvas32;
    FFiller: TBitmapPolygonFiller;
    FPoints: array of array of TPoint;
    FSolid: TSolidBrush;
    FStroke: TStrokeBrush;
    procedure Draw;
  end;

var
  FormPolygons: TFormPolygons;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math,
  Types,
{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG;
{$ELSE}
  LazJPG;
{$ENDIF}

{ TFormPolygons }

procedure TFormPolygons.FormCreate(Sender: TObject);
var
  ResStream: TResourceStream;
  JPEG: TJPEGImage;
begin
  // Load the textures (note size 256x256 is implicity expected!)
  JPEG := TJPEGImage.Create;
  try
    ResStream := TResourceStream.Create(HInstance, 'Delphi', RT_RCDATA);
    try
      JPEG.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
    BitmapList.Bitmap[0].Assign(JPEG);

    ResStream := TResourceStream.Create(HInstance, 'TextureB', RT_RCDATA);
    try
      JPEG.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
    BitmapList.Bitmap[1].Assign(JPEG);
  finally
    JPEG.Free;
  end;

  Image.SetupBitmap;

  SetLength(FPoints, 1);
  SetLength(FPoints[0], 0);

  FCanvas := TCanvas32.Create(Image.Bitmap);
  FCanvas.Brushes.Add(TSolidBrush);
  FSolid := TSolidBrush(FCanvas.Brushes[0]);
  FSolid.FillColor := SetAlpha(clGreen32, FillAlpha.Position);

  FCanvas.Brushes.Add(TStrokeBrush);
  FStroke := TStrokeBrush(FCanvas.Brushes[1]);
  FStroke.FillColor := SetAlpha(clBlack32, LineAlpha.Position);
  FStroke.StrokeWidth := 1;
  FStroke.Visible := False;

  ThickOutlineChange(Self);
end;

procedure TFormPolygons.FormDestroy(Sender: TObject);
begin
  FCanvas.Free;
  if Assigned(FFiller) then
    FFiller.Free;
end;

procedure TFormPolygons.Draw;
var
  Index, PointIndex: Integer;
begin
  Image.Bitmap.BeginUpdate;
  try
    Image.Bitmap.Clear(clWhite32);
    Image.Bitmap.Draw(50, 50, BitmapList.Bitmap[0]);

    for Index := 0 to Length(FPoints) - 1 do
    begin
      if Length(FPoints[Index]) = 0 then
        Continue;

      FCanvas.MoveTo(FPoints[Index, 0].X, FPoints[Index, 0].Y);

      for PointIndex := 1 to Length(FPoints[Index]) - 1 do
        FCanvas.LineTo(FPoints[Index, PointIndex].X, FPoints[Index, PointIndex].Y);

      FCanvas.EndPath(True);
    end;
  finally
    Image.Bitmap.EndUpdate;
  end;

  Image.Bitmap.Changed;
  Image.Refresh; // force repaint
end;

procedure TFormPolygons.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Index: Integer;
  PointIndex: Integer;
begin
  if Button = mbLeft then
  begin
    Index := Length(FPoints) - 1;
    PointIndex := Length(FPoints[Index]);
    SetLength(FPoints[Index], PointIndex + 1);
    FPoints[Index, PointIndex].X := X;
    FPoints[Index, PointIndex].Y := Y;
  end
  else
  begin
    SetLength(FPoints, 1);
    SetLength(FPoints[0], 0);
  end;

  Draw;
end;

procedure TFormPolygons.ImageResize(Sender: TObject);
begin
  Image.SetupBitmap;
  Draw;
end;

procedure TFormPolygons.LineAlphaChange(Sender: TObject);
begin
  FStroke.FillColor := SetAlpha(clBlack32, LineAlpha.Position);
  Draw;
end;

procedure TFormPolygons.MiterLimitChange(Sender: TObject);
begin
  FStroke.MiterLimit := MiterLimit.Position * 0.01;
  Draw;
end;

procedure TFormPolygons.ParamsChanged(Sender: TObject);
begin
  Draw;
end;

procedure TFormPolygons.FillAlphaChange(Sender: TObject);
begin
  FSolid.FillColor := SetAlpha(clGreen32, FillAlpha.Position);
  BitmapList.Bitmap[1].MasterAlpha := FillAlpha.Position;
  Draw;
end;

procedure TFormPolygons.FillModeChange(Sender: TObject);
begin
  FStroke.FillMode := TPolyFillMode(RgpFillMode.ItemIndex);
  FSolid.FillMode := TPolyFillMode(RgpFillMode.ItemIndex);
  Draw;
end;

procedure TFormPolygons.PatternFillingChange(Sender: TObject);
begin
  if CbxPattern.Checked then
  begin
    BitmapList.Bitmap[1].MasterAlpha := FillAlpha.Position;
    BitmapList.Bitmap[1].DrawMode := dmBlend;
    FFiller := TBitmapPolygonFiller.Create;
    FFiller.Pattern := BitmapList.Bitmap[1];
    FCanvas.Renderer.Filler := FFiller;
    FSolid.Filler := FFiller;
  end
  else
  begin
    FCanvas.Renderer.Filler := nil;
    FSolid.Filler := nil;
    FreeAndNil(FFiller);
  end;
  Draw;
end;

procedure TFormPolygons.JointModeChange(Sender: TObject);
begin
  FStroke.JoinStyle := TJoinStyle(RgpJointMode.ItemIndex);
  MiterLimit.Enabled := CbxThickOutline.Checked and
    (FStroke.JoinStyle in [jsMiter, jsRoundEx]);
  Draw;
end;

procedure TFormPolygons.BtnNewLineClick(Sender: TObject);
begin
  SetLength(FPoints, Length(FPoints) + 1);
end;

procedure TFormPolygons.ThicknessChanged(Sender: TObject);
begin
  FStroke.StrokeWidth := LineThickness.Position * 0.1;
  LblOutlineThicknessValue.Caption := Format('(%2.1f)', [FStroke.StrokeWidth]);
  Draw;
end;

procedure TFormPolygons.ThickOutlineChange(Sender: TObject);
begin
  FStroke.Visible := CbxThickOutline.Checked;
  LineThickness.Enabled := CbxThickOutline.Checked;
  RgpJointMode.Enabled := CbxThickOutline.Checked;
  MiterLimit.Enabled := CbxThickOutline.Checked and
    (FStroke.JoinStyle = jsMiter);
  Draw;
end;

end.
