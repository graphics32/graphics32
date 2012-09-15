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
  {$IFDEF FPC} LCLIntf, LResources, Buttons, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GR32, GR32_Image, GR32_Layers, GR32_Polygons, GR32_Paths, GR32_Brushes;

type
  TFormPolygons = class(TForm)
    BitmapList: TBitmap32List;
    BtnNewLine: TButton;
    FillAlpha: TScrollBar;
    Image: TImage32;
    LblFillOpacity: TLabel;
    LblLineOpacity: TLabel;
    LblOutlineThickness: TLabel;
    LblOutlineThicknessValue: TLabel;
    LineAlpha: TScrollBar;
    LineThickness: TScrollBar;
    MemoHint: TMemo;
    PanelControl: TPanel;
    CbxPattern: TCheckBox;
    RgpFillMode: TRadioGroup;
    CbxThickOutline: TCheckBox;
    RgpJointMode: TRadioGroup;
    LblMiterLimit: TLabel;
    MiterLimit: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageResize(Sender: TObject);
    procedure ParamsChanged(Sender: TObject);
    procedure BtnNewLineClick(Sender: TObject);
    procedure ThicknessChanged(Sender: TObject);
    procedure FillModeChange(Sender: TObject);
    procedure PatternFillingChange(Sender: TObject);
    procedure FillAlphaChange(Sender: TObject);
    procedure LineAlphaChange(Sender: TObject);
    procedure JointModeChange(Sender: TObject);
    procedure ThickOutlineChange(Sender: TObject);
    procedure MiterLimitChange(Sender: TObject);
  private
    FCanvas: TCanvas32;
    FFiller: TBitmapPolygonFiller;
    FPoints: array of array of TPoint;
    FSolid: TSolidBrush;
    FStroke: TStrokeBrush;
    procedure Draw;
    procedure GenerateTexture;
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
  Math, GR32_MediaPathLocator,
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
  PathMedia: TFileName;
begin
  PathMedia := GetMediaPath;

  // load example images
  if FileExists(PathMedia + 'delphi.jpg') then
    BitmapList.Bitmap[0].LoadFromFile(PathMedia + 'delphi.jpg');

(*
  if FileExists(PathMedia + 'texture_b.jpg') then
    BitmapList.Bitmap[1].LoadFromFile(PathMedia + 'texture_b.jpg')
  else
*)
    GenerateTexture;

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

procedure TFormPolygons.GenerateTexture;
var
  X, Y: Integer;
  G: TFloat;
  Row: PColor32Array;
begin
  with BitmapList.Bitmap[1] do
  begin
    SetSize(400, 400);

    G := 0.5;
    for Y := 0 to Height - 1 do
    begin
      Row := ScanLine[Y];
      for X := 0 to Width - 1 do
      begin
        G := EnsureRange(G * (0.99 + 0.02 * Random), 0.2, 0.8);

        Row^[X] := Color32(0, Round($FF * G), 0)
      end;
    end;
  end;
end;

procedure TFormPolygons.Draw;
var
  MyFiller: TBitmapPolygonFiller;
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

      FCanvas.Path.BeginPath;
      FCanvas.Path.MoveTo(FPoints[Index, 0].X, FPoints[Index, 0].Y);
      for PointIndex := 1 to Length(FPoints[Index]) - 1 do
        FCanvas.Path.LineTo(FPoints[Index, PointIndex].X, FPoints[Index, PointIndex].Y);
      FCanvas.Path.ClosePath;
      FCanvas.Path.EndPath;
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
  FStroke.MiterLimit := 0.1 * MiterLimit.Position;
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
    (FStroke.JoinStyle = jsMiter);
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
