{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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
 * The Original Code is Polygons Example
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

uses
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs, GR32, GR32_Image,
  GR32_Layers, GR32_Polygons, QStdCtrls, QExtCtrls;

type
  TForm1 = class(TForm)
    Image: TImage32;
    Panel1: TPanel;
    Antialiase: TCheckBox;
    Label1: TLabel;
    LineAlpha: TScrollBar;
    Label2: TLabel;
    FillAlpha: TScrollBar;
    FillMode: TRadioGroup;
    Button1: TButton;
    LineThickness: TScrollBar;
    Label3: TLabel;
    ThickOutline: TCheckBox;
    Label4: TLabel;
    BitmapList: TBitmap32List;
    AntialiasMode: TRadioGroup;
    Memo1: TMemo;
    Memo2: TMemo;
    Pattern: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageResize(Sender: TObject);
    procedure ParamsChanged(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ThicknessChanged(Sender: TObject);
  private
    Polygon: TPolygon32;
    Outline: TPolygon32;
    UseOutlinePoly: Boolean;
    LineSize: Single;
    procedure Build;
    procedure Draw;
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

uses
  JPEG;

procedure TForm1.Draw;
var
  MyFiller: TBitmapPolygonFiller;
begin
  Image.Bitmap.BeginUpdate;
  Image.Bitmap.Clear(clWhite32);
  Image.Bitmap.Draw(50, 50, BitmapList.Bitmap[0]);

  Polygon.Antialiased := Antialiase.Checked;
  Polygon.AntialiasMode := TAntialiasMode(AntialiasMode.ItemIndex);

  if UseOutlinePoly then
  begin
    Outline.Antialiased := Antialiase.Checked;
    Outline.AntialiasMode := TAntialiasMode(AntialiasMode.ItemIndex);
  end;

  if FillMode.ItemIndex = 0 then
    Polygon.FillMode := pfAlternate
  else
    Polygon.FillMode := pfWinding;

  if Pattern.Checked then
  begin
    BitmapList.Bitmap[1].MasterAlpha := FillAlpha.Position;
    BitmapList.Bitmap[1].DrawMode := dmBlend;
    MyFiller := TBitmapPolygonFiller.Create;
    try
      MyFiller.Pattern := BitmapList.Bitmap[1];
      Polygon.DrawFill(Image.Bitmap, MyFiller);
    finally
      MyFiller.Free;
    end;
  end
  else
    Polygon.DrawFill(Image.Bitmap, SetAlpha(clGreen32, FillAlpha.Position));

  if UseOutlinePoly then
    Outline.DrawFill(Image.Bitmap, SetAlpha(clBlack32, LineAlpha.Position))
  else
    Polygon.DrawEdge(Image.Bitmap, SetAlpha(clBlack32, LineAlpha.Position));

  Image.Bitmap.EndUpdate;
  Image.Bitmap.Changed;
  Image.Refresh; // force repaint
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BitmapList.Bitmap[0].LoadFromFile('..\..\..\Media\delphi.jpg');
  BitmapList.Bitmap[1].LoadFromFile('..\..\..\Media\texture_b.jpg');  
  Image.SetupBitmap;
  Polygon := TPolygon32.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Outline.Free;
  Polygon.Free;
end;

procedure TForm1.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Button = mbLeft then Polygon.Add(FixedPoint(X, Y))
  else Polygon.Clear;
  Build;
  Draw;
end;

procedure TForm1.ImageResize(Sender: TObject);
begin
  Image.SetupBitmap;
  Build;
  Draw;
end;

procedure TForm1.ParamsChanged(Sender: TObject);
begin
  AntialiasMode.Enabled := Antialiase.Checked;
  Draw;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Polygon.NewLine;
end;

procedure TForm1.Build;
var
  TmpPoly: TPolygon32;
begin
  Outline.Free;
  Outline := nil;

  if UseOutlinePoly then
  begin
    TmpPoly := Polygon.Outline;
    Outline := TmpPoly.Grow(Fixed(LineSize / 2), 0.5);
    Outline.FillMode := pfWinding;
    TmpPoly.Free;
  end;

  if UseOutlinePoly then
    Label4.Caption := Format('(%.1f)', [LineSize])
  else
    Label4.Caption := '(1)';
end;

procedure TForm1.ThicknessChanged(Sender: TObject);
begin
  AntialiasMode.Enabled := Antialiase.Checked;
  UseOutlinePoly := ThickOutline.Checked;
  LineSize := LineThickness.Position / 10;
  Build;
  Draw;
end;

end.
