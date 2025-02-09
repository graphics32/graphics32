unit UnitMain;

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
 * The Original Code is Thick Line example for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2023
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GR32_Image;

type
  TFormThickLineTest = class(TForm)
    PaintBoxGDIThin: TPaintBox;
    PaintBox32_Thin: TPaintBox32;
    Label1: TLabel;
    Label2: TLabel;
    PaintBoxGDIThick: TPaintBox;
    PaintBox32_Thick: TPaintBox32;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    Label5: TLabel;
    PaintBox32_ThickLine: TPaintBox32;
    procedure PaintBoxGDIThinPaint(Sender: TObject);
    procedure PaintBox32_ThinPaintBuffer(Sender: TObject);
    procedure PaintBoxGDIThickPaint(Sender: TObject);
    procedure PaintBox32_ThickPaintBuffer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PaintBox32_ThickLinePaintBuffer(Sender: TObject);
  private
    FDoPaint: boolean;
  public
  end;

var
  FormThickLineTest: TFormThickLineTest;

implementation

{$R *.dfm}

uses
  Diagnostics,
  Math,

  GR32.Lines.Thick,

  GR32,
  GR32_LowLevel,
  GR32_Paths,
  GR32_Brushes,
  GR32_Polygons;


const
  MinLineCount = 200000;
  MinTestTime = 1000;
  ThickLineWidth = 10;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.Button1Click(Sender: TObject);
begin
  FDoPaint := True;
  try

    Invalidate;
    PaintBox32_Thin.Invalidate;
    PaintBox32_Thick.Invalidate;
    PaintBox32_ThickLine.Invalidate;

    Update;

  finally
    FDoPaint := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.PaintBoxGDIThinPaint(Sender: TObject);
begin
  (*
  ** GDI, thin line. Aliased. No alpha blending.
  *)

  if (not FDoPaint) then
    exit;

  Screen.Cursor := crHourGlass;
  PaintBoxGDIThin.Canvas.Brush.Color := clWhite;
  PaintBoxGDIThin.Canvas.Brush.Style := bsSolid;
  PaintBoxGDIThin.Canvas.FillRect(PaintBoxGDIThin.Canvas.ClipRect);

  PaintBoxGDIThin.Canvas.Pen.Width := 1;
  PaintBoxGDIThin.Canvas.MoveTo(0,0);

  RandSeed := 0;
  var Stopwatch := TStopwatch.StartNew;

  var LineCount := 0;
  while (LineCount < MinLineCount) or (Stopwatch.ElapsedMilliseconds < MinTestTime) do
  begin
    Inc(LineCount);
    PaintBoxGDIThin.Canvas.Pen.Color := Random($00FFFFFF);
    PaintBoxGDIThin.Canvas.LineTo(Random(PaintBoxGDIThin.Width), Random(PaintBoxGDIThin.Height));
  end;

  Stopwatch.Stop;

  Label1.Caption := Format('TCanvas.LineTo, Width=1. Lines per second: %.0n', [LineCount / Stopwatch.ElapsedMilliseconds * 1000]);
  Screen.Cursor := crDefault;
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.PaintBoxGDIThickPaint(Sender: TObject);
begin
  (*
  ** GDI, thick line. Aliased. No alpha blending.
  *)

  if (not FDoPaint) then
    exit;

  Screen.Cursor := crHourGlass;
  PaintBoxGDIThick.Canvas.Pen.Width := ThickLineWidth;
  PaintBoxGDIThick.Canvas.MoveTo(0,0);
  PaintBoxGDIThick.Canvas.Brush.Color := clWhite;
  PaintBoxGDIThick.Canvas.Brush.Style := bsSolid;
  PaintBoxGDIThick.Canvas.FillRect(PaintBoxGDIThick.Canvas.ClipRect);

  RandSeed := 0;
  var Stopwatch := TStopwatch.StartNew;

  var LineCount := 0;
  while (LineCount < MinLineCount) or (Stopwatch.ElapsedMilliseconds < MinTestTime) do
  begin
    Inc(LineCount);
    PaintBoxGDIThick.Canvas.Pen.Color := Random($00FFFFFF);
    PaintBoxGDIThick.Canvas.LineTo(Random(PaintBoxGDIThick.Width), Random(PaintBoxGDIThick.Height));
  end;

  Stopwatch.Stop;

  Label3.Caption := Format('TCanvas.LineTo, Width=%d. Lines per second: %.0n', [ThickLineWidth, LineCount / Stopwatch.ElapsedMilliseconds * 1000]);
  Screen.Cursor := crDefault;
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.PaintBox32_ThinPaintBuffer(Sender: TObject);
begin
  (*
  ** Graphics32, thin line. Anti-aliased & Alpha blended.
  *)

  if (not FDoPaint) then
    exit;

  Screen.Cursor := crHourGlass;
  PaintBox32_Thin.Buffer.Clear(clWhite32);
  PaintBox32_Thin.Buffer.DrawMode := dmOpaque;
  PaintBox32_Thin.Buffer.CombineMode := cmBlend;
  PaintBox32_Thin.Buffer.BeginLockUpdate; // No need for update handling, we will redraw everything

  PaintBox32_Thin.Buffer.MoveTo(0, 0);

  RandSeed := 0;
  var Stopwatch := TStopwatch.StartNew;

  var LineCount := 0;
  while (LineCount < MinLineCount) or (Stopwatch.ElapsedMilliseconds < MinTestTime) do
  begin
    Inc(LineCount);
    PaintBox32_Thin.Buffer.PenColor := Color32(Random($00FFFFFF)); // Color32 to swap R and B
    PaintBox32_Thin.Buffer.LineToAS(Random(PaintBox32_Thin.Width), Random(PaintBox32_Thin.Height));
  end;

  Stopwatch.Stop;

  PaintBox32_Thin.Buffer.EndLockUpdate;
  Label2.Caption := Format('TBitmap32.LineToAS, Width=1. Lines per second: %.0n', [LineCount / Stopwatch.ElapsedMilliseconds * 1000]);
  Screen.Cursor := crDefault;
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.PaintBox32_ThickPaintBuffer(Sender: TObject);
begin
  (*
  ** Graphics32, thick line via TCanvas32. Anti-aliased & Alpha blended.
  *)

  if (not FDoPaint) then
    exit;

  Screen.Cursor := crHourGlass;
  PaintBox32_Thick.Buffer.Clear(clWhite32);
  PaintBox32_Thick.Buffer.DrawMode := dmOpaque;
  PaintBox32_Thick.Buffer.CombineMode := cmBlend;
  PaintBox32_Thick.Buffer.BeginLockUpdate; // No need for update handling, we will redraw everything

  var Canvas := TCanvas32.Create(PaintBox32_Thick.Buffer);
  try
    var Stroke := TStrokeBrush(Canvas.Brushes.Add(TStrokeBrush));
    Stroke.StrokeWidth := ThickLineWidth;

    var LastPoint := FloatPoint(0, 0);

    RandSeed := 0;
    var Stopwatch := TStopwatch.StartNew;

    var LineCount := 0;
    while (LineCount < MinLineCount) or (Stopwatch.ElapsedMilliseconds < MinTestTime) do
    begin
      Inc(LineCount);
      Stroke.FillColor := Color32(Random($00FFFFFF)); // Color32 to swap R and B

      Canvas.MoveTo(LastPoint); // EndPath clears last point so we have to set it manually
      LastPoint := FloatPoint(Random(PaintBox32_Thick.Width), Random(PaintBox32_Thick.Height));

      Canvas.LineTo(LastPoint);

      Canvas.EndPath; // Each line must be its own path, with its own stroke color
    end;

    Stopwatch.Stop;

    PaintBox32_Thick.Buffer.EndLockUpdate;
    Label4.Caption := Format('TCanvas32.LineTo, Width=%d. Lines per second: %.0n', [ThickLineWidth, LineCount / Stopwatch.ElapsedMilliseconds * 1000]);
  finally
    Canvas.Free;
  end;
  Screen.Cursor := crDefault;
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.PaintBox32_ThickLinePaintBuffer(Sender: TObject);
begin
  (*
  ** Graphics32, thick line via DrawThickLine.  Aliased. No alpha blending.
  *)

  if (not FDoPaint) then
    exit;

  Screen.Cursor := crHourGlass;
  PaintBox32_ThickLine.Buffer.Clear(clWhite32);
  PaintBox32_ThickLine.Buffer.DrawMode := dmOpaque;
  PaintBox32_ThickLine.Buffer.CombineMode := cmBlend;
  PaintBox32_ThickLine.Buffer.BeginLockUpdate; // No need for update handling, we will redraw everything
  var LastPos := Point(0, 0);

  RandSeed := 0;
  var Stopwatch := TStopwatch.StartNew;

  var LineCount := 0;
  while (LineCount < MinLineCount) or (Stopwatch.ElapsedMilliseconds < MinTestTime) do
  begin
    Inc(LineCount);
    var Color: TColor32 := Color32(Random($00FFFFFF)); // Color32 to swap R and B
    var NewPos := Point(Random(PaintBox32_ThickLine.Width), Random(PaintBox32_ThickLine.Height));
    DrawThickLine(PaintBox32_ThickLine.Buffer, LastPos.X, LastPos.Y, NewPos.X, NewPos.Y, ThickLineWidth, Color);
    LastPos := NewPos;
  end;

  Stopwatch.Stop;

  PaintBox32_ThickLine.Buffer.EndLockUpdate;
  Label5.Caption := Format('Graphics32 DrawThickLine, Width=%d. Lines per second: %.0n', [ThickLineWidth, LineCount / Stopwatch.ElapsedMilliseconds * 1000]);
  Screen.Cursor := crDefault;
end;

//------------------------------------------------------------------------------

end.
