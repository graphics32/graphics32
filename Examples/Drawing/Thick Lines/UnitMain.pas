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
    PaintBox32_ThinAlpha: TPaintBox32;
    Label1: TLabel;
    Label2: TLabel;
    PaintBoxGDIThick: TPaintBox;
    PaintBox32_Thick: TPaintBox32;
    Label3: TLabel;
    Label4: TLabel;
    ButtonRedraw: TButton;
    Label5: TLabel;
    PaintBox32_ThickLine: TPaintBox32;
    Label6: TLabel;
    PaintBox32_Thin: TPaintBox32;
    procedure PaintBoxGDIThinPaint(Sender: TObject);
    procedure PaintBox32_ThinAlphaPaintBuffer(Sender: TObject);
    procedure PaintBoxGDIThickPaint(Sender: TObject);
    procedure PaintBox32_ThickPaintBuffer(Sender: TObject);
    procedure ButtonRedrawClick(Sender: TObject);
    procedure PaintBox32_ThickLinePaintBuffer(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure PaintBox32Click(Sender: TObject);
    procedure PaintBox32_ThinPaintBuffer(Sender: TObject);
  private
    FDoPaint: boolean;
    procedure NotHung;
  public
  end;

var
  FormThickLineTest: TFormThickLineTest;

implementation

{$R *.dfm}

uses
  System.Types,
  System.Math,

  GR32_System,
  GR32.Lines.Thick,

  GR32,
  GR32_LowLevel,
  GR32_Paths,
  GR32_Brushes,
  GR32_Polygons;


const
  MinLineCount = 200000;
  MinTestTime = 1000;
  MaxTestTime = 4500; // Windows will consider the application hung after 5 seconds
  ThickLineWidth = 10;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.NotHung;
var
  Msg: TMsg;
begin
  // Pump WM_NULL so Windows doesn't consider application hung
  if PeekMessage(Msg, Handle, WM_NULL, WM_NULL, PM_NOREMOVE) and (Msg.message = WM_NULL) then
    PeekMessage(Msg, Handle, WM_NULL, WM_NULL, PM_REMOVE);
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.ButtonRedrawClick(Sender: TObject);
begin
  ButtonRedraw.Enabled := False;
  ButtonRedraw.Update;
  FDoPaint := True;
  try

    Invalidate;
    PaintBox32_ThinAlpha.Invalidate;
    PaintBox32_Thin.Invalidate;
    PaintBox32_Thick.Invalidate;
    PaintBox32_ThickLine.Invalidate;

    Update;

  finally
    FDoPaint := False;
    ButtonRedraw.Enabled := True;
  end;
end;

procedure TFormThickLineTest.PaintBoxClick(Sender: TObject);
begin
  FDoPaint := True;
  try

    TPaintBox(Sender).Invalidate;
    TPaintBox(Sender).Update;

  finally
    FDoPaint := False;
  end;
end;

procedure TFormThickLineTest.PaintBox32Click(Sender: TObject);
begin
  FDoPaint := True;
  try

    TPaintBox32(Sender).Invalidate;
    TPaintBox32(Sender).Update;

  finally
    FDoPaint := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.PaintBoxGDIThinPaint(Sender: TObject);
var
  Stopwatch: TStopwatch;
  LineCount: integer;
begin
  (*
  ** GDI, thin line. Aliased. No alpha blending.
  *)

  if (not FDoPaint) then
    exit;

  Screen.Cursor := crHourGlass;

  TPaintBox(Sender).Canvas.Brush.Color := clWhite;
  TPaintBox(Sender).Canvas.Brush.Style := bsSolid;
  TPaintBox(Sender).Canvas.FillRect(PaintBoxGDIThin.Canvas.ClipRect);

  TPaintBox(Sender).Canvas.Pen.Width := 1;
  TPaintBox(Sender).Canvas.MoveTo(0,0);

  RandSeed := 0;
  Stopwatch := TStopwatch.StartNew;

  LineCount := 0;
  while ((LineCount < MinLineCount) or (Stopwatch.ElapsedMilliseconds < MinTestTime)) and (Stopwatch.ElapsedMilliseconds < MaxTestTime) do
  begin
    Inc(LineCount);
    TPaintBox(Sender).Canvas.Pen.Color := Random($00FFFFFF);
    TPaintBox(Sender).Canvas.LineTo(Random(TPaintBox(Sender).Width), Random(TPaintBox(Sender).Height));
  end;

  Stopwatch.Stop;

  Label1.Caption := Format('TCanvas.LineTo, Width=1.'#13'Lines per second: %.0n', [LineCount / Stopwatch.ElapsedMilliseconds * 1000]);
  NotHung;
  Screen.Cursor := crDefault;
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.PaintBoxGDIThickPaint(Sender: TObject);
var
  Stopwatch: TStopwatch;
  LineCount: integer;
begin
  (*
  ** GDI, thick line. Aliased. No alpha blending.
  *)

  if (not FDoPaint) then
    exit;

  Screen.Cursor := crHourGlass;
  TPaintBox(Sender).Canvas.Pen.Width := ThickLineWidth;
  TPaintBox(Sender).Canvas.MoveTo(0,0);
  TPaintBox(Sender).Canvas.Brush.Color := clWhite;
  TPaintBox(Sender).Canvas.Brush.Style := bsSolid;
  TPaintBox(Sender).Canvas.FillRect(TPaintBox(Sender).Canvas.ClipRect);

  RandSeed := 0;
  Stopwatch := TStopwatch.StartNew;

  LineCount := 0;
  while ((LineCount < MinLineCount) or (Stopwatch.ElapsedMilliseconds < MinTestTime)) and (Stopwatch.ElapsedMilliseconds < MaxTestTime) do
  begin
    Inc(LineCount);
    TPaintBox(Sender).Canvas.Pen.Color := Random($00FFFFFF);
    TPaintBox(Sender).Canvas.LineTo(Random(TPaintBox(Sender).Width), Random(TPaintBox(Sender).Height));
  end;

  Stopwatch.Stop;

  Label3.Caption := Format('TCanvas.LineTo, Width=%d.'#13'Lines per second: %.0n', [ThickLineWidth, LineCount / Stopwatch.ElapsedMilliseconds * 1000]);
  NotHung;
  Screen.Cursor := crDefault;
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.PaintBox32_ThinAlphaPaintBuffer(Sender: TObject);
var
  Stopwatch: TStopwatch;
  LineCount: integer;
begin
  (*
  ** Graphics32, thin line. Anti-aliased & Alpha blended.
  *)

  if (not FDoPaint) then
    exit;

  Screen.Cursor := crHourGlass;
  TPaintBox32(Sender).Buffer.Clear(clWhite32);
  TPaintBox32(Sender).Buffer.DrawMode := dmOpaque;
  TPaintBox32(Sender).Buffer.CombineMode := cmBlend;
  TPaintBox32(Sender).Buffer.BeginLockUpdate; // No need for update handling, we will redraw everything

  TPaintBox32(Sender).Buffer.MoveTo(0, 0);

  RandSeed := 0;
  Stopwatch := TStopwatch.StartNew;

  LineCount := 0;
  while ((LineCount < MinLineCount) or (Stopwatch.ElapsedMilliseconds < MinTestTime)) and (Stopwatch.ElapsedMilliseconds < MaxTestTime) do
  begin
    Inc(LineCount);
    TPaintBox32(Sender).Buffer.PenColor := Color32(Random($00FFFFFF)); // Color32 to swap R and B
    TPaintBox32(Sender).Buffer.LineToAS(Random(TPaintBox32(Sender).Width), Random(TPaintBox32(Sender).Height));
  end;

  Stopwatch.Stop;

  TPaintBox32(Sender).Buffer.EndLockUpdate;
  Label2.Caption := Format('TBitmap32.LineToAS, Width=1.'#13'Lines per second: %.0n', [LineCount / Stopwatch.ElapsedMilliseconds * 1000]);
  NotHung;
  Screen.Cursor := crDefault;
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.PaintBox32_ThinPaintBuffer(Sender: TObject);
var
  Stopwatch: TStopwatch;
  LineCount: integer;
begin
  (*
  ** Graphics32, thin line. Aliased. No alpha blending.
  *)

  if (not FDoPaint) then
    exit;

  Screen.Cursor := crHourGlass;
  TPaintBox32(Sender).Buffer.Clear(clWhite32);
  TPaintBox32(Sender).Buffer.DrawMode := dmOpaque;
  TPaintBox32(Sender).Buffer.CombineMode := cmBlend;
  TPaintBox32(Sender).Buffer.BeginLockUpdate; // No need for update handling, we will redraw everything

  TPaintBox32(Sender).Buffer.MoveTo(0, 0);

  RandSeed := 0;
  Stopwatch := TStopwatch.StartNew;

  LineCount := 0;
  while ((LineCount < MinLineCount) or (Stopwatch.ElapsedMilliseconds < MinTestTime)) and (Stopwatch.ElapsedMilliseconds < MaxTestTime) do
  begin
    Inc(LineCount);
    TPaintBox32(Sender).Buffer.PenColor := Color32(Random($00FFFFFF)); // Color32 to swap R and B
    TPaintBox32(Sender).Buffer.LineToS(Random(TPaintBox32(Sender).Width), Random(TPaintBox32(Sender).Height));
  end;

  Stopwatch.Stop;

  TPaintBox32(Sender).Buffer.EndLockUpdate;
  Label6.Caption := Format('TBitmap32.LineToS, Width=1.'#13'Lines per second: %.0n', [LineCount / Stopwatch.ElapsedMilliseconds * 1000]);
  NotHung;
  Screen.Cursor := crDefault;
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.PaintBox32_ThickPaintBuffer(Sender: TObject);
var
  Canvas: TCanvas32;
  Stroke: TStrokeBrush;
  LastPoint: TFloatPoint;
  Stopwatch: TStopwatch;
  LineCount: integer;
begin
  (*
  ** Graphics32, thick line via TCanvas32. Anti-aliased & Alpha blended.
  *)

  if (not FDoPaint) then
    exit;

  Screen.Cursor := crHourGlass;
  TPaintBox32(Sender).Buffer.Clear(clWhite32);
  TPaintBox32(Sender).Buffer.DrawMode := dmOpaque;
  TPaintBox32(Sender).Buffer.CombineMode := cmBlend;
  TPaintBox32(Sender).Buffer.BeginLockUpdate; // No need for update handling, we will redraw everything

  Canvas := TCanvas32.Create(TPaintBox32(Sender).Buffer);
  try
    Stroke := TStrokeBrush(Canvas.Brushes.Add(TStrokeBrush));
    Stroke.StrokeWidth := ThickLineWidth;

    LastPoint := GR32.FloatPoint(0, 0);

    RandSeed := 0;
    Stopwatch := TStopwatch.StartNew;

    LineCount := 0;
    while ((LineCount < MinLineCount) or (Stopwatch.ElapsedMilliseconds < MinTestTime)) and (Stopwatch.ElapsedMilliseconds < MaxTestTime) do
    begin
      Inc(LineCount);
      Stroke.FillColor := Color32(Random($00FFFFFF)); // Color32 to swap R and B

      Canvas.MoveTo(LastPoint); // EndPath clears last point so we have to set it manually
      LastPoint := GR32.FloatPoint(Random(TPaintBox32(Sender).Width), Random(TPaintBox32(Sender).Height));

      Canvas.LineTo(LastPoint);

      Canvas.EndPath; // Each line must be its own path, with its own stroke color
    end;

    Stopwatch.Stop;

    TPaintBox32(Sender).Buffer.EndLockUpdate;
  finally
    Canvas.Free;
  end;

  Label4.Caption := Format('TCanvas32.LineTo, Width=%d.'#13'Lines per second: %.0n', [ThickLineWidth, LineCount / Stopwatch.ElapsedMilliseconds * 1000]);
  NotHung;
  Screen.Cursor := crDefault;
end;

//------------------------------------------------------------------------------

procedure TFormThickLineTest.PaintBox32_ThickLinePaintBuffer(Sender: TObject);
var
  LastPos, NewPos: TPoint;
  Stopwatch: TStopwatch;
  LineCount: integer;
  Color: TColor32;
begin
  (*
  ** Graphics32, thick line via DrawThickLine.  Aliased. No alpha blending.
  *)

  if (not FDoPaint) then
    exit;

  Screen.Cursor := crHourGlass;
  TPaintBox32(Sender).Buffer.Clear(clWhite32);
  TPaintBox32(Sender).Buffer.DrawMode := dmOpaque;
  TPaintBox32(Sender).Buffer.CombineMode := cmBlend;
  TPaintBox32(Sender).Buffer.BeginLockUpdate; // No need for update handling, we will redraw everything
  LastPos := GR32.Point(0, 0);

  RandSeed := 0;
  Stopwatch := TStopwatch.StartNew;

  LineCount := 0;
  while ((LineCount < MinLineCount) or (Stopwatch.ElapsedMilliseconds < MinTestTime)) and (Stopwatch.ElapsedMilliseconds < MaxTestTime) do
  begin
    Inc(LineCount);
    Color := Color32(Random($00FFFFFF)); // Color32 to swap R and B
    NewPos := GR32.Point(Random(TPaintBox32(Sender).Width), Random(TPaintBox32(Sender).Height));
    DrawThickLine(TPaintBox32(Sender).Buffer, LastPos.X, LastPos.Y, NewPos.X, NewPos.Y, ThickLineWidth, Color);
    LastPos := NewPos;
  end;

  Stopwatch.Stop;

  TPaintBox32(Sender).Buffer.EndLockUpdate;
  Label5.Caption := Format('Graphics32 DrawThickLine, Width=%d.'#13'Lines per second: %.0n', [ThickLineWidth, LineCount / Stopwatch.ElapsedMilliseconds * 1000]);
  NotHung;
  Screen.Cursor := crDefault;
end;

//------------------------------------------------------------------------------

end.
