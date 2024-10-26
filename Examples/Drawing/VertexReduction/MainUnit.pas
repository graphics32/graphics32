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
 * The Original Code is Lion Example
 *
 * The Initial Developer(s) of the Original Code is:
 * Christian-W. Budde <Christian@savioursofsoul.de>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2012
 * the Initial Developer. All Rights Reserved.
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ENDIF} Classes, ComCtrls, Controls, Forms,
  GR32,
  GR32_Image,
  GR32_Paths,
  GR32_Brushes;

type
  TFrmLineSimplification = class(TForm)
    PaintBox32: TPaintBox32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure PaintBox32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FPoints: TArrayOfFloatPoint;
    FSimplifiedPoints: TArrayOfFloatPoint;
    FEpsilon: TFloat;
    FCanvas: TCanvas32;
    FSourceBrush: TStrokeBrush;
    FSimplifiedBrush: TStrokeBrush;
  end;

var
  FrmLineSimplification: TFrmLineSimplification;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Types,
  SysUtils,
  Windows,
  GR32_VectorUtils;

const
  StartEpsilon = 1;
  MinEpsilon = 0.01;
  MaxEpsilon = 500;

resourcestring
  sHelp = 'Use the mouse to draw an arbitrary polyline.'+#13#13+
    'Use the + and - keys to control how aggresively the line is simplified.';
  sInfo = 'Source points: %.0n'#13+
    'Simplified points: %.0n'#13+
    'Epsilon: %.2n';

{ TFrmLineSimplification }

procedure TFrmLineSimplification.FormCreate(Sender: TObject);
begin
  FCanvas := TCanvas32.Create(PaintBox32.Buffer);

  FSourceBrush := FCanvas.Brushes.Add(TStrokeBrush) as TStrokeBrush;
  FSourceBrush.FillColor := clTrRed32;
  FSourceBrush.StrokeWidth := 5;

  FSimplifiedBrush := FCanvas.Brushes.Add(TStrokeBrush) as TStrokeBrush;
  FSimplifiedBrush.FillColor := clTrBlack32;
  FSimplifiedBrush.StrokeWidth := 1;

  FEpsilon := StartEpsilon;
end;

procedure TFrmLineSimplification.FormDestroy(Sender: TObject);
begin
  FCanvas.Free;
end;

procedure TFrmLineSimplification.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ADD, VK_SUBTRACT:
      begin
        case Key of
          VK_SUBTRACT:
            begin
              if (FEpsilon <= MinEpsilon) then
                exit;
              FEpsilon := FEpsilon / 2;
            end;

          VK_ADD:
            begin
              if (FEpsilon >= MaxEpsilon) then
                exit;
              FEpsilon := FEpsilon * 2;
            end;
        end;

        if Length(FPoints) > 0 then
          FSimplifiedPoints := VertexReduction(FPoints, FEpsilon);

        PaintBox32.Invalidate;
      end;

    VK_ESCAPE: // Escape
      Close;
  end;
end;

procedure TFrmLineSimplification.PaintBox32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetLength(FSimplifiedPoints, 0);
  SetLength(FPoints, 1);
  FPoints[0] := FloatPoint(X, Y);
  PaintBox32.OnMouseMove := PaintBox32MouseMove;
end;

procedure TFrmLineSimplification.PaintBox32MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  Index := High(FPoints);
  if (FPoints[Index].X <> X) and (FPoints[Index].Y <> Y) then
  begin
    SetLength(FPoints, Length(FPoints)+1);
    FPoints[High(FPoints)] := FloatPoint(X, Y);
    PaintBox32.Invalidate;
  end;
end;

procedure TFrmLineSimplification.PaintBox32MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  Index := High(FPoints);
  if (FPoints[Index].X <> X) and (FPoints[Index].Y <> Y) then
  begin
    SetLength(FPoints, Length(FPoints)+1);
    FPoints[High(FPoints)] := FloatPoint(X, Y);
  end;

  // Enable the next line to close the polyline (i.e. a polygon). See issue #87
  // FPoints[High(FPoints)] := FPoints[0];

  // Simplify the polyline
  FSimplifiedPoints := VertexReduction(FPoints, FEpsilon);

  PaintBox32.Invalidate;
  PaintBox32.OnMouseMove := nil;
end;

procedure TFrmLineSimplification.PaintBox32PaintBuffer(Sender: TObject);
var
  Index: Integer;
  r: TRect;
  rf: TFloatRect;
  ColorPoint: TColor32;
begin
  PaintBox32.Buffer.Clear(clWhite32);

  r := PaintBox32.Buffer.BoundsRect;

  if (Length(FPoints) = 0) then
  begin
    PaintBox32.Buffer.Textout(r, DT_CENTER or DT_NOPREFIX or DT_CALCRECT, sHelp);
    GR32.OffsetRect(r, r.Left + (PaintBox32.Buffer.Width - r.Width) div 2, r.Top + (PaintBox32.Buffer.Height - r.Height) div 2);

    PaintBox32.Buffer.Textout(r, DT_CENTER or DT_NOPREFIX, sHelp);
    exit;
  end;

  PaintBox32.Buffer.Textout(r, 0, Format(sInfo, [Length(FPoints)*1.0, Length(FSimplifiedPoints)*1.0, FEpsilon]));

  if (Length(FPoints) > 0) then
  begin
    FSourceBrush.Visible := True;
    FSimplifiedBrush.Visible := False;
    FCanvas.PolyLine(FPoints);
    FCanvas.EndPath;

    ColorPoint := SetAlpha(FSourceBrush.FillColor, 255);

    for Index := 0 to High(FPoints) do
    begin
      rf := FloatRect(FPoints[Index], FPoints[Index]);
      rf.Inflate(1.0, 1.0);
      r := MakeRect(rf, rrClosest);

      PaintBox32.Buffer.FillRectTS(r, ColorPoint);
    end;
  end;

  if (Length(FSimplifiedPoints) > 0) then
  begin
    FSourceBrush.Visible := False;
    FSimplifiedBrush.Visible := True;
    FCanvas.PolyLine(FSimplifiedPoints);
    FCanvas.EndPath;

    ColorPoint := SetAlpha(FSimplifiedBrush.FillColor, 255);

    for Index := 0 to High(FSimplifiedPoints) do
    begin
      rf := FloatRect(FSimplifiedPoints[Index], FSimplifiedPoints[Index]);
      rf.Inflate(4.0, 4.0);
      r := MakeRect(rf, rrClosest);

      PaintBox32.Buffer.FrameRectTS(r, ColorPoint);
    end;
  end;
end;

end.

