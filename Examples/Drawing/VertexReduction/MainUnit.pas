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

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ENDIF} Classes, ComCtrls, Controls, Forms,
  GR32, GR32_Image, GR32_Polygons, GR32_Paths;

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
    FLastEpsilon: TFloat;
    FRenderer: TPolygonRenderer32VPR;
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
  GR32_VectorUtils;


{ TFrmLineSimplification }

procedure TFrmLineSimplification.FormCreate(Sender: TObject);
begin
  FRenderer := TPolygonRenderer32VPR.Create(PaintBox32.Buffer);
  FRenderer.Color := clBlack32;
end;

procedure TFrmLineSimplification.FormDestroy(Sender: TObject);
begin
  FRenderer.Free;
end;

procedure TFrmLineSimplification.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13:
      begin
        FLastEpsilon := 2 * FLastEpsilon;
        if Length(FPoints) > 0 then
          FPoints := VertexReduction(FPoints, FLastEpsilon);
        PaintBox32.Invalidate;
      end;
    27:
      Close;
  end;
end;

procedure TFrmLineSimplification.PaintBox32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetLength(FPoints, 1);
  FPoints[0] := FloatPoint(X, Y);
  PaintBox32.OnMouseMove := PaintBox32MouseMove;
end;

procedure TFrmLineSimplification.PaintBox32MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  Index := Length(FPoints) - 1;
  if (FPoints[Index].X <> X) and (FPoints[Index].Y <> Y) then
  begin
    Index := Length(FPoints);
    SetLength(FPoints, Index + 1);
    FPoints[Index] := FloatPoint(X, Y);
    PaintBox32.Invalidate;
  end;
end;

procedure TFrmLineSimplification.PaintBox32MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  Index := Length(FPoints) - 1;
  if (FPoints[Index].X <> X) and (FPoints[Index].Y <> Y) then
  begin
    Index := Length(FPoints);
    SetLength(FPoints, Index + 1);
    FPoints[Index] := FloatPoint(X, Y);
  end;

  FLastEpsilon := 1;
  if ssShift in Shift then
    FLastEpsilon := 5
  else
  if ssCtrl in Shift then
    FLastEpsilon := 0.5;

  FPoints := VertexReduction(FPoints, FLastEpsilon);

  PaintBox32.Invalidate;
  PaintBox32.OnMouseMove := nil;
end;

procedure TFrmLineSimplification.PaintBox32PaintBuffer(Sender: TObject);
var
  Index: Integer;
  r: TRect;
  rf: TFloatRect;
begin
  with PaintBox32.Buffer do
  begin
    Clear($FFFFFFFF);

    FRenderer.PolygonFS(BuildPolyline(FPoints, 2));

    for Index := 0 to High(FPoints) do
    begin
      rf := FloatRect(FPoints[Index], FPoints[Index]);
      rf.Inflate(4.0, 4.0);

      r := MakeRect(rf, rrClosest);
      FillRectS(r, clBlack32);
    end;
  end;
end;

end.

