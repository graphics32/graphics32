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
 * The Original Code is GR32_Clipper Example
 *
 * The Initial Developer of the Original Code is
 * Angus Johnson
 *
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFNDEF FPC}Windows, {$ELSE} LCLIntf, LCLType, {$ENDIF} SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, GR32, GR32_Image,
  GR32_Polygons, GR32_Layers, GR32_Geometry, GR32_VectorUtils, GR32_Clipper;

type
  TFrmClipper = class(TForm)
    BtnExit: TButton;
    ImgView32: TImgView32;
    PnlControl: TPanel;
    rgClipping: TRadioGroup;
    BtnClear: TButton;
    RgpObject: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    procedure ImgView32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure ImgView32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure BtnClearClick(Sender: TObject);
    procedure ImgView32MouseLeave(Sender: TObject);
  private
    Polys: TArrayOfArrayOfFixedPoint;
    OutlinePolygon: TArrayOfFixedPoint;
    procedure AddPolygon(const Pts: TArrayOfFixedPoint);
    function MakeRectangle(const NewPoint: TPoint): TArrayOfFixedPoint;
    function MakeEllipse(const NewPoint: TPoint): TArrayOfFixedPoint;
    function MakeStar(const NewPoint: TPoint): TArrayOfFixedPoint;
    procedure DrawPolygons;
  end;

var
  FrmClipper: TFrmClipper;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}


function Translate(const Pts: TArrayOfArrayOfFixedPoint;
  dx, dy: TFixed): TArrayOfArrayOfFixedPoint;
var
  I, J: Integer;
begin
  SetLength(Result, Length(pts));
  for I := 0 to High(pts) do
  begin
    SetLength(Result[I], Length(pts[I]));
    for J := 0 to High(pts[I]) do
    begin
      Result[I, J].X := pts[I, J].X + dx;
      Result[I, J].Y := pts[I, J].Y + dy;
    end;
  end;
end;

procedure DrawStippled(Bitmap: TBitmap32;
  const Afp: TArrayOfFixedPoint;
  StippleColors: array of TColor32; StippleStep: Single);
var
  i: Integer;
begin
  if Afp = nil then Exit;
  Bitmap.StippleStep := StippleStep;
  Bitmap.SetStipple(StippleColors);
  Bitmap.MoveToX(Afp[0].X, Afp[0].Y);
  for i := 1 to High(Afp) do
    Bitmap.LineToXSP(Afp[i].X, Afp[i].Y);
  Bitmap.LineToXSP(Afp[0].X, Afp[0].Y);
end;

function ArrayOfFloatPointToArrayOfFixedPoint(
  const Pts: TArrayOfFloatPoint): TArrayOfFixedPoint;
var
  I, Len: Integer;
begin
  Len := Length(Pts);
  SetLength(Result, Len);
  for I := 0 to Len -1 do
    Result[I] := FixedPoint(Pts[I]);
end;

{ TFrmClipper methods }

procedure TFrmClipper.FormCreate(Sender: TObject);
begin
  ImgView32.Bitmap.SetSize(640, 480);
  AddPolygon(MakeStar(Point(125,150)));
  ImgView32.ScrollToCenter(0, 0);
end;

procedure TFrmClipper.AddPolygon(const Pts: TArrayOfFixedPoint);
begin
  with TClipper.Create do
  try
    //add multiple contours of existing polygons as subject polygons ...
    AddArrayOfArrayOfFixedPoint(Polys, ptSubject);
    //add the single contour of the new polygon as the clipping polygon ...
    AddArrayOfFixedPoint(Pts, ptClip);
    //do the clipping operation (result => Polys) ...
    case rgClipping.ItemIndex of
      0:  Execute(ctIntersection, Polys, pftNonZero);
      1:  Execute(ctUnion, Polys, pftNonZero);
      2:  Execute(ctDifference, Polys, pftNonZero);
      else  Execute(ctXor, Polys, pftNonZero);
    end;
  finally
    free;
  end;
  DrawPolygons;
end;

function TFrmClipper.MakeRectangle(const NewPoint: TPoint): TArrayOfFixedPoint;
begin
  SetLength(Result, 4);
  Result[0] := FixedPoint(Integer(NewPoint.X - 50), Integer(NewPoint.Y - 30));
  Result[1] := FixedPoint(Integer(NewPoint.X + 50), Integer(NewPoint.Y - 30));
  Result[2] := FixedPoint(Integer(NewPoint.X + 50), Integer(NewPoint.Y + 30));
  Result[3] := FixedPoint(Integer(NewPoint.X - 50), Integer(NewPoint.Y + 30));
end;

function TFrmClipper.MakeEllipse(const NewPoint: TPoint): TArrayOfFixedPoint;
var
  EllipseF: TArrayOfFloatPoint;
begin
  EllipseF := Ellipse(FloatPoint(NewPoint), FloatPoint(60,40));
  SetLength(Result, 1);
  Result := ArrayOfFloatPointToArrayOfFixedPoint(EllipseF);
end;

function TFrmClipper.MakeStar(const NewPoint: TPoint): TArrayOfFixedPoint;
var
  StarF: TArrayOfFloatPoint;
begin
  StarF := Star(FloatPoint(NewPoint), 40.0, 60.0, 7);
  SetLength(Result, 1);
  Result := ArrayOfFloatPointToArrayOfFixedPoint(StarF);
end;

procedure TFrmClipper.DrawPolygons;
begin
  ImgView32.Bitmap.FillRectS(ImgView32.Bitmap.BoundsRect, clWhite32);
  PolyPolyLineXS(ImgView32.Bitmap, Polys, clRed32, True, Fixed(2));
  PolyPolygonXS(ImgView32.Bitmap, Polys, $40FF0000, pfWinding);
  DrawStippled(ImgView32.Bitmap, OutlinePolygon, [clBlue32, clBlue32, $000000FF], 0.35);
end;

procedure TFrmClipper.ImgView32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  AddPolygon(OutlinePolygon);
end;

procedure TFrmClipper.ImgView32MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  NewPt: TPoint;
begin
  NewPt := ImgView32.ControlToBitmap(Point(X,Y));
  case RgpObject.ItemIndex of
    0: OutlinePolygon := MakeStar(NewPt);
    1: OutlinePolygon := MakeEllipse(NewPt);
    else OutlinePolygon := MakeRectangle(NewPt);
  end;
  DrawPolygons;
end;

procedure TFrmClipper.ImgView32MouseLeave(Sender: TObject);
begin
  OutlinePolygon := nil;
  DrawPolygons;
end;

procedure TFrmClipper.BtnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmClipper.BtnClearClick(Sender: TObject);
begin
  Polys := nil;
  DrawPolygons;
end;

end.
