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
  Types, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Math,
  GR32, GR32_Image, GR32_Polygons, GR32_Layers, GR32_Geometry,
  GR32_Math, GR32_VectorUtils, GR32_Clipper;

type
  TFrmClipper = class(TForm)
    BtnClear: TButton;
    BtnExit: TButton;
    ImgView32: TImgView32;
    PnlControl: TPanel;
    rgClipping: TRadioGroup;
    RgpObject: TRadioGroup;
    BtnInflate: TButton;
    BtnDeflate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    procedure ImgView32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure ImgView32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure BtnClearClick(Sender: TObject);
    procedure ImgView32MouseLeave(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtnInflateClick(Sender: TObject);
    procedure BtnDeflateClick(Sender: TObject);
  private
    Polys: TArrayOfArrayOfFloatPoint;
    OutlinePolygon: TArrayOfFloatPoint;
    procedure AddPolygon(const Pts: TArrayOfFloatPoint);
    function MakeRectangle(const NewPoint: TPoint): TArrayOfFloatPoint;
    function MakeEllipse(const NewPoint: TPoint): TArrayOfFloatPoint;
    function MakeStar(const NewPoint: TPoint): TArrayOfFloatPoint;
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

procedure DrawStippled(Bitmap: TBitmap32;
  const Afp: TArrayOfFloatPoint;
  StippleColors: array of TColor32; StippleStep: TFloat);
var
  i: Integer;
begin
  if Afp = nil then Exit;
  Bitmap.StippleStep := StippleStep;
  Bitmap.SetStipple(StippleColors);
  Bitmap.MoveToF(Afp[0].X, Afp[0].Y);
  for i := 1 to High(Afp) do
    Bitmap.LineToFSP(Afp[i].X, Afp[i].Y);
  Bitmap.LineToFSP(Afp[0].X, Afp[0].Y);
end;


{ TFrmClipper methods }

procedure TFrmClipper.FormCreate(Sender: TObject);
begin
  ImgView32.SetupBitmap(true);
  AddPolygon(MakeStar(GR32.Point(125, 150)));
  ImgView32.ScrollToCenter(0, 0);
end;

procedure TFrmClipper.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
    Exit;
end;

procedure TFrmClipper.AddPolygon(const Pts: TArrayOfFloatPoint);
var
  ct: TClipType;
  Clipper: TClipper32;
begin
  Clipper := TClipper32.Create;
  try
    //add multiple contours of existing polygons as subject polygons ...
    Clipper.AddPaths(Polys, ptSubject);
    //add the single contour of the new polygon as the clipping polygon ...
    Clipper.AddPath(Pts, ptClip);

    //do the clipping operation (result => Polys) ...
    case rgClipping.ItemIndex of
      0: ct := ctIntersection;
      1:  ct := ctUnion;
      2:  ct := ctDifference;
      else  ct := ctXor;
    end;
    Clipper.Execute(ct, frNonZero, Polys);
  finally
    Clipper.Free;
  end;
  DrawPolygons;
end;

function TFrmClipper.MakeRectangle(const NewPoint: TPoint): TArrayOfFloatPoint;
begin
  SetLength(Result, 4);
  Result[0] := FloatPoint(NewPoint.X - 50, NewPoint.Y - 30);
  Result[1] := FloatPoint(NewPoint.X + 50, NewPoint.Y - 30);
  Result[2] := FloatPoint(NewPoint.X + 50, NewPoint.Y + 30);
  Result[3] := FloatPoint(NewPoint.X - 50, NewPoint.Y + 30);
end;

function TFrmClipper.MakeEllipse(const NewPoint: TPoint): TArrayOfFloatPoint;
begin
  Result := Ellipse(FloatPoint(NewPoint), FloatPoint(60,40));
end;

function TFrmClipper.MakeStar(const NewPoint: TPoint): TArrayOfFloatPoint;
begin
  Result := Star(FloatPoint(NewPoint), 40.0, 60.0, 7);
end;

procedure TFrmClipper.DrawPolygons;
begin
  ImgView32.Bitmap.FillRectS(ImgView32.Bitmap.BoundsRect, clWhite32);
  PolyPolyLineFS(ImgView32.Bitmap, Polys, clRed32, True, 2);
  PolyPolygonFS(ImgView32.Bitmap, Polys, $40FF0000, pfWinding);
  DrawStippled(ImgView32.Bitmap,
    OutlinePolygon, [clBlue32, clBlue32, $000000FF], 0.35);
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
  NewPt := ImgView32.ControlToBitmap(GR32.Point(X, Y));
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

procedure TFrmClipper.BtnInflateClick(Sender: TObject);
begin
  Polys := InflatePaths(Polys, 10, jtRound, etPolygon);
  DrawPolygons;
end;

procedure TFrmClipper.BtnDeflateClick(Sender: TObject);
begin
  Polys := InflatePaths(Polys, -10, jtRound, etPolygon);
  DrawPolygons;
end;

end.
