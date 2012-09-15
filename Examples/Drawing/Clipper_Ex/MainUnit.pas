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

uses
  {$IFNDEF FPC}Windows, {$ELSE} LCLIntf, LCLType, {$ENDIF} SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, GR32, GR32_Image,
  GR32_Polygons, GR32_Clipper,
  //The following clipper unit is open source freeware and is available from
  //SourceForge here: http://sourceforge.net/projects/polyclipping/
  clipper;

type
  TFrmClipper = class(TForm)
    BtnClose: TButton;
    ImgView32: TImgView32;
    PnlControl: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
  end;

var
  FrmClipper: TFrmClipper;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

const
  Ellipse: array [0..129] of Single = (
    120.00, 70.00, 119.74, 75.11, 118.98, 80.08, 117.75, 84.87, 116.07,
    89.46, 113.97, 93.83, 111.46, 97.96, 108.58, 101.80, 105.36, 105.36,
    101.80, 108.58, 97.96, 111.46, 93.83, 113.97, 89.46, 116.07, 84.87,
    117.75, 80.08, 118.98, 75.11, 119.74, 70.00, 120.00, 64.89, 119.74,
    59.92, 118.98, 55.13, 117.75, 50.54, 116.07, 46.17, 113.97, 42.04,
    111.46, 38.20, 108.58, 34.64, 105.36, 31.42, 101.80, 28.54, 97.96,
    26.03, 93.83, 23.93, 89.46, 22.25, 84.87, 21.02, 80.08, 20.26, 75.11,
    20.00, 70.00, 20.26, 64.89, 21.02, 59.92, 22.25, 55.13, 23.93, 50.54,
    26.03, 46.17, 28.54, 42.04, 31.42, 38.20, 34.64, 34.64, 38.20, 31.42,
    42.04, 28.54, 46.17, 26.03, 50.54, 23.93, 55.13, 22.25, 59.92, 21.02,
    64.89, 20.26, 70.00, 20.00, 75.11, 20.26, 80.08, 21.02, 84.87, 22.25,
    89.46, 23.93, 93.83, 26.03, 97.96, 28.54, 101.80, 31.42, 105.36, 34.64,
    108.58, 38.20, 111.46, 42.04, 113.97, 46.17, 116.07, 50.54, 117.75,
    55.13, 118.98, 59.92, 119.74, 64.89, 120.00, 70.00
  );

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function Translate(const Pts: TArrayOfArrayOfFloatPoint;
  dx, dy: Integer): TArrayOfArrayOfFloatPoint;
var
  i, j: Integer;
begin
  SetLength(Result, Length(pts));
  for i := 0 to High(pts) do
  begin
    SetLength(Result[i], Length(pts[i]));
    for j := 0 to High(pts[i]) do
    begin
      Result[i, j].X := pts[i, j].X + dx;
      Result[i, j].Y := pts[i, j].Y + dy;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawStippled(Bitmap: TBitmap32;
  const aafp: TArrayOfArrayOfFloatPoint;
  StippleColors: array of TColor32; StippleStep: Single);
var
  i, j: Integer;
begin
  Bitmap.StippleStep := StippleStep;
  Bitmap.SetStipple(StippleColors);
  for i := 0 to High(aafp) do
  begin
    Bitmap.MoveToF(aafp[i, 0].X,aafp[i, 0].Y);
    for j := 1 to High(aafp[i]) do
      Bitmap.LineToFSP(aafp[i, j].X, aafp[i, j].Y);
    Bitmap.LineToFSP(aafp[i, 0].X, aafp[i, 0].Y);
  end;
end;
//------------------------------------------------------------------------------

procedure TFrmClipper.FormCreate(Sender: TObject);
var
  i, Len: Integer;
  Circs1, Circs2: TArrayOfArrayOfFloatPoint;
begin
  Caption := Application.Title;

  //make the first circle from the array of Ellipse coords above ...
  Len := Length(Ellipse) div 2;
  SetLength(Circs1, 1);
  SetLength(Circs1[0], Len);
  for i := 0 to Len - 1 do
  begin
    //*2 because the Ellipse coords were bit small
    Circs1[0, i].X := Ellipse[i * 2] * 2;
    Circs1[0, i].Y := Ellipse[i * 2 + 1] * 2;
  end;

  //make a second inner circle by offsetting the first ...
  Circs2 := OffsetPolygons32(Circs1, -10);
  //and copy Circs2[0] into Circs1[1] ...
  SetLength(Circs1, 2);
  Circs1[1] := copy(Circs2[0], 0, Length(Circs2[0]));

  //now duplicate Circs1 and translate/move right 70px ...
  Circs2 := Translate(Circs1, 70, 0);

  ImgView32.SetupBitmap(True, clWhite32);
  //PolyPolyLineFS(ImgView321.Bitmap, Circs1, clLightGray32, True, 0.5);
  //PolyPolyLineFS(ImgView321.Bitmap, Circs2, clLightGray32, True, 0.5);

  //get the UNION of Circs1 and Circs2 and save Result into Circs1 ...
  with TClipper32.Create do
  try
    AddArrayOfArrayOfFloatPoint(Circs1, ptSubject);
    AddArrayOfArrayOfFloatPoint(Circs2, ptClip);
    Execute(ctUnion, Circs1, pftEvenOdd, pftEvenOdd);
  finally
    free;
  end;
  //draw the union (Circs1) Result in red ...
  PolyPolyLineFS(ImgView32.Bitmap, Circs1, clRed32, True, 2);
  PolyPolygonFS(ImgView32.Bitmap, Circs1, $40FF0000);

  //finally, offset Circs1 by 4px ...
  Circs2 := OffsetPolygons32(Circs1, 4);
  //and draw (stippled) this offsetted outline of Circs1 ...
  //PolyPolyLineFS(ImgView321.Bitmap, Circs2, clGray32, True, 1.0);
  DrawStippled(ImgView32.Bitmap, Circs2, [clBlue32, clBlue32, $000000FF], 0.25);
end;
//------------------------------------------------------------------------------

procedure TFrmClipper.BtnCloseClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
