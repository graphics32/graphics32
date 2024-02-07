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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Andre Beckedorf <andre@metaexception.de>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ENDIF}
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, Math, Vcl.ExtDlgs, Vcl.Menus, System.Actions, Vcl.ActnList,

  GR32_Paths, GR32_Polygons,
  GR32_VectorUtils, GR32, GR32_Gamma, GR32_Blend, GR32_Image,
  Gr32_Clipper;

type
  TFormGrow = class(TForm)
    Image: TImage32;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    N1: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemRefresh: TMenuItem;
    MenuItemOptions: TMenuItem;
    MenuItemOptionsInflatePolygon: TMenuItem;
    MenuItemOptionsInflatePolyLine: TMenuItem;
    ActionList: TActionList;
    ActionRefresh: TAction;
    ActionFileExit: TAction;
    ActionOptionShapePolygon: TAction;
    ActionOptionShapePolyLine: TAction;
    Joinstyle1: TMenuItem;
    N2: TMenuItem;
    Endstyle1: TMenuItem;
    Miterjoin1: TMenuItem;
    Beveljoin1: TMenuItem;
    Beveljoin2: TMenuItem;
    RoundExjoin1: TMenuItem;
    ActionOptionJoinMiter: TAction;
    ActionOptionJoinBevel: TAction;
    ActionOptionJoinRound: TAction;
    ActionOptionJoinRoundEx: TAction;
    ActionOptionEndButt: TAction;
    ActionOptionEndSquare: TAction;
    ActionOptionEndRound: TAction;
    Action51: TMenuItem;
    Action52: TMenuItem;
    Action71: TMenuItem;
    ActionOptionJoinStyle: TAction;
    ActionOptionEndStyle: TAction;
    procedure ImageClick(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionFileExitExecute(Sender: TObject);
    procedure ActionOptionShapeExecute(Sender: TObject);
    procedure ActionOptionJoinStyleExecute(Sender: TObject);
    procedure ActionOptionEndStyleExecute(Sender: TObject);
    procedure ActionDummyExecute(Sender: TObject);
    procedure ActionOptionEndStylesUpdate(Sender: TObject);
    procedure ActionOptionJoinStyleUpdate(Sender: TObject);
    procedure ActionOptionEndStyleUpdate(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
  private
    FJoinStyle: TJoinStyle;
    FEndStyle: TEndStyle;
    FPolyPoints: TArrayOfArrayOfFloatPoint;
    function GeneratePolygon(MaxWidth, MaxHeight, EdgeCount: integer): TArrayOfFloatPoint;
    procedure ApplyOptionsAndRedraw;
    procedure CreateNewPolygonAndApplyOptions;
  public
  end;

var
  FormGrow: TFormGrow;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{$IFDEF Darwin}
uses
  MacOSAll;
{$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function Area(const Path: TArrayOfFloatPoint): Single;
var
  i, j, HighI: Integer;
  d: Single;
begin
  Result := 0.0;
  HighI := High(Path);
  if (HighI < 2) then Exit;
  j := HighI;
  for i := 0 to HighI do
  begin
    d := (Path[j].X + Path[i].X);
    Result := Result + d * (Path[j].Y - Path[i].Y);
    j := i;
  end;
  Result := -Result * 0.5;
end;
//------------------------------------------------------------------------------

function MakePath(const pts: array of integer): TArrayOfFloatPoint;
var
  i, len: Integer;
begin
  Result := nil;
  len := length(pts) div 2;
  Setlength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FloatPoint(pts[i*2], pts[i*2 +1]);
end;
//------------------------------------------------------------------------------

function MakeRandomPath(MaxWidth, MaxHeight, Count: Integer): TArrayOfFloatPoint;
var
  i: Integer;
begin
  Setlength(Result, Count);
  for i := 0 to Count -1 do
  begin
    Result[i].X := 20 + Random(MaxWidth - 40);
    Result[i].Y := 20 + Random(MaxHeight - 40);
  end;
end;
//------------------------------------------------------------------------------

function TFormGrow.GeneratePolygon(MaxWidth, MaxHeight, EdgeCount: integer): TArrayOfFloatPoint;

  function Union(const Paths: TArrayOfArrayOfFloatPoint; FillRule: TFillRule = frEvenOdd): TArrayOfArrayOfFloatPoint;
  var
    Clipper: TClipper;
  begin
    Clipper := TClipper.Create;
    try
      Clipper.AddPaths(Paths, ptSubject, False);
      Clipper.Execute(ctUnion, FillRule, Result);
    finally
      Clipper.Free;
    end;
  end;

var
  PolyPts: TArrayOfArrayOfFloatPoint;
  i,j: integer;
  Area, a: Single;
begin
  SetLength(Result, 4);
  Result[0].X := 50;
  Result[0].Y := 50;
  Result[1].X := 80;
  Result[1].Y := 50;
  Result[2].X := 80;
  Result[2].Y := 80;
  Result[3].X := 50;
  Result[3].Y := 80;
  exit;

  Setlength(PolyPts, 1);
  PolyPts[0] := MakeRandomPath(MaxWidth, MaxHeight, EdgeCount);

  // NOTE: INFLATEPATHS WILL BEHAVE IN AN UNDERTERMINED FASHION
  // WHENEVER SELF-INTERSECTING POLYGONS ARE ENCOUNTERED.

  // so, remove self-intersections
  PolyPts := Union(PolyPts);

  // and find the largest polygon ...
  j := 0;
  Area := Abs(MainUnit.Area(PolyPts[0]));
  for i := 1 to high(PolyPts) do
  begin
    a := Abs(MainUnit.Area(PolyPts[i]));
    if a <= Area then
      Continue;
    j := i;
    Area := a;
  end;

  Result := PolyPts[j];
end;
//------------------------------------------------------------------------------

procedure TFormGrow.ActionOptionEndStylesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not ActionOptionShapePolygon.Checked;
end;

procedure TFormGrow.ActionDummyExecute(Sender: TObject);
begin
//
end;

procedure TFormGrow.ActionFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormGrow.ActionOptionEndStyleUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := (FEndStyle = TEndStyle(TAction(Sender).Tag));
end;

procedure TFormGrow.ActionOptionEndStyleExecute(Sender: TObject);
begin
  FEndStyle := TEndStyle(TAction(Sender).Tag);
  ApplyOptionsAndRedraw;
end;

procedure TFormGrow.ActionOptionShapeExecute(Sender: TObject);
begin
  CreateNewPolygonAndApplyOptions;
end;

procedure TFormGrow.ActionOptionJoinStyleUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := (FJoinStyle = TJoinStyle(TAction(Sender).Tag));
end;

procedure TFormGrow.ActionRefreshExecute(Sender: TObject);
begin
  CreateNewPolygonAndApplyOptions;
end;

procedure TFormGrow.ActionOptionJoinStyleExecute(Sender: TObject);
begin
  FJoinStyle := TJoinStyle(TAction(Sender).Tag);
  ApplyOptionsAndRedraw;
end;

procedure TFormGrow.ApplyOptionsAndRedraw;
var
  PolyPts: TArrayOfArrayOfFloatPoint;
  Closed: boolean;
const
  JoinStyleToJoinType: array[TJoinStyle] of TJoinType = (jtMiter, jtSquare, jtRound, jtRoundEx);
  EndStyleToEndType: array[TEndStyle] of TEndType = (etOpenButt, etOpenSquare, etOpenRound);
begin
  // Apply options to existing polyline/polygon and repaint

  Closed := not ActionOptionShapePolyLine.Checked;

  Image.Bitmap.Clear(clWhite32);

  if (Closed) then
    PolyPolygonFS(image.Bitmap, FPolyPoints, $100000FF);
  PolyPolylineFS(image.Bitmap, FPolyPoints, clBlack32, Closed, 1);

  if ActionOptionShapePolyLine.Checked then
    // INFLATE (GROW / OFFSET) A POLYLINE ...
    PolyPts := InflatePaths(FPolyPoints, 20, JoinStyleToJoinType[FJoinStyle], EndStyleToEndType[FEndStyle])
  else
    // INFLATE (GROW / OFFSET) A POLYGON ...
    PolyPts := InflatePaths(FPolyPoints, 10, JoinStyleToJoinType[FJoinStyle], etPolygon, 10);

  PolyPolylineFS(image.Bitmap, PolyPts, clRed32, True, 1);
  PolyPolygonFS(image.Bitmap, PolyPts, $10FF0000);
end;

procedure TFormGrow.FormCreate(Sender: TObject);
begin
  SetGamma(1.4);
end;

procedure TFormGrow.ImageResize(Sender: TObject);
begin
  Image.Bitmap.SetSize(Image.ClientWidth, Image.ClientHeight);
  CreateNewPolygonAndApplyOptions;
end;

procedure TFormGrow.CreateNewPolygonAndApplyOptions;
begin
  Setlength(FPolyPoints, 1);

  if ActionOptionShapePolyLine.Checked then
    // Generate a polyline
    FPolyPoints[0] := MakeRandomPath(Image.Bitmap.Width, Image.Bitmap.Height, 7)
  else
    // Generate a closed polygon
    repeat
      FPolyPoints[0] := GeneratePolygon(Image.Bitmap.Width, Image.Bitmap.Height, 5);
    until Length(FPolyPoints[0]) > 3;

  ApplyOptionsAndRedraw;
end;

procedure TFormGrow.ImageClick(Sender: TObject);
begin
  CreateNewPolygonAndApplyOptions;
end;

end.
