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
 * The Original Code is Clipper grow example
 *
 * The Initial Developer of the Original Code is
 * Angus Johnson (http://www.angusj.com)
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ENDIF}
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, Math, Vcl.ExtDlgs, Vcl.Menus, System.Actions, Vcl.ActnList,

  GR32,
  GR32_Polygons,
  GR32_VectorUtils,
  GR32_Image;

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
    ActionOptionJoinSquare: TAction;
    ActionOptionEndButt: TAction;
    ActionOptionEndSquare: TAction;
    ActionOptionEndRound: TAction;
    Action51: TMenuItem;
    Action52: TMenuItem;
    Action71: TMenuItem;
    ActionOptionJoinStyle: TAction;
    ActionOptionEndStyle: TAction;
    ActionOptionGrowClipper: TAction;
    ActionOptionGrowGraphics32: TAction;
    N3: TMenuItem;
    Growusing1: TMenuItem;
    Graphics321: TMenuItem;
    Clipper1: TMenuItem;
    ActionOptionGrowAngus: TAction;
    Image321: TMenuItem;
    ActionOptionJoinRoundEx: TAction;
    RoundExjoin2: TMenuItem;
    procedure ImageClick(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure ActionFileExitExecute(Sender: TObject);
    procedure ActionOptionShapeExecute(Sender: TObject);
    procedure ActionOptionJoinStyleExecute(Sender: TObject);
    procedure ActionOptionEndStyleExecute(Sender: TObject);
    procedure ActionDummyExecute(Sender: TObject);
    procedure ActionOptionEndStylesUpdate(Sender: TObject);
    procedure ActionOptionJoinStyleUpdate(Sender: TObject);
    procedure ActionOptionEndStyleUpdate(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionRedrawExecute(Sender: TObject);
  private
    FJoinStyle: TJoinStyle;
    FEndStyle: TEndStyle;
    FPolyPoints: TArrayOfArrayOfFloatPoint;
    function GeneratePolygon(MaxWidth, MaxHeight, EdgeCount: integer): TArrayOfFloatPoint;
    procedure ApplyOptionsAndRedraw;
    procedure CreateNewPolygonAndApplyOptions;
    function PolyLineBuilderClass: TPolyLineBuilderClass;
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

uses
  GR32_Clipper,
  GR32_Paths,
  GR32_VectorUtils.Reference,
  GR32_VectorUtils.Angus,
  GR32_VectorUtils.Clipper2;

const
  MARGIN = 40;

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

function MakeRandomPath(MaxWidth, MaxHeight, Count: Integer): TArrayOfFloatPoint;
var
  i: Integer;
begin
  Setlength(Result, Count);
  for i := 0 to Count -1 do
  begin
    Result[i].X := MARGIN + Random(MaxWidth - MARGIN * 2);
    Result[i].Y := MARGIN + Random(MaxHeight - MARGIN * 2);
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
  Setlength(PolyPts, 1);
  PolyPts[0] := MakeRandomPath(MaxWidth, MaxHeight, EdgeCount);

  // NOTE: INFLATEPATHS WILL BEHAVE IN AN UNDETERMINED FASHION
  // WHENEVER SELF-INTERSECTING POLYGONS ARE ENCOUNTERED.

  // so, remove self-intersections
  PolyPts := Union(PolyPts);

  if (Length(PolyPts) = 0) then
    // Most likely user has resized window to zero size
    Abort;

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

function TFormGrow.PolyLineBuilderClass: TPolyLineBuilderClass;
begin
  if (ActionOptionGrowClipper.Checked) then
    Result := PolyLineBuilderClipper
  else
  if (ActionOptionGrowAngus.Checked) then
    Result := PolyLineBuilderAngus
  else
    Result := PolyLineBuilderReference;
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
  TAction(Sender).Enabled := (TEndStyle(TAction(Sender).Tag) in PolyLineBuilderClass.SupportedEndStyles);
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
  TAction(Sender).Enabled := (TJoinStyle(TAction(Sender).Tag) in PolyLineBuilderClass.SupportedJoinStyles);
  TAction(Sender).Checked := (FJoinStyle = TJoinStyle(TAction(Sender).Tag));
end;

procedure TFormGrow.ActionRedrawExecute(Sender: TObject);
begin
  ApplyOptionsAndRedraw;
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
  Builder: TPolyLineBuilderClass;
begin
  // Apply options to existing polyline/polygon and repaint

  Closed := not ActionOptionShapePolyLine.Checked;

  Image.Bitmap.Clear(clWhite32);

  if (Closed) then
    PolyPolygonFS(image.Bitmap, FPolyPoints, $100000FF, pfNonZero);
  PolyPolylineFS(image.Bitmap, FPolyPoints, clBlack32, Closed, 1);

  Builder := PolyLineBuilderClass;

  PolyPts := Builder.BuildPolyPolyLine(FPolyPoints, Closed, 20, FJoinStyle, FEndStyle);

  PolyPolylineFS(image.Bitmap, PolyPts, clRed32, True, 1);
  PolyPolygonFS(image.Bitmap, PolyPts, $10FF0000, pfNonZero);
end;

procedure TFormGrow.ImageResize(Sender: TObject);
begin
  Image.Bitmap.SetSize(Image.ClientWidth, Image.ClientHeight);
  CreateNewPolygonAndApplyOptions;
end;

procedure TFormGrow.CreateNewPolygonAndApplyOptions;
begin
  if (Image.Bitmap.Width < 2*MARGIN) or (Image.Bitmap.Height < 2*MARGIN) then
  begin
    Image.Bitmap.Clear(clRed32);
    exit;
  end;

  Caption := IntToStr(RandSeed);

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
