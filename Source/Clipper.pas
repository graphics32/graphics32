unit Clipper;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  2 November 2022                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  This module provides a simple interface to the Clipper Library  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Clipper.inc}

uses
  Math, SysUtils,
  Clipper.Core, Clipper.Engine, Clipper.Offset, Clipper.RectClip;

// Redeclare here a number of structures defined in
// other units so those units won't need to be declared
// just to use the following functions.
type
  TClipper    = Clipper.Engine.TClipper64;
  TClipper64  = Clipper.Engine.TClipper64;
  TPoint64    = Clipper.Core.TPoint64;
  TRect64     = Clipper.Core.TRect64;
  TPath64     = Clipper.Core.TPath64;
  TPaths64    = Clipper.Core.TPaths64;
  TPointD     = Clipper.Core.TPointD;
  TRectD      = Clipper.Core.TRectD;
  TPathD      = Clipper.Core.TPathD;
  TPathsD     = Clipper.Core.TPathsD;
  TFillRule   = Clipper.Core.TFillRule;
  TPolyTree64 = Clipper.Engine.TPolyTree64;
  TPolyTreeD  = Clipper.Engine.TPolyTreeD;
  TJoinType   = Clipper.Offset.TJoinType;
  TEndType    = Clipper.Offset.TEndType;
const
  frEvenOdd   = Clipper.Core.frEvenOdd;
  frNonZero   = Clipper.Core.frNonZero;
  frPositive  = Clipper.Core.frPositive;
  frNegative  = Clipper.Core.frNegative;
  jtSquare    = Clipper.Offset.jtSquare;
  jtRound     = Clipper.Offset.jtRound;
  jtMiter     = Clipper.Offset.jtMiter;
  etPolygon   = Clipper.Offset.etPolygon;
  etJoined    = Clipper.Offset.etJoined;
  etButt      = Clipper.Offset.etButt;
  etSquare    = Clipper.Offset.etSquare;
  etRound     = Clipper.Offset.etRound;

  ctNone          = Clipper.Core.ctNone;
  ctIntersection  = Clipper.Core.ctIntersection;
  ctUnion         = Clipper.Core.ctUnion;
  ctDifference    = Clipper.Core.ctDifference;
  ctXor           = Clipper.Core.ctXor;

function BooleanOp(clipType: TClipType;
  const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64; overload;
function BooleanOp(clipType: TClipType; const subjects, clips:
  TPathsD; fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
procedure BooleanOp(clipType: TClipType; const subjects, clips: TPaths64;
  fillRule: TFillRule; polytree: TPolyTree64); overload;

function Intersect(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function Union(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function Union(const subjects: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function Difference(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function XOR_(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;

function Intersect(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function Union(const subjects: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function Union(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function Difference(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function XOR_(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;

function InflatePaths(const paths: TPaths64; delta: Double;
  jt: TJoinType = jtRound; et: TEndType = etPolygon;
  MiterLimit: double = 2.0): TPaths64; overload;
function InflatePaths(const paths: TPathsD; delta: Double;
jt: TJoinType = jtRound; et: TEndType = etPolygon;
miterLimit: double = 2.0; precision: integer = 2): TPathsD; overload;

function RectClip(const rect: TRect64; const path: TPath64): TPath64; overload;
function RectClip(const rect: TRect64; const paths: TPaths64): TPaths64; overload;
function RectClip(const rect: TRectD; const path: TPathD;
  precision: integer = 2): TPathD; overload;
function RectClip(const rect: TRectD; const paths: TPathsD;
  precision: integer = 2): TPathsD; overload;

function RectClipLines(const rect: TRect64; const path: TPath64): TPaths64; overload;
function RectClipLines(const rect: TRect64; const paths: TPaths64): TPaths64; overload;
function RectClipLines(const rect: TRectD; const path: TPathD;
  precision: integer): TPathsD; overload;
function RectClipLines(const rect: TRectD; const paths: TPathsD;
  precision: integer = 2): TPathsD; overload;

function TranslatePath(const path: TPath64; dx, dy: Int64): TPath64; overload;
function TranslatePath(const path: TPathD; dx, dy: double): TPathD; overload;
function TranslatePaths(const paths: TPaths64; dx, dy: Int64): TPaths64; overload;
function TranslatePaths(const paths: TPathsD; dx, dy: double): TPathsD; overload;

function MinkowskiSum(const pattern, path: TPath64;
  pathIsClosed: Boolean): TPaths64;

function PolyTreeToPaths64(PolyTree: TPolyTree64): TPaths64;
function PolyTreeToPathsD(PolyTree: TPolyTreeD): TPathsD;

function MakePath(const ints: TArrayOfInteger): TPath64; overload;
function MakePath(const dbls: TArrayOfDouble): TPathD; overload;

function TrimCollinear(const p: TPath64;
  isOpenPath: Boolean = false): TPath64; overload;
function TrimCollinear(const path: TPathD;
  precision: integer; isOpenPath: Boolean = false): TPathD; overload;

function PointInPolygon(const pt: TPoint64;
  const polygon: TPath64): TPointInPolygonResult;

implementation

uses
  Clipper.Minkowski;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function MakePath(const ints: TArrayOfInteger): TPath64;
var
  i, len: integer;
begin
  len := length(ints) div 2;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := ints[i*2];
    Result[i].Y := ints[i*2 +1];
  end;
end;
//------------------------------------------------------------------------------

function MakePath(const dbls: TArrayOfDouble): TPathD; overload;
var
  i, len: integer;
begin
  len := length(dbls) div 2;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := dbls[i*2];
    Result[i].Y := dbls[i*2 +1];
  end;
end;
//------------------------------------------------------------------------------

procedure AddPolyNodeToPaths(Poly: TPolyPath64; var Paths: TPaths64);
var
  i: Integer;
begin
  if (Length(Poly.Polygon) > 0) then
  begin
    i := Length(Paths);
    SetLength(Paths, i +1);
    Paths[i] := Poly.Polygon;
  end;
  for i := 0 to Poly.Count - 1 do
    AddPolyNodeToPaths(Poly[i], Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeToPaths64(PolyTree: TPolyTree64): TPaths64;
begin
  Result := nil;
  AddPolyNodeToPaths(PolyTree, Result);
end;
//------------------------------------------------------------------------------

procedure AddPolyNodeToPathsD(Poly: TPolyPathD; var Paths: TPathsD);
var
  i: Integer;
begin
  if (Length(Poly.Polygon) > 0) then
  begin
    i := Length(Paths);
    SetLength(Paths, i +1);
    Paths[i] := Poly.Polygon;
  end;
  for i := 0 to Poly.Count - 1 do
    AddPolyNodeToPathsD(Poly[i], Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeToPathsD(PolyTree: TPolyTreeD): TPathsD;
begin
  Result := nil;
  AddPolyNodeToPathsD(PolyTree, Result);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function BooleanOp(clipType: TClipType;
  const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  with TClipper64.Create do
  try
    AddSubject(subjects);
    AddClip(clips);
    Execute(clipType, fillRule, Result);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function BooleanOp(clipType: TClipType; const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  with TClipperD.Create(decimalPrec) do
  try
    AddSubject(subjects);
    AddClip(clips);
    Execute(clipType, fillRule, Result);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

procedure BooleanOp(clipType: TClipType; const subjects, clips: TPaths64;
  fillRule: TFillRule; polytree: TPolyTree64);
var
  dummy: TPaths64;
begin
  with TClipper64.Create do
  try
    AddSubject(subjects);
    AddClip(clips);
    Execute(clipType, fillRule, polytree, dummy);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function Intersect(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctIntersection, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function Union(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctUnion, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function Union(const subjects: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctUnion, subjects, nil, fillRule);
end;
//------------------------------------------------------------------------------

function Difference(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctDifference, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function XOR_(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctXor, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function Intersect(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctIntersection, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function Union(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctUnion, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function Union(const subjects: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctUnion, subjects, nil, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function Difference(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctDifference, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function XOR_(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctXor, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function InflatePaths(const paths: TPaths64; delta: Double;
  jt: TJoinType; et: TEndType; MiterLimit: double): TPaths64;
var
  co: TClipperOffset;
begin
  co := TClipperOffset.Create(MiterLimit);
  try
    co.MergeGroups := true;
    co.AddPaths(paths, jt, et);
    Result := co.Execute(delta);
  finally
    co.free;
  end;
end;
//------------------------------------------------------------------------------

function InflatePaths(const paths: TPathsD; delta: Double;
  jt: TJoinType; et: TEndType; miterLimit: double;
  precision: integer): TPathsD;
var
  pp: TPaths64;
  scale, invScale: double;
begin
  CheckPrecisionRange(precision);
  scale := Power(10, precision);
  invScale := 1/scale;
  pp := ScalePaths(paths, scale, scale);

  with TClipperOffset.Create(miterLimit) do
  try
    AddPaths(pp, jt, et);
    pp := Execute(delta * scale);
  finally
    free;
  end;
  Result := ScalePathsD(pp, invScale, invScale);
end;
//------------------------------------------------------------------------------

function RectClip(const rect: TRect64; const path: TPath64): TPath64;
begin
  Result := nil;
  if rect.IsEmpty or (Length(path) = 0) or
    not rect.Intersects(GetBounds(path)) then Exit;
  with TRectClip.Create(rect) do
  try
    Result := Execute(path);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function RectClip(const rect: TRect64; const paths: TPaths64): TPaths64;
var
  i,j, len: integer;
  pathRec: TRect64;
begin
  Result := nil;
  len := Length(paths);
  if rect.IsEmpty or (len = 0) then Exit;
  SetLength(Result, len);
  j := 0;
  with TRectClip.Create(rect) do
  try
    for i := 0 to len -1 do
    begin
      pathRec := GetBounds(paths[i]);
      if not rect.Intersects(pathRec) then
        Continue
      else if rect.Contains(pathRec) then
        Result[j] := Copy(paths[i], 0, MaxInt)
      else
      begin
        Result[j] := Execute(paths[i]);
        if Result[j] = nil then Continue;
      end;
      inc(j);
    end;
  finally
    Free;
  end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function RectClip(const rect: TRectD; const path: TPathD;
  precision: integer): TPathD;
var
  scale: double;
  tmpPath: TPath64;
  rec: TRect64;
begin
  Result := nil;
  if not rect.Intersects(GetBounds(path)) then Exit;
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));
  tmpPath := ScalePath(path, scale);
  tmpPath := RectClip(rec, tmpPath);
  Result := ScalePathD(tmpPath, 1/scale);
end;
//------------------------------------------------------------------------------

function RectClip(const rect: TRectD;
  const paths: TPathsD; precision: integer): TPathsD;
var
  i,j, len: integer;
  scale: double;
  tmpPath: TPath64;
  rec: TRect64;
  pathRec: TRectD;
begin
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));

  j := 0;
  len := Length(paths);
  SetLength(Result, len);

  with TRectClip.Create(rec) do
  try
    for i := 0 to len -1 do
    begin
      pathRec := GetBounds(paths[i]);
      if not rect.Intersects(pathRec) then
        Continue
      else if rect.Contains(pathRec) then
        Result[j] := Copy(paths[i], 0, MaxInt)
      else
      begin
        tmpPath := ScalePath(paths[i], scale);
        tmpPath := Execute(tmpPath);
        if tmpPath = nil then Continue;
        Result[j] := ScalePathD(tmpPath, 1/scale);
      end;
      inc(j);
    end;
  finally
    Free;
  end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function RectClipLines(const rect: TRect64; const path: TPath64): TPaths64; overload;
begin
  Result := nil;
  if rect.IsEmpty or (Length(path) = 0) or
    not rect.Intersects(GetBounds(path)) then Exit;
  with TRectClipLines.Create(rect) do
  try
    Result := Execute(path);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function RectClipLines(const rect: TRect64; const paths: TPaths64): TPaths64; overload;
var
  i,len: integer;
  pathRec: TRect64;
  tmp: TPaths64;
begin
  Result := nil;
  len := Length(paths);
  if rect.IsEmpty or (len = 0) then Exit;
  SetLength(Result, len);
  with TRectClipLines.Create(rect) do
  try
    for i := 0 to len -1 do
    begin
      pathRec := GetBounds(paths[i]);
      if not rect.Intersects(pathRec) then
        Continue
      else if rect.Contains(pathRec) then
        AppendPath(Result, paths[i])
      else
      begin
        tmp := Execute(paths[i]);
        AppendPaths(Result, tmp);
      end;
    end;
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function RectClipLines(const rect: TRectD; const path: TPathD;
  precision: integer): TPathsD;
var
  scale: double;
  tmpPath: TPath64;
  tmpPaths: TPaths64;
  rec: TRect64;
begin
  Result := nil;
  if not rect.Intersects(GetBounds(path)) then Exit;
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));
  tmpPath := ScalePath(path, scale);
  tmpPaths := RectClipLines(rec, tmpPath);
  Result := ScalePathsD(tmpPaths, 1/scale);
end;
//------------------------------------------------------------------------------

function RectClipLines(const rect: TRectD; const paths: TPathsD;
  precision: integer = 2): TPathsD;
var
  i: integer;
  scale: double;
  tmpPath: TPath64;
  tmpPaths: TPaths64;
  rec: TRect64;
  pathRec: TRectD;
begin
  Result := nil;
  if rect.IsEmpty then Exit;
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));

  with TRectClipLines.Create(rec) do
  try
    for i := 0 to High(paths) do
    begin
      pathRec := GetBounds(paths[i]);
      if not rect.Intersects(pathRec) then
        Continue
      else if rect.Contains(pathRec) then
        AppendPath(Result, paths[i])
      else
      begin
        tmpPath := ScalePath(paths[i], scale);
        tmpPaths := Execute(tmpPath);
        if tmpPaths = nil then Continue;
        AppendPaths(Result, ScalePathsD(tmpPaths, 1/scale));
      end;
    end;
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function TranslatePath(const path: TPath64; dx, dy: Int64): TPath64;
var
  i, len: integer;
begin
  len := length(path);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].x := path[i].x + dx;
    result[i].y := path[i].y + dy;
  end;
end;
//------------------------------------------------------------------------------

function TranslatePath(const path: TPathD; dx, dy: double): TPathD;
var
  i, len: integer;
begin
  len := length(path);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].x := path[i].x + dx;
    result[i].y := path[i].y + dy;
  end;
end;
//------------------------------------------------------------------------------

function TranslatePaths(const paths: TPaths64; dx, dy: Int64): TPaths64;
var
  i, len: integer;
begin
  len := length(paths);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i] := TranslatePath(paths[i], dx, dy);
  end;
end;
//------------------------------------------------------------------------------

function TranslatePaths(const paths: TPathsD; dx, dy: double): TPathsD;
var
  i, len: integer;
begin
  len := length(paths);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i] := TranslatePath(paths[i], dx, dy);
  end;
end;
//------------------------------------------------------------------------------

function MinkowskiSum(const pattern, path: TPath64;
  pathIsClosed: Boolean): TPaths64;
begin
 Result := Clipper.Minkowski.MinkowskiSum(pattern, path, pathIsClosed);
end;
//------------------------------------------------------------------------------

function TrimCollinear(const p: TPath64; isOpenPath: Boolean = false): TPath64;
var
  i,j, len: integer;
begin
  len := Length(p);

  i := 0;
  if not isOpenPath then
  begin
    while (i < len -1) and
      (CrossProduct(p[len -1], p[i], p[i+1]) = 0) do inc(i);
    while (i < len -1) and
      (CrossProduct(p[len -2], p[len -1], p[i]) = 0) do dec(len);
  end;
  if (len - i < 3) then
  begin
    if not isOpenPath or (len < 2) or PointsEqual(p[0], p[1]) then
      Result := nil else
      Result := p;
    Exit;
  end;

  SetLength(Result, len -i);

  Result[0] := p[i];
  j := 0;
  for i := i+1 to len -2 do
    if CrossProduct(result[j], p[i], p[i+1]) <> 0 then
    begin
      inc(j);
      result[j] := p[i];
    end;

  if isOpenPath then
  begin
    inc(j);
    result[j] := p[len-1];
  end
  else if CrossProduct(result[j], p[len-1], result[0]) <> 0 then
  begin
    inc(j);
    result[j] := p[len-1];
  end else
  begin
    while (j > 1) and
      (CrossProduct(result[j-1], result[j], result[0]) = 0) do dec(j);
    if j < 2 then j := -1;
  end;
  SetLength(Result, j +1);
end;
//------------------------------------------------------------------------------

function TrimCollinear(const path: TPathD;
  precision: integer; isOpenPath: Boolean = false): TPathD;
var
  p: TPath64;
  scale: double;
begin
  scale := power(10, precision);
  p := ScalePath(path, scale);
  p := TrimCollinear(p, isOpenPath);
  Result := ScalePathD(p, 1/scale);
end;
//------------------------------------------------------------------------------

function PointInPolygon(const pt: TPoint64;
  const polygon: TPath64): TPointInPolygonResult;
begin
  Result := Clipper.Core.PointInPolygon(pt, polygon);
end;
//------------------------------------------------------------------------------

end.

