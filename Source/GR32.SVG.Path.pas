unit GR32.SVG.Path;

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
 * The Original Code is SVG Image Format support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2025-2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  SysUtils, Classes, Math,
  GR32,
  GR32_Paths,
  GR32_Transforms,
  GR32_Math,
  GR32.SVG.Utf8,
  GR32.SVG.Types;

//------------------------------------------------------------------------------
//
//      TSvgPathCommand
//
//------------------------------------------------------------------------------
type
  TSvgPathCommandKind = (pckMoveTo, pckLineTo, pckCurveTo, pckConicTo, pckEllipticalArc, pckEndPath);

  TSvgPathCommand = record
    Kind: TSvgPathCommandKind;
    Closed: Boolean;
    IsRelative: Boolean; // Note that coordinates are absolute even when IsRelative=True
    ParamPairCount: Byte; // Number of coordinate pairs specified in original command (e.g. 2 vs 3 for cubic curve)
    case TSvgPathCommandKind of
      pckMoveTo:        (MoveTo:  record P: TFloatPoint; end);
      pckLineTo:        (LineTo:  record P: TFloatPoint; end);
      pckCurveTo:       (CurveTo: record C1, C2, P: TFloatPoint; end);
      pckConicTo:       (ConicTo: record P1, P: TFloatPoint; end);
      pckEllipticalArc: (Arc:     record EndPoint: TFloatPoint; RadiusX, RadiusY, AngleRad: Single; Part: TArcPart; Direction: TArcDirection; end);
  end;

//------------------------------------------------------------------------------
//
//      TSvgPathCommands
//
//------------------------------------------------------------------------------
type
  TSvgPathCommands = class(TCustomPath)
  private type
    TSvgPathCommandArray = TArray<TSvgPathCommand>;
    PSvgPathCommand = ^TSvgPathCommand;
  private
    FCommands: TArray<TSvgPathCommand>;
    FCount: Integer;
    FClosed: boolean;
    FNextCommand: PSvgPathCommand;
    FCommandLevel: integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginCommand;
    procedure EndCommand(AIsRelative: boolean = False);
    function GetInProgress: boolean;
    property NextCommand: PSvgPathCommand read FNextCommand;
  public
    constructor Create; override;
    procedure Clear; override;

    procedure MoveTo(const P: TFloatPoint); override;
    procedure MoveToRelative(const P: TFloatPoint); override;
    procedure LineTo(const P: TFloatPoint); override;
    procedure LineToRelative(const P: TFloatPoint); override;
    procedure CurveTo(const C1, C2, P: TFloatPoint); override;
    procedure CurveToRelative(const C1, C2, P: TFloatPoint); override;
    procedure ConicTo(const P1, P: TFloatPoint); override;
    procedure ConicToRelative(const P1, P: TFloatPoint); override;
    procedure EllipticalArc(AEndPoint: TFloatPoint; ARadiusX, ARadiusY: TFloat; ARotation: TFloat; APart: TArcPart; ASweep: TArcDirection); override;
    procedure EllipticalArcRelative(AEndPoint: TFloatPoint; ARadiusX, ARadiusY: TFloat; ARotation: TFloat; APart: TArcPart; ASweep: TArcDirection); override;
    procedure EndPath(Close: Boolean = False); override;

    function Flatten(ATransformation: TAffineTransformation): TArrayOfArrayOfFloatPoint;

    property Commands: TSvgPathCommandArray read FCommands;
    property Count: Integer read FCount;
    property Closed: boolean read FClosed;
    property InProgress: boolean read GetInProgress;
  end;


//------------------------------------------------------------------------------
//
//      ParseSvgPathData
//
//------------------------------------------------------------------------------
// svg <path> parser
//------------------------------------------------------------------------------
procedure ParseSvgPathData(const APathData: TValuePUtf8Char; APath: TCustomPath);


{$if defined(UNIT_TEST)}
function CreatePolylinePath(const APathData: UTF8String): TFlattenedPath;
function SvgPathDataToPoints(const APathData: UTF8String): TArrayOfArrayOfFloatPoint;
{$ifend}


//------------------------------------------------------------------------------
//
//      Primitive Shape Converters
//
//------------------------------------------------------------------------------
function CreateRectPath(X, Y, Width, Height, Rx, Ry: Single): TArrayOfArrayOfFloatPoint;
function CreateCirclePath(Cx, Cy, Radius: Single): TArrayOfArrayOfFloatPoint; overload;
function CreateCirclePath(Cx, Cy, Radius: Single; ATransformation: TAffineTransformation): TArrayOfArrayOfFloatPoint; overload;
function CreateEllipsePath(Cx, Cy, Rx, Ry: Single): TArrayOfArrayOfFloatPoint; overload;
function CreateEllipsePath(Cx, Cy, Rx, Ry: Single; ATransformation: TAffineTransformation): TArrayOfArrayOfFloatPoint; overload;
function CreateLinePath(X1, Y1, X2, Y2: Single): TArrayOfArrayOfFloatPoint;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Types,
  GR32_VectorUtils;

const
  sWhiteSpaceSeparators = [' ', #9, #10, #13];
  sListSeparators = [' ', ',', #9, #10, #13];

{$if defined(UNIT_TEST)}
function CreatePolylinePath(const APathData: UTF8String): TFlattenedPath;
var
  Data: TValuePUtf8Char;
begin
  Data := TValuePUtf8Char.FromString(APathData);

  Result := TFlattenedPath.Create;
  try

    ParseSvgPathData(Data, Result);
    Result.EndPath;

  except
    Result.Free;
    raise;
  end;
end;

function SvgPathDataToPoints(const APathData: UTF8String): TArrayOfArrayOfFloatPoint;
var
  Data: TValuePUtf8Char;
  p: TFlattenedPath;
begin
  Data := TValuePUtf8Char.FromString(APathData);

  p := CreatePolylinePath(APathData);
  try

    Result := p.Path;

  finally
    p.Free;
  end;
end;
{$ifend}


//------------------------------------------------------------------------------
//
//      Elliptical Arc Transmogrififier
//
//------------------------------------------------------------------------------
type
  TTransformedArcProps = record
    AxisA: TFloatPoint;
    RadiusX: Single;
    RadiusY: Single;
    AngleRad: Single;
    Sweep: TArcDirection;
  end;

// Transform elliptical arc radii (rx, ry) and rotation (phi) under matrix M
// into equivalent orthogonal ellipse parameters (Rx, Ry, PhiWorld) using 2D
// conjugate diameter decomposition.
function TransformArcProps(RadiusX, RadiusY, AngleRad: Single; Sweep: TArcDirection;
  const AMatrix: TFloatMatrix): TTransformedArcProps;
var
  SinPhi, CosPhi: Single;
  UxUser, VxUser: TFloatPoint;
  UxWorld, VxWorld: TFloatPoint;
  DotUV, LenUSqr, LenVSqr: Single;
  Theta0, SinT, CosT: Single;
  AxisA, AxisB: TFloatPoint;
  Det: Single;
begin
  GR32_Math.SinCos(AngleRad, SinPhi, CosPhi);

  // 1. User-space semi-axis vectors
  UxUser := FloatPoint(RadiusX * CosPhi, RadiusX * SinPhi);
  VxUser := FloatPoint(-RadiusY * SinPhi, RadiusY * CosPhi);

  // 2. Transform semi-axis vectors to world space using 2x2 matrix portion of AMatrix
  UxWorld.X := UxUser.X * AMatrix[0, 0] + UxUser.Y * AMatrix[1, 0];
  UxWorld.Y := UxUser.X * AMatrix[0, 1] + UxUser.Y * AMatrix[1, 1];

  VxWorld.X := VxUser.X * AMatrix[0, 0] + VxUser.Y * AMatrix[1, 0];
  VxWorld.Y := VxUser.X * AMatrix[0, 1] + VxUser.Y * AMatrix[1, 1];

  // 3. Find parameter angle theta0 where distance from center reaches maximum
  DotUV := UxWorld.X * VxWorld.X + UxWorld.Y * VxWorld.Y;
  LenUSqr := UxWorld.X * UxWorld.X + UxWorld.Y * UxWorld.Y;
  LenVSqr := VxWorld.X * VxWorld.X + VxWorld.Y * VxWorld.Y;

  if Abs(DotUV) < 1e-6 then
    Theta0 := 0
  else
    Theta0 := 0.5 * ArcTan2(2.0 * DotUV, LenUSqr - LenVSqr);

  GR32_Math.SinCos(Theta0, SinT, CosT);

  // 4. Calculate world-space orthogonal principal semi-major & semi-minor axis vectors
  AxisA.X := UxWorld.X * CosT + VxWorld.X * SinT;
  AxisA.Y := UxWorld.Y * CosT + VxWorld.Y * SinT;

  AxisB.X := -UxWorld.X * SinT + VxWorld.X * CosT;
  AxisB.Y := -UxWorld.Y * SinT + VxWorld.Y * CosT;

  Result.AxisA := AxisA;
  Result.RadiusX := GR32_Math.Hypot(AxisA.X, AxisA.Y);
  Result.RadiusY := GR32_Math.Hypot(AxisB.X, AxisB.Y);
  Result.AngleRad := ArcTan2(AxisA.Y, AxisA.X);

  // 5. If matrix determinant is negative (reflection transform), flip sweep direction
  Det := AMatrix[0, 0] * AMatrix[1, 1] - AMatrix[0, 1] * AMatrix[1, 0];
  if Det < 0 then
  begin
    if Sweep = adPositive then
      Result.Sweep := adNegative
    else
      Result.Sweep := adPositive;
  end else
    Result.Sweep := Sweep;
end;

//------------------------------------------------------------------------------
//
//      Primitive Shape Converters
//
//------------------------------------------------------------------------------
function CreateRectPath(X, Y, Width, Height, Rx, Ry: Single): TArrayOfArrayOfFloatPoint;
var
  Path: TFlattenedPath;
begin
  if (Width <= 0) or (Height <= 0) then
    Exit(nil);

  if (Rx <= 0) and (Ry <= 0) then
  begin
    Result := GR32_VectorUtils.PolyPolygon(GR32_VectorUtils.Rectangle(FloatRect(X, Y, X + Width, Y + Height)));
    Exit;
  end;

  if Rx <= 0 then Rx := Ry;
  if Ry <= 0 then Ry := Rx;
  Rx := Min(Rx, Width * 0.5);
  Ry := Min(Ry, Height * 0.5);

  Path := TFlattenedPath.Create;
  try
    Path.MoveTo(X + Rx, Y);
    Path.LineTo(X + Width - Rx, Y);
    Path.EllipticalArc(FloatPoint(X + Width - Rx, Y + Ry), Rx, Ry, 0, -Pi * 0.5, 0);
    Path.LineTo(X + Width, Y + Height - Ry);
    Path.EllipticalArc(FloatPoint(X + Width - Rx, Y + Height - Ry), Rx, Ry, 0, 0, Pi * 0.5);
    Path.LineTo(X + Rx, Y + Height);
    Path.EllipticalArc(FloatPoint(X + Rx, Y + Height - Ry), Rx, Ry, 0, Pi * 0.5, Pi);
    Path.LineTo(X, Y + Ry);
    Path.EllipticalArc(FloatPoint(X + Rx, Y + Ry), Rx, Ry, 0, Pi, Pi * 1.5);
    Path.EndPath(True);
    Result := Path.Path;
  finally
    Path.Free;
  end;
end;

//------------------------------------------------------------------------------

function CreateCirclePath(Cx, Cy, Radius: Single): TArrayOfArrayOfFloatPoint;
begin
  if (Radius <= 0) then
    Exit(nil);

  Result := GR32_VectorUtils.PolyPolygon(GR32_VectorUtils.Circle(FloatPoint(Cx, Cy), Radius));
end;

function CreateCirclePath(Cx, Cy, Radius: Single; ATransformation: TAffineTransformation): TArrayOfArrayOfFloatPoint;
var
  Path: TFlattenedPath;
  ArcProps: TTransformedArcProps;
  WorldCenter, StartPt, MidPt: TFloatPoint;
begin
  if (Radius <= 0) then
    Exit(nil);

  // Convert circle to two elliptical arcs, transform, and flatten
  Path := TFlattenedPath.Create;
  try
    WorldCenter := ATransformation.Transform(FloatPoint(Cx, Cy));
    ArcProps := TransformArcProps(Radius, Radius, 0.0, adPositive, ATransformation.Matrix);

    StartPt := FloatPoint(WorldCenter.X + ArcProps.AxisA.X, WorldCenter.Y + ArcProps.AxisA.Y);
    MidPt := FloatPoint(WorldCenter.X - ArcProps.AxisA.X, WorldCenter.Y - ArcProps.AxisA.Y);

    Path.MoveTo(StartPt);
    Path.EllipticalArc(MidPt, ArcProps.RadiusX, ArcProps.RadiusY, ArcProps.AngleRad, apMinor, ArcProps.Sweep);
    Path.EllipticalArc(StartPt, ArcProps.RadiusX, ArcProps.RadiusY, ArcProps.AngleRad, apMinor, ArcProps.Sweep);

    Path.EndPath(True);
    Result := Path.Path;
  finally
    Path.Free;
  end;
end;

//------------------------------------------------------------------------------

function CreateEllipsePath(Cx, Cy, Rx, Ry: Single): TArrayOfArrayOfFloatPoint;
begin
  if (Rx <= 0) or (Ry <= 0) then
    Exit(nil);
  Result := GR32_VectorUtils.PolyPolygon(GR32_VectorUtils.Ellipse(Cx, Cy, Rx, Ry));
end;

function CreateEllipsePath(Cx, Cy, Rx, Ry: Single; ATransformation: TAffineTransformation): TArrayOfArrayOfFloatPoint;
var
  Path: TFlattenedPath;
  ArcProps: TTransformedArcProps;
  WorldCenter, StartPt, MidPt: TFloatPoint;
begin
  if (Rx <= 0) or (Ry <= 0) then
    Exit(nil);

  // Convert ellipse to two elliptical arcs, transform, and flatten
  Path := TFlattenedPath.Create;
  try
    WorldCenter := ATransformation.Transform(FloatPoint(Cx, Cy));

    // Transform elliptical arc parameters from user to world space
    ArcProps := TransformArcProps(Rx, Ry, 0.0, adPositive, ATransformation.Matrix);

    StartPt := FloatPoint(WorldCenter.X + ArcProps.AxisA.X, WorldCenter.Y + ArcProps.AxisA.Y);
    MidPt := FloatPoint(WorldCenter.X - ArcProps.AxisA.X, WorldCenter.Y - ArcProps.AxisA.Y);

    Path.MoveTo(StartPt);

    Path.EllipticalArc(MidPt, ArcProps.RadiusX, ArcProps.RadiusY, ArcProps.AngleRad, apMinor, ArcProps.Sweep);
    Path.EllipticalArc(StartPt, ArcProps.RadiusX, ArcProps.RadiusY, ArcProps.AngleRad, apMinor, ArcProps.Sweep);

    Path.EndPath(True);
    Result := Path.Path;
  finally
    Path.Free;
  end;
end;

//------------------------------------------------------------------------------

function CreateLinePath(X1, Y1, X2, Y2: Single): TArrayOfArrayOfFloatPoint;
begin
  Result := GR32_VectorUtils.PolyPolygon(GR32_VectorUtils.Line(X1, Y1, X2, Y2));
end;


//------------------------------------------------------------------------------
//
//      TSvgPathScanner
//
//------------------------------------------------------------------------------
type
  TSvgPathScanner = record
  private
    FText: TValuePUtf8Char;
  public
    procedure Init(const AText: TValuePUtf8Char);
    function HasMore: Boolean;
    function IsCommand(var ACmd: AnsiChar): Boolean;
    function ReadCommand(var ACmd: AnsiChar): Boolean;
    function ReadNumber(out AValue: Single): Boolean;
    function ReadFlag(out AFlag: Boolean): Boolean;
  end;

procedure TSvgPathScanner.Init(const AText: TValuePUtf8Char);
begin
  FText := AText;
end;

function TSvgPathScanner.HasMore: Boolean;
begin
  FText.Trim;
  Result := (FText.Len > 0);
end;

function TSvgPathScanner.IsCommand(var ACmd: AnsiChar): Boolean;
begin
  FText.Trim;
  if (FText.Len > 0) and (FText.Text^ in ['M', 'm', 'L', 'l', 'H', 'h', 'V', 'v', 'C', 'c', 'S', 's', 'Q', 'q', 'T', 't', 'A', 'a', 'Z', 'z']) then
  begin
    ACmd := FText.Text^;
    Result := True;
  end else
    Result := False;
end;

function TSvgPathScanner.ReadCommand(var ACmd: AnsiChar): Boolean;
begin
  if IsCommand(ACmd) then
  begin
    FText.Skip;
    Result := True;
  end else
    Result := False;
end;

function TSvgPathScanner.ReadNumber(out AValue: Single): Boolean;
var
  Value: TValuePUtf8Char;
begin
  FText.Trim(sListSeparators);
  if (FText.Len = 0) then
    Exit(False);

  Value := FText;
  if (FText.Text^ in ['+', '-']) then
    FText.Skip;

  // W3C SVG Path Data Syntax Optimization:
  // In condensed SVG path strings (e.g. 'A29,29,01061,32'), a number starting with '0'
  // (such as x-axis-rotation 0) may be followed immediately by single-digit arc flags
  // ('0' or '1') without whitespace or commas separating them (e.g. '01061').
  // If the first digit is '0' and followed by another digit without a decimal point,
  // '0' is a standalone numeric value, and subsequent digits belong to flags or
  // coordinates. Stop reading after '0' so ReadFlag can parse subsequent flag digits.
  if (FText.Len > 1) and (FText.Text^ = '0') and (FText.Text[1] in ['0'..'9']) then
  begin
    FText.Skip;
  end else
  begin
    FText.SkipDigits;
    if (FText.Len > 0) and (FText.Text^ = '.') then
      FText.Skip;
    FText.SkipDigits;
  end;

  if (FText.Len > 1) and (FText.Text^ in ['e', 'E']) and (FText.Text[1] in ['0'..'9', '+', '-']) then
  begin
    FText.Skip;
    if (FText.Text^ in ['+', '-']) then
      FText.Skip;
    FText.SkipDigits;
  end;

  Value.Len := Value.Len - FText.Len;
  if (Value.Len = 0) then
    Exit(False);

  Result := Value.TryToFloat(AValue);
end;

function TSvgPathScanner.ReadFlag(out AFlag: Boolean): Boolean;
begin
  FText.Trim(sListSeparators);
  if (FText.Len > 0) and (FText.Text^ in ['0', '1']) then
  begin
    AFlag := (FText.Text^ = '1');
    FText.Skip;
    Result := True;
  end else
    Result := False;
end;

//------------------------------------------------------------------------------
//
//      ParseSvgPathData
//
//------------------------------------------------------------------------------
procedure ParseSvgPathData(const APathData: TValuePUtf8Char; APath: TCustomPath);
var
  PathCommands: TSvgPathCommands;

  procedure SetParamPairCount(Value: byte);
  begin
    if (PathCommands = nil) then
      exit;

    if (PathCommands.Count > 0) then
      PathCommands.Commands[PathCommands.Count - 1].ParamPairCount := Value;
  end;

var
  Scanner: TSvgPathScanner;
  Command, LastCommand: AnsiChar;
  Value1, Value2, Value3, Value4, Value5, Value6: Single;
  Flag1, Flag2: Boolean;
const
  LargeArcMap: array[boolean] of TArcPart = (apMinor, apMajor);
  SweepMap: array[boolean] of TArcDirection = (adNegative, adPositive);
begin
  if (APath = nil) then
    Exit;

  Scanner.Init(APathData);
  Command := #0;
  LastCommand := #0;

  if (APath is TSvgPathCommands) then
    PathCommands := TSvgPathCommands(APath)
  else
    PathCommands := nil;

  while Scanner.HasMore do
  begin
    if not Scanner.ReadCommand(Command) then
    begin
      // Implicit command repetition:
      //
      // - If a command is followed by extra coordinate numbers without a new
      //   command letter, the previous command repeats implicitly.
      //   For example, "L 10 20 30 40" is equivalent to "L 10 20 L 30 40".
      //
      // - Moveto exception:
      //   If a moveto command (M or m) is followed by multiple coordinate
      //   pairs, the first pair executes moveto, and all subsequent implicit
      //   pairs execute lineto (L or l).
      case LastCommand of
        #0: break; // Invalid: No previous command
        'M': Command := 'L';
        'm': Command := 'l';
      else
        Command := LastCommand;
      end;
    end;

    // When a closepath command ('z' / 'Z') is followed immediately by a
    // non-moveto command (e.g. 'L'), a new subpath is implicitly started
    // at the initial point of the previous closed subpath.
    if (LastCommand in ['Z', 'z']) and not (Command in ['M', 'm']) then
      APath.MoveTo(APath.CurrentPoint);

    case Command of
      'M':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2)) then break;
          APath.MoveTo(FloatPoint(Value1, Value2));
        end;

      'm':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2)) then break;
          APath.MoveToRelative(FloatPoint(Value1, Value2));
        end;

      'L':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2)) then break;
          APath.LineTo(FloatPoint(Value1, Value2));
        end;

      'l':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2)) then break;
          APath.LineToRelative(FloatPoint(Value1, Value2));
        end;

      'H':
        begin
          if not Scanner.ReadNumber(Value1) then break;
          APath.HorizontalLineTo(Value1);
        end;

      'h':
        begin
          if not Scanner.ReadNumber(Value1) then break;
          APath.HorizontalLineToRelative(Value1);
        end;

      'V':
        begin
          if not Scanner.ReadNumber(Value2) then break;
          APath.VerticalLineTo(Value2);
        end;

      'v':
        begin
          if not Scanner.ReadNumber(Value2) then break;
          APath.VerticalLineToRelative(Value2);
        end;

      'C':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2)) then
            Break;

          if not (Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4)) then
          begin
            Value3 := Value1;
            Value4 := Value2;
            Value5 := Value1;
            Value6 := Value2;
            APath.CurveTo(FloatPoint(Value1, Value2), FloatPoint(Value3, Value4), FloatPoint(Value5, Value6));
            SetParamPairCount(1);
          end else
          if not (Scanner.ReadNumber(Value5) and Scanner.ReadNumber(Value6)) then
          begin
            Value5 := Value3;
            Value6 := Value4;
            APath.CurveTo(FloatPoint(Value1, Value2), FloatPoint(Value3, Value4), FloatPoint(Value5, Value6));
            SetParamPairCount(2);
          end else
          begin
            APath.CurveTo(FloatPoint(Value1, Value2), FloatPoint(Value3, Value4), FloatPoint(Value5, Value6));
            SetParamPairCount(3);
          end;
        end;

      'c':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2)) then
            Break;

          if not (Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4)) then
          begin
            Value3 := Value1;
            Value4 := Value2;
            Value5 := Value1;
            Value6 := Value2;
            APath.CurveToRelative(FloatPoint(Value1, Value2), FloatPoint(Value3, Value4), FloatPoint(Value5, Value6));
            SetParamPairCount(1);
          end else
          if not (Scanner.ReadNumber(Value5) and Scanner.ReadNumber(Value6)) then
          begin
            Value5 := Value3;
            Value6 := Value4;
            APath.CurveToRelative(FloatPoint(Value1, Value2), FloatPoint(Value3, Value4), FloatPoint(Value5, Value6));
            SetParamPairCount(2);
          end else
          begin
            APath.CurveToRelative(FloatPoint(Value1, Value2), FloatPoint(Value3, Value4), FloatPoint(Value5, Value6));
            SetParamPairCount(3);
          end;
        end;

      'S':
        begin
          if not (Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4)) then
            Break;

          if not (Scanner.ReadNumber(Value5) and Scanner.ReadNumber(Value6)) then
          begin
            Value5 := Value3;
            Value6 := Value4;
          end;

          APath.CurveTo(FloatPoint(Value3, Value4), FloatPoint(Value5, Value6));
        end;

      's':
        begin
          if not (Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4)) then
            Break;

          if not (Scanner.ReadNumber(Value5) and Scanner.ReadNumber(Value6)) then
          begin
            Value5 := Value3;
            Value6 := Value4;
          end;

          APath.CurveToRelative(FloatPoint(Value3, Value4), FloatPoint(Value5, Value6));
        end;

      'Q':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2) and
                  Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4)) then break;
          APath.ConicTo(FloatPoint(Value1, Value2), FloatPoint(Value3, Value4));
        end;

      'q':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2) and
                  Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4)) then break;
          APath.ConicToRelative(FloatPoint(Value1, Value2), FloatPoint(Value3, Value4));
        end;

      'T':
        begin
          if not (Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4)) then break;
          APath.ConicTo(FloatPoint(Value3, Value4));
        end;

      't':
        begin
          if not (Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4)) then break;
          APath.ConicToRelative(FloatPoint(Value3, Value4));
        end;

      'A':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2) and
                  Scanner.ReadNumber(Value3) and Scanner.ReadFlag(Flag1) and
                  Scanner.ReadFlag(Flag2) and Scanner.ReadNumber(Value4) and
                  Scanner.ReadNumber(Value5)) then break;
          APath.EllipticalArc(FloatPoint(Value4, Value5), Value1, Value2, DegToRad(Value3), LargeArcMap[Flag1], SweepMap[Flag2]);
        end;

      'a':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2) and
                  Scanner.ReadNumber(Value3) and Scanner.ReadFlag(Flag1) and
                  Scanner.ReadFlag(Flag2) and Scanner.ReadNumber(Value4) and
                  Scanner.ReadNumber(Value5)) then break;
          APath.EllipticalArcRelative(FloatPoint(Value4, Value5), Value1, Value2, DegToRad(Value3), LargeArcMap[Flag1], SweepMap[Flag2]);
        end;

      'Z', 'z':
        begin
          APath.EndPath(True);
        end;
    end;

    LastCommand := Command;
  end;
end;

//------------------------------------------------------------------------------
//
//      TSvgPathCommands
//
//------------------------------------------------------------------------------
constructor TSvgPathCommands.Create;
begin
  inherited Create;
  FCount := 0;
  SetLength(FCommands, 16);
end;

procedure TSvgPathCommands.Clear;
begin
  inherited Clear;
  FCount := 0;
end;

procedure TSvgPathCommands.AssignTo(Dest: TPersistent);
begin
  inherited;
  if (Dest is TSvgPathCommands) then
  begin
    TSvgPathCommands(Dest).FCommands := Copy(FCommands);
    TSvgPathCommands(Dest).FCount := FCount;
    TSvgPathCommands(Dest).FClosed := FClosed;
  end else
    inherited;
end;

procedure TSvgPathCommands.BeginCommand;
begin
  Inc(FCommandLevel);
  if (FCommandLevel > 1) then
    exit;

  if (FCount >= Length(FCommands)) then
    SetLength(FCommands, Length(FCommands) * 2);

  FNextCommand := @FCommands[Count];
  FNextCommand^ := Default(TSvgPathCommand);
end;

procedure TSvgPathCommands.EndCommand(AIsRelative: boolean);
begin
  Dec(FCommandLevel);
  if (FCommandLevel > 0) then
    exit;

  Assert(FCount <= High(FCommands));

  FNextCommand.IsRelative := AIsRelative;

  Inc(FCount);
  FNextCommand := nil;
end;

procedure TSvgPathCommands.MoveTo(const P: TFloatPoint);
begin
  inherited;

  BeginCommand;
  begin
    NextCommand.Kind := pckMoveTo;
    NextCommand.MoveTo.P := P;
  end;
  EndCommand;
end;

procedure TSvgPathCommands.MoveToRelative(const P: TFloatPoint);
begin
  BeginCommand;
  begin
    inherited;
  end;
  EndCommand(True);
end;

procedure TSvgPathCommands.LineTo(const P: TFloatPoint);
begin
  inherited;

  BeginCommand;
  begin
    NextCommand.Kind := pckLineTo;
    NextCommand.LineTo.P := P;
  end;
  EndCommand;
end;

procedure TSvgPathCommands.LineToRelative(const P: TFloatPoint);
begin
  BeginCommand;
  begin
    inherited;
  end;
  EndCommand(True);
end;

procedure TSvgPathCommands.CurveTo(const C1, C2, P: TFloatPoint);
begin
  inherited;

  BeginCommand;
  begin
    NextCommand.Kind := pckCurveTo;
    NextCommand.CurveTo.C1 := C1;
    NextCommand.CurveTo.C2 := C2;
    NextCommand.CurveTo.P := P;
  end;
  EndCommand;
end;

procedure TSvgPathCommands.CurveToRelative(const C1, C2, P: TFloatPoint);
begin
  BeginCommand;
  begin
    inherited;
  end;
  EndCommand(True);
end;

procedure TSvgPathCommands.ConicTo(const P1, P: TFloatPoint);
begin
  inherited;

  BeginCommand;
  begin
    NextCommand.Kind := pckConicTo;
    NextCommand.ConicTo.P1 := P1;
    NextCommand.ConicTo.P := P;
  end;
  EndCommand;
end;

procedure TSvgPathCommands.ConicToRelative(const P1, P: TFloatPoint);
begin
  BeginCommand;
  begin
    inherited;
  end;
  EndCommand(True);
end;

procedure TSvgPathCommands.EllipticalArc(AEndPoint: TFloatPoint; ARadiusX, ARadiusY, ARotation: TFloat; APart: TArcPart;
  ASweep: TArcDirection);
begin
  inherited;

  BeginCommand;
  begin
    NextCommand.Kind := pckEllipticalArc;
    NextCommand.Arc.EndPoint := AEndPoint;
    NextCommand.Arc.RadiusX := ARadiusX;
    NextCommand.Arc.RadiusY := ARadiusY;
    NextCommand.Arc.AngleRad := ARotation;
    NextCommand.Arc.Part := APart;
    NextCommand.Arc.Direction := ASweep;
  end;
  EndCommand;
end;

procedure TSvgPathCommands.EllipticalArcRelative(AEndPoint: TFloatPoint; ARadiusX, ARadiusY: TFloat; ARotation: TFloat; APart: TArcPart; ASweep: TArcDirection);
begin
  BeginCommand;
  begin
    inherited;
  end;
  EndCommand(True);
end;

procedure TSvgPathCommands.EndPath(Close: Boolean);
begin
  inherited;

  if (not InProgress) then
    exit;

  FClosed := Close;

  BeginCommand;
  begin
    NextCommand.Kind := pckEndPath;
    NextCommand.Closed := Close;
  end;
  EndCommand;
end;

function TSvgPathCommands.Flatten(ATransformation: TAffineTransformation): TArrayOfArrayOfFloatPoint;
var
  Scale: Single;

  procedure ReplayCommands(Path: TCustomPath);
  var
    i: Integer;
    ArcProps: TTransformedArcProps;
  begin
    for i := 0 to FCount - 1 do
    begin
      case FCommands[i].Kind of
        pckMoveTo:
          Path.MoveTo(ATransformation.Transform(FCommands[i].MoveTo.P));

        pckLineTo:
          Path.LineTo(ATransformation.Transform(FCommands[i].LineTo.P));

        pckCurveTo:
          Path.CurveTo(ATransformation.Transform(FCommands[i].CurveTo.C1), ATransformation.Transform(FCommands[i].CurveTo.C2), ATransformation.Transform(FCommands[i].CurveTo.P));

        pckConicTo:
          Path.ConicTo(ATransformation.Transform(FCommands[i].ConicTo.P1), ATransformation.Transform(FCommands[i].ConicTo.P));

        pckEllipticalArc:
          begin
            if (not TFloatMatrixHelper(ATransformation.Matrix).IsAxisAligned) then
            begin
              // Transforms elliptical arc parameters from user to world space
              ArcProps := TransformArcProps(FCommands[i].Arc.RadiusX, FCommands[i].Arc.RadiusY,
                FCommands[i].Arc.AngleRad, FCommands[i].Arc.Direction, ATransformation.Matrix);

              Path.EllipticalArc(ATransformation.Transform(FCommands[i].Arc.EndPoint), ArcProps.RadiusX, ArcProps.RadiusY,
                ArcProps.AngleRad, FCommands[i].Arc.Part, ArcProps.Sweep);
            end else
            begin
              if (IsNaN(Scale)) then
                Scale := TFloatMatrixHelper(ATransformation.Matrix).GetScale;

              Path.EllipticalArc(ATransformation.Transform(FCommands[i].Arc.EndPoint), FCommands[i].Arc.RadiusX * Scale, FCommands[i].Arc.RadiusY * Scale,
                FCommands[i].Arc.AngleRad, FCommands[i].Arc.Part, FCommands[i].Arc.Direction);
            end;
          end;

        pckEndPath:
          Path.EndPath(FCommands[i].Closed);
      end;
    end;
  end;

var
  Path: TFlattenedPath;
begin
  if FCount = 0 then
    Exit(nil);

  Scale := NaN;

  Path := TFlattenedPath.Create;
  try

    ReplayCommands(Path);

    Path.EndPath;

    Result := Path.Path;

  finally
    Path.Free;
  end;
end;

function TSvgPathCommands.GetInProgress: boolean;
begin
  Result := (FCount > 0) and (FCommands[FCount-1].Kind <> pckEndPath);
end;

end.
