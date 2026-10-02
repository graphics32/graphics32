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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  SysUtils, Classes, Math,
  GR32,
  GR32_Paths,
  GR32_Math,
  GR32.SVG.Utf8,
  GR32.SVG.Types;

procedure ParseSvgPathData(const APathData: TValuePUtf8Char; APath: TCustomPath);
function SvgPathDataToPath(const APathData: TValuePUtf8Char): TFlattenedPath; overload;
function SvgPathDataToPoints(const APathData: TValuePUtf8Char): TArrayOfArrayOfFloatPoint; overload;

{$if defined(UNIT_TEST)}
function SvgPathDataToPath(const APathData: AnsiString): TFlattenedPath; overload;
function SvgPathDataToPoints(const APathData: AnsiString): TArrayOfArrayOfFloatPoint; overload;
{$ifend}

implementation

uses
  Types;

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
  FText.Trim([' ', #9, #10, #13, ',']);
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
    while (FText.Len > 0) and (FText.Text^ in ['0'..'9']) do
      FText.Skip;
    if (FText.Len > 0) and (FText.Text^ = '.') then
      FText.Skip;
    while (FText.Len > 0) and (FText.Text^ in ['0'..'9']) do
      FText.Skip;
  end;

  if (FText.Len > 1) and (FText.Text^ in ['e', 'E']) and (FText.Text[1] in ['0'..'9', '+', '-']) then
  begin
    FText.Skip;
    if (FText.Text^ in ['+', '-']) then
      FText.Skip;
    while (FText.Len > 0) and (FText.Text^ in ['0'..'9']) do
      FText.Skip;
  end;

  Value.Len := Value.Len - FText.Len;
  if (Value.Len = 0) then
    Exit(False);

  Result := Value.TryToFloat(AValue);
end;

function TSvgPathScanner.ReadFlag(out AFlag: Boolean): Boolean;
begin
  FText.Trim([' ', #9, #10, #13, ',']);
  if (FText.Len > 0) and (FText.Text^ in ['0', '1']) then
  begin
    AFlag := (FText.Text^ = '1');
    FText.Skip;
    Result := True;
  end else
    Result := False;
end;

procedure ParseSvgPathData(const APathData: TValuePUtf8Char; APath: TCustomPath);
var
  LastCommand: AnsiChar;
  CurrentPoint, LastControlPoint: TFloatPoint;

  function GetCubicControl1: TFloatPoint;
  begin
    if (LastCommand in ['C', 'c', 'S', 's']) then
    begin
      Result.X := 2 * CurrentPoint.X - LastControlPoint.X;
      Result.Y := 2 * CurrentPoint.Y - LastControlPoint.Y;
    end else
      Result := CurrentPoint;
  end;

  function GetQuadControl1: TFloatPoint;
  begin
    if (LastCommand in ['Q', 'q', 'T', 't']) then
    begin
      Result.X := 2 * CurrentPoint.X - LastControlPoint.X;
      Result.Y := 2 * CurrentPoint.Y - LastControlPoint.Y;
    end else
      Result := CurrentPoint;
  end;

var
  Scanner: TSvgPathScanner;
  Command: AnsiChar;
  IsRel: Boolean;
  StartPoint, NextPoint: TFloatPoint;
  Value1, Value2, Value3, Value4, Value5, Value6: Single;
  Flag1, Flag2: Boolean;
const
  LargeArcMap: array[boolean] of TArcPart = (apMinor, apMajor);
  SweepMap: array[boolean] of TArcDirection = (adNegative, adPositive);
begin
  if (APath = nil) then
    Exit;

  Scanner.Init(APathData);
  CurrentPoint := FloatPoint(0, 0);
  StartPoint := FloatPoint(0, 0);
  LastControlPoint := FloatPoint(0, 0);
  Command := #0;
  LastCommand := #0;

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

    IsRel := (Command in ['m', 'l', 'h', 'v', 'c', 's', 'q', 't', 'a']);

    case Command of
      'M', 'm':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2)) then
            break;

          if IsRel then
          begin
            CurrentPoint.X := CurrentPoint.X + Value1;
            CurrentPoint.Y := CurrentPoint.Y + Value2;
          end else
          begin
            CurrentPoint.X := Value1;
            CurrentPoint.Y := Value2;
          end;

          StartPoint := CurrentPoint;
          LastControlPoint := CurrentPoint;

          APath.MoveTo(CurrentPoint);
        end;

      'L', 'l':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2)) then
            break;

          if IsRel then
          begin
            CurrentPoint.X := CurrentPoint.X + Value1;
            CurrentPoint.Y := CurrentPoint.Y + Value2;
          end else
          begin
            CurrentPoint.X := Value1;
            CurrentPoint.Y := Value2;
          end;

          LastControlPoint := CurrentPoint;

          APath.LineTo(CurrentPoint);
        end;

      'H', 'h':
        begin
          if (not Scanner.ReadNumber(Value1)) then
            Break;

          if IsRel then
            CurrentPoint.X := CurrentPoint.X + Value1
          else
            CurrentPoint.X := Value1;
          LastControlPoint := CurrentPoint;

          APath.LineTo(CurrentPoint);
        end;

      'V', 'v':
        begin
          if (not Scanner.ReadNumber(Value2)) then
            Break;

          if IsRel then
            CurrentPoint.Y := CurrentPoint.Y + Value2
          else
            CurrentPoint.Y := Value2;
          LastControlPoint := CurrentPoint;

          APath.LineTo(CurrentPoint);
        end;

      'C', 'c':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2) and
                  Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4) and
                  Scanner.ReadNumber(Value5) and Scanner.ReadNumber(Value6)) then
            Break;

          if IsRel then
          begin
            NextPoint.X := CurrentPoint.X + Value1;
            NextPoint.Y := CurrentPoint.Y + Value2;
            LastControlPoint.X := CurrentPoint.X + Value3;
            LastControlPoint.Y := CurrentPoint.Y + Value4;
            CurrentPoint.X := CurrentPoint.X + Value5;
            CurrentPoint.Y := CurrentPoint.Y + Value6;
          end else
          begin
            NextPoint.X := Value1;
            NextPoint.Y := Value2;
            LastControlPoint.X := Value3;
            LastControlPoint.Y := Value4;
            CurrentPoint.X := Value5;
            CurrentPoint.Y := Value6;
          end;

          APath.CurveTo(NextPoint, LastControlPoint, CurrentPoint);
        end;

      'S', 's':
        begin
          if not (Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4) and
                  Scanner.ReadNumber(Value5) and Scanner.ReadNumber(Value6)) then
            Break;

          NextPoint := GetCubicControl1;

          if IsRel then
          begin
            LastControlPoint.X := CurrentPoint.X + Value3;
            LastControlPoint.Y := CurrentPoint.Y + Value4;
            CurrentPoint.X := CurrentPoint.X + Value5;
            CurrentPoint.Y := CurrentPoint.Y + Value6;
          end else
          begin
            LastControlPoint.X := Value3;
            LastControlPoint.Y := Value4;
            CurrentPoint.X := Value5;
            CurrentPoint.Y := Value6;
          end;

          APath.CurveTo(NextPoint, LastControlPoint, CurrentPoint);
        end;

      'Q', 'q':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2) and
                  Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4)) then
            Break;

          if IsRel then
          begin
            LastControlPoint.X := CurrentPoint.X + Value1;
            LastControlPoint.Y := CurrentPoint.Y + Value2;
            CurrentPoint.X := CurrentPoint.X + Value3;
            CurrentPoint.Y := CurrentPoint.Y + Value4;
          end else
          begin
            LastControlPoint.X := Value1;
            LastControlPoint.Y := Value2;
            CurrentPoint.X := Value3;
            CurrentPoint.Y := Value4;
          end;

          APath.ConicTo(LastControlPoint, CurrentPoint);
        end;

      'T', 't':
        begin
          if not (Scanner.ReadNumber(Value3) and Scanner.ReadNumber(Value4)) then
            Break;

          NextPoint := GetQuadControl1;
          LastControlPoint := NextPoint;

          if IsRel then
          begin
            CurrentPoint.X := CurrentPoint.X + Value3;
            CurrentPoint.Y := CurrentPoint.Y + Value4;
          end else
          begin
            CurrentPoint.X := Value3;
            CurrentPoint.Y := Value4;
          end;

          APath.ConicTo(LastControlPoint, CurrentPoint);
        end;

      'A', 'a':
        begin
          if not (Scanner.ReadNumber(Value1) and Scanner.ReadNumber(Value2) and
                  Scanner.ReadNumber(Value3) and Scanner.ReadFlag(Flag1) and
                  Scanner.ReadFlag(Flag2) and Scanner.ReadNumber(Value4) and
                  Scanner.ReadNumber(Value5)) then
            Break;

          if IsRel then
          begin
            CurrentPoint.X := APath.CurrentPoint.X + Value4;
            CurrentPoint.Y := APath.CurrentPoint.Y + Value5;
          end else
          begin
            CurrentPoint.X := Value4;
            CurrentPoint.Y := Value5;
          end;

          APath.EllipticalArc(CurrentPoint, Value1, Value2, DegToRad(Value3), LargeArcMap[Flag1], SweepMap[Flag2]);
          LastControlPoint := CurrentPoint;
        end;

      'Z', 'z':
        begin
          APath.EndPath(True);
          CurrentPoint := StartPoint;
          LastControlPoint := CurrentPoint;
        end;
    end;

    LastCommand := Command;
  end;
end;

{$if defined(UNIT_TEST)}
function SvgPathDataToPath(const APathData: AnsiString): TFlattenedPath;
begin
   Result := SvgPathDataToPath(TValuePUtf8Char.FromString(APathData));
end;
{$ifend}

function SvgPathDataToPath(const APathData: TValuePUtf8Char): TFlattenedPath;
begin
  Result := TFlattenedPath.Create;
  try

    ParseSvgPathData(APathData, Result);
    Result.EndPath;

  except
    Result.Free;
    raise;
  end;
end;

{$if defined(UNIT_TEST)}
function SvgPathDataToPoints(const APathData: AnsiString): TArrayOfArrayOfFloatPoint;
begin
   Result := SvgPathDataToPoints(TValuePUtf8Char.FromString(APathData));
end;
{$ifend}

function SvgPathDataToPoints(const APathData: TValuePUtf8Char): TArrayOfArrayOfFloatPoint;
var
  p: TFlattenedPath;
begin
  p := SvgPathDataToPath(APathData);
  try

    Result := p.Path;

  finally
    p.Free;
  end;
end;

end.
