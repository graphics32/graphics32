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
  SysUtils, Classes, Math, GR32, GR32_Paths, GR32_Math, GR32.SVG.Types;

procedure ParseSvgPathData(const APathData: string; APath: TCustomPath);
function SvgPathDataToPath(const APathData: string): TFlattenedPath;
function SvgPathDataToPoints(const APathData: string): TArrayOfArrayOfFloatPoint;

implementation

uses
  Types;

type
  TSvgPathScanner = record
  private
    FText: string;
    FPos: Integer;
    FLen: Integer;
    procedure SkipWhitespace;
  public
    procedure Init(const AText: string);
    function HasMore: Boolean;
    function IsCommand(out ACmd: Char): Boolean;
    function ReadCommand(out ACmd: Char): Boolean;
    function ReadNumber(out AValue: Single): Boolean;
    function ReadFlag(out AFlag: Boolean): Boolean;
  end;

procedure TSvgPathScanner.Init(const AText: string);
begin
  FText := AText;
  FPos := 1;
  FLen := Length(AText);
end;

function TSvgPathScanner.HasMore: Boolean;
begin
  SkipWhitespace;
  Result := FPos <= FLen;
end;

procedure TSvgPathScanner.SkipWhitespace;
begin
  while (FPos <= FLen) and (FText[FPos] in [' ', #9, #10, #13, ',']) do
    Inc(FPos);
end;

function TSvgPathScanner.IsCommand(out ACmd: Char): Boolean;
begin
  SkipWhitespace;
  if (FPos <= FLen) and (FText[FPos] in ['M', 'm', 'L', 'l', 'H', 'h', 'V', 'v', 'C', 'c', 'S', 's', 'Q', 'q', 'T', 't', 'A', 'a', 'Z', 'z']) then
  begin
    ACmd := FText[FPos];
    Exit(True);
  end;
  ACmd := #0;
  Result := False;
end;

function TSvgPathScanner.ReadCommand(out ACmd: Char): Boolean;
begin
  if IsCommand(ACmd) then
  begin
    Inc(FPos);
    Exit(True);
  end;
  Result := False;
end;

function TSvgPathScanner.ReadNumber(out AValue: Single): Boolean;
var
  startPos: Integer;
  numStr: string;
begin
  SkipWhitespace;
  if FPos > FLen then
    Exit(False);

  startPos := FPos;
  if FText[FPos] in ['+', '-'] then
    Inc(FPos);

  while (FPos <= FLen) and (FText[FPos] in ['0'..'9', '.']) do
  begin
    if (FText[FPos] = '.') and (Pos('.', Copy(FText, startPos, FPos - startPos)) > 0) then
      Break;
    Inc(FPos);
  end;

  if (FPos <= FLen) and (FText[FPos] in ['e', 'E']) and
     (FPos < FLen) and (FText[FPos + 1] in ['0'..'9', '+', '-']) then
  begin
    Inc(FPos);
    if (FPos <= FLen) and (FText[FPos] in ['+', '-']) then
      Inc(FPos);
    while (FPos <= FLen) and (FText[FPos] in ['0'..'9']) do
      Inc(FPos);
  end;

  if FPos = startPos then
    Exit(False);

  numStr := Copy(FText, startPos, FPos - startPos);
  if not TryStrToFloat(numStr, AValue, SvgFormatSettings) then
    Exit(False);

  Result := True;
end;

function TSvgPathScanner.ReadFlag(out AFlag: Boolean): Boolean;
begin
  SkipWhitespace;
  if (FPos <= FLen) and (FText[FPos] in ['0', '1']) then
  begin
    AFlag := (FText[FPos] = '1');
    Inc(FPos);
    Exit(True);
  end;
  Result := False;
end;

procedure ParseSvgPathData(const APathData: string; APath: TCustomPath);
var
  scanner: TSvgPathScanner;
  cmd, lastCmd: Char;
  isRel: Boolean;
  currPt, startPt, lastCtrlPt: TFloatPoint;
  val1, val2, val3, val4, val5, val6: Single;
  flag1, flag2: Boolean;

  function GetCubicControl1: TFloatPoint;
  begin
    if CharInSet(lastCmd, ['C', 'c', 'S', 's']) then
      Result := FloatPoint(2 * currPt.X - lastCtrlPt.X, 2 * currPt.Y - lastCtrlPt.Y)
    else
      Result := currPt;
  end;

  function GetQuadControl1: TFloatPoint;
  begin
    if CharInSet(lastCmd, ['Q', 'q', 'T', 't']) then
      Result := FloatPoint(2 * currPt.X - lastCtrlPt.X, 2 * currPt.Y - lastCtrlPt.Y)
    else
      Result := currPt;
  end;

const
  LargeArcMap: array[boolean] of TArcPart = (apMinor, apMajor);
  SweepMap: array[boolean] of TArcDirection = (adNegative, adPositive);
begin
  if APath = nil then Exit;

  scanner.Init(APathData);
  currPt := FloatPoint(0, 0);
  startPt := FloatPoint(0, 0);
  lastCtrlPt := FloatPoint(0, 0);
  cmd := #0;
  lastCmd := #0;

  while scanner.HasMore do
  begin
    if not scanner.ReadCommand(cmd) then
    begin
      if cmd = #0 then Break;
      if (cmd = 'M') or (cmd = 'm') then
      begin
        if cmd = 'M' then cmd := 'L' else cmd := 'l';
      end;
    end;

    isRel := CharInSet(cmd, ['m', 'l', 'h', 'v', 'c', 's', 'q', 't', 'a']);

    case cmd of
      'M', 'm':
        begin
          if not (scanner.ReadNumber(val1) and scanner.ReadNumber(val2)) then Break;
          if isRel then
            currPt := FloatPoint(currPt.X + val1, currPt.Y + val2)
          else
            currPt := FloatPoint(val1, val2);
          startPt := currPt;
          lastCtrlPt := currPt;
          APath.MoveTo(currPt);
        end;

      'L', 'l':
        begin
          if not (scanner.ReadNumber(val1) and scanner.ReadNumber(val2)) then Break;
          if isRel then
            currPt := FloatPoint(currPt.X + val1, currPt.Y + val2)
          else
            currPt := FloatPoint(val1, val2);
          lastCtrlPt := currPt;
          APath.LineTo(currPt);
        end;

      'H', 'h':
        begin
          if not scanner.ReadNumber(val1) then Break;
          if isRel then
            currPt.X := currPt.X + val1
          else
            currPt.X := val1;
          lastCtrlPt := currPt;
          APath.LineTo(currPt);
        end;

      'V', 'v':
        begin
          if not scanner.ReadNumber(val2) then Break;
          if isRel then
            currPt.Y := currPt.Y + val2
          else
            currPt.Y := val2;
          lastCtrlPt := currPt;
          APath.LineTo(currPt);
        end;

      'C', 'c':
        begin
          if not (scanner.ReadNumber(val1) and scanner.ReadNumber(val2) and
                  scanner.ReadNumber(val3) and scanner.ReadNumber(val4) and
                  scanner.ReadNumber(val5) and scanner.ReadNumber(val6)) then Break;
          if isRel then
          begin
            APath.CurveTo(FloatPoint(currPt.X + val1, currPt.Y + val2),
                          FloatPoint(currPt.X + val3, currPt.Y + val4),
                          FloatPoint(currPt.X + val5, currPt.Y + val6));
            lastCtrlPt := FloatPoint(currPt.X + val3, currPt.Y + val4);
            currPt := FloatPoint(currPt.X + val5, currPt.Y + val6);
          end
          else
          begin
            APath.CurveTo(FloatPoint(val1, val2), FloatPoint(val3, val4), FloatPoint(val5, val6));
            lastCtrlPt := FloatPoint(val3, val4);
            currPt := FloatPoint(val5, val6);
          end;
        end;

      'S', 's':
        begin
          if not (scanner.ReadNumber(val3) and scanner.ReadNumber(val4) and
                  scanner.ReadNumber(val5) and scanner.ReadNumber(val6)) then Break;
          val1 := GetCubicControl1.X;
          val2 := GetCubicControl1.Y;
          if isRel then
          begin
            APath.CurveTo(FloatPoint(val1, val2),
                          FloatPoint(currPt.X + val3, currPt.Y + val4),
                          FloatPoint(currPt.X + val5, currPt.Y + val6));
            lastCtrlPt := FloatPoint(currPt.X + val3, currPt.Y + val4);
            currPt := FloatPoint(currPt.X + val5, currPt.Y + val6);
          end
          else
          begin
            APath.CurveTo(FloatPoint(val1, val2), FloatPoint(val3, val4), FloatPoint(val5, val6));
            lastCtrlPt := FloatPoint(val3, val4);
            currPt := FloatPoint(val5, val6);
          end;
        end;

      'Q', 'q':
        begin
          if not (scanner.ReadNumber(val1) and scanner.ReadNumber(val2) and
                  scanner.ReadNumber(val3) and scanner.ReadNumber(val4)) then Break;
          if isRel then
          begin
            APath.ConicTo(FloatPoint(currPt.X + val1, currPt.Y + val2),
                          FloatPoint(currPt.X + val3, currPt.Y + val4));
            lastCtrlPt := FloatPoint(currPt.X + val1, currPt.Y + val2);
            currPt := FloatPoint(currPt.X + val3, currPt.Y + val4);
          end
          else
          begin
            APath.ConicTo(FloatPoint(val1, val2), FloatPoint(val3, val4));
            lastCtrlPt := FloatPoint(val1, val2);
            currPt := FloatPoint(val3, val4);
          end;
        end;

      'T', 't':
        begin
          if not (scanner.ReadNumber(val3) and scanner.ReadNumber(val4)) then Break;
          val1 := GetQuadControl1.X;
          val2 := GetQuadControl1.Y;
          if isRel then
          begin
            APath.ConicTo(FloatPoint(val1, val2),
                          FloatPoint(currPt.X + val3, currPt.Y + val4));
            lastCtrlPt := FloatPoint(val1, val2);
            currPt := FloatPoint(currPt.X + val3, currPt.Y + val4);
          end
          else
          begin
            APath.ConicTo(FloatPoint(val1, val2), FloatPoint(val3, val4));
            lastCtrlPt := FloatPoint(val1, val2);
            currPt := FloatPoint(val3, val4);
          end;
        end;

      'A', 'a':
        begin
          if not (scanner.ReadNumber(val1) and scanner.ReadNumber(val2) and
                  scanner.ReadNumber(val3) and scanner.ReadFlag(flag1) and
                  scanner.ReadFlag(flag2) and scanner.ReadNumber(val4) and
                  scanner.ReadNumber(val5)) then
            Break;

          if isRel then
          begin
            currPt.X := APath.CurrentPoint.X + val4;
            currPt.Y := APath.CurrentPoint.Y + val5;
          end else
          begin
            currPt.X := val4;
            currPt.Y := val5;
          end;
          APath.EllipticalArc(currPt, val1, val2, DegToRad(val3), LargeArcMap[flag1], SweepMap[flag2]);
          lastCtrlPt := currPt;
        end;

      'Z', 'z':
        begin
          APath.EndPath(True);
          currPt := startPt;
          lastCtrlPt := currPt;
        end;
    end;

    lastCmd := cmd;
  end;
end;

function SvgPathDataToPath(const APathData: string): TFlattenedPath;
begin
  Result := TFlattenedPath.Create;
  ParseSvgPathData(APathData, Result);
  Result.EndPath;
end;

function SvgPathDataToPoints(const APathData: string): TArrayOfArrayOfFloatPoint;
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
