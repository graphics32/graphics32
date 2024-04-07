unit GR32.Design.Color32;

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
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Classes, SysUtils,
  Generics.Collections,
{$IFDEF FPC}
  RTLConsts, LazIDEIntf, PropEdits, Graphics, Dialogs, Forms,
  {$IFDEF Windows}
    Windows, Registry,
  {$ENDIF}
{$ELSE}
  Consts,
  DesignIntf, DesignEditors, VCLEditors,
  Windows, Registry, Graphics, Dialogs, Forms, Controls,
{$ENDIF}
  GR32, GR32_Image;

type
  { TColorManager }
  TColorEntry = record
    Name: string;
    Color: TColor32;
  end;

  TColorManager = class(TList<TColorEntry>)
  public
    procedure AddColor(const AName: string; AColor: TColor32);
    procedure EnumColors(Proc: TGetStrProc);
    function  FindColor(const AName: string): TColor32;
    function  GetColor(const AName: string): TColor32;
    function  GetColorName(AColor: TColor32): string;
    procedure RegisterDefaultColors;
    procedure RemoveColor(const AName: string);
  end;

  { TColor32Property }
  TColor32Property = class(TIntegerProperty
{$IFDEF EXT_PROP_EDIT}
    , ICustomPropertyListDrawing, ICustomPropertyDrawing, ICustomPropertyDrawing80
{$ENDIF}
  )
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
{$IFDEF EXT_PROP_EDIT}
    procedure Edit; override;
    { ICustomPropertyListDrawing }
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    { ICustomPropertyDrawing80 }
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
{$ENDIF}
  end;

procedure RegisterColor(const AName: string; AColor: TColor32);
procedure UnregisterColor(const AName: string);

var ColorManager: TColorManager;

implementation

uses
  GR32.Design.ColorPicker;

{ TColorManager }

procedure TColorManager.AddColor(const AName: string; AColor: TColor32);
var
  Entry: TColorEntry;
begin
  Entry.Name := AName;
  Entry.Color := AColor;

  Add(Entry);
end;

procedure TColorManager.EnumColors(Proc: TGetStrProc);
var
  Entry: TColorEntry;
begin
  for Entry in Self do
    Proc(Entry.Name);
end;

function TColorManager.FindColor(const AName: string): TColor32;
var
  Entry: TColorEntry;
begin
  Result := clBlack32;
  for Entry in Self do
    if SameText(Entry.Name, AName) then
    begin
      Result := Entry.Color;
      break;
    end;
end;

function TColorManager.GetColor(const AName: string): TColor32;

  function HexToColor(const HexStr: string): Cardinal;
  var
    c: Char;
  begin
    Result := 0;
    for c in HexStr do
    begin
      case c of
        '0'..'9': Result := (Result shl 4) + Cardinal(Ord(c) - Ord('0'));
        'A'..'F': Result := (Result shl 4) + Cardinal(Ord(c) - Ord('A') + 10);
        'a'..'f': Result := (Result shl 4) + Cardinal(Ord(c) - Ord('a') + 10);
      else
        Result := clBlack32;
        break;
      end;
      if (Result >= $1FFFFFFF) then
        break; // Next digit would overflow
    end;
  end;

var
  s: string;
begin
  s := Trim(AName);
  if s[1] = '$' then
    System.Delete(s, 1, 1);

  if (s[1] = 'c') and (s[2] = 'l') then
    Result := FindColor(s)
  else
    Result := HexToColor(s);
end;

function TColorManager.GetColorName(AColor: TColor32): string;
var
  Entry: TColorEntry;
begin
  for Entry in Self do
    if Entry.Color = AColor then
    begin
      Result := string(Entry.Name);
      Exit;
    end;
  Result := '$' + IntToHex(AColor, 8);
end;

procedure TColorManager.RegisterDefaultColors;
begin
  Capacity := 50;
  AddColor('clBlack32',                clBlack32);
  AddColor('clDimGray32',              clDimGray32);
  AddColor('clGray32',                 clGray32);
  AddColor('clLightGray32',            clLightGray32);
  AddColor('clWhite32',                clWhite32);
  AddColor('clMaroon32',               clMaroon32);
  AddColor('clGreen32',                clGreen32);
  AddColor('clOlive32',                clOlive32);
  AddColor('clNavy32',                 clNavy32);
  AddColor('clPurple32',               clPurple32);
  AddColor('clTeal32',                 clTeal32);
  AddColor('clRed32',                  clRed32);
  AddColor('clLime32',                 clLime32);
  AddColor('clYellow32',               clYellow32);
  AddColor('clBlue32',                 clBlue32);
  AddColor('clFuchsia32',              clFuchsia32);
  AddColor('clAqua32',                 clAqua32);

  AddColor('clTrWhite32',              clTrWhite32);
  AddColor('clTrBlack32',              clTrBlack32);
  AddColor('clTrRed32',                clTrRed32);
  AddColor('clTrGreen32',              clTrGreen32);
  AddColor('clTrBlue32',               clTrBlue32);

  AddColor('clAliceBlue32',            clAliceBlue32);
  AddColor('clAntiqueWhite32',         clAntiqueWhite32);
  AddColor('clAquamarine32',           clAquamarine32);
  AddColor('clAzure32',                clAzure32);
  AddColor('clBeige32',                clBeige32);
  AddColor('clBisque32',               clBisque32);
  AddColor('clBlancheDalmond32',       clBlancheDalmond32);
  AddColor('clBlueViolet32',           clBlueViolet32);
  AddColor('clBrown32',                clBrown32);
  AddColor('clBurlyWood32',            clBurlyWood32);
  AddColor('clCadetblue32',            clCadetblue32);
  AddColor('clChartReuse32',           clChartReuse32);
  AddColor('clChocolate32',            clChocolate32);
  AddColor('clCoral32',                clCoral32);
  AddColor('clCornFlowerBlue32',       clCornFlowerBlue32);
  AddColor('clCornSilk32',             clCornSilk32);
  AddColor('clCrimson32',              clCrimson32);
  AddColor('clDarkBlue32',             clDarkBlue32);
  AddColor('clDarkCyan32',             clDarkCyan32);
  AddColor('clDarkGoldenRod32',        clDarkGoldenRod32);
  AddColor('clDarkGray32',             clDarkGray32);
  AddColor('clDarkGreen32',            clDarkGreen32);
  AddColor('clDarkGrey32',             clDarkGrey32);
  AddColor('clDarkKhaki32',            clDarkKhaki32);
  AddColor('clDarkMagenta32',          clDarkMagenta32);
  AddColor('clDarkOliveGreen32',       clDarkOliveGreen32);
  AddColor('clDarkOrange32',           clDarkOrange32);
  AddColor('clDarkOrchid32',           clDarkOrchid32);
  AddColor('clDarkRed32',              clDarkRed32);
  AddColor('clDarkSalmon32',           clDarkSalmon32);
  AddColor('clDarkSeaGreen32',         clDarkSeaGreen32);
  AddColor('clDarkSlateBlue32',        clDarkSlateBlue32);
  AddColor('clDarkSlateGray32',        clDarkSlateGray32);
  AddColor('clDarkSlateGrey32',        clDarkSlateGrey32);
  AddColor('clDarkTurquoise32',        clDarkTurquoise32);
  AddColor('clDarkViolet32',           clDarkViolet32);
  AddColor('clDeepPink32',             clDeepPink32);
  AddColor('clDeepSkyBlue32',          clDeepSkyBlue32);
  AddColor('clDodgerBlue32',           clDodgerBlue32);
  AddColor('clFireBrick32',            clFireBrick32);
  AddColor('clFloralWhite32',          clFloralWhite32);
  AddColor('clGainsBoro32',            clGainsBoro32);
  AddColor('clGhostWhite32',           clGhostWhite32);
  AddColor('clGold32',                 clGold32);
  AddColor('clGoldenRod32',            clGoldenRod32);
  AddColor('clGreenYellow32',          clGreenYellow32);
  AddColor('clGrey32',                 clGrey32);
  AddColor('clHoneyDew32',             clHoneyDew32);
  AddColor('clHotPink32',              clHotPink32);
  AddColor('clIndianRed32',            clIndianRed32);
  AddColor('clIndigo32',               clIndigo32);
  AddColor('clIvory32',                clIvory32);
  AddColor('clKhaki32',                clKhaki32);
  AddColor('clLavender32',             clLavender32);
  AddColor('clLavenderBlush32',        clLavenderBlush32);
  AddColor('clLawnGreen32',            clLawnGreen32);
  AddColor('clLemonChiffon32',         clLemonChiffon32);
  AddColor('clLightBlue32',            clLightBlue32);
  AddColor('clLightCoral32',           clLightCoral32);
  AddColor('clLightCyan32',            clLightCyan32);
  AddColor('clLightGoldenRodYellow32', clLightGoldenRodYellow32);
  AddColor('clLightGray32',            clLightGray32);
  AddColor('clLightGreen32',           clLightGreen32);
  AddColor('clLightGrey32',            clLightGrey32);
  AddColor('clLightPink32',            clLightPink32);
  AddColor('clLightSalmon32',          clLightSalmon32);
  AddColor('clLightSeagreen32',        clLightSeagreen32);
  AddColor('clLightSkyblue32',         clLightSkyblue32);
  AddColor('clLightSlategray32',       clLightSlategray32);
  AddColor('clLightSlategrey32',       clLightSlategrey32);
  AddColor('clLightSteelblue32',       clLightSteelblue32);
  AddColor('clLightYellow32',          clLightYellow32);
  AddColor('clLtGray32',               clLtGray32);
  AddColor('clMedGray32',              clMedGray32);
  AddColor('clDkGray32',               clDkGray32);
  AddColor('clMoneyGreen32',           clMoneyGreen32);
  AddColor('clLegacySkyBlue32',        clLegacySkyBlue32);
  AddColor('clCream32',                clCream32);
  AddColor('clLimeGreen32',            clLimeGreen32);
  AddColor('clLinen32',                clLinen32);
  AddColor('clMediumAquamarine32',     clMediumAquamarine32);
  AddColor('clMediumBlue32',           clMediumBlue32);
  AddColor('clMediumOrchid32',         clMediumOrchid32);
  AddColor('clMediumPurple32',         clMediumPurple32);
  AddColor('clMediumSeaGreen32',       clMediumSeaGreen32);
  AddColor('clMediumSlateBlue32',      clMediumSlateBlue32);
  AddColor('clMediumSpringGreen32',    clMediumSpringGreen32);
  AddColor('clMediumTurquoise32',      clMediumTurquoise32);
  AddColor('clMediumVioletRed32',      clMediumVioletRed32);
  AddColor('clMidnightBlue32',         clMidnightBlue32);
  AddColor('clMintCream32',            clMintCream32);
  AddColor('clMistyRose32',            clMistyRose32);
  AddColor('clMoccasin32',             clMoccasin32);
  AddColor('clNavajoWhite32',          clNavajoWhite32);
  AddColor('clOldLace32',              clOldLace32);
  AddColor('clOliveDrab32',            clOliveDrab32);
  AddColor('clOrange32',               clOrange32);
  AddColor('clOrangeRed32',            clOrangeRed32);
  AddColor('clOrchid32',               clOrchid32);
  AddColor('clPaleGoldenRod32',        clPaleGoldenRod32);
  AddColor('clPaleGreen32',            clPaleGreen32);
  AddColor('clPaleTurquoise32',        clPaleTurquoise32);
  AddColor('clPaleVioletred32',        clPaleVioletred32);
  AddColor('clPapayaWhip32',           clPapayaWhip32);
  AddColor('clPeachPuff32',            clPeachPuff32);
  AddColor('clPeru32',                 clPeru32);
  AddColor('clPlum32',                 clPlum32);
  AddColor('clPowderBlue32',           clPowderBlue32);
  AddColor('clPurple32',               clPurple32);
  AddColor('clRosyBrown32',            clRosyBrown32);
  AddColor('clRoyalBlue32',            clRoyalBlue32);
  AddColor('clSaddleBrown32',          clSaddleBrown32);
  AddColor('clSalmon32',               clSalmon32);
  AddColor('clSandyBrown32',           clSandyBrown32);
  AddColor('clSeaGreen32',             clSeaGreen32);
  AddColor('clSeaShell32',             clSeaShell32);
  AddColor('clSienna32',               clSienna32);
  AddColor('clSilver32',               clSilver32);
  AddColor('clSkyblue32',              clSkyblue32);
  AddColor('clSlateBlue32',            clSlateBlue32);
  AddColor('clSlateGray32',            clSlateGray32);
  AddColor('clSlateGrey32',            clSlateGrey32);
  AddColor('clSnow32',                 clSnow32);
  AddColor('clSpringgreen32',          clSpringgreen32);
  AddColor('clSteelblue32',            clSteelblue32);
  AddColor('clTan32',                  clTan32);
  AddColor('clThistle32',              clThistle32);
  AddColor('clTomato32',               clTomato32);
  AddColor('clTurquoise32',            clTurquoise32);
  AddColor('clViolet32',               clViolet32);
  AddColor('clWheat32',                clWheat32);
  AddColor('clWhitesmoke32',           clWhitesmoke32);
  AddColor('clYellowgreen32',          clYellowgreen32);
end;

procedure TColorManager.RemoveColor(const AName: string);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, AName) then
    begin
      Delete(i);
      break;
    end;
end;

procedure RegisterColor(const AName: string; AColor: TColor32);
begin
  ColorManager.AddColor(AName, AColor);
end;

procedure UnregisterColor(const AName: string);
begin
  ColorManager.RemoveColor(AName);
end;


{ TColor32Property }

{$IFDEF EXT_PROP_EDIT}
procedure TColor32Property.Edit;
var
  ColorPicker: TFormColorPicker;
begin
  ColorPicker := TFormColorPicker.Create(nil);
  try
    ColorPicker.Color := GetOrdValue;

    if (ColorPicker.Execute) then
      SetOrdValue(Cardinal(ColorPicker.Color));
  finally
    ColorPicker.Free;
  end;
end;
{$ENDIF}

function TColor32Property.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable {$IFDEF EXT_PROP_EDIT}, paDialog{$ENDIF}];
end;

procedure TColor32Property.GetValues(Proc: TGetStrProc);
begin
  try
    ColorManager.EnumColors(Proc);
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

function TColor32Property.GetValue: string;
begin
  try
    Result := ColorManager.GetColorName(Cardinal(GetOrdValue));
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

procedure TColor32Property.SetValue(const Value: string);
begin
  try
    SetOrdValue(Cardinal(ColorManager.GetColor(Value)));
    Modified;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

{$IFDEF EXT_PROP_EDIT}

procedure TColor32Property.ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
begin
  // implementation dummie to satisfy interface. Don't change default value.
end;

procedure TColor32Property.ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
begin
  // implementation dummie to satisfy interface. Don't change default value.
end;

procedure TColor32Property.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  Right: Integer;
  C: TColor32;
  i, j: Integer;
  W, H: Integer;
  Bitmap32: TBitmap32;
begin
  try
    Right := (ARect.Bottom - ARect.Top) + ARect.Left;
    Bitmap32 := TBitmap32.Create;
    try
      W := Right - ARect.Left - 2;
      H := ARect.Bottom - ARect.Top - 2;
      Bitmap32.SetSize(W, H);
      if Assigned(ColorManager) then
        C := ColorManager.GetColor(Value)
      else
        C := clWhite32;
      if (W > 8) and (H > 8) then
      begin
        if not (C and $FF000000 = $FF000000) then
        begin
          for j := 0 to H - 1 do
            for i := 0 to W - 1 do
              if Odd(i div 3) = Odd(j div 3) then
                Bitmap32[i, j] := clBlack32
              else
                Bitmap32[i, j] := clWhite32;
        end;
        Bitmap32.FillRectT(0, 0, W, H, C);
      end;
      Bitmap32.FrameRectTS(0, 0, W, H, $DF000000);
      Bitmap32.DrawTo(ACanvas.Handle, ARect.Left + 1, ARect.Top + 1);
    finally
      Bitmap32.Free;
      DefaultPropertyListDrawValue(Value, ACanvas,
        Rect(Right, ARect.Top, ARect.Right, ARect.Bottom), ASelected);
    end;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

procedure TColor32Property.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True{ASelected})
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TColor32Property.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

function TColor32Property.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

function TColor32Property.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;

{$ENDIF}


initialization
  ColorManager := TColorManager.Create;
  ColorManager.RegisterDefaultColors;

finalization
  ColorManager.Free;

end.
