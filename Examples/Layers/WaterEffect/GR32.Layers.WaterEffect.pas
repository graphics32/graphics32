unit GR32.Layers.WaterEffect;

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
 * The Original Code is WaterEffect Example
 *
 * The Initial Developers of the Original Code is:
 *
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2025
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  GR32,
  GR32_OrdinalMaps,
  GR32_Layers;

(*
  Based on:

  - 2D Water
    Hugo Elias, 2 dec 1998
    https://web.archive.org/web/20160607052007/http://freespace.virgin.net/hugo.elias/graphics/x_water.htm

  - 2D Water Effects
    Game Developer magazine, December 1999
    http://www.darwin3d.com/gdm1999.htm#gdm1299

  - The Water Effect Explained
    gamedev.net, Graphics and GPU Programming, February 15, 2000
    Roy Willemse
    https://www.gamedev.net/tutorials/programming/graphics/the-water-effect-explained-r915/

  - TortoiseGit
    https://github.com/TortoiseGit/TortoiseGit/blob/master/src/Utils/MiscUI/WaterEffect.cpp

*)
type
  TWaterEffect = class(TObject)
  private const
    DefaultDecay = 0.1;
  private
    FDistortionMap: TIntegerMap;
    FNextDistortionMap: TIntegerMap;
    FDecay: Single;
    FLightModifier: Integer;

  private
    procedure SetDecay(Value: Single);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SetSize(AWidth, AHeight: Integer);

    function UpdateDistortionMap: boolean;
    procedure Draw(BitmapSource, BitmapDest: TBitmap32);
    procedure Drop(X, Y: Integer; DropRadius, DropAmplitude: Integer);

    // Decay: 0..1
    property Decay: Single read FDecay write SetDecay;
  end;

type
  TWaterEffectLayer32 = class(TCustomLayer)
  private
    FWaterEffect: TWaterEffect;
    FHadRipples: boolean;

  private
    function GetWaveDecay: Single;
    procedure SetWaveDecay(const Value: Single);

  protected
    procedure Paint(Buffer: TBitmap32); override;

  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    procedure UpdateWater;
    procedure WaterDrop(X, Y: Integer; DropRadius, DropAmplitude: Integer);
    property WaveDecay: Single read GetWaveDecay write SetWaveDecay;
  end;

implementation

uses
  Math,
  GR32_LowLevel;

constructor TWaterEffect.Create;
begin
  inherited;

  FLightModifier := 10;
  FDecay := DefaultDecay;

  FDistortionMap := TIntegerMap.Create;
  FNextDistortionMap := TIntegerMap.Create;
end;

destructor TWaterEffect.Destroy;
begin
  FDistortionMap.Free;
  FNextDistortionMap.Free;

  inherited;
end;

procedure TWaterEffect.Drop(X, Y: Integer; DropRadius, DropAmplitude: Integer);
var
  SquareRadius: Integer;
  DropX, DropY, SquareY: Integer;
  DropRect: TRect;
begin
  if (X < 0) or (X > FDistortionMap.Width - 1) then
    X := Random(FDistortionMap.Width);

  if (Y < 0) or (Y > FDistortionMap.Height - 1) then
    Y := Random(FDistortionMap.Height);

  DropRect.Left := -Min(X, DropRadius);
  DropRect.Right := Min(FDistortionMap.Width - 1 - X, DropRadius);
  DropRect.Top := -Min(Y, DropRadius);
  DropRect.Bottom := Min(FDistortionMap.Height - 1 - Y, DropRadius);

  SquareRadius := DropRadius * DropRadius;
  for DropY := DropRect.Top to DropRect.Bottom do
  begin
    SquareY := DropY * DropY;

    for DropX := DropRect.Left to DropRect.Right do
    begin
      if (DropX * DropX + SquareY <= SquareRadius) then
        Inc(FDistortionMap.ValPtr[DropX + X, DropY + Y]^, DropAmplitude);
    end;
  end;
end;

{$R-}{$Q-}  // switch off overflow and range checking

function TWaterEffect.UpdateDistortionMap: boolean;
var
  DecayRate: Integer;
  Amplitude: Integer;
  X, Y, XLeft, XRight: Integer;
  TargetRow: PIntegerArray;
  SourceRowAbove, SourceRow, SourceRowBelow: PIntegerArray;
  Target: PInteger;
  Temp: TIntegerMap;
begin
  Result := False;

  DecayRate := Round((1.0 - FDecay) * 256 * 256);

  // Note:
  // Reflection is implemented by using Abs() and Reflect() instead of Max() and Min().

  for Y := 0 to FDistortionMap.Height - 1 do
  begin
    TargetRow := FNextDistortionMap.Scanline[Y];

    SourceRowAbove := FDistortionMap.Scanline[Abs(Y - 1)];
    SourceRow := FDistortionMap.Scanline[Y];
    SourceRowBelow := FDistortionMap.Scanline[Reflect(Y + 1, FDistortionMap.Height - 1)];

    for X := 0 to FDistortionMap.Width - 1 do
    begin
      XLeft := Abs(X - 1);
      XRight := Reflect(X + 1, FDistortionMap.Width - 1);
      Target := @TargetRow[X];

      Amplitude := (SourceRowAbove[XLeft] + SourceRowAbove[X] + SourceRowAbove[XRight] + SourceRow[XLeft] + SourceRow[XRight] + SourceRowBelow[XLeft] + SourceRowBelow[X] + SourceRowBelow[XRight]) div 4 - Target^;
      Target^ := (Amplitude * DecayRate) div (256 * 256);

      Result := Result or (Target^ <> 0);
    end;
  end;

  Temp := FDistortionMap;
  FDistortionMap := FNextDistortionMap;
  FNextDistortionMap := Temp;
end;

procedure TWaterEffect.Clear;
begin
  FDistortionMap.Clear;
  FNextDistortionMap.Clear;
end;

procedure TWaterEffect.Draw(BitmapSource, BitmapDest: TBitmap32);
var
  X, Y: Integer;
  SourceX, SourceY: Integer;
  DistortionX, DistortionY: Integer;
  DistortionAbove, Distortion, DistortionBelow: PIntegerArray;
  PixelSource, PixelDest: PColor32Entry;
begin
  for Y := 0 to FDistortionMap.Height - 1 do
  begin
    PixelDest := PColor32Entry(BitmapDest.PixelPtr[0, Y]);

    DistortionAbove := FDistortionMap.Scanline[Abs(Y - 1)];
    Distortion := FDistortionMap.Scanline[Y];
    DistortionBelow := FDistortionMap.Scanline[Reflect(Y + 1, FDistortionMap.Height - 1)];

    for X := 0 to FDistortionMap.Width - 1 do
    begin
      DistortionX := Distortion[Abs(X - 1)] - Distortion[Reflect(X + 1, FDistortionMap.Width - 1)];
      DistortionY := DistortionAbove[X] - DistortionBelow[X];

      SourceX := X + DistortionX;
      SourceY := Y + DistortionY;

      if (SourceX >= 0) and (SourceX < FDistortionMap.Width) and (SourceY + DistortionY >= 0) and (SourceY < FDistortionMap.Height) then
      begin
        PixelSource := PColor32Entry(BitmapSource.PixelPtr[SourceX, SourceY]);

        PixelDest.R := Clamp(PixelSource.R - DistortionX);
        PixelDest.G := Clamp(PixelSource.G - DistortionX);
        PixelDest.B := Clamp(PixelSource.B - DistortionX);
      end else
        PixelDest^ := TColor32Entry(BitmapSource.Pixel[X, Y]);

      Inc(PixelDest);
    end;
  end;
end;

procedure TWaterEffect.SetDecay(Value: Single);
begin
  FDecay := Constrain(Value, 0, 1);
end;

procedure TWaterEffect.SetSize(AWidth, AHeight: Integer);
begin
  if (AWidth <= 0) or (AHeight <= 0) then
  begin
    AWidth := 0;
    AHeight := 0;
  end;

  if (AWidth = FDistortionMap.Width) and (AHeight = FDistortionMap.Height) then
    exit;

  FDistortionMap.SetSize(AWidth, AHeight);
  FNextDistortionMap.SetSize(AWidth, AHeight);

  Clear;
end;

{ TWaterEffectLayer32 }

constructor TWaterEffectLayer32.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FWaterEffect := TWaterEffect.Create;
  MouseEvents := True;
end;

destructor TWaterEffectLayer32.Destroy;
begin
  FWaterEffect.Free;
  inherited;
end;

function TWaterEffectLayer32.GetWaveDecay: Single;
begin
  Result := FWaterEffect.Decay;
end;

procedure TWaterEffectLayer32.Paint(Buffer: TBitmap32);
var
  BitmapDest: TBitmap32;
begin
  FWaterEffect.SetSize(Buffer.Width, Buffer.Height);

  BitmapDest := TBitmap32.Create(Buffer.Width, Buffer.Height);
  try
    FWaterEffect.Draw(Buffer, BitmapDest);

    BitmapDest.CopyMapTo(Buffer);
  finally
    BitmapDest.Free;
  end;
end;

procedure TWaterEffectLayer32.SetWaveDecay(const Value: Single);
begin
  FWaterEffect.Decay := Value;
end;

procedure TWaterEffectLayer32.UpdateWater;
var
  HasRipples: boolean;
begin
  HasRipples := FWaterEffect.UpdateDistortionMap;

  if (HasRipples or FHadRipples) then
    Changed;

  FHadRipples := HasRipples;
end;

procedure TWaterEffectLayer32.WaterDrop(X, Y, DropRadius, DropAmplitude: Integer);
begin
  FWaterEffect.Drop(X, Y, DropRadius, DropAmplitude);
  Changed;
end;

end.
