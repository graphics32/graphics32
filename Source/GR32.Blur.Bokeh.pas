unit GR32.Blur.Bokeh experimental;

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
 * The Original Code is Bokeh Blur for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{$if defined(DCC) and (CompilerVersion >= 28.0)}
  {$define USE_PPL}
{$ifend}

uses
  GR32,
  GR32_LowLevel, // inlining
  GR32.Blur;

(*
 *       Bokeh Blur
 *
 * Bokeh is a photographic effect where out-of-focus points of light are
 * rendered as blurred shapes, typically determined by the shape of the
 * camera aperture.
 *
 * This implementation renders hexagonal bokeh by decomposing a hexagonal
 * convolution kernel into three rhombi. Each rhombus is implemented as an
 * optimized sequence of 1D directional blurs.
 *
 * To minimize the number of samples required for a smooth result, jittered
 * sampling is used. By adding a pseudo-random offset to the sample positions
 * along the blur line, any potential banding or structural artifacts caused
 * by the finite number of samples are converted into low-frequency noise,
 * which is much less noticeable to the human eye.
 *
 * Based on:
 *
 * - "More Performance! Five Rendering Ideas From Battlefield 3 and Need For Speed: The Run"
 *   John White and Colin Barré-Brisebois
 *   http://advances.realtimerendering.com/s2011/White, BarreBrisebois- Rendering in BF3 (Siggraph 2011 Advances in Real-Time Rendering Course).pptx
 *
 * - glfx.js
 *   Evan Wallace
 *   https://github.com/evanw/glfx.js/blob/master/src/filters/blur/lensblur.js
 *)

// Radius: Blur radius in pixels
// Angle: [0 .. 2*PI] (Radians)
// Brightness: [-1 .. 1]
procedure BokehBlur32(ASource, ADest: TBitmap32; Radius: TFloat; Brightness: TFloat = 0; Angle: TFloat = 0; Threaded: Boolean = False); overload;
procedure BokehBlur32(Bitmap: TBitmap32; Radius: TFloat; Brightness: TFloat = 0; Angle: TFloat = 0; Threaded: Boolean = False); overload;

procedure GammaBokehBlur32(ASource, ADest: TBitmap32; Radius: TFloat; Brightness: TFloat = 0; Angle: TFloat = 0; Threaded: Boolean = False); overload;
procedure GammaBokehBlur32(Bitmap: TBitmap32; Radius: TFloat; Brightness: TFloat = 0; Angle: TFloat = 0; Threaded: Boolean = False); overload;

procedure AlphaBokehBlur32(ASource, ADest: TBitmap32; Radius: TFloat; Brightness: TFloat = 0; Angle: TFloat = 0; Threaded: Boolean = False); overload;
procedure AlphaBokehBlur32(Bitmap: TBitmap32; Radius: TFloat; Brightness: TFloat = 0; Angle: TFloat = 0; Threaded: Boolean = False); overload;

procedure GammaAlphaBokehBlur32(ASource, ADest: TBitmap32; Radius: TFloat; Brightness: TFloat = 0; Angle: TFloat = 0; Threaded: Boolean = False); overload;
procedure GammaAlphaBokehBlur32(Bitmap: TBitmap32; Radius: TFloat; Brightness: TFloat = 0; Angle: TFloat = 0; Threaded: Boolean = False); overload;

implementation

uses
  Math, SysUtils,
{$if defined(USE_PPL)}
  System.Threading,
{$ifend}
  GR32_System,
  GR32_Blend,
  GR32_Gamma;

type
  PFloatArray = GR32.PFloatArray;

const
  Inv255: TFloat = 1.0 / 255.0;
  Inv255x255: TFloat = 1.0 / (255.0 * 255.0);

// Fast pseudo-random for jittering
function GetJitter(x, y: Integer): TFloat; {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := ((x * 15731 + y * 789221 + 1376312589) and $FFFF) / $FFFF;
end;

function SampleClamped(Src: PFloatArray; Width, Height: Integer; x, y: TFloat): TFloat; {$IFDEF USEINLINING} inline; {$ENDIF}
var
  ix, iy: Integer;
begin
  ix := GR32_LowLevel.Clamp(FastFloor(x), 0, Width - 1);
  iy := GR32_LowLevel.Clamp(FastFloor(y), 0, Height - 1);
  Result := Src[iy * Width + ix];
end;

procedure Blur1D_TaskProcessRow(y: Integer; Src: PFloatArray; Width, Height: Integer; dx, dy: TFloat; Dest: PFloatArray; NumSamples: Integer; OffsX, OffsY: PFloatArray);
var
  x, i: Integer;
  Sum, JitterOffset, BaseX, BaseY: TFloat;
  InvNumSamples: TFloat;
  StepX, StepY: TFloat;
begin
  InvNumSamples := 1.0 / NumSamples;
  StepX := dx * InvNumSamples; // = dx / NumSamples
  StepY := dy * InvNumSamples; // = dy / NumSamples

  for x := 0 to Width - 1 do
  begin
    Sum := 0;
    JitterOffset := GetJitter(x, y);
    BaseX := x + StepX * JitterOffset;
    BaseY := y + StepY * JitterOffset;

    for i := 0 to NumSamples - 1 do
      Sum := Sum + SampleClamped(Src, Width, Height, BaseX + OffsX[i], BaseY + OffsY[i]);

    Dest[y * Width + x] := Sum * InvNumSamples;
  end;
end;

procedure Blur1D(Src: PFloatArray; Width, Height: Integer; dx, dy: TFloat; Dest: PFloatArray; NumSamples: Integer; Threaded: Boolean);
var
  StepX, StepY: TFloat;
  OffsX, OffsY: PFloatArray;
  InvNumSamples: TFloat;

  procedure ProcessRow(y: Integer);
  var
    x, i: Integer;
    Sum, JitterOffset, BaseX, BaseY: TFloat;
  begin
    for x := 0 to Width - 1 do
    begin
      Sum := 0;
      JitterOffset := GetJitter(x, y);
      BaseX := x + StepX * JitterOffset;
      BaseY := y + StepY * JitterOffset;

      for i := 0 to NumSamples - 1 do
        Sum := Sum + SampleClamped(Src, Width, Height, BaseX + OffsX[i], BaseY + OffsY[i]);

      Dest[y * Width + x] := Sum * InvNumSamples;
    end;
  end;

var
  y, i: Integer;
begin
  if (NumSamples <= 1) then
  begin
    Move(Src^, Dest^, Width * Height * SizeOf(TFloat));
    Exit;
  end;

  InvNumSamples := 1.0 / NumSamples;
  StepX := dx * InvNumSamples; // = dx / NumSamples
  StepY := dy * InvNumSamples; // = dy / NumSamples

  GetMem(OffsX, 2 * NumSamples * SizeOf(TFloat));
  OffsY := PFloatArray(@OffsX[NumSamples]);
  try

    for i := 0 to NumSamples - 1 do
    begin
      OffsX[i] := StepX * i;
      OffsY[i] := StepY * i;
    end;

    if Threaded then
    begin
{$if defined(USE_PPL)}
      TParallel.For(0, Height - 1, procedure(y: Integer)
      begin
        Blur1D_TaskProcessRow(y, Src, Width, Height, dx, dy, Dest, NumSamples, OffsX, OffsY);
      end);
{$else}
      for y := 0 to Height - 1 do
        ProcessRow(y);
{$ifend}
    end else
    begin
      for y := 0 to Height - 1 do
        ProcessRow(y);
    end;

  finally
    FreeMem(OffsX);
  end;
end;

procedure Blur1D_Two_TaskProcessRow(y: Integer; Src: PFloatArray; Width, Height: Integer; dx1, dy1, dx2, dy2: TFloat; Dest: PFloatArray; NumSamples: integer; OffsX1, OffsY1, OffsX2, OffsY2: PFloatArray);
var
  x, i: Integer;
  Sum, JitterOffset, BaseX1, BaseY1, BaseX2, BaseY2: TFloat;
  StepX1, StepY1, StepX2, StepY2: TFloat;
  InvNumSamples: TFloat;
  InvTwoNumSamples: TFloat;
begin
  InvNumSamples := 1.0 / NumSamples;
  StepX1 := dx1 * InvNumSamples; // = dx1 / NumSamples
  StepY1 := dy1 * InvNumSamples; // = dy1 / NumSamples
  StepX2 := dx2 * InvNumSamples; // = dx2 / NumSamples
  StepY2 := dy2 * InvNumSamples; // = dy2 / NumSamples
  InvTwoNumSamples := 0.5 * InvNumSamples; // = 1.0 / (2.0 * NumSamples)

  for x := 0 to Width - 1 do
  begin
    Sum := 0;
    JitterOffset := GetJitter(x, y);
    BaseX1 := x + StepX1 * JitterOffset;
    BaseY1 := y + StepY1 * JitterOffset;
    BaseX2 := x + StepX2 * JitterOffset;
    BaseY2 := y + StepY2 * JitterOffset;

    for i := 0 to NumSamples - 1 do
    begin
      Sum := Sum + SampleClamped(Src, Width, Height, BaseX1 + OffsX1[i], BaseY1 + OffsY1[i]);
      Sum := Sum + SampleClamped(Src, Width, Height, BaseX2 + OffsX2[i], BaseY2 + OffsY2[i]);
    end;

    Dest[y * Width + x] := Sum * InvTwoNumSamples;
  end;
end;

procedure Blur1D_Two(Src: PFloatArray; Width, Height: Integer; dx1, dy1, dx2, dy2: TFloat; Dest: PFloatArray; NumSamples: Integer; Threaded: Boolean);
var
  StepX1, StepY1, StepX2, StepY2: TFloat;
  OffsX1, OffsY1, OffsX2, OffsY2: PFloatArray;
  InvNumSamples: TFloat;
  InvTwoNumSamples: TFloat;

  procedure ProcessRow(y: Integer);
  var
    x, i: Integer;
    Sum, JitterOffset, BaseX1, BaseY1, BaseX2, BaseY2: TFloat;
  begin
    for x := 0 to Width - 1 do
    begin
      Sum := 0;
      JitterOffset := GetJitter(x, y);
      BaseX1 := x + StepX1 * JitterOffset;
      BaseY1 := y + StepY1 * JitterOffset;
      BaseX2 := x + StepX2 * JitterOffset;
      BaseY2 := y + StepY2 * JitterOffset;

      for i := 0 to NumSamples - 1 do
      begin
        Sum := Sum + SampleClamped(Src, Width, Height, BaseX1 + OffsX1[i], BaseY1 + OffsY1[i]);
        Sum := Sum + SampleClamped(Src, Width, Height, BaseX2 + OffsX2[i], BaseY2 + OffsY2[i]);
      end;

      Dest[y * Width + x] := Sum * InvTwoNumSamples;
    end;
  end;

var
  y, i: Integer;
begin
  if (NumSamples <= 1) then
  begin
    Move(Src^, Dest^, Width * Height * SizeOf(TFloat));
    Exit;
  end;

  InvNumSamples := 1.0 / NumSamples;
  StepX1 := dx1 * InvNumSamples; // = dx1 / NumSamples
  StepY1 := dy1 * InvNumSamples; // = dy1 / NumSamples
  StepX2 := dx2 * InvNumSamples; // = dx2 / NumSamples
  StepY2 := dy2 * InvNumSamples; // = dy2 / NumSamples
  InvTwoNumSamples := 0.5 * InvNumSamples; // = 1.0 / (2.0 * NumSamples)

  GetMem(OffsX1, 4 * NumSamples * SizeOf(TFloat));
  OffsY1 := PFloatArray(@OffsX1[NumSamples]);
  OffsX2 := PFloatArray(@OffsY1[NumSamples]);
  OffsY2 := PFloatArray(@OffsX2[NumSamples]);
  try

    for i := 0 to NumSamples - 1 do
    begin
      OffsX1[i] := StepX1 * i;
      OffsY1[i] := StepY1 * i;
      OffsX2[i] := StepX2 * i;
      OffsY2[i] := StepY2 * i;
    end;

    if Threaded then
    begin
{$if defined(USE_PPL)}
      TParallel.For(0, Height - 1, procedure(y: Integer)
      begin
        Blur1D_Two_TaskProcessRow(y, Src, Width, Height, dx1, dy1, dx2, dy2, Dest, NumSamples, OffsX1, OffsY1, OffsX2, OffsY2);
      end);
{$else}
      for y := 0 to Height - 1 do
        ProcessRow(y);
{$ifend}
    end else
    begin
      for y := 0 to Height - 1 do
        ProcessRow(y);
    end;

  finally
    FreeMem(OffsX1);
  end;
end;

type
  TFloatBuffers = array[0..3] of PFloatArray;

procedure TaskStep4(y, Width: Integer; Buf: PFloatArray; p: TFloat);
var
  x, offset: Integer;
begin
  offset := y * Width;
  for x := 0 to Width - 1 do
    Buf[offset + x] := Power(Max(0, Buf[offset + x]), p);
end;


procedure TaskProcessChannelRow(y: Integer; AChannel: TColor32Component; Width: integer; OriginalAlpha: PByteArray; SrcBits: PColor32EntryArray; FloatBuffer: PFloatArray; GammaAware, AlphaAware: Boolean);
var
  x, offset: Integer;
  V: TFloat;
  A: Byte;
  Color: Byte;
begin
  offset := y * Width;

  for x := 0 to Width - 1 do
  begin
    if (OriginalAlpha <> nil) then
      A := OriginalAlpha[offset + x]
    else
      A := SrcBits[offset + x].A;

    if (AChannel = ccAlpha) then
      V := A * Inv255
    else
    begin
      Color := SrcBits[offset + x].Components[AChannel];

      if AlphaAware then
        // AlphaAware assumes GammaAware.
        // Result = (decoded_color / 255) * (alpha / 255)
        V := GAMMA_DECODING_TABLE[Color] * A * Inv255x255
      else
      if GammaAware then
        V := GAMMA_DECODING_TABLE[Color] * Inv255
      else
        V := Color * Inv255;
    end;

    FloatBuffer[offset + x] := V;
  end;
end;

procedure TaskSaveChannelRow(y: Integer; AChannel: TColor32Component; Width: integer; FloatBuffer: PFloatArray; DstBits: PColor32EntryArray; GammaAware, AlphaAware: Boolean);
var
  x, offset: Integer;
  V, AV: TFloat;
begin
  offset := y * Width;
  for x := 0 to Width - 1 do
  begin
    V := FloatBuffer[offset + x];

    if (AChannel = ccAlpha) then
      DstBits[offset + x].A := GR32_LowLevel.Clamp(Round(V * 255), 0, 255)
    else
    begin
      if AlphaAware then
      begin
        AV := DstBits[offset + x].A * Inv255;
        if (AV > 0) then
          V := V / AV
        else
          V := 0;
      end;

      if GammaAware or AlphaAware then
        V := GAMMA_ENCODING_TABLE[GR32_LowLevel.Clamp(Round(V * 255), 0, 255)]
      else
        V := V * 255;

      DstBits[offset + x].Components[AChannel] := GR32_LowLevel.Clamp(Round(V), 0, 255);
    end;
  end;
end;

procedure InternalBokehBlur(ASource, ADest: TBitmap32; Radius: TFloat; Brightness, Angle: TFloat; GammaAware, AlphaAware, Threaded: Boolean);
var
  Width, Height: Integer;
  FloatBuffers: TFloatBuffers;
  idx: Integer;
  Channel: TColor32Component;
  dx, dy: array[0..2] of TFloat;
  PowerVal: TFloat;
  SrcBits: PColor32EntryArray;
  DstBits: PColor32EntryArray;
  InvPower: TFloat;
  NumSamples: Integer;
  OriginalAlpha: PByteArray;
{$if not defined(USE_PPL)}
  i: Integer;
{$ifend}

  procedure ApplyPower(Buf: PFloatArray; p: TFloat);
  var
    j: Integer;
  begin
    if (p = 1.0) then
      Exit;

    if Threaded then
    begin
{$if defined(USE_PPL)}
      TParallel.For(0, Height - 1, procedure(y: Integer)
      begin
        TaskStep4(y, Width, Buf, p);
      end);
{$else}
      for j := 0 to Width * Height - 1 do
        Buf[j] := Power(Max(0, Buf[j]), p);
{$ifend}
    end else
    begin
      for j := 0 to Width * Height - 1 do
        Buf[j] := Power(Max(0, Buf[j]), p);
    end;
  end;

  procedure ProcessChannel(AChannel: TColor32Component);

    procedure ProcessChannelRow(y: Integer);
    var
      x, offset: Integer;
      V: TFloat;
      A: Byte;
      Color: Byte;
    begin
      offset := y * Width;
      for x := 0 to Width - 1 do
      begin
        if OriginalAlpha <> nil then
          A := OriginalAlpha[offset + x]
        else
          A := SrcBits[offset + x].A;

        if (AChannel = ccAlpha) then
          V := A * Inv255
        else
        begin
          Color := SrcBits[offset + x].Components[AChannel];

          if AlphaAware then
          begin
            // AlphaAware assumes GammaAware.
            // Result = (decoded_color / 255) * (alpha / 255)
            V := GAMMA_DECODING_TABLE[Color] * A * Inv255x255;
          end else
          if GammaAware then
            V := GAMMA_DECODING_TABLE[Color] * Inv255
          else
            V := Color * Inv255;
        end;
        FloatBuffers[0][offset + x] := V;
      end;
    end;

    procedure SaveChannelRow(y: Integer);
    var
      x, offset: Integer;
      V, AV: TFloat;
    begin
      offset := y * Width;
      for x := 0 to Width - 1 do
      begin
        V := FloatBuffers[0][offset + x];

        if (AChannel = ccAlpha) then
          DstBits[offset + x].A := GR32_LowLevel.Clamp(Round(V * 255), 0, 255)
        else
        begin
          if AlphaAware then
          begin
            AV := DstBits[offset + x].A * Inv255;
            if AV > 0 then
              V := V / AV
            else
              V := 0;
          end;

          if GammaAware or AlphaAware then
            V := GAMMA_ENCODING_TABLE[GR32_LowLevel.Clamp(Round(V * 255), 0, 255)]
          else
            V := V * 255;

          DstBits[offset + x].Components[AChannel] := GR32_LowLevel.Clamp(Round(V), 0, 255);
        end;
      end;
    end;

  var
    y, i: Integer;
  begin
    // Load and Pre-process
    if Threaded then
    begin
{$if defined(USE_PPL)}
      TParallel.For(0, Height - 1,
        procedure(y: Integer)
        begin
          TaskProcessChannelRow(y, AChannel, Width, OriginalAlpha, SrcBits, FloatBuffers[0], GammaAware, AlphaAware);
        end);
{$else}
      for y := 0 to Height - 1 do
        ProcessChannelRow(y);
{$ifend}
    end else
    begin
      for y := 0 to Height - 1 do
        ProcessChannelRow(y);
    end;

    if (AChannel <> ccAlpha) then
      ApplyPower(FloatBuffers[0], PowerVal);

    // 4-pass 1D blur logic to approximate sum of 3 rhombi
    // 1. T1 = Blur1D(image, d0)
    Blur1D(FloatBuffers[0], Width, Height, dx[0], dy[0], FloatBuffers[1], NumSamples, Threaded);

    // 2. T2 = (Blur1D(T1, d1) + Blur1D(T1, d2)) / 2
    Blur1D_Two(FloatBuffers[1], Width, Height, dx[1], dy[1], dx[2], dy[2], FloatBuffers[2], NumSamples, Threaded);

    // 3. T1 = Blur1D(image, d1)
    Blur1D(FloatBuffers[0], Width, Height, dx[1], dy[1], FloatBuffers[1], NumSamples, Threaded);

    // 4. T3 = Blur1D(T1, d2)
    Blur1D(FloatBuffers[1], Width, Height, dx[2], dy[2], FloatBuffers[3], NumSamples, Threaded);

    // Result = (T3 + 2 * T2) / 3
    if Threaded then
    begin
{$if defined(USE_PPL)}
      TParallel.For(0, Height - 1, procedure(y: Integer)
      var
        x, offset: Integer;
      begin
        offset := y * Width;
        for x := 0 to Width - 1 do
          FloatBuffers[0][offset + x] := (FloatBuffers[3][offset + x] + 2.0 * FloatBuffers[2][offset + x]) / 3.0;
      end);
{$else}
      for y := 0 to Height - 1 do
      begin
        for i := 0 to Width - 1 do
          FloatBuffers[0][y * Width + i] := (FloatBuffers[3][y * Width + i] + 2.0 * FloatBuffers[2][y * Width + i]) / 3.0;
      end;
{$ifend}
    end else
    begin
      for i := 0 to Width * Height - 1 do
        FloatBuffers[0][i] := (FloatBuffers[3][i] + 2.0 * FloatBuffers[2][i]) / 3.0;
    end;

    if (AChannel <> ccAlpha) then
      ApplyPower(FloatBuffers[0], InvPower);

    // Save back
    if Threaded then
    begin
{$if defined(USE_PPL)}
      TParallel.For(0, Height - 1,
        procedure(y: Integer)
        begin
          TaskSaveChannelRow(y, AChannel, Width, FloatBuffers[0], DstBits, GammaAware, AlphaAware);
        end);
{$else}
      for y := 0 to Height - 1 do
        SaveChannelRow(y);
{$ifend}
    end else
    begin
      for y := 0 to Height - 1 do
        SaveChannelRow(y);
    end;
  end;

begin
  if Radius < 0.5 then
  begin
    ASource.CopyMapTo(ADest);
    Exit;
  end;

  Width := ASource.Width;
  Height := ASource.Height;
  if (Width <= 0) or (Height <= 0) then
    Exit;

  ADest.SetSize(Width, Height, False);

  // Calculate directions (120 degrees apart)
  for idx := 0 to 2 do
  begin
    dx[idx] := Radius * Sin(Angle + idx * PI * 2 / 3);
    dy[idx] := Radius * Cos(Angle + idx * PI * 2 / 3);
  end;

  PowerVal := Power(10, Constrain(Brightness, -1.0, 1.0));
  InvPower := 1.0 / PowerVal;
  NumSamples := Max(30, Round(Radius));

  FloatBuffers[0] := nil;
  OriginalAlpha := nil;

  try
    GetMem(FloatBuffers[0], 4 * Width * Height * SizeOf(TFloat));
    FloatBuffers[1] := PFloatArray(@FloatBuffers[0][Width * Height]);
    FloatBuffers[2] := PFloatArray(@FloatBuffers[1][Width * Height]);
    FloatBuffers[3] := PFloatArray(@FloatBuffers[2][Width * Height]);

    SrcBits := PColor32EntryArray(ASource.Bits);
    DstBits := PColor32EntryArray(ADest.Bits);

    if AlphaAware and (ASource = ADest) then
    begin
      GetMem(OriginalAlpha, Width * Height * SizeOf(Byte));
      for idx := 0 to Width * Height - 1 do
        OriginalAlpha[idx] := SrcBits[idx].A;
    end;

    // Alpha must be first for AlphaAware unpremultiplying
    for Channel := High(TColor32Component) downto Low(TColor32Component) do
      ProcessChannel(Channel);

  finally
    if (OriginalAlpha <> nil) then
      FreeMem(OriginalAlpha);

    if (FloatBuffers[0] <> nil) then
      FreeMem(FloatBuffers[0]);
  end;
end;

procedure BokehBlur32(ASource, ADest: TBitmap32; Radius: TFloat; Brightness, Angle: TFloat; Threaded: Boolean);
begin
  InternalBokehBlur(ASource, ADest, Radius, Brightness, Angle, False, False, Threaded);
end;

procedure BokehBlur32(Bitmap: TBitmap32; Radius: TFloat; Brightness, Angle: TFloat; Threaded: Boolean);
var
  Tmp: TBitmap32;
begin
  Tmp := TBitmap32.Create;
  try
    BokehBlur32(Bitmap, Tmp, Radius, Brightness, Angle, Threaded);
    Tmp.CopyMapTo(Bitmap);
  finally
    Tmp.Free;
  end;
end;

procedure GammaBokehBlur32(ASource, ADest: TBitmap32; Radius: TFloat; Brightness, Angle: TFloat; Threaded: Boolean);
begin
  InternalBokehBlur(ASource, ADest, Radius, Brightness, Angle, True, False, Threaded);
end;

procedure GammaBokehBlur32(Bitmap: TBitmap32; Radius: TFloat; Brightness, Angle: TFloat; Threaded: Boolean);
var
  Tmp: TBitmap32;
begin
  Tmp := TBitmap32.Create;
  try
    GammaBokehBlur32(Bitmap, Tmp, Radius, Brightness, Angle, Threaded);
    Tmp.CopyMapTo(Bitmap);
  finally
    Tmp.Free;
  end;
end;

procedure AlphaBokehBlur32(ASource, ADest: TBitmap32; Radius: TFloat; Brightness, Angle: TFloat; Threaded: Boolean);
begin
  InternalBokehBlur(ASource, ADest, Radius, Brightness, Angle, False, True, Threaded);
end;

procedure AlphaBokehBlur32(Bitmap: TBitmap32; Radius: TFloat; Brightness, Angle: TFloat; Threaded: Boolean);
var
  Tmp: TBitmap32;
begin
  Tmp := TBitmap32.Create;
  try
    AlphaBokehBlur32(Bitmap, Tmp, Radius, Brightness, Angle, Threaded);
    Tmp.CopyMapTo(Bitmap);
  finally
    Tmp.Free;
  end;
end;

procedure GammaAlphaBokehBlur32(ASource, ADest: TBitmap32; Radius: TFloat; Brightness, Angle: TFloat; Threaded: Boolean);
begin
  InternalBokehBlur(ASource, ADest, Radius, Brightness, Angle, True, True, Threaded);
end;

procedure GammaAlphaBokehBlur32(Bitmap: TBitmap32; Radius: TFloat; Brightness, Angle: TFloat; Threaded: Boolean);
var
  Tmp: TBitmap32;
begin
  Tmp := TBitmap32.Create;
  try
    GammaAlphaBokehBlur32(Bitmap, Tmp, Radius, Brightness, Angle, Threaded);
    Tmp.CopyMapTo(Bitmap);
  finally
    Tmp.Free;
  end;
end;

end.
