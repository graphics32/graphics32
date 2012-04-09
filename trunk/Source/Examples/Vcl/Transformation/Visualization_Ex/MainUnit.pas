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
 * The Original Code is Visualization Example
 *
 * The Initial Developers of the Original Code is:
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Mattias Andersson <mattias@centaurix.com>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  ExtCtrls, GR32, GR32_Image, GR32_VectorMaps, GR32_ExtImage, GR32_Rasterizers;

type
  TMainForm = class(TForm)
    MovementTimer: TTimer;
    RenderTimer: TTimer;
    ColorTimer: TTimer;
    FPSTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MovementTimerTimer(Sender: TObject);
    procedure RenderTimerTimer(Sender: TObject);
    procedure ColorTimerTimer(Sender: TObject);
    procedure FPSTimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CurrentBuffer : Boolean;
    Buffers: array [Boolean] of TBitmap32;
    CurrentMap : Boolean;
    VectorMap: TVectorMap;
    RenderThread: TRenderThread;

    LastPoint: array [0..15] of array [0..1] of TPoint;
    procedure RenderMovement;
    procedure TransformFrame(Sender: TObject; var Done: Boolean);
    procedure BufferFeedBack(Feedback, BlendContrast: Byte; BlendBrightness,
      BufferBrightness: Integer);
    procedure ToggleTimers(Enabled: Boolean);
    procedure RenderHelpScreen;
  end;

  // Callback prototype procedure following Winamps Vis. Studio naming and usage
  // See 'Movement' under 'Trans' in Winamps Vis. Studio
  // Sw, Sh: Display width and height (read only)
  // X,Y: Cartesian/Rectangular coordinate in range [-1..1]
  // D, R: Polar coordinate, D in range 0..1, R in range 0..2Pi
  TMovementProc = procedure(Sw, Sh: Single; var X, Y, D, R : Single);

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, GR32_Lowlevel, GR32_Math, GR32_Blend;

var
  MovementIndex : Integer = 9;
  vShowHelp: Boolean = True;
  vShowFPS: Boolean = False;
  FPS : Integer = 0;
  FPSMeasure: Integer = 0;
  IsClosing : Boolean = false;
  CX, CY, NX, NY: Integer;
  PathAngle : Single = 0;
  PathRadius: Single;
  PathStep: Single = 0.1;
  PathRadiusStep : Single = 2;

  // This number is randomized frequently, and used to control various factors
  // in rendering and movement
  ARandomNumber : Single;

  // Hue, Saturation & Lightness cycler vars, ensures coherent color variations
  Hue: Single;
  HueIncreaser : Single = 3/360;
  Sat: Single;
  SatIncreaser : Single = 1/10;
  Lns: Single;
  LnsIncreaser : Single = 1/10;


  // Variables used to control balanced drawing of the different render types
  Render_Worms : Single = 0;
  Render_PathSpots : Single = 0;
  Render_RandomSpots : Single = 0;
  Render_Noise : Single = 0;
  Feedback : Single = 0.7;
  BlendLevel : Byte = 0;
  BlendContrast : Byte = 0;
  BlendBrightness : Integer = 0;
  TimeDarkening : Integer = -1;
  // Rough FPS controlled movement speed..
  // MovementSpeed controls desired movement FPS
  MovementSpeed : Single = 75;
  FPS_Adaption : Single = 1;

  FixedMouseX, FixedMouseY: TFixed;


// Movements, some formulas were inspired by those present in winamp

procedure Movement1(Sw, Sh: Single; var X, Y, D, R : Single);
begin
  d := d * (0.96 + Cos(d * PI) * 0.05);
  r := r + (ARandomNumber - 0.5) * 0.05;
end;

procedure Movement2(Sw, Sh: Single; var X, Y, D, R : Single);
begin
  d := d * (0.94 + (Cos((r - Pi * 0.5) * 32 * ARandomNumber) * 0.06));
  r := r + (ARandomNumber - 0.5) * 0.05;
end;

procedure Movement3(Sw, Sh: Single; var X, Y, D, R : Single);
begin
  d := d * (1 - (Sin((r - Pi * 0.5) * 7) * 0.03)) * (0.96 + Cos(d * Pi) * 0.05);
  r := r + (Cos(d * 12) * 0.03) + 0.04;
end;

procedure Movement4(Sw, Sh: Single; var X, Y, D, R : Single);
begin
  d := d * Trunc(((1 - (sin((x - Pi * 0.5) * 7) * 0.03)) *
    (0.96 + Cos(y * Pi) * 0.05)) * 11) / 11;
  r :=  r + (Cos(Abs(y) * 10) * 0.03) + 0.04;
end;

procedure Movement5(Sw, Sh: Single; var X, Y, D, R : Single);
begin
  r := r + 0.01 - 0.02 * d;
  d := d * 0.96;
  x := Cos(r) * d + 8/120 - (ARandomNumber - 0.5) * 0.1;
end;

procedure Movement6(Sw, Sh: Single; var X, Y, D, R : Single);
var
  p,w: Single;
begin
  p := d * Round(((1 - (Sin((r - Pi * 0.5) * 7) * 0.01)) *
    (0.96 + Cos(GR32_Math.Hypot(x, y) * Pi) * 0.05)) * 11) / 11;
  d := d * (0.96 + Cos(d * Pi) * 0.05);
  w := 1 - abs(x * y);
  d := d + (p - d) * w;
  p := x + (Cos(y * 18) * x * 0.01);
  x := x + (p - x) * w;
  p := y + (Sin(x * 14) * y * 0.01);
  y := y + (p - y) * w;
  r := r - 0.005;
end;

procedure Movement7(Sw, Sh: Single; var X, Y, D, R : Single);
var
  t: Single;
begin
  t := Cos(d * Pi * 0.5) + x * y * 0.1;
  r := r - 0.1 * t * t * t;
  x := x / (0.9 + ARandomNumber);
  y := y / (0.9 + ARandomNumber);
end;

procedure Movement8(Sw, Sh: Single; var X, Y, D, R : Single);
begin
  x := x + (Cos(y * 18) * ARandomNumber * 0.05);
  y := y + (Sin(x * 14) * (ARandomNumber * 0.05 + 0.01));
end;

procedure Movement9(Sw, Sh: Single; var X, Y, D, R : Single);
begin
  d := d * (0.9974 - (Cos(Min(d * Pi, Pi)) + ARandomNumber) * 0.03);
end;


const
  Movements : array [1..9] of TMovementProc = (Movement1, Movement2, Movement3,
    Movement4, Movement5, Movement6, Movement7, Movement8, Movement9);


{ Delphi 5 compatibility }

{$IFNDEF DELPHI6}
function InRange(const AValue, AMin, AMax: Double): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;
{$ENDIF}

procedure FastRemap(Src, Dst: TBitmap32; VectorMap: TVectorMap);
var
  I, J, W, H: Integer;
  DstPtr: PColor32;
  MapPtr: PFixedPoint;
begin
  W := Src.Width - 1;
  H := Src.Height - 1;
  DstPtr:= @Dst.PixelPtr[0, 0]^;
  MapPtr:= @VectorMap.Vectors[0];
  with Src do
    for J:= 0 to H do
      for I:= 0 to W do
      begin
        DstPtr^:= PixelXS[MapPtr.X + I shl 16 - FixedMouseX,
          MapPtr.Y + J shl 16 - FixedMouseY];
        Inc(DstPtr);
        Inc(MapPtr);
      end;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  B: Boolean;
begin
  for B := False to True do Buffers[B] := TBitmap32.Create;

  VectorMap := TVectorMap.Create;
  CurrentBuffer := True;
  CurrentMap := True;

  Application.OnIdle:= TransformFrame;
  DoubleBuffered := True; // avoid flicker

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := clWhite;

  Hue := Random;
  Sat := Random;
  Lns := Random;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  B: Boolean;
begin
  ToggleTimers(False);
  IsClosing := True;
  for B := False to True do Buffers[B].Free;
  VectorMap.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
const
  Line1 = 'Graphics32';
  Line2 = '----visualization example----';

var
  w, h, I: Integer;
  B: Boolean;
begin
  if IsClosing then Exit;

  ToggleTimers(False);

  w := ClientWidth;
  h := ClientHeight;

  NX := w - 1;
  NY := h - 1;
  CX := NX div 2;
  CY := NY div 2;
  FixedMouseX := 0;
  FixedMouseY := 0;
  
  for B := False to True do with Buffers[B] do
  begin
    SetSize(w, h);
    Font.Size := 30;
    RenderText(CX - TextWidth(Line1) div 2, CY - 120, Line1, 4, $DEADBEEF);
    Font.Size := 20;
    RenderText(CX - TextWidth(Line2) div 2, CY + 60, Line2, 4, $DEADF00D);
  end;
  VectorMap.SetSize(w, h);

  PathRadius := Min(Width, Height) / 2;
  PathRadius := Max(1, PathRadius - PathRadius / 2);
  for I := 0 to High(LastPoint) do
  begin
    LastPoint[I][0].X := CX;
    LastPoint[I][0].Y := CY;
    LastPoint[I][1].X := CX;
    LastPoint[I][1].Y := CY;
  end;

  ToggleTimers(True);
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  FixedMouseX := Fixed((X - Width div 2) / 10);
  FixedMouseY := Fixed((Y - Height div 2) / 10);
  Buffers[CurrentBuffer].FrameRectS(X - 10, Y - 10, X + 10, Y + 10,
    HSLtoRGB(Hue + X / Clientwidth, Y / ClientHeight, 0.5));
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  C: Char;
  I: Integer;
begin
{$IFNDEF FPC}
  C := Lowercase(Char(Key))[1];
{$ELSE}
  C := Lowercase(Char(Key));
{$ENDIF}
  if TryStrToInt(C, I) and InRange(I, 1, 9) then
  begin
    if ssShift in Shift then
      Feedback := I / 10
    else
    begin
      MovementIndex := I;
      RenderMovement;
    end;
  end
  else
  case C of
    'f': vShowFPS := not vShowFPS;
    'h': vShowHelp := not vShowHelp;
    'r': MovementTimer.Enabled := not MovementTimer.Enabled;
  end;
end;

procedure TMainForm.RenderHelpScreen;
const
  Line1 = 'HELP';
  Line2 = '1..9: Chooses different movements';
  Line3 = 'F: Toggles FPS counter';
  Line4 = 'H: Toggles this help';
  Line5 = 'R: Toggles random movement change';

var
  T, L, R, B, H, W: Integer;
begin
  with Buffers[CurrentBuffer] do
  begin
    R := 280;
    B := 100;

    L := CX - R div 2;
    T := CY - B div 2;
    R := CX + R div 2;
    B := CY + B div 2;

    H := 5;
    W := 5;
    Font.Size := 10;
    Font.Name := 'Courier New';
    FillRectTS(L, T, R, B, $FF9F9F9F);
    FrameRectTS(L, T, R, B, $FFFFFFFF);

    RenderText(L + W, T + H, Line1, 3, clBlack32);
    Inc(H, TextHeight(Line1));
    RenderText(L + W, T + H, Line2, 3, clBlack32);
    Inc(H, TextHeight(Line2));
    RenderText(L + W, T + H, Line3, 3, clBlack32);
    Inc(H, TextHeight(Line3));
    RenderText(L + W, T + H, Line4, 3, clBlack32);
    Inc(H, TextHeight(Line4));
    RenderText(L + W, T + H, Line5, 3, clBlack32);
  end;
end;

procedure TMainForm.BufferFeedBack(Feedback, BlendContrast: Byte;
  BlendBrightness, BufferBrightness: Integer);
var
  I, L: Integer;
  PSrc, PDst: PColor32;
  C, S, D: TColor32;
begin
  PSrc := Buffers[not CurrentBuffer].PixelPtr[0,0];
  PDst := Buffers[CurrentBuffer].PixelPtr[0,0];
  C := FeedBack shl 24;
  for I := 0 to Buffers[CurrentBuffer].Width * Buffers[CurrentBuffer].Height - 1 do
  begin
    D := PDst^;
    S := ColorAverage(PSrc^, D);
    L := S and $FF + S shr 8 and $FF + S shr 16 and $FF;
    L := L - 382;
    L := SAR_9(L * BlendContrast) + BlendBrightness;
    S := Lighten(S, L) and $00FFFFFF;
    D := BlendReg(C + S, D);
    PDst^ := Lighten(D, BufferBrightness);
    Inc(PSrc);
    Inc(PDst);
  end;
  EMMS;
end;

procedure TMainForm.TransformFrame(Sender: TObject; var Done: Boolean);
begin
  if FPS <> 0 then
    FPS_Adaption := MovementSpeed / FPS;
  
  if vShowHelp then
    RenderHelpScreen
  else
    FastRemap(Buffers[CurrentBuffer], Buffers[not CurrentBuffer], VectorMap);

  CurrentBuffer := not CurrentBuffer; //Swap Buffer Index
  Buffers[CurrentBuffer].DrawTo(MainForm.Canvas.Handle, 0,0);

  if vShowFPS then
  begin
    Canvas.TextOut(3, 3, IntToStr(FPS) + ' FPS');
    Inc(FPSMeasure);
  end;

  EMMS;
  if Random > Feedback then
    BufferFeedBack(BlendLevel, BlendContrast, BlendBrightness, TimeDarkening);

  Done := False;
end;

procedure RenderSpot(Dst: TBitmap32; Size, X,Y: Integer; Color: TColor32; Additive: Boolean);
// Use a crude, but fast approach to spot rendering
var
  I, J, L: Integer;
  S: Single;
  C1, C2: TColor32;
  Table: array of TFixed;
begin
  X := X - Size shr 1;
  Y := Y - Size shr 1;

  S := 1 / Max(1, Size - 1);
  SetLength(Table, Size);
  for I := 0 to Size - 1 do
    Table[I] := Fixed(255 * (1 - (1 + Cos((I * S) * PI * 2)) * 0.5));

  for J := 0 to Size - 1 do
    for I := 0 to Size - 1 do
    begin
      L := FixedMul(Table[I], Table[J]) and $FF000000;

      C1 := Dst.PixelS[X + I, Y + J];
      if Additive then
        C2 := ColorAdd(Color, C1) and $00FFFFFF + Cardinal(L)
      else
        C2 := Color and $00FFFFFF + Cardinal(L);
      Dst.PixelS[X + I, Y + J] := BlendReg(C2, C1);
    end;
  EMMS;
end;

procedure TMainForm.RenderMovement;
const
  Jit = 1/8; //Jitter level: adds random displacement
var
  I, J: Integer;
  x,y, rx, ry, nrx, nry, sx, sy,
  r, d, CosV, SinV: Single;

begin
  with VectorMap do
  begin

  rx := Width - 1;
  ry := Height - 1;
  nrx := 2 / rx;
  nry := 2 / ry;

  for J := 0 to Height - 1 do
    for I := 0 to Width - 1 do
    begin
      // normalize dimensions
      x := I * nrx - 1;
      y := J * nry - 1;

      // back up x & y
      sx := x;
      sy := y;

      // Switch to radial space
      d := GR32_Math.Hypot(x, y);
      r := ArcTan2(y, x);

      // Callback to the current movement, this is what actually creates the displacement
      Movements[MovementIndex](rx, ry, x, y, d, r);

      // Switch to rectangular space, and add potential x,y difference
      GR32_Math.SinCos(r, SinV, CosV);
      x := d * CosV + (x - sx);
      y := d * SinV + (y - sy);

      // Scale result for transformationmap, we make displacement relative by subtracting I,J
      x := (x + 1) * rx * 0.5 - I;
      y := (y + 1) * ry * 0.5 - J;

      // Write values to transformationmap
      FixedVector[I, J] := FixedPoint(x * FPS_Adaption + (random - 0.5) * Jit, y * FPS_Adaption
        + (random - 0.5) * Jit);
    end;
  end;
  CurrentMap := not CurrentMap;
end;

procedure TMainForm.MovementTimerTimer(Sender: TObject);
// Here we change the perceptual look of the visualization
// by setting up most parameters from a few random numbers
// The actual code here, is the result of a few minutes of
// experimentation - go ahead and fiddle with the setup :)
var
  Activity: Single;
begin
  if CurrentMap = False then
  begin
    Render_Worms := Random;
    Render_PathSpots := (1 - Render_Worms) * ARandomNumber;
    Render_RandomSpots := 1 - Render_Worms;
    Render_Noise := Random;

    Activity := (Render_Worms + Render_PathSpots + Render_RandomSpots
      + Render_Noise)/4;

    BlendLevel := Round(10 + (100 * Activity) + 100 * ARandomNumber);
    BlendContrast := Round(32 * Activity + 5 * (ARandomNumber - 0.5));
    BlendBrightness := Round(Activity * 10 + 5 * (ARandomNumber - 0.5));
    TimeDarkening := - Round(7 * Activity);
    FeedBack := Activity;
  end;
  MovementIndex := Constrain(Random(10), 1, 9);
  RenderMovement;
  vShowHelp := False;
end;

procedure TMainForm.RenderTimerTimer(Sender: TObject);
const
  PI2 = 2 * PI;
  MaxCount = 50;
var
  x,y :Single;
  Count, Size: Integer;
  CosV, SinV, PathCX, PathCY: Single;
  OX, OY, I: Integer;
  C: TColor32;
  L1: TPoint;
  Additive: Boolean;
begin
  with Buffers[CurrentBuffer] do
  begin

    if Random < Render_Noise then
    begin
      C := HSLtoRGB(Hue + 0.1, Sat, Lns);
      for I := 0 to 10 do
      begin
        OX := CX + Random(200) - 100;
        OY := CY + Random(200) - 100;
        PixelS[OX, OY] := C;
      end;
    end;

    if Random < Render_RandomSpots then
    begin
      Count := Random(MaxCount);
      Size := MaxCount - Count;
      Additive := Boolean(Random(2));
      for I:= 0 to Count do
      begin
        GR32_Math.SinCos(Random * PI * 2, SinV, CosV);
        x := CX + Size * 5 * SinV;
        y := CY + Size * 5 * CosV;
        RenderSpot(Buffers[CurrentBuffer], 1 + Size + Random(5), Round(x),
          Round(y), HSLtoRGB(ARandomNumber + Random * 0.1, ARandomNumber, 0.5),
            Additive);
      end;
    end;

    if Random < Render_PathSpots then
    begin
      C := HSLtoRGB(PathAngle/PI2, 1 - Sat, 1 - Lns);

      GR32_Math.SinCos(PathAngle, SinV, CosV);
      PathCX := CX + PathRadius * SinV;
      PathCY := CY + PathRadius * CosV;
      RenderSpot(Buffers[CurrentBuffer], Round(1 + PathRadius/5),
        Round(PathCX), Round(PathCY), C, True);

      GR32_Math.SinCos(PathAngle - PI, SinV, CosV);
      PathCX := CX + PathRadius * SinV;
      PathCY := CY + PathRadius * CosV;
      RenderSpot(Buffers[CurrentBuffer], Round(1 + PathRadius/5),
        Round(PathCX), Round(PathCY), C, True);

      PathAngle := PathAngle - PathStep;
      if not InRange(PathAngle, 0, PI2) then
      begin
        Constrain(PathAngle, 0, PI2);
        PathStep := - PathStep;
      end;

      PathRadius := PathRadius - PathRadiusStep;
      if not InRange(Abs(PathRadius), 1, Min(CX, CY) * Random) then
      begin
        Constrain(PathRadius, 1, Min(CX, CY));
        PathRadiusStep := - PathRadiusStep;
      end;

    end;

    if Random < Render_Worms then
    begin
      for I := 0 to High(LastPoint) do
      begin
        L1 := LastPoint[I][1];
        with LastPoint[I][0] do
        begin
          OX := Constrain(X - L1.X, -2, 2) + Random(3) - 1 + X;
          OY := Constrain(Y - L1.Y, -2, 2) + Random(3) - 1 + Y;
          OX := Constrain(OX, CX - 60, CX + 80);
          OY := Constrain(OY, CY - 40, CY + 60);
          LineS(X, Y, OX, OY, HSLtoRGB(Hue - 0.2 * random, Sat, 0));
          LineS(X, Y+1, OX, OY+1, HSLtoRGB(Hue, Sat, 0.4));
          LineS(X, Y-1, OX, OY-1, HSLtoRGB(Hue, Sat, 0.6));
          LineS(X+1, Y, OX + 1, OY, HSLtoRGB(Hue + 0.2 * random, Sat, 1));
        end;
        LastPoint[I][1] := LastPoint[I][0];
        LastPoint[I][0].X := OX;
        LastPoint[I][0].Y := OY;
      end;
    end;
  end;
end;

procedure TMainForm.ColorTimerTimer(Sender: TObject);
begin
  Hue := Hue + (Random - 0.15) * HueIncreaser;
  if (Hue > 1) or (Hue < 0) then
  begin
    Hue := Constrain(Hue, 0, 1);
    HueIncreaser := - HueIncreaser;
  end;
  Sat := Sat + (Random - 0.15) * SatIncreaser;
  if (Sat > 0.75) or (Sat < 0.25) then
  begin
    Sat := Constrain(Sat, 0.25, 0.75);
    SatIncreaser := - SatIncreaser;
  end;
  Lns := Lns + (Random - 0.15) * LnsIncreaser;
  if (Lns > 1) or (Lns < 0) then
  begin
    Lns := Constrain(Lns, 0, 1);
    LnsIncreaser := - LnsIncreaser;
  end;
  ARandomNumber := Random;
end;

procedure TMainForm.FPSTimerTimer(Sender: TObject);
begin
  FPS := FPSMeasure;
  FPSMeasure := 0;
end;

procedure TMainForm.ToggleTimers(Enabled: Boolean);
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TTimer then
      TTimer(Components[I]).Enabled := Enabled;
end;

end.
