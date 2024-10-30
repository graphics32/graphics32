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
 * The Original Code is Particle Swarm example for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 *
 * Portions created by the Initial Developer are Copyright (C) 2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{-$define FADE_BLEND}
{$define PARTICLE_AA}

uses
  Messages,
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types,
  GR32,
  GR32_System,
  GR32_Image,
  GR32.Noise.Simplex;

//------------------------------------------------------------------------------
//
//      Particle Swarm example
//
//------------------------------------------------------------------------------
// Control the motion of a swarm of particles using 3D Simplex Noise, the third
// dimension being time.
//------------------------------------------------------------------------------
//
// Based on ideas by:
//
// - Mattias Fagerlund
//   https://lotsacode.wordpress.com/2013/04/14/colored-perlin-particle-field/
//
// - Jake Weary
//   https://codepen.io/thepheer/pen/VwqqQG
//
// - Sadik Mussah
//   https://gist.github.com/smussah/118ff6b385feac2bde349dd21053e75d
//
//------------------------------------------------------------------------------
const
  ParamParticlesCount = 5000;           // Initial number of particles

  ParamFade: Byte = 240;                // Fade-to-black factor: [0..255], 255 = no fade.
  ParamColorAlpha: Byte = 192;          // Alpha of paint color
  ParamColorHueShift = 0.002;           // Hue shift per frame
  ParamColorSaturation = 0.75;
  ParamColorLightness = 0.5;

  ParamParticleSpaceFactor = 0.003;     // How much does the current position affect the simplex noise
  ParamParticleTimeFactor = 0.001;     // How much does the current time affect the simplex noise
  ParamParticleVectorFactor = 0.25;     // Amount of randomness in vector
  ParamParticleSpeedFactor = 0.95;      // Velocity decay; <=1, 1=none

type
  TParticleControl = (pcNone, pcAttract, pcRepulse, pcSlowmo);

  TParticle = record
  const
    MinLifetime = 1000;
    MaxLifetime = 10000;
  private
    FPosition: TFloatPoint;
    FTrail: TFloatPoint;
    FVelocity: TFloatPoint;
    FNoise: TSimplexNoise;
    FIteration: integer;
    FLifetime: integer;
  public
    procedure Initialize(ANoise: TSimplexNoise);

    procedure Reset(const ABounds: TRect);
    procedure Step(const ABounds: TRect; ParticleControl: TParticleControl; const ControlPoint: TPoint);
    procedure Render(Buffer: TBitmap32);
  end;

const
  MSG_AFTERSHOW = WM_USER;

type
  TFormMain = class(TForm)
    PaintBox: TPaintBox32;
    TimerFrameRate: TTimer;
    procedure TimerFrameRateTimer(Sender: TObject);
    procedure PaintBoxResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FOptionAnimateColors: boolean;
    FOptionFade: boolean;
    FOptionRenderNoise: boolean;
    FHue: Single;

    FParticles: TArray<TParticle>;
    FNoise: TSimplexNoise;
    FFrameCount: integer;
    FFrameRateStopwatch: TStopwatch;

    FBenchmark: boolean;
    FIteration: integer;

    FFormHelp: TForm;

    procedure SetParticleCount(Value: integer);
    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure Render;
    procedure MsgAfterShow(var Msg: TMessage); message MSG_AFTERSHOW;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Windows,
  Math,
  GR32_Blend,
  GR32_LowLevel,
{$ifdef FPC}
  GR32_Geometry,
{$endif FPC}
  HelpUnit;

{ TFormMain }

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;

  FNoise := TSimplexNoise.Create;

  FOptionAnimateColors := True;
  FOptionFade := True;
  FFrameRateStopwatch := TStopwatch.StartNew;

  FBenchmark := FindCmdLineSwitch('benchmark');

  Application.OnIdle := AppEventsIdle;

  FFormHelp := TFormHelp.Create(Self);
end;

destructor TFormMain.Destroy;
begin
  FNoise.Free;

  inherited;
end;

procedure TFormMain.AppEventsIdle(Sender: TObject; var Done: Boolean);
begin
  Render;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;

    VK_F1:
      FFormHelp.Visible := not FFormHelp.Visible;

    VK_ADD:
      SetParticleCount(Length(FParticles) + 500);

    VK_SUBTRACT:
      if (Length(FParticles) > 500) then
        SetParticleCount(Length(FParticles) - 500);

    Ord('C'):
      FOptionAnimateColors := not FOptionAnimateColors;

    Ord('F'):
      FOptionFade := not FOptionFade;

    Ord('N'):
      FOptionRenderNoise := not FOptionRenderNoise;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  PostMessage(Handle, MSG_AFTERSHOW, 0, 0);
end;

procedure TFormMain.MsgAfterShow(var Msg: TMessage);
var
  r: TRect;
begin
  r := ClientToScreen(ClientRect);

  FFormHelp.Top := r.Top;
  FFormHelp.left := r.Right - FFormHelp.Width;

  ShowWindow(FFormHelp.Handle, SW_SHOWNA);
  FFormHelp.Visible := True;

  SetParticleCount(ParamParticlesCount);
end;

procedure TFormMain.PaintBoxResize(Sender: TObject);
begin
  // Clear to some color other than black so we avoid the ghosts caused
  // by fade using blend never reaching complete black.
{$ifdef FADE_BLEND}
  PaintBox.Buffer.Clear(HSLtoRGB(0.0, 0.75, 0.5, 255));
{$endif}
end;

procedure TFormMain.Render;
var
  i: integer;
  x, y: integer;
  z: Single;
  Pixel: PColor32;
  ParticleControl: TParticleControl;
  MousePos: TPoint;
begin
  Inc(FFrameCount);
  Inc(FIteration);

  PaintBox.Buffer.BeginUpdate;
  try

    if (FOptionRenderNoise) then
    begin

      // Display 3D Simplex Noise as color Hue where time is the third dimension
      z := GetTickCount * ParamParticleTimeFactor;
      Pixel := PColor32(PaintBox.Buffer.Bits); // We could have used PaintBox.Buffer.Pixel[x, y] here
                                               // but the loop is slow enough without it...
      for y := 0 to PaintBox.Buffer.Height-1 do
        for x := 0 to PaintBox.Buffer.Width-1 do
        begin
          Pixel^ := HSLtoRGB(
            (FNoise.Noise(X*ParamParticleSpaceFactor, Y*ParamParticleSpaceFactor, z) + 0.5) / 2,
            ParamColorSaturation,
            ParamColorLightness,
            255);
          Inc(Pixel);
        end;

      PaintBox.Buffer.Changed;

    end else
    begin

      // Fade to black
      // Ideally we would fade by adjusting the L channel of a HSL color but that is far too expensive
      if (FOptionFade) then
{$ifdef FADE_BLEND}
        // We fade out the existing image by blending black onto it. The alpha controls how fast we fade.
        BlendMems($09000000, PColor32(PaintBox.Buffer.Bits), PaintBox.Buffer.Width*PaintBox.Buffer.Height);
{$else}
        // Fade out by scaling the RGB: Faded = Colors * Weight / 255
        ScaleMems(PColor32(PaintBox.Buffer.Bits), PaintBox.Buffer.Width*PaintBox.Buffer.Height, ParamFade);
{$endif}

      // Color cycle
      PaintBox.Buffer.PenColor := HSLtoRGB(FHue, ParamColorSaturation, ParamColorLightness, ParamColorAlpha);
      if (FOptionAnimateColors) then
      begin
        FHue := FHue + ParamColorHueShift;
        FHue := FHue - Floor(FHue);
      end;


      // Live user interaction
      MousePos := PaintBox.ScreenToClient(Mouse.CursorPos);
      if (GetAsyncKeyState(VK_LBUTTON) and $8000 <> 0) then
        ParticleControl := pcAttract
      else
      if (GetAsyncKeyState(VK_RBUTTON) and $8000 <> 0) then
        ParticleControl := pcRepulse
      else
      if (GetAsyncKeyState(VK_MBUTTON) and $8000 <> 0) then
        ParticleControl := pcSlowmo
      else
        ParticleControl := pcNone;

      // Move and render particles
      for i := 0 to High(FParticles) do
      begin
        FParticles[i].Step(PaintBox.Buffer.BoundsRect, ParticleControl, MousePos);
        FParticles[i].Render(PaintBox.Buffer);
      end;

      if (FOptionFade) then
        PaintBox.ForceFullInvalidate;

    end;

  finally
    PaintBox.Buffer.EndUpdate;
  end;

  if (FBenchmark) and (FIteration > 10000) then
    Application.Terminate;
end;

procedure TFormMain.SetParticleCount(Value: integer);
var
  Count: integer;
begin
  Count := Length(FParticles);
  SetLength(FParticles, Value);

  while (Count < Value) do
  begin
    FParticles[Count].Initialize(FNoise);
    FParticles[Count].Reset(PaintBox.Buffer.BoundsRect);
    Inc(Count);
  end;

  PaintBox.Invalidate;
end;

procedure TFormMain.TimerFrameRateTimer(Sender: TObject);
var
  FPS: Single;
begin
  FFrameRateStopwatch.Stop;

  TTimer(Sender).Enabled := False;

  if (FFrameRateStopwatch.ElapsedMilliseconds <> 0) then
    FPS := 1000 * FFrameCount / FFrameRateStopwatch.ElapsedMilliseconds
  else
    FPS := 0;

  Caption := Format('%.0n particles @ %.0n fps', [Length(FParticles)*1.0, FPS]);

  FFrameCount := 0;
  TTimer(Sender).Enabled := True;

  FFrameRateStopwatch := TStopwatch.StartNew;
end;

{ TParticle }

procedure TParticle.Initialize(ANoise: TSimplexNoise);
begin
  FNoise := ANoise;
end;

procedure TParticle.Render(Buffer: TBitmap32);
begin
{$if defined(PARTICLE_AA)}
  Buffer.MoveToF(FTrail.X, FTrail.Y);
  Buffer.LineToFS(FPosition.X, FPosition.Y);
{$else}
  Buffer.MoveTo(Round(FTrail.X), Round(FTrail.Y));
  Buffer.LineToS(Round(FPosition.X), Round(FPosition.Y));
{$ifend}
end;

procedure TParticle.Reset(const ABounds: TRect);
begin
  FPosition.X := Random(ABounds.Width);
  FPosition.Y := Random(ABounds.Height);
  FTrail := FPosition;

  FVelocity.X := 1;
  FVelocity.Y := 1;

  FIteration := 0;
  FLifetime := MinLifetime + Random(MaxLifetime-MinLifetime);
end;

procedure TParticle.Step(const ABounds: TRect; ParticleControl: TParticleControl; const ControlPoint: TPoint);
var
  x, y, z: Single;
  Angle: Single;
  Factor: Single;
  Wrapped: boolean;
  MouseDistance: Single;
begin
  Inc(FIteration);
  if (FIteration > FLifetime) then
    Reset(ABounds); // "Respawn"

  x := FPosition.X * ParamParticleSpaceFactor;
  y := FPosition.Y * ParamParticleSpaceFactor;
  z := GetTickCount * ParamParticleTimeFactor;

  Angle := Random * 2 * PI;
  Factor := Random * ParamParticleVectorFactor;


  // Calculate the new velocity based on the noise; Random velocity in a random direction
  FVelocity.X := FVelocity.X + Factor * Sin(Angle) + FNoise.Noise(x, y, -z);
  FVelocity.Y := FVelocity.Y + Factor * Cos(Angle) + FNoise.Noise(x, y,  z);


  // Alter the vector according to user interaction
  case ParticleControl of
    pcAttract:
      // Add a difference between mouse pos and particle pos (a fraction of it) to the velocity.
      begin
        FVelocity.X := FVelocity.X + (ControlPoint.X - FPosition.X) * 0.00085;
        FVelocity.Y := FVelocity.Y + (ControlPoint.Y - FPosition.Y) * 0.00085;
      end;

    pcRepulse:
      // Repulse the particles if the right mouse button is down and the distance between
      // the mouse and particle is below an arbitrary value between 200 and 250.
      begin
{$ifndef FPC}
        MouseDistance := FPosition.Distance(ControlPoint);
{$else FPC}
        MouseDistance := Distance(FPosition, FloatPoint(Mouse.CursorPos));
{$endif FPC}
        if (MouseDistance < 200+Random(50)) then
        begin
          FVelocity.X := FVelocity.X + (FPosition.X - ControlPoint.X) * 0.02;
          FVelocity.Y := FVelocity.Y + (FPosition.Y - ControlPoint.Y) * 0.02;
        end;
      end;

    pcSlowmo:
      // Time dilation field, stuff moves slower here, depending on distance
      begin
{$ifndef FPC}
        MouseDistance := FPosition.Distance(ControlPoint);
{$else FPC}
        MouseDistance := Distance(FPosition, FloatPoint(Mouse.CursorPos));
{$endif FPC}
        Factor := MouseDistance / (200 + Random * 50);

        if (Factor < 1) then
        begin
          FVelocity.X := FVelocity.X * Factor;
          FVelocity.Y := FVelocity.Y * Factor;
        end;
      end;
  end;

  // Update position
  FTrail := FPosition;
  FPosition.X := FPosition.X + FVelocity.X;
  FPosition.Y := FPosition.Y + FVelocity.Y;


  // Slow down the velocity slightly
  FVelocity.X := FVelocity.X * ParamParticleSpeedFactor;
  FVelocity.Y := FVelocity.Y * ParamParticleSpeedFactor;


  // Wrap around the edges
  Wrapped := False;
  if (FPosition.X < ABounds.Left) then
  begin
    FPosition.X := ABounds.Right;
    Wrapped := True;
  end else
  if (FPosition.X > ABounds.Right) then
  begin
    FPosition.X := ABounds.Left;
    Wrapped := True;
  end;
  if (FPosition.Y < ABounds.Top) then
  begin
    FPosition.Y := ABounds.Bottom;
    Wrapped := True;
  end else
  if (FPosition.Y > ABounds.Bottom) then
  begin
    FPosition.Y := ABounds.Top;
    Wrapped := True;
  end;
  if (Wrapped) then
    FTrail := FPosition;
end;

end.
