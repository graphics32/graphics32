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

{$I GR32.inc}

uses
  Messages,
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types,
  GR32,
  GR32_Image,
  GR32.Noise.Simplex;

//------------------------------------------------------------------------------
//
//      Particle Swarm example
//
//------------------------------------------------------------------------------
// Control the motion of a swarm of particles using 3D Simplex Noise, the third
// dimentions being time.
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

type
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
    procedure Step(const ABounds: TRect);
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
    FLastTick: UInt64;

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
  FLastTick := GetTickCount;

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

  SetParticleCount(5000);
end;

procedure TFormMain.PaintBoxResize(Sender: TObject);
begin
  // Clear to some color other than black so we avoid the ghosts caused
  // by fade using blend never reaching complete black.
  PaintBox.Buffer.Clear(HSLtoRGB(0.0, 0.75, 0.5, 255));
end;

procedure TFormMain.Render;
var
  i: integer;
  x, y: integer;
  z: Single;
  Pixel: PColor32;
begin
  Inc(FFrameCount);

  PaintBox.Buffer.BeginUpdate;
  try

    if (FOptionRenderNoise) then
    begin

      // Display 3D Simplex Noise as color Hue where time is the third dimension
      z := GetTickCount * 0.0001;
      Pixel := PColor32(PaintBox.Buffer.Bits); // We could have used PaintBox.Buffer.Pixel[x, y] here
                                                   // but the loop is slow enough without it...
      for y := 0 to PaintBox.Buffer.Height-1 do
        for x := 0 to PaintBox.Buffer.Width-1 do
        begin
          Pixel^ := HSLtoRGB((FNoise.Noise(X*0.005, Y*0.005, z) + 0.5) / 2, 0.75, 0.5, 255);
          Inc(Pixel);
        end;

      PaintBox.Buffer.Changed;

    end else
    begin

      // Fade
      if (FOptionFade) then
        // We fade out the existing image by blending black onto it. The alpha controls how fast we fade.
        BlendMems($09000000, PColor32(PaintBox.Buffer.Bits), PaintBox.Buffer.Width*PaintBox.Buffer.Height);

      // Color cycle
      PaintBox.Buffer.PenColor := HSLtoRGB(FHue, 0.75, 0.5, 128);
      if (FOptionAnimateColors) then
      begin
        FHue := FHue + 0.002; // Constant controls speed of color change
        FHue := FHue - Floor(FHue);
      end;

      // Move and render particles
      for i := 0 to High(FParticles) do
      begin
        FParticles[i].Step(PaintBox.Buffer.BoundsRect);
        FParticles[i].Render(PaintBox.Buffer);
      end;

      if (FOptionFade) then
        PaintBox.ForceFullInvalidate;

    end;

  finally
    PaintBox.Buffer.EndUpdate;
  end;
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
  TimeElapsed: Cardinal;
  FPS: Single;
begin
  TTimer(Sender).Enabled := False;
  TimeElapsed := GetTickCount64 - FLastTick;

  if (TimeElapsed <> 0) then
    FPS := 1000 * FFrameCount / TimeElapsed
  else
    FPS := 0;

  Caption := Format('%.0n particles @ %.0n fps', [Length(FParticles)*1.0, FPS]);

  FFrameCount := 0;
  FLastTick := GetTickCount;
  TTimer(Sender).Enabled := True;
end;

{ TParticle }

procedure TParticle.Initialize(ANoise: TSimplexNoise);
begin
  FNoise := ANoise;
end;

procedure TParticle.Render(Buffer: TBitmap32);
begin
  Buffer.MoveToF(FTrail.X, FTrail.Y);
  Buffer.LineToFS(FPosition.X, FPosition.Y);
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

procedure TParticle.Step(const ABounds: TRect);
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

  x := FPosition.X * 0.005;
  y := FPosition.Y * 0.005;
  z := GetTickCount * 0.0001;

  Angle := Random * 2 * PI;
  Factor := Random * 0.25;


  // Calculate the new velocity based on the noise; Random velocity in a random direction
  FVelocity.X := FVelocity.X + (Factor * Sin(Angle) + FNoise.Noise(x, y, -z));
  FVelocity.Y := FVelocity.Y + (Factor * Cos(Angle) + FNoise.Noise(x, y,  z));


  // Live user interaction:
  if (GetAsyncKeyState(VK_LBUTTON) and $8000 <> 0) then
  begin
    // Add a difference between mouse pos and particle pos (a fraction of it) to the velocity.
    FVelocity.X := FVelocity.X + (Mouse.CursorPos.X - FPosition.X) * 0.00085;
    FVelocity.Y := FVelocity.Y + (Mouse.CursorPos.Y - FPosition.Y) * 0.00085;
  end else
  if (GetAsyncKeyState(VK_RBUTTON) and $8000 <> 0) then
  begin
    // Repulse the particles if the right mouse button is down and the distance between
    // the mouse and particle is below an arbitrary value between 200 and 250.
{$ifndef FPC}
    MouseDistance := FPosition.Distance(Mouse.CursorPos);
{$else FPC}
    MouseDistance := Distance(FPosition, FloatPoint(Mouse.CursorPos));
{$endif FPC}
    if (MouseDistance < 200+Random(50)) then
    begin
      FVelocity.X := FVelocity.X + (FPosition.X - Mouse.CursorPos.X) * 0.02;
      FVelocity.Y := FVelocity.Y + (FPosition.Y - Mouse.CursorPos.Y) * 0.02;
    end;
  end else
  if (GetAsyncKeyState(VK_MBUTTON) and $8000 <> 0) then
  begin
    // Time dilation field, stuff moves slower here, depending on distance
{$ifndef FPC}
    MouseDistance := FPosition.Distance(Mouse.CursorPos);
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


  // Update position
  FTrail := FPosition;
  FPosition.X := FPosition.X + FVelocity.X;
  FPosition.Y := FPosition.Y + FVelocity.Y;


  // Slow down the velocity slightly
  FVelocity.X := FVelocity.X * 0.95;
  FVelocity.Y := FVelocity.Y * 0.95;


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
