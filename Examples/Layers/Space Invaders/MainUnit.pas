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
 * The Original Code is Invaders32 Example
 *
 * Contributor(s):
 * Graphics32 Contributors
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LCLType, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Generics.Collections,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Math,
  GR32, GR32_Image, GR32_Layers, GR32_Resamplers;

const
  GAME_WIDTH = 224;
  GAME_HEIGHT = 256;
  INVADER_ROWS = 5;
  INVADER_COLS = 11;
  MAX_ENEMY_MISSILES = 3;
  BUNKER_COUNT = 4;
  FONT_WIDTH = 3;
  FONT_HEIGHT = 5;
  FONT_SPACE = 1;

type
  TGameState = (gsTitle, gsPlaying, gsPlayerHit, gsGameOver, gsVictory);

  TInvaderType = (itSquid, itCrab, itOctopus);

  TInvaderInfo = record
    Active: Boolean;
    InvType: TInvaderType;
    Row, Col: Integer;
    Layer: TIndirectBitmapLayer;
  end;

  TMainForm = class(TForm)
    Image32: TImage32;
    TimerGame: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TimerGameTimer(Sender: TObject);
  private
    GameState: TGameState;
    MasterAlpha: integer;
    Score: Integer;
    HighScore: Integer;
    Lives: Integer;
    StateTimer: Integer;

    // Control flags
    KeyLeft: Boolean;
    KeyRight: Boolean;
    KeyFire: Boolean;

    // Bitmaps for procedural retro sprites
    BitmapAssets: TObjectList<TBitmap32>;
    BitmapPlayer: TBitmap32;
    BitmapPlayerHit: TBitmap32;
    BitmapSquid: array[0..1] of TBitmap32;
    BitmapCrab: array[0..1] of TBitmap32;
    BitmapOctopus: array[0..1] of TBitmap32;
    BitmapUFO: TBitmap32;
    BitmapExplosion: TBitmap32;
    BitmapPlayerBullet: TBitmap32;
    BitmapSquiglyShot: array[0..3] of TBitmap32;

    // Game Entities
    PlayerLayer: TIndirectBitmapLayer;
    PlayerX, PlayerY: Single;

    Invaders: array[0..INVADER_ROWS - 1, 0..INVADER_COLS - 1] of TInvaderInfo;
    InvaderDir: Integer; // +1 right, -1 left
    InvaderStepTimer: Integer;
    InvaderStepInterval: Integer;
    InvaderFrame: Integer;

    PlayerBulletActive: Boolean;
    PlayerBulletX, PlayerBulletY: Single;
    PlayerBulletLayer: TIndirectBitmapLayer;

    EnemyMissilesActive: array[0..MAX_ENEMY_MISSILES - 1] of Boolean;
    EnemyMissilesX, EnemyMissilesY: array[0..MAX_ENEMY_MISSILES - 1] of Single;
    EnemyMissileLayers: array[0..MAX_ENEMY_MISSILES - 1] of TIndirectBitmapLayer;
    EnemyMissileTimer: Integer;

    BunkerLayers: array[0..BUNKER_COUNT - 1] of TBitmapLayer;

    UfoActive: Boolean;
    UfoX, UfoY: Single;
    UfoDir: Integer;
    UfoLayer: TIndirectBitmapLayer;
    UfoTimer: Integer;

    // Sprite management
    function AllocateSpriteBitmap: TBitmap32;
    procedure ResetSpriteBitmaps;
    procedure FadeSpriteBitmaps;
    procedure GenerateSpriteBitmaps;
    procedure CreateSpriteBitmapFromPattern(Bitmap: TBitmap32; const Pattern: array of string; Color: TColor32);

    // Animation helpers
    procedure MoveLayer(Layer: TCustomIndirectBitmapLayer; X, Y: TFloat);

    // Game flow methods
    procedure StartNewGame;
    procedure ResetRound;
    procedure UpdatePlaying;
    procedure MoveInvaders;
    procedure FirePlayerBullet;
    procedure UpdateBullets;
    procedure SpawnEnemyMissile;
    procedure UpdateEnemyMissiles;
    procedure UpdateUfo;
    procedure CheckCollisions;
    procedure DamageBunker(BunkerIdx: Integer; LocalX, LocalY: Integer);
    procedure DrawHUD;
    procedure DrawCenteredRetroText(const Text: string; Y: Integer; Color: TColor32);
    procedure DrawRetroText(const Text: string; X, Y: Integer; Color: TColor32);
    procedure DrawRetroChar(Ch: Char; X, Y: Integer; Color: TColor32);
    function AliveInvaderCount: Integer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

// Pixel art partially based on the original Space Invaders ROM data:
// https://www.computerarcheology.com/Arcade/SpaceInvaders/Code.html

uses
  Types;

{ Retro Font Patterns (3x5 pixels per char) }

type
  TStringArray = array of string;

function GetCharPattern(Ch: Char): TStringArray;
begin
  case UpCase(Ch) of
    '0': Result := ['XXX', 'X X', 'X X', 'X X', 'XXX'];
    '1': Result := ['XX ', ' X ', ' X ', ' X ', 'XXX'];
    '2': Result := ['XXX', '  X', 'XXX', 'X  ', 'XXX'];
    '3': Result := ['XXX', '  X', 'XXX', '  X', 'XXX'];
    '4': Result := ['X X', 'X X', 'XXX', '  X', '  X'];
    '5': Result := ['XXX', 'X  ', 'XXX', '  X', 'XXX'];
    '6': Result := ['XXX', 'X  ', 'XXX', 'X X', 'XXX'];
    '7': Result := ['XXX', '  X', '  X', '  X', '  X'];
    '8': Result := ['XXX', 'X X', 'XXX', 'X X', 'XXX'];
    '9': Result := ['XXX', 'X X', 'XXX', '  X', 'XXX'];
    'A': Result := [' X ', 'X X', 'XXX', 'X X', 'X X'];
    'B': Result := ['XX ', 'X X', 'XX ', 'X X', 'XX '];
    'C': Result := ['XXX', 'X  ', 'X  ', 'X  ', 'XXX'];
    'D': Result := ['XX ', 'X X', 'X X', 'X X', 'XX '];
    'E': Result := ['XXX', 'X  ', 'XX ', 'X  ', 'XXX'];
    'F': Result := ['XXX', 'X  ', 'XX ', 'X  ', 'X  '];
    'G': Result := ['XXX', 'X  ', 'X X', 'X X', 'XXX'];
    'H': Result := ['X X', 'X X', 'XXX', 'X X', 'X X'];
    'I': Result := ['XXX', ' X ', ' X ', ' X ', 'XXX'];
    'J': Result := ['  X', '  X', '  X', 'X X', 'XXX'];
    'K': Result := ['X X', 'XX ', 'X  ', 'XX ', 'X X'];
    'L': Result := ['X  ', 'X  ', 'X  ', 'X  ', 'XXX'];
    'M': Result := ['X X', 'XXX', 'XXX', 'X X', 'X X'];
    'N': Result := ['XX ', 'X X', 'X X', 'X X', 'X X'];
    'O': Result := ['XXX', 'X X', 'X X', 'X X', 'XXX'];
    'P': Result := ['XXX', 'X X', 'XXX', 'X  ', 'X  '];
    'Q': Result := ['XXX', 'X X', 'XXX', ' XX', '  X'];
    'R': Result := ['XXX', 'X X', 'XX ', 'X X', 'X X'];
    'S': Result := ['XXX', 'X  ', 'XXX', '  X', 'XXX'];
    'T': Result := ['XXX', ' X ', ' X ', ' X ', ' X '];
    'U': Result := ['X X', 'X X', 'X X', 'X X', 'XXX'];
    'V': Result := ['X X', 'X X', 'X X', 'X X', ' X '];
    'W': Result := ['X X', 'X X', 'XXX', 'XXX', 'X X'];
    'X': Result := ['X X', 'X X', ' X ', 'X X', 'X X'];
    'Y': Result := ['X X', 'X X', 'XXX', '  X', 'XXX'];
    'Z': Result := ['XXX', '  X', ' X ', 'X  ', 'XXX'];
    '-': Result := ['   ', '   ', 'XXX', '   ', '   '];
    '<': Result := ['  X', ' X ', 'X  ', ' X ', '  X'];
    '>': Result := ['X  ', ' X ', '  X', ' X ', 'X  '];
    '=': Result := ['   ', 'XXX', '   ', 'XXX', '   '];
    '!': Result := [' X ', ' X ', ' X ', '   ', ' X '];
  else
    Result := ['   ', '   ', '   ', '   ', '   '];
  end;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i, j: Integer;
begin
  ClientWidth := 672;
  ClientHeight := 768;
  KeyPreview := True;

  // Initialize low-res background bitmap on Image32
  Image32.Bitmap.SetSize(GAME_WIDTH, GAME_HEIGHT, False);
  Image32.Bitmap.Clear(clBlack32);
  Image32.BitmapAlign := baCenter;
  Image32.ScaleMode := smResize;
  Image32.Bitmap.ResamplerClassName := 'TNearestResampler';

  BitmapAssets := TObjectList<TBitmap32>.Create;

  GenerateSpriteBitmaps;

  // Create Player Layer
  PlayerLayer := TIndirectBitmapLayer.Create(Image32.Layers, BitmapPlayer);
  PlayerLayer.Bitmap := BitmapPlayer;
  PlayerLayer.Scaled := True;
  PlayerLayer.Visible := False;

  // Create Invader Layers
  for i := 0 to High(Invaders) do
    for j := 0 to High(Invaders[i]) do
    begin
      Invaders[i, j].Layer := TIndirectBitmapLayer.Create(Image32.Layers);
      Invaders[i, j].Layer.Scaled := True;
      Invaders[i, j].Layer.Visible := False;
    end;

  // Create Player Bullet Layer
  PlayerBulletLayer := TIndirectBitmapLayer.Create(Image32.Layers, BitmapPlayerBullet);
  PlayerBulletLayer.Scaled := True;
  PlayerBulletLayer.Visible := False;

  // Create Enemy Missile Layers
  for i := 0 to High(EnemyMissileLayers) do
  begin
    EnemyMissileLayers[i] := TIndirectBitmapLayer.Create(Image32.Layers, BitmapSquiglyShot[0]);
    EnemyMissileLayers[i].Scaled := True;
    EnemyMissileLayers[i].Visible := False;
  end;

  // Create UFO Layer
  UfoLayer := TIndirectBitmapLayer.Create(Image32.Layers, BitmapUFO);
  UfoLayer.Scaled := True;
  UfoLayer.Visible := False;

  // Create Bunker Layers
  for i := 0 to High(BunkerLayers) do
  begin
    BunkerLayers[i] := TBitmapLayer.Create(Image32.Layers);
    BunkerLayers[i].Bitmap.SetSize(20, 16, False);
    BunkerLayers[i].Bitmap.DrawMode := dmBlend;
    BunkerLayers[i].Scaled := True;
    BunkerLayers[i].Visible := False;
  end;

  HighScore := 0;
  GameState := gsTitle;
  DrawHUD;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Image32.Layers.Count-1 do
    if (Image32.Layers[i] is TIndirectBitmapLayer) then
      TIndirectBitmapLayer(Image32.Layers[i]).Bitmap := nil;

  BitmapAssets.Free;
end;

procedure TMainForm.CreateSpriteBitmapFromPattern(Bitmap: TBitmap32;
  const Pattern: array of string; Color: TColor32);
var
  w, h, x, y: Integer;
begin
  h := Length(Pattern);
  if h = 0 then
    Exit;
  w := Length(Pattern[0]);

  Bitmap.SetSize(w, h);
  Bitmap.DrawMode := dmBlend;

  for y := 0 to h - 1 do
    for x := 0 to w - 1 do
      if (x < Length(Pattern[y])) and (Pattern[y][x + 1] <> ' ') then
        Bitmap.Pixel[x, y] := Color;
end;

procedure TMainForm.GenerateSpriteBitmaps;
begin
  BitmapPlayer := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapPlayer, [
    '      X      ',
    '     XXX     ',
    '     XXX     ',
    ' XXXXXXXXXXX ',
    'XXXXXXXXXXXXX',
    'XXXXXXXXXXXXX',
    'XXXXXXXXXXXXX',
    'XXXXXXXXXXXXX'
  ], clLime32);

  BitmapPlayerHit := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapPlayerHit, [
    '     X       ',
    '          X  ',
    '     X X X   ',
    '  X  X       ',
    '      XX XX  ',
    'X   X XX X X ',
    ' XXXXXXXXX  X',
    'XXXXXXXXXXX X'
  ], clLime32);

  // Squid (Top row, 30 points)
  BitmapSquid[0] := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapSquid[0], [
    '   XX   ',
    '  XXXX  ',
    ' XXXXXX ',
    'XX XX XX',
    'XXXXXXXX',
    '  X  X  ',
    ' X XX X ',
    'X X  X X'
  ], clFuchsia32);

  BitmapSquid[1] := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapSquid[1], [
    '   XX   ',
    '  XXXX  ',
    ' XXXXXX ',
    'XX XX XX',
    'XXXXXXXX',
    ' X XX X ',
    'X      X',
    ' X    X '
  ], clFuchsia32);

  // Crab (Middle rows, 20 points)
  BitmapCrab[0] := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapCrab[0], [
    '  X     X  ',
    '   X   X   ',
    '  XXXXXXX  ',
    ' XX XXX XX ',
    'XXXXXXXXXXX',
    'X XXXXXXX X',
    'X X     X X',
    '   XX XX   '
  ], clAqua32);

  BitmapCrab[1] := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapCrab[1], [
    '  X     X  ',
    'X  X   X  X',
    'X XXXXXXX X',
    'XXX XXX XXX',
    'XXXXXXXXXXX',
    '  XXXXXXX  ',
    '   X   X   ',
    '  X     X  '
  ], clAqua32);

  // Octopus (Bottom rows, 10 points)
  BitmapOctopus[0] := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapOctopus[0], [
    '    XXXX    ',
    ' XXXXXXXXXX ',
    'XXXXXXXXXXXX',
    'XXX  XX  XXX',
    'XXXXXXXXXXXX',
    '   XX  XX   ',
    '  XX XX XX  ',
    'XX        XX'
  ], clYellow32);

  BitmapOctopus[1] := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapOctopus[1], [
    '    XXXX    ',
    ' XXXXXXXXXX ',
    'XXXXXXXXXXXX',
    'XXX  XX  XXX',
    'XXXXXXXXXXXX',
    '  XXX  XXX  ',
    ' XX  XX  XX ',
    '  XX    XX  '
  ], clYellow32);

  // Mystery UFO
  BitmapUFO := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapUFO, [
    '     XXXXXX     ',
    '   XXXXXXXXXX   ',
    '  XXXXXXXXXXXX  ',
    ' XX  XX  XX  XX ',
    'XXXXXXXXXXXXXXXX',
    '  XXX  XX  XXX  ',
    '   X        X   '
  ], clRed32); // Red

  // Explosion
  BitmapExplosion := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapExplosion, [
    '  X   X  ',
    ' X  X  X ',
    '  XXXXX  ',
    'XX X X XX',
    '  XXXXX  ',
    ' X  X  X ',
    '  X   X  '
  ], clWhite32);

  // Player Bullet
  BitmapPlayerBullet := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapPlayerBullet, [
    'X',
    'X',
    'X',
    'X'
  ], clWhite32);

  // Enemy Missile
  BitmapSquiglyShot[0] := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapSquiglyShot[0], [
    ' X ',
    'X  ',
    ' X ',
    '  X',
    ' X ',
    'X  ',
    ' X '
  ], clRed32);

  BitmapSquiglyShot[1] := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapSquiglyShot[1], [
    'X  ',
    ' X ',
    '  X',
    ' X ',
    'X  ',
    ' X ',
    '  X'
  ], clRed32);

  BitmapSquiglyShot[2] := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapSquiglyShot[2], [
    ' X ',
    '  X',
    ' X ',
    'X  ',
    ' X ',
    '  X',
    ' X '
  ], clRed32);

  BitmapSquiglyShot[3] := AllocateSpriteBitmap;
  CreateSpriteBitmapFromPattern(BitmapSquiglyShot[3], [
    '  X',
    ' X ',
    'X  ',
    ' X ',
    '  X',
    ' X ',
    'X  '
  ], clRed32);
end;

procedure TMainForm.StartNewGame;
var
  i: Integer;
  BunkerBmp: TBitmap32;
  BX, BY: Single;
begin
  Score := 0;
  Lives := 3;

  Image32.BeginUpdate;
  try

    // Initialize Bunker layers
    for i := 0 to High(BunkerLayers) do
    begin
      BunkerBmp := BunkerLayers[i].Bitmap;
      CreateSpriteBitmapFromPattern(BunkerBmp, [
        '    XXXXXXXXXXXX    ',
        '  XXXXXXXXXXXXXXXX  ',
        ' XXXXXXXXXXXXXXXXXX ',
        'XXXXXXXXXXXXXXXXXXXX',
        'XXXXXXXXXXXXXXXXXXXX',
        'XXXXXXXXXXXXXXXXXXXX',
        'XXXXXXXXXXXXXXXXXXXX',
        'XXXXXXXXXXXXXXXXXXXX',
        'XXXXXXXXXXXXXXXXXXXX',
        'XXXXXXXXXXXXXXXXXXXX',
        'XXXXXXXXXXXXXXXXXXXX',
        'XXXXXX        XXXXXX',
        'XXXXX          XXXXX',
        'XXXX            XXXX',
        'XXXX            XXXX',
        'XXXX            XXXX'
      ], clLime32);

      BX := 20 + i * 48;
      BY := 192;
      MoveLayer(BunkerLayers[i], BX, BY);
      BunkerLayers[i].Visible := True;
    end;

    ResetRound;
    ResetSpriteBitmaps;
    Image32.Bitmap.Clear(clBlack32);

  finally
    Image32.EndUpdate;
  end;

  GameState := gsPlaying;
end;

procedure TMainForm.ResetSpriteBitmaps;
var
  i: Integer;
begin
  for i := 0 to BitmapAssets.Count-1 do
    BitmapAssets[i].MasterAlpha := 255;
  MasterAlpha := 255;
end;

procedure TMainForm.ResetRound;
var
  i, j: Integer;
  IX, IY: Single;
begin
  PlayerX := 105;
  PlayerY := 224;
  PlayerLayer.Bitmap := BitmapPlayer;
  MoveLayer(PlayerLayer, PlayerX, PlayerY);
  PlayerLayer.Visible := True;

  InvaderDir := 1;
  InvaderStepTimer := 0;
  InvaderStepInterval := 30;
  InvaderFrame := 0;

  // Populate Invader Grid
  for i := 0 to High(Invaders) do
    for j := 0 to High(Invaders[i]) do
    begin
      Invaders[i, j].Active := True;

      case i of
        0:
          begin
            Invaders[i, j].InvType := itSquid;
            Invaders[i, j].Layer.Bitmap := BitmapSquid[0];
          end;

        1, 2:
          begin
            Invaders[i, j].InvType := itCrab;
            Invaders[i, j].Layer.Bitmap := BitmapCrab[0];
          end;
      else
        Invaders[i, j].InvType := itOctopus;
        Invaders[i, j].Layer.Bitmap := BitmapOctopus[0];
      end;

      IX := 16 + j * 16;
      IY := 36 + i * 14;
      MoveLayer(Invaders[i, j].Layer, IX, IY);
      Invaders[i, j].Layer.Visible := True;
    end;

  PlayerBulletActive := False;
  PlayerBulletLayer.Visible := False;

  for i := 0 to High(EnemyMissilesActive) do
  begin
    EnemyMissilesActive[i] := False;
    EnemyMissileLayers[i].Visible := False;
  end;

  UfoActive := False;
  UfoLayer.Visible := False;
  UfoTimer := 0;
end;

function TMainForm.AliveInvaderCount: Integer;
var
  i, j, Count: Integer;
begin
  Count := 0;
  for i := 0 to High(Invaders) do
    for j := 0 to High(Invaders[i]) do
      if Invaders[i, j].Active then
        Inc(Count);
  Result := Count;
end;

procedure TMainForm.MoveInvaders;
var
  i, j, AliveCount: Integer;
  R: TFloatRect;
  HitEdge: Boolean;
  ShiftY: Single;
begin
  HitEdge := False;
  ShiftY := 0;

  // Check if any active invader hits the canvas bounds
  for i := 0 to High(Invaders) do
  begin
    for j := 0 to High(Invaders[i]) do
    begin
      if not Invaders[i, j].Active then
        continue;

      R := Invaders[i, j].Layer.Location;

      if ((InvaderDir > 0) and (R.Right >= Image32.Bitmap.Width - 8)) or
         ((InvaderDir < 0) and (R.Left <= 8)) then
      begin
        HitEdge := True;
        break;
      end;
    end;

    if HitEdge then
      break;
  end;

  if HitEdge then
  begin
    InvaderDir := -InvaderDir;
    ShiftY := 4;
  end;

  InvaderFrame := 1 - InvaderFrame;

  // Move all invaders
  for i := 0 to High(Invaders) do
    for j := 0 to High(Invaders[i]) do
    begin
      if not Invaders[i, j].Active then
        continue;

      R := Invaders[i, j].Layer.Location;

      if HitEdge then
        GR32.OffsetRect(R, 0, ShiftY);

      GR32.OffsetRect(R, InvaderDir * 2, 0);

      Invaders[i, j].Layer.Location := R;

      // Update animation frame
      case Invaders[i, j].InvType of
        itSquid: Invaders[i, j].Layer.Bitmap := BitmapSquid[InvaderFrame];
        itCrab: Invaders[i, j].Layer.Bitmap := BitmapCrab[InvaderFrame];
        itOctopus: Invaders[i, j].Layer.Bitmap := BitmapOctopus[InvaderFrame];
      end;

      // Check if invaders reached player line
      if R.Bottom >= PlayerY then
      begin
        GameState := gsGameOver;
        StateTimer := 0;
      end;
    end;

  // Update step interval based on remaining invaders count
  AliveCount := AliveInvaderCount;
  if AliveCount > 0 then
    InvaderStepInterval := Max(1, Round((AliveCount / (INVADER_ROWS * INVADER_COLS)) * 28))
  else
  begin
    GameState := gsVictory;
    StateTimer := 0;
  end;
end;

procedure TMainForm.MoveLayer(Layer: TCustomIndirectBitmapLayer; X, Y: TFloat);
begin
  Layer.Location := FloatRect(X, Y, X + Layer.Bitmap.Width, Y + Layer.Bitmap.Height);
end;

procedure TMainForm.FadeSpriteBitmaps;
var
  i: Integer;
begin
  // Fade-out all layers
  if (MasterAlpha <= 0) then
    exit;

  MasterAlpha := Max(0, MasterAlpha-8);

  for i := 0 to BitmapAssets.Count-1 do
    BitmapAssets[i].MasterAlpha := MasterAlpha;

  Image32.ForceFullInvalidate;
end;

procedure TMainForm.FirePlayerBullet;
begin
  if PlayerBulletActive then
    exit;

  PlayerBulletActive := True;
  PlayerBulletX := PlayerX + 6;
  PlayerBulletY := PlayerY - 4;
  MoveLayer(PlayerBulletLayer, PlayerBulletX, PlayerBulletY);
  PlayerBulletLayer.Visible := True;
end;

procedure TMainForm.UpdateBullets;
begin
  if not PlayerBulletActive then
    exit;

  PlayerBulletY := PlayerBulletY - 4.0;
  if PlayerBulletY < 20 then
  begin
    PlayerBulletActive := False;
    PlayerBulletLayer.Visible := False;
  end else
    MoveLayer(PlayerBulletLayer, PlayerBulletX, PlayerBulletY);
end;

procedure TMainForm.SpawnEnemyMissile;
var
  Col, Row, Slot: Integer;
  IX, IY: Single;
  Found: Boolean;
begin
  // Find available missile slot
  Found := False;
  for Slot := 0 to High(EnemyMissilesActive) do
    if not EnemyMissilesActive[Slot] then
    begin
      Found := True;
      Break;
    end;

  if (not Found) then
    Exit;

  // Choose a random column that has active invaders
  Col := Random(INVADER_COLS);
  Found := False;
  IX := 0;
  IY := 0;
  for Row := High(Invaders) downto 0 do
    if Invaders[Row, Col].Active then
    begin
      Found := True;
      IX := Invaders[Row, Col].Layer.Location.Left + (Invaders[Row, Col].Layer.Bitmap.Width / 2);
      IY := Invaders[Row, Col].Layer.Location.Bottom;
      Break;
    end;

  if not Found then
    exit;

  EnemyMissilesActive[Slot] := True;
  EnemyMissilesX[Slot] := IX;
  EnemyMissilesY[Slot] := IY;
  MoveLayer(EnemyMissileLayers[Slot], IX, IY);
  EnemyMissileLayers[Slot].Visible := True;
end;

procedure TMainForm.UpdateEnemyMissiles;
var
  i, FrameIdx: Integer;
begin
  for i := 0 to High(EnemyMissilesActive) do
  begin
    if not EnemyMissilesActive[i] then
      continue;

    EnemyMissilesY[i] := EnemyMissilesY[i] + 1.5;

    if EnemyMissilesY[i] > Image32.Bitmap.Height - 10 then
    begin
      EnemyMissilesActive[i] := False;
      EnemyMissileLayers[i].Visible := False;
    end else
    begin
      FrameIdx := (Trunc(EnemyMissilesY[i]) div 4) mod Length(BitmapSquiglyShot);
      EnemyMissileLayers[i].Bitmap := BitmapSquiglyShot[FrameIdx];
      MoveLayer(EnemyMissileLayers[i], EnemyMissilesX[i], EnemyMissilesY[i]);
    end;
  end;
end;

procedure TMainForm.UpdateUfo;
begin
  if not UfoActive then
    exit;

  UfoX := UfoX + (UfoDir * 1.0);
  if (UfoDir > 0) and (UfoX > Image32.Bitmap.Width + 20) then
    UfoActive := False
  else
  if (UfoDir < 0) and (UfoX < -20) then
    UfoActive := False
  else
    MoveLayer(UfoLayer, UfoX, UfoY);

  UfoLayer.Visible := UfoActive;
end;

procedure TMainForm.DamageBunker(BunkerIdx: Integer; LocalX, LocalY: Integer);
var
  x, y: Integer;
  Bmp: TBitmap32;
begin
  Bmp := BunkerLayers[BunkerIdx].Bitmap;
  for y := Max(0, LocalY - 2) to Min(Bmp.Height - 1, LocalY + 2) do
    for x := Max(0, LocalX - 2) to Min(Bmp.Width - 1, LocalX + 2) do
      Bmp.Pixel[x, y] := 0;
  Bmp.Changed;
end;

function TMainForm.AllocateSpriteBitmap: TBitmap32;
begin
  Result := TBitmap32.Create;
  BitmapAssets.Add(Result);
end;

procedure TMainForm.CheckCollisions;
var
  i, j, k, px, py: Integer;
  BulletRect, InvaderRect, PlayerRect, UfoRect, BunkerRect, MissileRect: TFloatRect;
  BX, BY_Top, BY_Bottom, MX, MY_Top, MY_Bottom: Integer;
  BunkerBmp: TBitmap32;
  Hit: Boolean;
begin
  BulletRect := PlayerBulletLayer.Location;
  PlayerRect := PlayerLayer.Location;

  // 1. Player Bullet vs Invaders
  if PlayerBulletActive then
  begin
    for i := 0 to High(Invaders) do
    begin
      for j := 0 to High(Invaders[i]) do
      begin
        if not Invaders[i, j].Active then
          continue;

        InvaderRect := Invaders[i, j].Layer.Location;

        if (BulletRect.Right >= InvaderRect.Left) and (BulletRect.Left <= InvaderRect.Right) and
           (BulletRect.Bottom >= InvaderRect.Top) and (BulletRect.Top <= InvaderRect.Bottom) then
        begin
          Invaders[i, j].Active := False;
          Invaders[i, j].Layer.Visible := False;
          PlayerBulletActive := False;
          PlayerBulletLayer.Visible := False;

          case Invaders[i, j].InvType of
            itSquid: Inc(Score, 30);
            itCrab: Inc(Score, 20);
            itOctopus: Inc(Score, 10);
          end;

          if Score > HighScore then
            HighScore := Score;

          Break;
        end;
      end;

      if not PlayerBulletActive then
        Break;
    end;
  end;

  // 2. Player Bullet vs UFO
  if PlayerBulletActive and UfoActive then
  begin
    UfoRect := UfoLayer.Location;

    if (BulletRect.Right >= UfoRect.Left) and (BulletRect.Left <= UfoRect.Right) and
       (BulletRect.Bottom >= UfoRect.Top) and (BulletRect.Top <= UfoRect.Bottom) then
    begin
      UfoActive := False;
      UfoLayer.Visible := False;
      PlayerBulletActive := False;
      PlayerBulletLayer.Visible := False;

      Inc(Score, 100);

      if Score > HighScore then
        HighScore := Score;
    end;
  end;

  // 3. Player Bullet vs Bunkers
  if PlayerBulletActive then
  begin
    for k := 0 to High(BunkerLayers) do
    begin
      BunkerRect := BunkerLayers[k].Location;

      if (BulletRect.Right >= BunkerRect.Left) and (BulletRect.Left <= BunkerRect.Right) and
         (BulletRect.Bottom >= BunkerRect.Top) and (BulletRect.Top <= BunkerRect.Bottom) then
      begin
        BunkerBmp := BunkerLayers[k].Bitmap;
        BX := Round(PlayerBulletX - BunkerRect.Left);
        BY_Top := Round(PlayerBulletY - BunkerRect.Top);
        BY_Bottom := Round((PlayerBulletY + 4) - BunkerRect.Top);

        Hit := False;
        // Bullet moving UP: test bottom to top
        for py := BY_Bottom downto BY_Top do
        begin
          if (py < 0) or (py >= BunkerBmp.Height) then
            continue;
          for px := Max(0, BX - 1) to Min(BunkerBmp.Width - 1, BX + 1) do
            if BunkerBmp.Pixel[px, py] <> 0 then
            begin
              DamageBunker(k, px, py);
              PlayerBulletActive := False;
              PlayerBulletLayer.Visible := False;
              Hit := True;
              Break;
            end;

          if Hit then
            Break;
        end;
        if Hit then
          Break;
      end;
    end;
  end;

  // 4. Enemy Missiles vs Player / Bunkers
  for i := 0 to High(EnemyMissilesActive) do
  begin
    if not EnemyMissilesActive[i] then
      continue;

    MissileRect := EnemyMissileLayers[i].Location;

    // Missile vs Player
    if (MissileRect.Right >= PlayerRect.Left) and (MissileRect.Left <= PlayerRect.Right) and
       (MissileRect.Bottom >= PlayerRect.Top) and (MissileRect.Top <= PlayerRect.Bottom) then
    begin
      EnemyMissilesActive[i] := False;
      EnemyMissileLayers[i].Visible := False;
      Dec(Lives);
      PlayerLayer.Bitmap := BitmapPlayerHit;
      GameState := gsPlayerHit;
      StateTimer := 0;
      Break;
    end;

    // Missile vs Bunkers
    for k := 0 to High(BunkerLayers) do
    begin
      BunkerRect := BunkerLayers[k].Location;

      if (MissileRect.Right >= BunkerRect.Left) and (MissileRect.Left <= BunkerRect.Right) and
         (MissileRect.Bottom >= BunkerRect.Top) and (MissileRect.Top <= BunkerRect.Bottom) then
      begin
        BunkerBmp := BunkerLayers[k].Bitmap;
        MX := Round(EnemyMissilesX[i] - BunkerRect.Left);
        MY_Top := Round(EnemyMissilesY[i] - BunkerRect.Top);
        MY_Bottom := Round((EnemyMissilesY[i] + 6) - BunkerRect.Top);

        Hit := False;
        // Missile moving DOWN: test top to bottom
        for py := MY_Top to MY_Bottom do
        begin
          if (py >= 0) and (py < BunkerBmp.Height) then
          begin
            for px := Max(0, MX) to Min(BunkerBmp.Width - 1, MX + 2) do
            begin
              if BunkerBmp.Pixel[px, py] <> 0 then
              begin
                DamageBunker(k, px, py);
                EnemyMissilesActive[i] := False;
                EnemyMissileLayers[i].Visible := False;
                Hit := True;
                Break;
              end;
            end;
          end;
          if Hit then
            Break;
        end;
        if Hit then
          Break;
      end;
    end;
  end;
end;

procedure TMainForm.UpdatePlaying;
begin
  Image32.BeginUpdate;
  try

    // Player movement
    if KeyLeft and (PlayerX > 8) then
      PlayerX := PlayerX - 1.5
    else
    if KeyRight and (PlayerX < Image32.Bitmap.Width - 21) then
      PlayerX := PlayerX + 1.5;

    MoveLayer(PlayerLayer, PlayerX, PlayerY);

    // Player shooting
    if KeyFire then
      FirePlayerBullet;

    // Invader movement timer
    Inc(InvaderStepTimer);
    if InvaderStepTimer >= InvaderStepInterval then
    begin
      InvaderStepTimer := 0;
      MoveInvaders;
    end;

    // Enemy missile spawner
    Inc(EnemyMissileTimer);
    if EnemyMissileTimer >= 40 then
    begin
      EnemyMissileTimer := 0;
      SpawnEnemyMissile;
    end;

    // UFO spawner
    Inc(UfoTimer);
    if (UfoTimer >= 600) and (not UfoActive) then
    begin
      UfoTimer := 0;
      UfoActive := True;
      if Random(2) = 0 then
      begin
        UfoDir := 1;
        UfoX := -16;
      end else
      begin
        UfoDir := -1;
        UfoX := Image32.Bitmap.Width;
      end;
      UfoY := 18;
      MoveLayer(UfoLayer, UfoX, UfoY);
    end;

    UpdateBullets;
    UpdateEnemyMissiles;
    UpdateUfo;
    CheckCollisions;

  finally
    Image32.EndUpdate;
  end;
end;

procedure TMainForm.TimerGameTimer(Sender: TObject);
begin
  Image32.BeginUpdate;
  try

    case GameState of
      gsTitle:
        begin
          if KeyFire then
            StartNewGame;
        end;

      gsPlaying:
        begin
          UpdatePlaying;
        end;

      gsPlayerHit:
        begin
          Inc(StateTimer);
          if StateTimer >= 40 then
          begin
            if Lives <= 0 then
            begin
              GameState := gsGameOver;
              StateTimer := 0;
            end else
            begin
              PlayerX := 105;
              PlayerY := 224;
              PlayerLayer.Bitmap := BitmapPlayer;
              MoveLayer(PlayerLayer, PlayerX, PlayerY);
              GameState := gsPlaying;
            end;
          end;
        end;

      gsGameOver, gsVictory:
        begin
          Inc(StateTimer);
          if KeyFire and (StateTimer > 30) then
            StartNewGame;
        end;
    end;

    DrawHUD;

  finally
    Image32.EndUpdate;
  end;
end;

procedure TMainForm.DrawRetroChar(Ch: Char; X, Y: Integer; Color: TColor32);
var
  Pat: TStringArray;
  r, c: Integer;
begin
  Pat := GetCharPattern(Ch);
  for r := 0 to High(Pat) do
    for c := 1 to Length(Pat[r]) do
    begin
      if Pat[r][c] <> ' ' then
        Image32.Bitmap.Pixel[X + c - 1, Y + r] := Color;
    end;
end;

procedure TMainForm.DrawCenteredRetroText(const Text: string; Y: Integer; Color: TColor32);
var
  X: integer;
begin
  X := (Image32.Bitmap.Width - Length(Text) * (FONT_WIDTH + FONT_SPACE) - FONT_SPACE) div 2;
  DrawRetroText(Text, X, Y, Color);
end;

procedure TMainForm.DrawRetroText(const Text: string; X, Y: Integer; Color: TColor32);
var
  i, CurrentX: Integer;
begin
  Image32.Bitmap.FillRect(X, Y, X + Length(Text) * (FONT_WIDTH + FONT_SPACE) - FONT_SPACE, Y + FONT_HEIGHT, clBlack32);

  CurrentX := X;
  for i := 1 to Length(Text) do
  begin
    DrawRetroChar(Text[i], CurrentX, Y, Color);
    Inc(CurrentX, FONT_WIDTH+FONT_SPACE);
  end;
end;

procedure TMainForm.DrawHUD;
var
  i: Integer;
begin
  // Top Bar HUD
  DrawRetroText('SCORE', 10, 4, clWhite32);
  DrawRetroText(Format('%.4d', [Score]), 10, 11, clLime32);

  DrawRetroText('HIGH', 150, 4, clWhite32);
  DrawRetroText(Format('%.4d', [HighScore]), 150, 11, clLime32);

  // Bottom Line & Lives
  for i := 0 to Image32.Bitmap.Width - 1 do
    Image32.Bitmap.HorzLine(0, Image32.Bitmap.Height-12, Image32.Bitmap.Width-1, clLime32);

  DrawRetroText(Format('LIVES %d', [Lives]), 10, Image32.Bitmap.Height-8, clWhite32);

  case GameState of
    gsTitle:
      begin
        DrawCenteredRetroText('INVADERS32', 90, clLime32);
        DrawCenteredRetroText('PRESS SPACE TO PLAY', 130, clWhite32);
        DrawCenteredRetroText('USE ARROWS TO MOVE', 150, clYellow32);
      end;

    gsGameOver:
      begin
        DrawCenteredRetroText('GAME OVER', 120, clRed32);
        DrawCenteredRetroText('PRESS SPACE', 140, clWhite32);
        FadeSpriteBitmaps;
      end;

    gsVictory:
      begin
        DrawCenteredRetroText('VICTORY!', 120, clLime32);
        DrawCenteredRetroText('PRESS SPACE', 140, clWhite32);
      end;
  end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT: KeyLeft := True;
    VK_RIGHT: KeyRight := True;
    VK_SPACE: KeyFire := True;
  end;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT: KeyLeft := False;
    VK_RIGHT: KeyRight := False;
    VK_SPACE: KeyFire := False;
    VK_ESCAPE: Close;
  end;
end;

end.
