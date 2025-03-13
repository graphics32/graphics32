unit GR32.Paint.Brush;

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
 * The Original Code is Paint tools for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander, anders@melander.dk
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2025
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$INCLUDE GR32.inc}

uses
  GR32;

//------------------------------------------------------------------------------
//
//      TCustomPaintBrush
//
//------------------------------------------------------------------------------
type
  TCustomPaintBrush = class
  private
  protected
    function GetHeight: integer; virtual; abstract;
    function GetWidth: integer; virtual; abstract;
  public
    constructor Create(Width, Height: integer); virtual;
    procedure BeginBrush(Bitmap: TBitmap32); virtual;
    procedure EndBrush; virtual;
    procedure Draw(Bitmap: TBitmap32; x, y: Single); virtual; abstract;
    procedure DrawPreview(Bitmap: TBitmap32); virtual; abstract;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
  end;


//------------------------------------------------------------------------------
//
//      TBitmapPaintBrush
//
//------------------------------------------------------------------------------
type
  TBitmapPaintBrush = class(TCustomPaintBrush)
  private
    FBitmap: TBitmap32;
    FBlendFunc: TPixelCombineEvent;
  protected
    function GetHeight: integer; override;
    function GetWidth: integer; override;
  public
    constructor Create(Width, Height: integer); override;
    destructor Destroy; override;

    procedure Draw(Bitmap: TBitmap32; x, y: Single); override;
    procedure DrawPreview(Bitmap: TBitmap32); override;

    property Bitmap: TBitmap32 read FBitmap;
    property BlendFunc: TPixelCombineEvent read FBlendFunc write FBlendFunc;
  end;


//------------------------------------------------------------------------------
//
//      TSmudgePaintBrush
//
//------------------------------------------------------------------------------
type
  TSmudgePaintBrush = class(TBitmapPaintBrush)
  private
    FFirst: boolean;
    FFinger: TBitmap32;
    FPaper: TBitmap32;
    FPressure: integer;
  protected
    procedure SmudgeBlend(Finger, Paper, Mask: TBitmap32; Pressure: integer);
  public
    constructor Create(Width, Height: integer); override;
    destructor Destroy; override;

    procedure Draw(Bitmap: TBitmap32; x, y: Single); override;

    property Pressure: integer read FPressure write FPressure; // 0..100
  end;


//------------------------------------------------------------------------------
//
//      Brush lines
//
//------------------------------------------------------------------------------
type
  TPaintBrushLineState = record
  private
    LastX, LastY: Single;
    Offset: Single;
  public
    Valid: boolean;
  end;

  TPaintBrushLine = class
  private
    FLastX, FLastY: Single;
    FBitmap: TBitmap32;
    FBrush: TCustomPaintBrush;
    FStep: Single;
    FOffset: Single;
  public
    constructor Create(Bitmap: TBitmap32; Brush: TCustomPaintBrush; Step: Single);
    procedure MoveTo(X, Y: Single; Reset: boolean = True);
    procedure LineTo(X, Y: Single; Reset: boolean = False);

    procedure SaveState(var State: TPaintBrushLineState);
    procedure RestoreState(const State: TPaintBrushLineState);
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$if defined(UseInlining)}
  Types,
{$ifend}
  Math,
  GR32_Resamplers,
  GR32_Math,
  GR32_Blend;



//------------------------------------------------------------------------------
//
//      TCustomPaintBrush
//
//------------------------------------------------------------------------------
constructor TCustomPaintBrush.Create(Width, Height: integer);
begin
end;

procedure TCustomPaintBrush.BeginBrush(Bitmap: TBitmap32);
begin
end;

procedure TCustomPaintBrush.EndBrush;
begin
end;


//------------------------------------------------------------------------------
//
//      TBitmapPaintBrush
//
//------------------------------------------------------------------------------
constructor TBitmapPaintBrush.Create(Width, Height: integer);
begin
  inherited Create(Width, Height);

  FBitmap := TBitmap32.Create;
  FBitmap.SetSize(Width, Height);
  FBitmap.DrawMode := dmBlend;
  FBitmap.CombineMode := cmMerge;
end;

//------------------------------------------------------------------------------

destructor TBitmapPaintBrush.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TBitmapPaintBrush.Draw(Bitmap: TBitmap32; x, y: Single);
var
  DestX, DestY: integer;
begin
  DestX := Ceil(x - FBitmap.Width / 2);
  DestY := Ceil(y - FBitmap.Height / 2);

  if (not Bitmap.MeasuringMode) then
  begin
    if (Assigned(FBlendFunc)) then
      BlockTransfer(Bitmap, DestX, DestY, Bitmap.ClipRect, FBitmap, FBitmap.BoundsRect, dmCustom, FBlendFunc)
    else
      FBitmap.DrawTo(Bitmap, DestX, DestY);
  end;

  Bitmap.Changed(MakeRect(DestX, DestY, DestX+FBitmap.Width, DestY+FBitmap.Height));
end;

//------------------------------------------------------------------------------

procedure TBitmapPaintBrush.DrawPreview(Bitmap: TBitmap32);
begin
  FBitmap.DrawTo(Bitmap);
end;

//------------------------------------------------------------------------------

function TBitmapPaintBrush.GetHeight: integer;
begin
  Result := FBitmap.Height;
end;

//------------------------------------------------------------------------------

function TBitmapPaintBrush.GetWidth: integer;
begin
  Result := FBitmap.Width;
end;


//------------------------------------------------------------------------------
//
//      TPaintBrushLine
//
//------------------------------------------------------------------------------
constructor TPaintBrushLine.Create(Bitmap: TBitmap32; Brush: TCustomPaintBrush; Step: Single);
begin
  FBitmap := Bitmap;
  FBrush := Brush;
  FStep := Step;
end;

//------------------------------------------------------------------------------

procedure TPaintBrushLine.LineTo(X, Y: Single; Reset: boolean);
var
  dX, dY: Single;
  LineLength: Single;
  StartX, StartY: Single;
  NextX, NextY: Single;
  i: integer;
  Steps: integer;
begin
  dX := X-FLastX;
  dY := Y-FLastY;

  if (dY = 0) and (dX = 0) then
    exit;

  LineLength := GR32_Math.Hypot(dX, dY);

  if (LineLength = 0) then
    exit;

  // Extend start of line to use remainder of old line length
  if (not Reset) then
  begin
    FLastX := FLastX-FOffset*dX/LineLength;
    FLastY := FLastY-FOffset*dY/LineLength;
  end;

  // Calculate step vector
  dX := FStep*dX/LineLength;
  dY := FStep*dY/LineLength;

  if (not Reset) then
    LineLength := LineLength+FOffset;

  StartX := FLastX;
  StartY := FLastY;

  i := 1; // Assume first point was set in MoveTo()
  Steps := Trunc(LineLength/FStep);

  // Calculate new offset
  FOffset := LineLength-Steps*FStep;

  while (Steps > 0) do
  begin
    NextX := StartX+i*dX;
    NextY := StartY+i*dY;

    FBrush.Draw(FBitmap, NextX, NextY);
//    FBrush.DrawTo(FBitmap, Ceil(NextX-FBrush.Width/2), Ceil(NextY-FBrush.Height/2));

    inc(i);
    dec(Steps);
  end;

  FLastX := X;
  FLastY := Y;
end;

//------------------------------------------------------------------------------

procedure TPaintBrushLine.MoveTo(X, Y: Single; Reset: boolean);
begin
  FLastX := X;
  FLastY := Y;

  if (Reset) then
  begin
    FBrush.Draw(FBitmap, X, Y);
//    FBrush.DrawTo(FBitmap, Ceil(X-FBrush.Width/2), Ceil(Y-FBrush.Height/2));
    FOffset := 0;
  end;
end;

procedure TPaintBrushLine.RestoreState(const State: TPaintBrushLineState);
begin
  FLastX := State.LastX;
  FLastY := State.LastY;
  FOffset := State.Offset;
end;

procedure TPaintBrushLine.SaveState(var State: TPaintBrushLineState);
begin
  State.LastX := FLastX;
  State.LastY := FLastY;
  State.Offset := FOffset;
  State.Valid := True;
end;


//------------------------------------------------------------------------------
//
//      TSmudgePaintBrush
//
//------------------------------------------------------------------------------
constructor TSmudgePaintBrush.Create(Width, Height: integer);
begin
  inherited Create(Width, Height);
  FFinger := TBitmap32.Create;
  FPaper := TBitmap32.Create;
  FPaper.DrawMode := dmOpaque;
  FFirst := True;
end;

//------------------------------------------------------------------------------

destructor TSmudgePaintBrush.Destroy;
begin
  FFinger.Free;
  FPaper.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TSmudgePaintBrush.SmudgeBlend(Finger, Paper, Mask: TBitmap32; Pressure: integer);

  procedure Blend(Finger: TColor32; var Paper: TColor32; Mask, Pressure: Byte);
  begin
    Pressure := MulDiv(Mask, Pressure, 255);

    if (Pressure = 0) then
      exit;

    if (Pressure = 255) then
    begin
      Paper := Finger;
      exit;
    end;

    if (Finger and $FF000000 = 0) then
      // This causes transparent to stick
      Paper := MergeRegEx(Paper, 0, 255-Pressure) // Modulate alpha - stupid but easy
    else
    if (Paper and $FF000000 = 0) then
      // This causes blend onto transparent to maintain color
      Paper := MergeRegEx(Finger, 0, Pressure) // Modulate alpha - stupid but easy
    else
      CombineMem(Finger, Paper, Pressure);
  end;

var
  pFinger, pPaper, pMask: PColor32;
  Count: integer;
begin
  ASSERT(Finger.Width = Paper.Width);
  ASSERT(Finger.Width = Mask.Width);
  ASSERT(Finger.Height = Paper.Height);
  ASSERT(Finger.Height = Mask.Height);

  pFinger := PColor32(Finger.Bits);
  pPaper := PColor32(Paper.Bits);
  pMask := PColor32(Mask.Bits);

  Count := Finger.Width*Finger.Height;

  while (Count > 0) do
  begin
    Blend(pFinger^, pPaper^, pMask^ shr 24, Pressure);
    pFinger^ := pPaper^;

    inc(pFinger);
    inc(pPaper);
    inc(pMask);
    dec(Count);
  end;
end;

//------------------------------------------------------------------------------

procedure TSmudgePaintBrush.Draw(Bitmap: TBitmap32; x, y: Single);
var
  r: TRect;
begin
  r := Self.Bitmap.BoundsRect;
  GR32.OffsetRect(r, Ceil(x - Width/2), Ceil(y - Height/2));

(*
  // Test: Blur brush
  if (FFirst) then
  begin
    FFinger.SetSize(Width, Height);
    FPaper.SetSize(Width, Height);
    FFirst := False;
  end;
  BlockTransfer(FFinger, 0, 0, FFinger.BoundsRect, Bitmap, r, dmOpaque);
  BoxBlur3(FFinger, 2);
*)

  if (not Bitmap.MeasuringMode) then
  begin

    if (not FFirst) then
    begin
      // Get ink from paper
      BlockTransfer(FPaper, 0, 0, FPaper.BoundsRect, Bitmap, r, dmOpaque);

      // Blend with ink on finger
      SmudgeBlend(FFinger, FPaper, Self.Bitmap, MulDiv(Pressure, 255, 100));

      // Copy back to paper
      FPaper.DrawTo(Bitmap, r);
    end;

    // If first time - then create ink bitmap
    if (FFirst) then
    begin
      FFinger.SetSize(Width, Height);
      FPaper.SetSize(Width, Height);
      FFirst := False;

      // Get ink from paper
      BlockTransfer(FFinger, 0, 0, FFinger.BoundsRect, Bitmap, r, dmOpaque);
    end;

  end;

  Bitmap.Changed(r);
end;

//------------------------------------------------------------------------------

end.
