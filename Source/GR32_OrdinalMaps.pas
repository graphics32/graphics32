unit GR32_OrdinalMaps;

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
 * Mattias Andersson
 * (parts of this unit were merged from GR32_ByteMaps.pas by Alex A. Denisov)
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Michael Hansen
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  Controls, Graphics,
  {$IFDEF Windows}
    Windows,
  {$ENDIF}
{$ELSE}
  Windows, Controls, Graphics,
{$ENDIF}
  Classes, SysUtils, GR32;

type
  TConversionType = (ctRed, ctGreen, ctBlue, ctAlpha, ctUniformRGB,
    ctWeightedRGB);

{$IFDEF FPC}
  PInteger = ^Integer;
{$ENDIF}

  TBooleanMap = class(TCustomMap)
  private
    function GetValue(X, Y: Integer): Boolean;
    procedure SetValue(X, Y: Integer; const Value: Boolean);
  protected
    FBits: PByteArray;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;

    function Empty: Boolean; override;
    procedure Clear(FillValue: Byte);
    procedure ToggleBit(X, Y: Integer);

    property Value[X, Y: Integer]: Boolean read GetValue write SetValue; default;
    property Bits: PByteArray read FBits;
  end;

  TByteMap = class(TCustomMap)
  private
    function GetValue(X, Y: Integer): Byte; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    function GetValPtr(X, Y: Integer): PByte; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    procedure SetValue(X, Y: Integer; Value: Byte); {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    function GetScanline(Y: Integer): PByteArray;
  protected
    FBits: PByteArray;
    procedure AssignTo(Dst: TPersistent); override;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Empty: Boolean; override;
    procedure Clear(FillValue: Byte);

    procedure Multiply(Value: Byte);
    procedure Add(Value: Byte);
    procedure Sub(Value: Byte);

    procedure ReadFrom(Source: TCustomBitmap32; Conversion: TConversionType);
    procedure WriteTo(Dest: TCustomBitmap32; Conversion: TConversionType); overload;
    procedure WriteTo(Dest: TCustomBitmap32; const Palette: TPalette32); overload;

    procedure DrawTo(Dest: TCustomBitmap32; X, Y: Integer; Color: TColor32); overload;
    procedure DrawTo(Dest: TCustomBitmap32; Rect: TRect; Color: TColor32); overload;

    procedure Downsample(Factor: Byte); overload;
    procedure Downsample(Dest: TByteMap; Factor: Byte); overload;

    procedure FlipHorz(Dst: TByteMap = nil);
    procedure FlipVert(Dst: TByteMap = nil);
    procedure Rotate90(Dst: TByteMap = nil);
    procedure Rotate180(Dst: TByteMap = nil);
    procedure Rotate270(Dst: TByteMap = nil);

    property Bits: PByteArray read FBits;
    property Scanline[Y: Integer]: PByteArray read GetScanline;
    property ValPtr[X, Y: Integer]: PByte read GetValPtr;
    property Value[X, Y: Integer]: Byte read GetValue write SetValue; default;
  end;

  { TWordMap }

  TWordMap = class(TCustomMap)
  private
    function GetValPtr(X, Y: Integer): PWord; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    function GetValue(X, Y: Integer): Word; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    procedure SetValue(X, Y: Integer; const Value: Word); {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    function GetScanline(Y: Integer): PWordArray;
  protected
    FBits: PWordArray;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Empty: Boolean; override;
    procedure Clear(FillValue: Word);

    property ValPtr[X, Y: Integer]: PWord read GetValPtr;
    property Value[X, Y: Integer]: Word read GetValue write SetValue; default;
    property Bits: PWordArray read FBits;
    property Scanline[Y: Integer]: PWordArray read GetScanline;
  end;

  { TIntegerMap }

  TIntegerMap = class(TCustomMap)
  private
    function GetValPtr(X, Y: Integer): PInteger; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    function GetValue(X, Y: Integer): Integer; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    procedure SetValue(X, Y: Integer; const Value: Integer); {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    function GetScanline(Y: Integer): PIntegerArray;
  protected
    FBits: PIntegerArray;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Empty: Boolean; override;
    procedure Clear(FillValue: Integer = 0);

    property ValPtr[X, Y: Integer]: PInteger read GetValPtr;
    property Value[X, Y: Integer]: Integer read GetValue write SetValue; default;
    property Bits: PIntegerArray read FBits;
    property Scanline[Y: Integer]: PIntegerArray read GetScanline;
  end;

  { TCardinalMap }

  TCardinalMap = class(TCustomMap)
  private
    function GetValPtr(X, Y: Cardinal): PCardinal; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    function GetValue(X, Y: Cardinal): Cardinal; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    procedure SetValue(X, Y: Cardinal; const Value: Cardinal); {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    function GetScanline(Y: Integer): PCardinalArray;
  protected
    FBits: PCardinalArray;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Empty: Boolean; override;
    procedure Clear(FillValue: Cardinal = 0);

    property ValPtr[X, Y: Cardinal]: PCardinal read GetValPtr;
    property Value[X, Y: Cardinal]: Cardinal read GetValue write SetValue; default;
    property Bits: PCardinalArray read FBits;
    property Scanline[Y: Integer]: PCardinalArray read GetScanline;
  end;

  { TFloatMap }

  TFloatMap = class(TCustomMap)
  private
    function GetValPtr(X, Y: Integer): GR32.PFloat; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    function GetValue(X, Y: Integer): TFloat; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    procedure SetValue(X, Y: Integer; const Value: TFloat); {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    function GetScanline(Y: Integer): PFloatArray;
  protected
    FBits: PFloatArray;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Empty: Boolean; override;
    procedure Clear; overload;
    procedure Clear(FillValue: TFloat); overload;

    property ValPtr[X, Y: Integer]: PFloat read GetValPtr;
    property Value[X, Y: Integer]: TFloat read GetValue write SetValue; default;
    property Bits: PFloatArray read FBits;
    property Scanline[Y: Integer]: PFloatArray read GetScanline;
  end;

{$IFDEF COMPILER2010}

  { TGenericMap<T> }

  TGenericMap<T> = class(TCustomMap)
  private
    function GetValue(X, Y: Integer): T; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
    procedure SetValue(X, Y: Integer; const Value: T); {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
  protected
    FBits: Pointer;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Empty: Boolean; override;
    procedure Clear; overload;
    procedure Clear(FillValue: T); overload;

    property Value[X, Y: Integer]: T read GetValue write SetValue; default;
    property Bits: Pointer read FBits;
  end;

{$ENDIF}

implementation

uses
  Math, GR32_LowLevel, GR32_Blend, GR32_Resamplers;

function Bytes(Bits: Integer): Integer;
begin
  Result := (Bits - 1) shr 3 + 1;
end;

{ TBooleanMap }

constructor TBooleanMap.Create;
begin
  FreeMem(FBits);
  inherited Create;
end;

procedure TBooleanMap.ChangeSize(var Width, Height: Integer; NewWidth,
  NewHeight: Integer);
begin
  ReallocMem(FBits, Bytes(NewWidth * NewHeight));
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TBooleanMap.Clear(FillValue: Byte);
begin
  FillChar(FBits^, Bytes(Width * Height), FillValue);
end;

destructor TBooleanMap.Destroy;
begin
  FBits := nil;
  inherited;
end;

function TBooleanMap.Empty: Boolean;
begin
  Result := not Assigned(FBits);
end;

function TBooleanMap.GetValue(X, Y: Integer): Boolean;
begin
  X := X + Y * Width;
  Result := FBits^[X shr 3] and (1 shl (X and 7)) <> 0; //Boolean(FBits^[X shr 3] and (1 shl (X and 7)));
end;

procedure TBooleanMap.SetValue(X, Y: Integer; const Value: Boolean);
begin
  X := Y * Width + X;
  if Value then
    FBits^[X shr 3] := FBits^[X shr 3] or (1 shl (X and 7))
  else
    FBits^[X shr 3] := FBits^[X shr 3] and ((1 shl (X and 7)) xor $FF);
end;

procedure TBooleanMap.ToggleBit(X, Y: Integer);
begin
  X := Y * Width + X;
  FBits^[X shr 3] := FBits^[X shr 3] xor (1 shl (X and 7));
end;

{ TByteMap }

constructor TByteMap.Create;
begin
  FBits := nil;
  inherited Create;
end;

destructor TByteMap.Destroy;
begin
  FreeMem(FBits);
  inherited;
end;

procedure TByteMap.Downsample(Factor: Byte);
begin
  // downsample inplace
  case Factor of
    2:
      DownsampleByteMap2x(Self, Self);
    3:
      DownsampleByteMap3x(Self, Self);
    4:
      DownsampleByteMap4x(Self, Self);
    6:
      begin
        DownsampleByteMap3x(Self, Self);
        DownsampleByteMap2x(Self, Self);
      end;
    8:
      begin
        DownsampleByteMap4x(Self, Self);
        DownsampleByteMap2x(Self, Self);
      end;
    9:
      begin
        DownsampleByteMap3x(Self, Self);
        DownsampleByteMap3x(Self, Self);
      end;
    12:
      begin
        DownsampleByteMap4x(Self, Self);
        DownsampleByteMap3x(Self, Self);
      end;
    16:
      begin
        DownsampleByteMap4x(Self, Self);
        DownsampleByteMap4x(Self, Self);
      end;
    18:
      begin
        DownsampleByteMap3x(Self, Self);
        DownsampleByteMap3x(Self, Self);
        DownsampleByteMap2x(Self, Self);
      end;
    24:
      begin
        DownsampleByteMap4x(Self, Self);
        DownsampleByteMap3x(Self, Self);
        DownsampleByteMap2x(Self, Self);
      end;
    27:
      begin
        DownsampleByteMap3x(Self, Self);
        DownsampleByteMap3x(Self, Self);
        DownsampleByteMap3x(Self, Self);
      end;
    32:
      begin
        DownsampleByteMap4x(Self, Self);
        DownsampleByteMap4x(Self, Self);
        DownsampleByteMap2x(Self, Self);
      end;
  end;
end;

procedure TByteMap.Downsample(Dest: TByteMap; Factor: Byte);

  procedure DownsampleAndMove;
  var
    Temp: TByteMap;
    Y: Integer;
  begin
    // clone destination and downsample inplace
    Temp := TByteMap.Create;
    Temp.Assign(Self);
    Temp.Downsample(Factor);

    // copy downsampled result
    Dest.SetSize(Width div Factor, Height div Factor);
    for Y := 0 to Dest.Height - 1 do
      Move(Temp.Scanline[Y]^, Dest.Scanline[Y]^, Dest.Width);
  end;

begin
  // downsample directly
  if (Dest = Self) or not (Factor in [2, 3, 4]) then
  begin
    DownsampleAndMove;
    Exit;
  end;

  case Factor of
    2:
      begin
        Dest.SetSize(Width div 2, Height div 2);
        DownsampleByteMap2x(Self, Dest);
      end;
    3:
      begin
        // downsample directly
        Dest.SetSize(Width div 3, Height div 3);
        DownsampleByteMap3x(Self, Dest);
      end;
    4:
      begin
        // downsample directly
        Dest.SetSize(Width div 4, Height div 4);
        DownsampleByteMap4x(Self, Dest);
      end;
  end;
end;

procedure TByteMap.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TByteMap then
    begin
      inherited SetSize(TByteMap(Source).Width, TByteMap(Source).Height);
      Move(TByteMap(Source).Bits[0], Bits[0], Width * Height);
    end
    else if Source is TBitmap32 then
      ReadFrom(TBitmap32(Source), ctWeightedRGB)
    else
      inherited;
  finally
    EndUpdate;
    Changed;
  end;
end;

procedure TByteMap.AssignTo(Dst: TPersistent);
begin
  if Dst is TBitmap32 then WriteTo(TBitmap32(Dst), ctUniformRGB)
  else inherited;
end;

procedure TByteMap.ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer);
begin
  ReallocMem(FBits, NewWidth * NewHeight);
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TByteMap.Clear(FillValue: Byte);
begin
  FillChar(Bits^, Width * Height, FillValue);
  Changed;
end;

function TByteMap.Empty: Boolean;
begin
  Result := False;
  if (Width = 0) or (Height = 0) or (FBits = nil) then Result := True;
end;

procedure TByteMap.FlipHorz(Dst: TByteMap);
var
  i, j: Integer;
  P1, P2: PByte;
  tmp: Byte;
  W, W2: Integer;
begin
  W := Width;
  if (Dst = nil) or (Dst = Self) then
  begin
    { In-place flipping }
    P1 := PByte(Bits);
    P2 := P1;
    Inc(P2, Width - 1);
    W2 := Width shr 1;
    for J := 0 to Height - 1 do
    begin
      for I := 0 to W2 - 1 do
      begin
        tmp := P1^;
        P1^ := P2^;
        P2^ := tmp;
        Inc(P1);
        Dec(P2);
      end;
      Inc(P1, W - W2);
      Inc(P2, W + W2);
    end;
    Changed;
  end
  else
  begin
    { Flip to Dst }
    Dst.BeginUpdate;
    Dst.SetSize(W, Height);
    P1 := PByte(Bits);
    P2 := PByte(Dst.Bits);
    Inc(P2, W - 1);
    for J := 0 to Height - 1 do
    begin
      for I := 0 to W - 1 do
      begin
        P2^ := P1^;
        Inc(P1);
        Dec(P2);
      end;
      Inc(P2, W shl 1);
    end;
    Dst.EndUpdate;
    Dst.Changed;
  end;
end;

procedure TByteMap.FlipVert(Dst: TByteMap);
var
  J, J2: Integer;
  Buffer: PByteArray;
  P1, P2: PByte;
begin
  if (Dst = nil) or (Dst = Self) then
  begin
    { in-place }
    J2 := Height - 1;
    GetMem(Buffer, Width);
    for J := 0 to Height div 2 - 1 do
    begin
      P1 := PByte(ScanLine[J]);
      P2 := PByte(ScanLine[J2]);
      Move(P1^, Buffer^, Width);
      Move(P2^, P1^, Width);
      Move(Buffer^, P2^, Width);
      Dec(J2);
    end;
    FreeMem(Buffer);
    Changed;
  end
  else
  begin
    Dst.SetSize(Width, Height);
    J2 := Height - 1;
    for J := 0 to Height - 1 do
    begin
      Move(ScanLine[J]^, Dst.ScanLine[J2]^, Width);
      Dec(J2);
    end;
    Dst.Changed;
  end;
end;

function TByteMap.GetScanline(Y: Integer): PByteArray;
begin
  Result := @FBits^[Y * Width];
end;

function TByteMap.GetValPtr(X, Y: Integer): PByte;
begin
  Result := @FBits^[X + Y * Width];
end;

function TByteMap.GetValue(X, Y: Integer): Byte;
begin
  Result := FBits^[X + Y * Width];
end;

procedure TByteMap.Multiply(Value: Byte);
var
  Index: Integer;
begin
  for Index := 0 to FWidth * FHeight - 1 do
    FBits^[Index] := ((FBits^[Index] * Value + $80) shr 8);
end;

procedure TByteMap.Add(Value: Byte);
var
  Index: Integer;
begin
  for Index := 0 to FWidth * FHeight - 1 do
    FBits^[Index] := Min(FBits^[Index] + Value, 255);
end;

procedure TByteMap.Sub(Value: Byte);
var
  Index: Integer;
begin
  for Index := 0 to FWidth * FHeight - 1 do
    FBits^[Index] := Max(FBits^[Index] + Value, 0);
end;

procedure TByteMap.ReadFrom(Source: TCustomBitmap32; Conversion: TConversionType);
var
  W, H, I, N: Integer;
  SrcC: PColor32;
  SrcB, DstB: PByte;
  Value: TColor32;
begin
  BeginUpdate;
  try
    SetSize(Source.Width, Source.Height);
    if Empty then Exit;

    W := Source.Width;
    H := Source.Height;
    N := W * H - 1;
    SrcC := Source.PixelPtr[0, 0];
    SrcB := Pointer(SrcC);
    DstB := @FBits^;
    case Conversion of

      ctRed:
        begin
          Inc(SrcB, 2);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;

      ctGreen:
        begin
          Inc(SrcB, 1);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;

      ctBlue:
        begin
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;

      ctAlpha:
        begin
          Inc(SrcB, 3);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB);
            Inc(SrcB, 4);
          end;
        end;

      ctUniformRGB:
        begin
          for I := 0 to N do
          begin
            Value := SrcC^;
            Value := (Value and $00FF0000) shr 16 + (Value and $0000FF00) shr 8 +
              (Value and $000000FF);
            Value := Value div 3;
            DstB^ := Value;
            Inc(DstB);
            Inc(SrcC);
          end;
        end;

      ctWeightedRGB:
        begin
          for I := 0 to N do
          begin
            DstB^ := Intensity(SrcC^);
            Inc(DstB);
            Inc(SrcC);
          end;
        end;
    end;
  finally
    EndUpdate;
    Changed;
  end;
end;

procedure TByteMap.Rotate180(Dst: TByteMap);
var
  Src: PByteArray;
  S, D: PByte;
  X, Y: Integer;
  T: Byte;
begin
  if (Dst = nil) or (Dst = Self) then
  begin
    for Y := 0 to FHeight - 1 do
    begin
      Src := Scanline[Y];
      for X := 0 to (FWidth div 2) - 1 do
      begin
        T := Src^[X];
        Src^[X] := Src^[Width - 1 - X];
        Src^[Width - 1 - X] := T;
      end;
    end;
  end
  else
  begin
    S := PByte(FBits);
    D := PByte(@Dst.Bits[FHeight * FWidth - 1]);
    for X := 0 to FHeight * FWidth - 1 do
    begin
      D^ := S^;
      Dec(D);
      Inc(S);
    end;
  end;
end;

procedure TByteMap.Rotate270(Dst: TByteMap);
var
  Src: PByteArray;
  Current: PByte;
  X, Y, W, H: Integer;
begin
  if (Dst = nil) or (Dst = Self) then
  begin
    W := FWidth;
    H := FHeight;

     // inplace replace
    GetMem(Src, W * H);

    // copy bits
    Move(Bits^, Src^, W * H);

    SetSize(H, W);

    Current := PByte(Src);
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
      begin
        Bits^[(W - 1 - X) * H + Y] := Current^;
        Inc(Current);
      end;

    // dispose old data pointer
    FreeMem(Src);
  end
  else
  begin
    // exchange dimensions
    Dst.SetSize(Height, Width);

    for Y := 0 to FHeight - 1 do
    begin
      Src := Scanline[Y];
      for X := 0 to FWidth - 1 do
        Dst.Bits^[X * FHeight + FHeight - 1 - Y] := Src^[X];
    end;
  end;
end;

procedure TByteMap.Rotate90(Dst: TByteMap);
var
  Src: PByteArray;
  Current: PByte;
  X, Y, W, H: Integer;
begin
  if (Dst = nil) or (Dst = Self) then
  begin
    W := FWidth;
    H := FHeight;

     // inplace replace
    GetMem(Src, W * H);

    // copy bits
    Move(Bits^, Src^, W * H);

    SetSize(H, W);

    Current := PByte(Src);
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
      begin
        Bits^[X * H + (H - 1 - Y)] := Current^;
        Inc(Current);
      end;

    // dispose old data pointer
    FreeMem(Src);
  end
  else
  begin
    // exchange dimensions
    Dst.SetSize(Height, Width);

    for Y := 0 to FHeight - 1 do
    begin
      Src := Scanline[Y];
      for X := 0 to FWidth - 1 do
        Dst.Bits^[(FWidth - 1 - X) * FHeight + Y] := Src^[X];
    end;
  end;
end;

procedure TByteMap.SetValue(X, Y: Integer; Value: Byte);
begin
  FBits^[X + Y * Width] := Value;
end;

procedure TByteMap.WriteTo(Dest: TCustomBitmap32; Conversion: TConversionType);
var
  W, H, I, N: Integer;
  DstC: PColor32;
  DstB, SrcB: PByte;
  Resized: Boolean;
begin
  Dest.BeginUpdate;
  Resized := False;
  try
    Resized := Dest.SetSize(Width, Height);
    if Empty then Exit;

    W := Width;
    H := Height;
    N := W * H - 1;
    DstC := Dest.PixelPtr[0, 0];
    DstB := Pointer(DstC);
    SrcB := @FBits^;
    case Conversion of

      ctRed:
        begin
          Inc(DstB, 2);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;

      ctGreen:
        begin
          Inc(DstB, 1);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;

      ctBlue:
        begin
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;

      ctAlpha:
        begin
          Inc(DstB, 3);
          for I := 0 to N do
          begin
            DstB^ := SrcB^;
            Inc(DstB, 4);
            Inc(SrcB);
          end;
        end;

      ctUniformRGB, ctWeightedRGB:
        begin
          for I := 0 to N do
          begin
            DstC^ := Gray32(SrcB^);
            Inc(DstC);
            Inc(SrcB);
          end;
        end;
    end;
  finally
    Dest.EndUpdate;
    Dest.Changed;
    if Resized then Dest.Resized;
  end;
end;

procedure TByteMap.WriteTo(Dest: TCustomBitmap32; const Palette: TPalette32);
var
  W, H, I, N: Integer;
  DstC: PColor32;
  SrcB: PByte;
begin
  Dest.BeginUpdate;
  try
    Dest.SetSize(Width, Height);
    if Empty then Exit;

    W := Width;
    H := Height;
    N := W * H - 1;
    DstC := Dest.PixelPtr[0, 0];
    SrcB := @FBits^;

    for I := 0 to N do
    begin
      DstC^ := Palette[SrcB^];
      Inc(DstC);
      Inc(SrcB);
    end;
  finally
    Dest.EndUpdate;
    Dest.Changed;
  end;
end;

procedure TByteMap.DrawTo(Dest: TCustomBitmap32; X, Y: Integer; Color: TColor32);
var
  ClipRect: TRect;
  IX, IY: Integer;
  RGB: Cardinal;
  NewColor: TColor32;
  ScnLn: PColor32Array;
  ByteLine: PByteArray;
  Alpha: Byte;
begin
  with ClipRect do
  begin
    Left := X;
    if Left < 0 then
      Left := 0;
    Top := Y;
    if Top < 0 then
      Top := 0;
    Right := X + Self.Width;
    if Right > Self.Width then
      Right := Self.Width;
    Bottom := Y + Self.Height;
    if Bottom > Self.Height then
      Bottom := Self.Height;

    // split RGB and alpha
    RGB := Color and $FFFFFF;
    Alpha := Color shr 24;

    // blend scanlines
    for IY := Top to Bottom - 1 do
    begin
      ScnLn := Dest.ScanLine[IY];
      ByteLine := Self.ScanLine[IY - Y];
      for IX := Left to Right - 1 do
      begin
        NewColor := (((ByteLine^[IX - X] * Alpha) shl 16) and $FF000000) or RGB;
        MergeMem(NewColor, ScnLn^[IX]);
      end;
    end;
    EMMS;
  end;
end;

procedure TByteMap.DrawTo(Dest: TCustomBitmap32; Rect: TRect; Color: TColor32);
var
  ClipRect: TRect;
  IX, IY: Integer;
  RGB: Cardinal;
  NewColor: TColor32;
  ScnLn: PColor32Array;
  ByteLine: PByteArray;
  Alpha: Byte;
begin
  with ClipRect do
  begin
    Left := Rect.Left;
    if Left < 0 then
      Left := 0;
    Top := Rect.Top;
    if Top < 0 then
      Top := 0;
    Right := Math.Min(Rect.Left + Self.Width, Rect.Right);
    Bottom := Math.Min(Rect.Top + Self.Height, Rect.Bottom);

    // split RGB and alpha
    RGB := Color and $FFFFFF;
    Alpha := Color shr 24;

    // blend scanlines
    for IY := Top to Bottom - 1 do
    begin
      ScnLn := Dest.ScanLine[IY];
      ByteLine := Self.ScanLine[IY - Rect.Top];
      for IX := Left to Right - 1 do
      begin
        NewColor := (((ByteLine^[IX - Rect.Left] * Alpha) shl 16) and $FF000000) or RGB;
        MergeMem(NewColor, ScnLn^[IX]);
      end;
    end;
    EMMS;
  end;
end;


{ TWordMap }

constructor TWordMap.Create;
begin
  FBits := nil;
  inherited Create;
end;

destructor TWordMap.Destroy;
begin
  FreeMem(FBits);
  inherited;
end;

procedure TWordMap.ChangeSize(var Width, Height: Integer; NewWidth,
  NewHeight: Integer);
begin
  ReallocMem(FBits, NewWidth * NewHeight * SizeOf(Word));
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TWordMap.Clear(FillValue: Word);
begin
  FillWord(FBits^, Width * Height, FillValue);
  Changed;
end;

procedure TWordMap.Assign(Source: TPersistent);
begin
  BeginUpdate;
    try
      if Source is TWordMap then
      begin
        inherited SetSize(TWordMap(Source).Width, TWordMap(Source).Height);
        Move(TWordMap(Source).Bits[0], Bits[0], Width * Height * SizeOf(Word));
      end
      //else if Source is TBitmap32 then
      //  ReadFrom(TBitmap32(Source), ctWeightedRGB)
      else
        inherited;
    finally
      EndUpdate;
      Changed;
    end;
end;

function TWordMap.Empty: Boolean;
begin
  Result := not Assigned(FBits);
end;

function TWordMap.GetScanline(Y: Integer): PWordArray;
begin
  Result := @FBits^[Y * Width];
end;

function TWordMap.GetValPtr(X, Y: Integer): PWord;
begin
  Result := @FBits^[X + Y * Width];
end;

function TWordMap.GetValue(X, Y: Integer): Word;
begin
  Result := FBits^[X + Y * Width];
end;

procedure TWordMap.SetValue(X, Y: Integer; const Value: Word);
begin
  FBits^[X + Y * Width] := Value;
end;


{ TIntegerMap }

constructor TIntegerMap.Create;
begin
  FBits := nil;
  inherited Create;
end;

destructor TIntegerMap.Destroy;
begin
  FreeMem(FBits);
  inherited;
end;

procedure TIntegerMap.ChangeSize(var Width, Height: Integer; NewWidth,
  NewHeight: Integer);
begin
  ReallocMem(FBits, NewWidth * NewHeight * SizeOf(Integer));
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TIntegerMap.Clear(FillValue: Integer);
begin
  FillLongword(FBits^, Width * Height, FillValue);
  Changed;
end;

procedure TIntegerMap.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TIntegerMap then
    begin
      inherited SetSize(TIntegerMap(Source).Width, TIntegerMap(Source).Height);
      Move(TIntegerMap(Source).Bits[0], Bits[0], Width * Height * SizeOf(Integer));
    end
    //else if Source is TBitmap32 then
    //  ReadFrom(TBitmap32(Source), ctWeightedRGB)
    else
      inherited;
  finally
    EndUpdate;
    Changed;
  end;
end;

function TIntegerMap.Empty: Boolean;
begin
  Result := not Assigned(FBits);
end;

function TIntegerMap.GetScanline(Y: Integer): PIntegerArray;
begin
  Result := @FBits^[Y * Width];
end;

function TIntegerMap.GetValPtr(X, Y: Integer): PInteger;
begin
  Result := @FBits^[X + Y * Width];
end;

function TIntegerMap.GetValue(X, Y: Integer): Integer;
begin
  Result := FBits^[X + Y * Width];
end;

procedure TIntegerMap.SetValue(X, Y: Integer; const Value: Integer);
begin
  FBits^[X + Y * Width] := Value;
end;


{ TCardinalMap }

constructor TCardinalMap.Create;
begin
  FBits := nil;
  inherited Create;
end;

destructor TCardinalMap.Destroy;
begin
  FreeMem(FBits);
  inherited;
end;

procedure TCardinalMap.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TCardinalMap then
    begin
      inherited SetSize(TCardinalMap(Source).Width, TCardinalMap(Source).Height);
      Move(TCardinalMap(Source).Bits[0], Bits[0], Width * Height * SizeOf(Cardinal));
    end
    //else if Source is TBitmap32 then
    //  ReadFrom(TBitmap32(Source), ctWeightedRGB)
    else
      inherited;
  finally
    EndUpdate;
    Changed;
  end;
end;

procedure TCardinalMap.ChangeSize(var Width, Height: Integer; NewWidth,
  NewHeight: Integer);
begin
  ReallocMem(FBits, NewWidth * NewHeight * SizeOf(Cardinal));
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TCardinalMap.Clear(FillValue: Cardinal);
begin
  FillLongword(FBits^, Width * Height, FillValue);
  Changed;
end;

function TCardinalMap.Empty: Boolean;
begin
  Result := not Assigned(FBits);
end;

function TCardinalMap.GetScanline(Y: Integer): PCardinalArray;
begin
  Result := @FBits^[Y * Width];
end;

function TCardinalMap.GetValPtr(X, Y: Cardinal): PCardinal;
begin
  Result := @FBits^[X + Y * Cardinal(Width)];
end;

function TCardinalMap.GetValue(X, Y: Cardinal): Cardinal;
begin
  Result := FBits^[X + Y * Cardinal(Width)];
end;

procedure TCardinalMap.SetValue(X, Y: Cardinal; const Value: Cardinal);
begin
  FBits^[X + Y * Cardinal(Width)] := Value;
end;


{ TFloatMap }

constructor TFloatMap.Create;
begin
  FBits := nil;
  inherited Create;
end;

destructor TFloatMap.Destroy;
begin
  FreeMem(FBits);
  inherited;
end;

procedure TFloatMap.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TFloatMap then
    begin
      inherited SetSize(TFloatMap(Source).Width, TFloatMap(Source).Height);
      Move(TFloatMap(Source).Bits[0], Bits[0], Width * Height * SizeOf(TFloat));
    end
    //else if Source is TBitmap32 then
    //  ReadFrom(TBitmap32(Source), ctWeightedRGB)
    else
      inherited;
  finally
    EndUpdate;
    Changed;
  end;
end;

procedure TFloatMap.ChangeSize(var Width, Height: Integer; NewWidth,
  NewHeight: Integer);
begin
  ReallocMem(FBits, NewWidth * NewHeight * SizeOf(TFloat));
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TFloatMap.Clear;
begin
  FillChar(FBits^, Width * Height * SizeOf(TFloat), 0);
  Changed;
end;

procedure TFloatMap.Clear(FillValue: TFloat);
var
  Index: Integer;
begin
  for Index := 0 to Width * Height - 1 do
    FBits^[Index] := FillValue;
  Changed;
end;

function TFloatMap.Empty: Boolean;
begin
  Result := not Assigned(FBits);
end;

function TFloatMap.GetScanline(Y: Integer): PFloatArray;
begin
  Result := @FBits^[Y * Width];
end;

function TFloatMap.GetValPtr(X, Y: Integer): GR32.PFloat;
begin
  Result := @FBits^[X + Y * Width];
end;

function TFloatMap.GetValue(X, Y: Integer): TFloat;
begin
  Result := FBits^[X + Y * Width];
end;

procedure TFloatMap.SetValue(X, Y: Integer; const Value: TFloat);
begin
  FBits^[X + Y * Width] := Value;
end;


{$IFDEF COMPILER2010}

{ TGenericMap<T> }

constructor TGenericMap<T>.Create;
begin
  FBits := nil;
  inherited Create;
end;

destructor TGenericMap<T>.Destroy;
begin
  FreeMem(FBits);
  inherited;
end;

procedure TGenericMap<T>.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
(*
    if Source is TFloatMap then
    begin
      inherited SetSize(TFloatMap(Source).Width, TFloatMap(Source).Height);
      Move(TFloatMap(Source).Bits[0], Bits[0], Width * Height * SizeOf(TFloat));
    end
    //else if Source is TBitmap32 then
    //  ReadFrom(TBitmap32(Source), ctWeightedRGB)
    else
      inherited;
*)
  finally
    EndUpdate;
    Changed;
  end;
end;

procedure TGenericMap<T>.ChangeSize(var Width, Height: Integer; NewWidth,
  NewHeight: Integer);
begin
  ReallocMem(FBits, NewWidth * NewHeight * SizeOf(T));
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TGenericMap<T>.Clear(FillValue: T);
var
  Index: Integer;
begin
  for Index := 0 to Width * Height - 1 do
    Move(FillValue, PByte(FBits)[Index], SizeOf(T));
  Changed;
end;

procedure TGenericMap<T>.Clear;
begin
  FillChar(FBits^, Width * Height * SizeOf(T), 0);
  Changed;
end;

function TGenericMap<T>.Empty: Boolean;
begin
  Result := not Assigned(FBits);
end;

function TGenericMap<T>.GetValue(X, Y: Integer): T;
begin
  Move(PByte(FBits)[(X + Y * Width) * SizeOf(T)], Result, SizeOf(T));
end;

procedure TGenericMap<T>.SetValue(X, Y: Integer; const Value: T);
begin
  Move(Value, PByte(FBits)[(X + Y * Width) * SizeOf(T)], SizeOf(T));
end;

{$ENDIF}

end.
