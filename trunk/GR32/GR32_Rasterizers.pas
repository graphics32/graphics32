unit GR32_Rasterizers;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF CLX}
  Qt, Types, {$IFDEF LINUX}Libc, {$ELSE}Windows, {$ENDIF}
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes, GR32, GR32_Blend, GR32_IntegerMaps;

type
  TAssignColor = procedure(var Dst: TColor32; Src: TColor32) of object;

  PCombineInfo = ^TCombineInfo;
  TCombineInfo = record
    SrcAlpha: Integer;
    DrawMode: TDrawMode;
    CombineMode: TCombineMode;
    CombineCallBack: TPixelCombineEvent;
    TransparentColor: TColor32;
  end;

type
  { TRasterizer }
  { A base class for TBitmap32-specific rasterizers. }
  TRasterizer = class(TThreadPersistent)
  private
    FSampler: TCustomSampler;
    FSrcAlpha: Integer;
    FBlendMemEx: TBlendMemEx;
    FCombineCallBack: TPixelCombineEvent;
    FAssignColor: TAssignColor;
    FTransparentColor: TColor32;
    procedure SetSampler(const Value: TCustomSampler);
    procedure AssignColorOpaque(var Dst: TColor32; Src: TColor32);
    procedure AssignColorBlend(var Dst: TColor32; Src: TColor32);
    procedure AssignColorCustom(var Dst: TColor32; Src: TColor32);
    procedure AssignColorTransparent(var Dst: TColor32; Src: TColor32);
  protected
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); virtual; abstract;
    procedure Rasterize(Dst: TBitmap32; const DstRect: TRect; SrcAlpha: TColor32;
      DrawMode: TDrawMode; CombineMode: TCombineMode;
      CombineCallBack: TPixelCombineEvent); overload;
    property AssignColor: TAssignColor read FAssignColor write FAssignColor;
  public
    procedure Rasterize(Dst: TBitmap32); overload;
    procedure Rasterize(Dst: TBitmap32; const DstRect: TRect); overload;
    procedure Rasterize(Dst: TBitmap32; const DstRect: TRect; const CombineInfo: TCombineInfo); overload;
    procedure Rasterize(Dst: TBitmap32; const DstRect: TRect; Src: TBitmap32); overload;
  published
    property Sampler: TCustomSampler read FSampler write SetSampler;
  end;

  TRasterizerClass = class of TRasterizer;

  { TRegularSamplingRasterizer }
  { This rasterizer simply picks one sample for each pixel in the output bitmap. }
  TRegularRasterizer = class(TRasterizer)
  private
    FUpdateRowCount: Integer;
  protected
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); override;
  public
    constructor Create; override;
  published
    property UpdateRowCount: Integer read FUpdateRowCount write FUpdateRowCount;
  end;

  { TSwizzlingRasterizer }
  { An interesting rasterization method where sample locations are choosen
    according to a fractal pattern called 'swizzling'. With a slight
    modification to the algorithm this routine will actually yield the
    well-known sierpinski triangle fractal. An advantage with this pattern
    is that it may benefit from local coherency in the sampling method used. }
  TSwizzlingRasterizer = class(TRasterizer)
  private
    FBlockSize: Integer;
    procedure SetBlockSize(const Value: Integer);
  protected
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); override;
  public
    constructor Create; override;
  published
    property BlockSize: Integer read FBlockSize write SetBlockSize;
  end;

  { TProgressiveRasterizer }
  { This class will perform rasterization in a progressive manner. It performs
    subsampling with a block size of 2^n and will successively decrease n in
    each iteration until n equals zero.  }
  TProgressiveRasterizer = class(TRasterizer)
  private
    FLevel: Integer;
    FProgressLines: Boolean;
    procedure SetLevel(const Value: Integer);
  protected
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); override;
  public
    constructor Create; override;
  published
    property Level: Integer read FLevel write SetLevel;
    property ProgressLines: Boolean read FProgressLines write FProgressLines;
  end;

  { TTesseralRasterizer }
  { This is a recursive rasterization method. It uses a divide-and-conquer
    scheme to subdivide blocks vertically and horizontally into smaller blocks. }
  TTesseralRasterizer = class(TRasterizer)
  protected
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); override;
  end;

  { TContourRasterizer }
  TContourRasterizer = class(TRasterizer)
  protected
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); override;
  end;


{ Auxiliary routines }
function CombineInfo(Bitmap: TBitmap32): TCombineInfo;

var
  DefaultRasterizerClass: TRasterizerClass = TRegularRasterizer;

implementation

uses
  GR32_Resamplers, GR32_Math, Math, SysUtils;

type
  TThreadPersistentAccess = class(TThreadPersistent);

function CombineInfo(Bitmap: TBitmap32): TCombineInfo;
begin
  with Result do
  begin
    SrcAlpha := Bitmap.MasterAlpha;
    DrawMode := Bitmap.DrawMode;
    CombineMode := Bitmap.CombineMode;
    CombineCallBack := Bitmap.OnPixelCombine;
    if (DrawMode = dmCustom) and not Assigned(CombineCallBack) then
      DrawMode := dmOpaque;
    TransparentColor := Bitmap.OuterColor;
  end;
end;


{ TRasterizer }

procedure TRasterizer.AssignColorBlend(var Dst: TColor32; Src: TColor32);
begin
  FBlendMemEx(Src, Dst, FSrcAlpha);
  EMMS;
end;

procedure TRasterizer.AssignColorOpaque(var Dst: TColor32; Src: TColor32);
begin
  Dst := Src;
end;

procedure TRasterizer.AssignColorCustom(var Dst: TColor32; Src: TColor32);
begin
  FCombineCallBack(Src, Dst, FSrcAlpha);
end;

procedure TRasterizer.AssignColorTransparent(var Dst: TColor32;
  Src: TColor32);
begin
  if Src <> FTransparentColor then Dst := Src;
end;

procedure TRasterizer.Rasterize(Dst: TBitmap32; const DstRect: TRect;
  Src: TBitmap32);
begin
  Rasterize(Dst, DstRect, CombineInfo(Src));
end;

procedure TRasterizer.Rasterize(Dst: TBitmap32; const DstRect: TRect);
const
  DEFAULT_COMBINE_INFO: TCombineInfo = (
    SrcAlpha: $FF;
    DrawMode: dmOpaque;
    CombineMode: cmBlend;
    CombineCallBack: nil;
    TransparentColor: clBlack32;
  );
begin
  Rasterize(Dst, DstRect, DEFAULT_COMBINE_INFO);
end;

procedure TRasterizer.Rasterize(Dst: TBitmap32; const DstRect: TRect;
  const CombineInfo: TCombineInfo);
begin
  FTransparentColor := CombineInfo.TransparentColor;
  with CombineInfo do
    Rasterize(Dst, DstRect, SrcAlpha, DrawMode, CombineMode, CombineCallBack);
end;

procedure TRasterizer.Rasterize(Dst: TBitmap32; const DstRect: TRect;
  SrcAlpha: TColor32; DrawMode: TDrawMode; CombineMode: TCombineMode;
  CombineCallBack: TPixelCombineEvent);
var
  UpdateCount: Integer;
  R: TRect;
begin
  FSrcAlpha := SrcAlpha;
  FBlendMemEx := BLEND_MEM_EX[CombineMode];
  FCombineCallBack := CombineCallBack;

  case DrawMode of
    dmOpaque: FAssignColor := AssignColorOpaque;
    dmBlend:  FAssignColor := AssignColorBlend;
    dmTransparent: FAssignColor := AssignColorTransparent;
  else
    if Assigned(FCombineCallback) then
      FAssignColor := AssignColorCustom
    else
      FAssignColor := AssignColorBlend;
  end;

  UpdateCount := TThreadPersistentAccess(Dst).UpdateCount;
  if Assigned(FSampler) then
  begin
    FSampler.PrepareSampling;
    IntersectRect(R, DstRect, Dst.BoundsRect);
    if FSampler.HasBounds then
      IntersectRect(R, DstRect, FSampler.GetSampleBounds);
    try
      DoRasterize(Dst, R);
    finally
      while TThreadPersistentAccess(Dst).UpdateCount > UpdateCount do
        TThreadPersistentAccess(Dst).EndUpdate;
      FSampler.FinalizeSampling;
    end;
  end;
end;

procedure TRasterizer.SetSampler(const Value: TCustomSampler);
begin
  if FSampler <> Value then
  begin
    FSampler := Value;
    Changed;
  end;
end;

procedure TRasterizer.Rasterize(Dst: TBitmap32);
begin
  Rasterize(Dst, Dst.BoundsRect);
end;


{ TRegularRasterizer }

constructor TRegularRasterizer.Create;
begin
  inherited;
  FUpdateRowCount := 0;
end;

procedure TRegularRasterizer.DoRasterize(Dst: TBitmap32; DstRect: TRect);
var
  I, J, UpdateCount: Integer;
  P: PColor32;
  GetSample: TGetSampleInt;
begin
  GetSample := FSampler.GetSampleInt;
  UpdateCount := 0;
  for J := DstRect.Top to DstRect.Bottom - 1 do
  begin
    P := @Dst.Bits[DstRect.Left + J * Dst.Width];
    for I := DstRect.Left to DstRect.Right - 1 do
    begin
      AssignColor(P^, GetSample(I, J));
      Inc(P);
    end;
    Inc(UpdateCount);
    if UpdateCount = FUpdateRowCount then
    begin
      Dst.Changed(Rect(DstRect.Left, J - UpdateCount, DstRect.Right, J));
      UpdateCount := 0;
    end;
  end;
  with DstRect do
    Dst.Changed(Rect(Left, Bottom - UpdateCount, Right, Bottom));
end;

{ TSwizzlingRasterizer }

constructor TSwizzlingRasterizer.Create;
begin
  inherited;
  FBlockSize := 3;
end;

procedure TSwizzlingRasterizer.DoRasterize(Dst: TBitmap32; DstRect: TRect);
var
  I, L, T, W, H, Size, RowSize, D: Integer;
  P1, P2, PBlock: TPoint;
  GetSample: TGetSampleInt;
  ForwardBuffer: array of Integer;

  function ScanReverse(Value: Integer): Integer;
  asm
    BSR EAX,EAX
  end;

  function GetDstCoord(P: TPoint): TPoint;
  var
    XI, YI: Integer;
  begin
    Result := P;
    Inc(Result.X);
    Inc(Result.Y);

    XI := ForwardBuffer[Result.X];
    YI := ForwardBuffer[Result.Y];

    if XI <= YI then
      Dec(Result.Y, 1 shl XI)
    else
      Dec(Result.X, 1 shl (YI + 1));

    if Result.Y >= H then
    begin
      Result.Y := P.Y + 1 shl YI;
      Result.X := P.X;
      Result := GetDstCoord(Result);
    end;

    if Result.X >= W then
    begin
      Result.X := P.X + 1 shl XI;
      Result.Y := P.Y;
      Result := GetDstCoord(Result);
    end;
  end;

begin
  W := DstRect.Right - DstRect.Left;
  H := DstRect.Bottom - DstRect.Top;
  L := DstRect.Left; T := DstRect.Top;
  Size := 1 shl (ScanReverse(Max(W, H)) + 1) + 1;

  SetLength(ForwardBuffer, Size);

  I := 2;
  while I < Size do
  begin
    ForwardBuffer[I] := ForwardBuffer[I shr 1] + 1;
    Inc(I, 2);
  end;

  Size := W * H - 1;
  GetSample := FSampler.GetSampleInt;

  D := 1 shl FBlockSize;
  PBlock := Point(L + D, T + D);
  P1 := Point(-1, 0);

  RowSize := Dst.Width;
  for I := 0 to Size do
  begin
    P1 := GetDstCoord(P1);
    P2.X := L + P1.X;
    P2.Y := T + P1.Y;
    AssignColor(Dst.Bits[P2.X + P2.Y * RowSize], GetSample(P2.X, P2.Y));

    // Invalidate the current block
    if (P2.X >= PBlock.X) or (P2.Y >= PBlock.Y) then
    begin
      Dst.Changed(Rect(PBlock.X - D, PBlock.Y - D, PBlock.X, PBlock.Y));
      PBlock.X := P2.X + D;
      PBlock.Y := P2.Y + D;
    end;
  end;
  Dst.Changed(Rect(PBlock.X - D, PBlock.Y - D, PBlock.X, PBlock.Y));
end;

procedure TSwizzlingRasterizer.SetBlockSize(const Value: Integer);
begin
  if FBlockSize <> Value then
  begin
    FBlockSize := Value;
    Changed;
  end;
end;

{ TProgressiveRasterizer }

constructor TProgressiveRasterizer.Create;
begin
  inherited;
  FLevel := 4;
  FProgressLines := True;
end;

procedure TProgressiveRasterizer.DoRasterize(Dst: TBitmap32;
  DstRect: TRect);
var
  I, J, Shift, W, H, B, Wk, Hk, X, Y: Integer;
  DoUpdate: Boolean;
  OnChanged: TAreaChangedEvent;
  Step: Integer;
  GetSample: TGetSampleInt;
begin
  GetSample := FSampler.GetSampleInt;
  OnChanged := Dst.OnAreaChanged;
  DoUpdate := (TThreadPersistentAccess(Dst).UpdateCount = 0) and Assigned(OnChanged);
  Dst.BeginUpdate;
  W := DstRect.Right - DstRect.Left;
  H := DstRect.Bottom - DstRect.Top;
  J := DstRect.Top;
  Step := 1 shl FLevel;
  while J < DstRect.Bottom do
  begin
    I := DstRect.Left;
    B := Min(J + Step, DstRect.Bottom);
    while I < DstRect.Right - Step do
    begin
      Dst.FillRect(I, J, I + Step, B, GetSample(I, J));
      Inc(I, Step);
    end;
    Dst.FillRect(I, J, DstRect.Right, B, GetSample(I, J));
    if DoUpdate and FProgressLines then
      OnChanged(Dst, Rect(DstRect.Left, J, DstRect.Right, J + Step), AREAHINT_RECT);
    Inc(J, Step);
  end;
  if DoUpdate and (not FProgressLines) then OnChanged(Dst, DstRect, AREAHINT_RECT);

  Shift := FLevel;
  repeat
    Dec(Shift);
    Step := Step div 2;
    Wk := W div Step - 1;
    Hk := H div Step;
    for J := 0 to Hk do
    begin
      Y := DstRect.Top + J shl Shift;
      B := Min(Y + Step, DstRect.Bottom);
      if Odd(J) then
        for I := 0 to Wk do
        begin
          X := DstRect.Left + I shl Shift;
          Dst.FillRect(X, Y, X + Step, B, GetSample(X, Y));
        end
      else
        for I := 0 to Wk do
          if Odd(I) then
          begin
            X := DstRect.Left + I shl Shift;
            Dst.FillRect(X, Y, X + Step, B, GetSample(X, Y));
          end;
      X := DstRect.Left + Wk shl Shift;
      Dst.FillRect(X, Y, DstRect.Right, B, GetSample(X, Y));
      if FProgressLines and DoUpdate then
        OnChanged(Dst, Rect(DstRect.Left, Y, DstRect.Right, Y + Step), AREAHINT_RECT);
    end;
    if DoUpdate and (not FProgressLines) then OnChanged(Dst, DstRect, AREAHINT_RECT);
  until Step = 1;
  Dst.EndUpdate;
end;

procedure TProgressiveRasterizer.SetLevel(const Value: Integer);
begin
  if FLevel <> Value then
  begin
    FLevel := Value;
    Changed;
  end;
end;

{ TTesseralRasterizer }

procedure TTesseralRasterizer.DoRasterize(Dst: TBitmap32; DstRect: TRect);
var
  W, H, I: Integer;
  GetSample: TGetSampleInt;

  procedure SplitHorizontal(X, Y, Width, Height: Integer); forward;

  procedure SplitVertical(X, Y, Width, Height: Integer);
  var
    HalfWidth, X2, I: Integer;
  begin
    HalfWidth := Width div 2;
    if HalfWidth > 0 then
    begin
      X2 := X + HalfWidth;
      for I := Y + 1 to Y + Height - 1 do
        AssignColor(Dst.PixelPtr[X2, I]^, GetSample(X2, I));
      Dst.Changed(Rect(X2, Y + 1, X2 + 1, Y + Height));
      SplitHorizontal(X, Y, HalfWidth, Height);
      SplitHorizontal(X2, Y, Width - HalfWidth, Height);
    end;
  end;

  procedure SplitHorizontal(X, Y, Width, Height: Integer);
  var
    HalfHeight, Y2, I: Integer;
  begin
    HalfHeight := Height div 2;
    if HalfHeight > 0 then
    begin
      Y2 := Y + HalfHeight;
      for I := X + 1 to X + Width - 1 do
        AssignColor(Dst.PixelPtr[I, Y2]^, GetSample(I, Y2));
      Dst.Changed(Rect(X + 1, Y2, X + Width, Y2 + 1));
      SplitVertical(X, Y, Width, HalfHeight);
      SplitVertical(X, Y2, Width, Height - HalfHeight);
    end;
  end;

begin
  GetSample := FSampler.GetSampleInt;
  with DstRect do
  begin
    W := Right - Left;
    H := Bottom - Top;
    for I := Left to Right - 1 do
      AssignColor(Dst.PixelPtr[I, Top]^, GetSample(I, Top));
    Dst.Changed(Rect(Left, Top, Right, Top + 1));
    for I := Top to Bottom - 1 do
      AssignColor(Dst.PixelPtr[Left, I]^, GetSample(Left, I));
    Dst.Changed(Rect(Left, Top, Left + 1, Bottom));
    if W > H then
      SplitVertical(Left, Top, W, H)
    else
      SplitHorizontal(Left, Top, W, H);
  end;
end;


{ TContourRasterizer }

procedure InflateRect(const P: TPoint; var R: TRect);
begin
  if P.X < R.Left then R.Left := P.X;
  if P.Y < R.Top then R.Top := P.Y;
  if P.X >= R.Right then R.Right := P.X + 1;
  if P.Y >= R.Bottom then R.Bottom := P.Y + 1;
end;

procedure TContourRasterizer.DoRasterize(Dst: TBitmap32; DstRect: TRect);
type
  TDirection = (North, East, South, West);
var
  I, J, D, Diff: Integer;
  C, CLast: TColor32;
  P, PLast: TPoint;
  GetSample: TGetSampleInt;
  NewDir, Dir: TDirection;
  Visited: TBooleanMap;
  UpdateRect: TRect;
const
  LEFT: array[TDirection] of TDirection = (West, North, East, South);
  RIGHT: array[TDirection] of TDirection = (East, South, West, North);
  COORDS: array[TDirection] of TPoint = ((X: 0; Y: -1), (X: 1; Y: 0), (X: 0; Y: 1), (X: -1; Y: 0));
  UpdateSteps = 100;
label
  MainLoop;
begin
  GetSample := FSampler.GetSampleInt;
  Visited := TBooleanMap.Create;
  try
    with DstRect do
      Visited.SetSize(Right - Left, Bottom - Top);

    I := 0; J := 0;
    Dir := East;
    NewDir := East;

    PLast := Point(DstRect.Left, DstRect.Top);
    CLast := GetSample(PLast.X, PLast.Y);
    AssignColor(Dst.PixelPtr[PLast.X, PLast.Y]^, CLast);

    UpdateRect := Rect(PLast.X, PLast.Y, PLast.X + 1, PLast.Y + 1);
    while True do
    begin
      MainLoop:

      Diff := MaxInt;

      // forward
      with COORDS[Dir] do P := Point(PLast.X + X, PLast.Y + Y);
      if PtInRect(DstRect, P) and (not Visited[P.X, P.Y]) then
      begin
        C := GetSample(P.X, P.Y);
        Diff := Intensity(ColorSub(C, CLast));
        EMMS;
        NewDir := Dir;
        AssignColor(Dst.PixelPtr[P.X, P.Y]^, C);
        Visited[P.X - DstRect.Left, P.Y - DstRect.Top] := True;
        InflateRect(P, UpdateRect);
      end;

      // left
      with COORDS[LEFT[Dir]] do P := Point(PLast.X + X, PLast.Y + Y);
      if PtInRect(DstRect, P) and (not Visited[P.X, P.Y]) then
      begin
        C := GetSample(P.X, P.Y);
        D := Intensity(ColorSub(C, CLast));
        EMMS;        
        if D < Diff then
        begin
          NewDir := LEFT[Dir];
          Diff := D;
        end;
        AssignColor(Dst.PixelPtr[P.X, P.Y]^, C);
        Visited[P.X - DstRect.Left, P.Y - DstRect.Top] := True;
        InflateRect(P, UpdateRect);
      end;

      // right
      with COORDS[RIGHT[Dir]] do P := Point(PLast.X + X, PLast.Y + Y);
      if PtInRect(DstRect, P) and (not Visited[P.X, P.Y]) then
      begin
        C := GetSample(P.X, P.Y);
        D := Intensity(ColorSub(C, CLast));
        EMMS;        
        if D < Diff then
        begin
          NewDir := RIGHT[Dir];
          Diff := D;
        end;
        AssignColor(Dst.PixelPtr[P.X, P.Y]^, C);
        Visited[P.X - DstRect.Left, P.Y - DstRect.Top] := True;
        InflateRect(P, UpdateRect);
      end;

      if Diff = MaxInt then
      begin
        Dst.Changed(UpdateRect);
        while J < Visited.Height do
        begin
          while I < Visited.Width do
          begin
            if not Visited[I, J] then
            begin
              Visited[I, J] := True;
              PLast := Point(DstRect.Left + I, DstRect.Top + J);
              CLast := GetSample(PLast.X, PLast.Y);
              AssignColor(Dst.PixelPtr[PLast.X, PLast.Y]^, CLast);
              UpdateRect := Rect(PLast.X, PLast.Y, PLast.X + 1, PLast.Y + 1);
              goto MainLoop;
            end;
            Inc(I);
          end;
          I := 0;
          Inc(J);
        end;
        Break;
      end;

      Dir := NewDir;
      with COORDS[Dir] do PLast := Point(PLast.X + X, PLast.Y + Y);
      CLast := Dst[PLast.X, PLast.Y];
    end;

  finally
    Visited.Free;
  end;
end;

end.
