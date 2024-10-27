unit GR32_Rasterizers;

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
 *
 * Portions created by the Initial Developer are Copyright (C) 2004-2009
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{$if defined(DCC) and (CompilerVersion >= 28.0)} // TODO : Test for PLATFORM_VCL when it is merged
  {$define USE_PPL} // Use Delphi's Parallel Programming Library (introduced XE7)
{$ifend}

uses
  Classes,
  GR32,
  GR32_Blend;

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


//------------------------------------------------------------------------------
//
//      TRasterizer
//
//------------------------------------------------------------------------------
// A base class for TCustomBitmap32-specific rasterizers.
//------------------------------------------------------------------------------
type
  TRasterizer = class(TThreadPersistent)
  private
    FSampler: TCustomSampler;
    FSrcAlpha: Integer;
    FBlendMemEx: TBlendMemEx;
    FCombineCallBack: TPixelCombineEvent;
    FAssignColor: TAssignColor;
    FTransparentColor: TColor32;
    procedure SetSampler(const Value: TCustomSampler);
    procedure SetCombineInfo(const CombineInfo: TCombineInfo);
    procedure AssignColorOpaque(var Dst: TColor32; Src: TColor32);
    procedure AssignColorBlend(var Dst: TColor32; Src: TColor32);
    procedure AssignColorCustom(var Dst: TColor32; Src: TColor32);
    procedure AssignColorTransparent(var Dst: TColor32; Src: TColor32);
  protected
    procedure AssignTo(Dst: TPersistent); override;
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); virtual; abstract;
    property AssignColor: TAssignColor read FAssignColor write FAssignColor;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Rasterize(Dst: TCustomBitmap32); overload;
    procedure Rasterize(Dst: TCustomBitmap32; const DstRect: TRect); overload;
    procedure Rasterize(Dst: TCustomBitmap32; const DstRect: TRect; const CombineInfo: TCombineInfo); overload;
    procedure Rasterize(Dst: TCustomBitmap32; const DstRect: TRect; Src: TCustomBitmap32); overload;
  published
    property Sampler: TCustomSampler read FSampler write SetSampler;
  end;

  TRasterizerClass = class of TRasterizer;


//------------------------------------------------------------------------------
//
//      TRegularSamplingRasterizer
//
//------------------------------------------------------------------------------
// This rasterizer simply picks one sample for each pixel in the output bitmap.
//------------------------------------------------------------------------------
type
  TRegularRasterizer = class(TRasterizer)
  private
    FUpdateRowCount: Integer;
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  public
    constructor Create; override;
  published
    property UpdateRowCount: Integer read FUpdateRowCount write FUpdateRowCount;
  end;


//------------------------------------------------------------------------------
//
//      TSwizzlingRasterizer
//
//------------------------------------------------------------------------------
// An interesting rasterization method where sample locations are choosen
// according to a fractal pattern called 'swizzling'. With a slight
// modification to the algorithm this routine will actually yield the
// well-known sierpinski triangle fractal. An advantage with this pattern
// is that it may benefit from local coherency in the sampling method used.
//------------------------------------------------------------------------------
type
  TSwizzlingRasterizer = class(TRasterizer)
  private
    FBlockSize: Integer;
    procedure SetBlockSize(const Value: Integer);
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  public
    constructor Create; override;
  published
    property BlockSize: Integer read FBlockSize write SetBlockSize default 3;
  end;


//------------------------------------------------------------------------------
//
//      TProgressiveRasterizer
//
//------------------------------------------------------------------------------
// This class will perform rasterization in a progressive manner. It performs
// subsampling with a block size of 2^n and will successively decrease n in
// each iteration until n equals zero.
//------------------------------------------------------------------------------
type
  TProgressiveRasterizer = class(TRasterizer)
  private
    FSteps: Integer;
    FUpdateRows: Boolean;
    procedure SetSteps(const Value: Integer);
    procedure SetUpdateRows(const Value: Boolean);
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  public
    constructor Create; override;
  published
    property Steps: Integer read FSteps write SetSteps default 4;
    property UpdateRows: Boolean read FUpdateRows write SetUpdateRows default True;
  end;


//------------------------------------------------------------------------------
//
//      TTesseralRasterizer
//
//------------------------------------------------------------------------------
// This is a recursive rasterization method. It uses a divide-and-conquer
// scheme to subdivide blocks vertically and horizontally into smaller blocks.
//------------------------------------------------------------------------------
type
  TTesseralRasterizer = class(TRasterizer)
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  end;


//------------------------------------------------------------------------------
//
//      TContourRasterizer
//
//------------------------------------------------------------------------------
type
  TContourRasterizer = class(TRasterizer)
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  end;


//------------------------------------------------------------------------------
//
//      TDraftRasterizer
//
//------------------------------------------------------------------------------
// A rasterizer that trades quality for performance by pixelating the output.
// Can be used to show live preview during long operations.
// Adapted from TBoxRasterizer by Marc Lafon, 16 oct 2005
//------------------------------------------------------------------------------
type
  TDraftRasterizer = class(TRasterizer)
  private
    FPixelSize: Integer;
    procedure SetPixelSize(const Value: Integer);
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  public
    constructor Create; override;
  published
    // Size of output pixels
    property PixelSize: Integer read FPixelSize write SetPixelSize default 4;
  end;


//------------------------------------------------------------------------------
//
//      TThreadRegularRasterizer
//
//------------------------------------------------------------------------------
// Multi-threaded rasterizer using TTread
//------------------------------------------------------------------------------
// Warning: This rasterizer will have terrible performance unless the
// rasterization process is more costly than the thread setup and destruction
// (which happens once for every call to DoRasterize).
// Don't assume that threads will solve your performance problems; Benchmark!
// If possible, use TParallelRegularRasterizer or TTaskRegularRasterizer instead.
//------------------------------------------------------------------------------
type
  TThreadRegularRasterizer = class(TRasterizer)
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  end {$if defined(USE_PPL)}deprecated 'Use TMultithreadedRegularRasterizer instead'{$ifend};

//------------------------------------------------------------------------------
//
//      TParallelRegularRasterizer
//
//------------------------------------------------------------------------------
// Multi-threaded rasterizer using TParallel.For
// Note: First invocation can incur a performance penalty as the thread pool is
// initialized.
//------------------------------------------------------------------------------
{$if defined(USE_PPL)}
type
  TParallelRegularRasterizer = class(TRasterizer)
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  end;
{$ifend}


//------------------------------------------------------------------------------
//
//      TTaskRegularRasterizer
//
//------------------------------------------------------------------------------
// Multi-threaded rasterizer using TTask
// Note: First invocation can incur a performance penalty as the thread pool is
// initialized.
//------------------------------------------------------------------------------
{$if defined(USE_PPL)}
type
  TTaskRegularRasterizer = class(TRasterizer)
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  end;
{$ifend}


//------------------------------------------------------------------------------
//
//      TMultithreadedRegularRasterizer
//
//------------------------------------------------------------------------------
// Multi-threaded rasterizer using whatever is available.
//------------------------------------------------------------------------------
type
{$if defined(USE_PPL)}
  TMultithreadedRegularRasterizer = class(TParallelRegularRasterizer);
{$else}
  TMultithreadedRegularRasterizer = class(TThreadRegularRasterizer);
{$ifend}



//------------------------------------------------------------------------------
//
//      Auxiliary routines
//
//------------------------------------------------------------------------------
function CombineInfo(Bitmap: TCustomBitmap32): TCombineInfo;

const
  DEFAULT_COMBINE_INFO: TCombineInfo = (
    SrcAlpha: $FF;
    DrawMode: dmOpaque;
    CombineMode: cmBlend;
    CombineCallBack: nil;
    TransparentColor: clBlack32;
  );

//------------------------------------------------------------------------------

var
  DefaultRasterizerClass: TRasterizerClass = TRegularRasterizer;
  NumberOfProcessors: Integer = 1;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$ifndef FPC}
  System.SyncObjs,
{$endif}
{$if defined(USE_PPL)}
  System.Types,
  System.SysUtils,
  System.Threading,
{$ifend}
  Math,
  GR32_Math,
  GR32_System,
  GR32_LowLevel,
  GR32_Resamplers,
  GR32_Containers,
  GR32_OrdinalMaps;

type
  TCustomBitmap32Access = class(TCustomBitmap32);


//------------------------------------------------------------------------------
//
//      Auxiliary routines
//
//------------------------------------------------------------------------------
function CombineInfo(Bitmap: TCustomBitmap32): TCombineInfo;
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


//------------------------------------------------------------------------------
//
//      TRasterizer
//
//------------------------------------------------------------------------------
procedure TRasterizer.AssignColorBlend(var Dst: TColor32; Src: TColor32);
begin
  FBlendMemEx(Src, Dst, FSrcAlpha);
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

procedure TRasterizer.AssignTo(Dst: TPersistent);
begin
  if Dst is TRasterizer then
    SmartAssign(Self, Dst)
  else
    inherited;
end;

procedure TRasterizer.Rasterize(Dst: TCustomBitmap32; const DstRect: TRect;
  Src: TCustomBitmap32);
begin
  Rasterize(Dst, DstRect, CombineInfo(Src));
end;

procedure TRasterizer.Rasterize(Dst: TCustomBitmap32; const DstRect: TRect;
  const CombineInfo: TCombineInfo);
begin
  SetCombineInfo(CombineInfo);
  Rasterize(Dst, DstRect);
end;

procedure TRasterizer.SetCombineInfo(const CombineInfo: TCombineInfo);
begin
  with CombineInfo do
  begin
    FTransparentColor := TransparentColor;

    FSrcAlpha := SrcAlpha;
    FBlendMemEx := BLEND_MEM_EX[CombineMode]^;
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
  end;
end;

procedure TRasterizer.Rasterize(Dst: TCustomBitmap32; const DstRect: TRect);
var
  UpdateCount: Integer;
  R: TRect;
begin
  UpdateCount := TCustomBitmap32Access(Dst).UpdateCount;
  if Assigned(FSampler) then
  begin
    FSampler.PrepareSampling;
    GR32.IntersectRect(R, DstRect, Dst.BoundsRect);
    if FSampler.HasBounds then
      GR32.IntersectRect(R, DstRect, MakeRect(FSampler.GetSampleBounds, rrOutside));
    try
      DoRasterize(Dst, R);
    finally
      while TCustomBitmap32Access(Dst).UpdateCount > UpdateCount do
        TCustomBitmap32Access(Dst).EndUpdate;
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

procedure TRasterizer.Rasterize(Dst: TCustomBitmap32);
begin
  Rasterize(Dst, Dst.BoundsRect);
end;

constructor TRasterizer.Create;
begin
  inherited;
  SetCombineInfo(DEFAULT_COMBINE_INFO);
end;

procedure TRasterizer.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TCustomBitmap32 then
      SetCombineInfo(CombineInfo(TCustomBitmap32(Source)))
    else
      inherited;
    Changed;
  finally
    EndUpdate;
  end;
end;


//------------------------------------------------------------------------------
//
//      TRegularRasterizer
//
//------------------------------------------------------------------------------
constructor TRegularRasterizer.Create;
begin
  inherited;
  FUpdateRowCount := 0;
end;

procedure TRegularRasterizer.DoRasterize(Dst: TCustomBitmap32; DstRect: TRect);
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
    Dst.Changed(Rect(Left, Bottom - UpdateCount - 1, Right, Bottom));
end;


//------------------------------------------------------------------------------
//
//      TSwizzlingRasterizer
//
//------------------------------------------------------------------------------
constructor TSwizzlingRasterizer.Create;
begin
  inherited;
  FBlockSize := 3;
end;

procedure TSwizzlingRasterizer.DoRasterize(Dst: TCustomBitmap32; DstRect: TRect);
var
  I, L, T, W, H, Size, RowSize, D: Integer;
  P1, P2, PBlock: TPoint;
  GetSample: TGetSampleInt;
  ForwardBuffer: array of Integer;

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
  Size := NextPowerOf2(Max(W, H));

  SetLength(ForwardBuffer, Size + 1);

  I := 2;
  while I <= Size do
  begin
    ForwardBuffer[I] := ForwardBuffer[I shr 1] + 1;
    Inc(I, 2);
  end;

  Size := W * H - 1;
  GetSample := FSampler.GetSampleInt;

  D := 1 shl FBlockSize;
  PBlock := GR32.Point(L + D, T + D);
  P1 := GR32.Point(-1, 0);

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


//------------------------------------------------------------------------------
//
//      TProgressiveRasterizer
//
//------------------------------------------------------------------------------
constructor TProgressiveRasterizer.Create;
begin
  inherited;
  FSteps := 4;
  FUpdateRows := True;
end;

{$DEFINE UseInternalFill}

procedure TProgressiveRasterizer.DoRasterize(Dst: TCustomBitmap32;
  DstRect: TRect);
var
  I, J, Shift, W, H, B, Wk, Hk, X, Y: Integer;
  DoUpdate: Boolean;
  OnChanged: TAreaChangedEvent;
  Step: Integer;
  GetSample: TGetSampleInt;

{$IFDEF UseInternalFill}
  Bits: PColor32Array;

procedure IntFillRect(X1, Y1, X2, Y2: Integer; C: TColor32);
var
  Y: Integer;
  P: PColor32Array;
begin
  for Y := Y1 to Y2 - 1 do
  begin
    P := Pointer(@Bits[Y * W]);
    FillLongword(P[X1], X2 - X1, C);
  end;
end;
{$ENDIF}

begin
  GetSample := FSampler.GetSampleInt;
  OnChanged := Dst.OnAreaChanged;
{$IFDEF UseInternalFill}
  Bits := Dst.Bits;
{$ENDIF}
  DoUpdate := (TCustomBitmap32Access(Dst).UpdateCount = 0) and Assigned(OnChanged);
  W := DstRect.Right - DstRect.Left;
  H := DstRect.Bottom - DstRect.Top;
  J := DstRect.Top;
  Step := 1 shl FSteps;
  while J < DstRect.Bottom do
  begin
    I := DstRect.Left;
    B := Min(J + Step, DstRect.Bottom);
    while I < DstRect.Right - Step do
    begin
      {$IFDEF UseInternalFill}
      IntFillRect(I, J, I + Step, B, GetSample(I, J));
      {$ELSE}
      Dst.FillRect(I, J, I + Step, B, GetSample(I, J));
      {$ENDIF}
      Inc(I, Step);
    end;
    {$IFDEF UseInternalFill}
    IntFillRect(I, J, DstRect.Right, B, GetSample(I, J));
    if DoUpdate and FUpdateRows then
      OnChanged(Dst, Rect(DstRect.Left, J, DstRect.Right, B), AREAINFO_RECT);
    {$ELSE}
    Dst.FillRect(I, J, DstRect.Right, B, GetSample(I, J));
    {$ENDIF}
    Inc(J, Step);
  end;
  if DoUpdate and (not FUpdateRows) then OnChanged(Dst, DstRect, AREAINFO_RECT);

  Shift := FSteps;
  while Step > 1 do
  begin
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
          {$IFDEF UseInternalFill}
          IntFillRect(X, Y, X + Step, B, GetSample(X, Y));
          {$ELSE}
          Dst.FillRect(X, Y, X + Step, B, GetSample(X, Y));
          {$ENDIF}
        end
      else
        for I := 0 to Wk do
          if Odd(I) then
          begin
            X := DstRect.Left + I shl Shift;
            {$IFDEF UseInternalFill}
            IntFillRect(X, Y, X + Step, B, GetSample(X, Y));
            {$ELSE}
            Dst.FillRect(X, Y, X + Step, B, GetSample(X, Y));
            {$ENDIF}
          end;
      X := DstRect.Left + Wk shl Shift;
      {$IFDEF UseInternalFill}
      IntFillRect(X, Y, DstRect.Right, B, GetSample(X, Y));
      if FUpdateRows and DoUpdate then
        OnChanged(Dst, Rect(DstRect.Left, Y, DstRect.Right, B), AREAINFO_RECT);
      {$ELSE}
      Dst.FillRect(X, Y, DstRect.Right, B, GetSample(X, Y));
      {$ENDIF}
    end;
    if DoUpdate and (not FUpdateRows) then OnChanged(Dst, DstRect, AREAINFO_RECT);
  end;
end;

procedure TProgressiveRasterizer.SetSteps(const Value: Integer);
begin
  if FSteps <> Value then
  begin
    FSteps := Value;
    Changed;
  end;
end;

procedure TProgressiveRasterizer.SetUpdateRows(const Value: Boolean);
begin
  if FUpdateRows <> Value then
  begin
    FUpdateRows := Value;
    Changed;
  end;
end;


//------------------------------------------------------------------------------
//
//      TTesseralRasterizer
//
//------------------------------------------------------------------------------
procedure TTesseralRasterizer.DoRasterize(Dst: TCustomBitmap32; DstRect: TRect);
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
      Dst.Changed(Rect(X2, Y, X2 + 1, Y + Height));
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
      Dst.Changed(Rect(X, Y2, X + Width, Y2 + 1));
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


//------------------------------------------------------------------------------
//
//      TContourRasterizer
//
//------------------------------------------------------------------------------
procedure InflateRect(const P: TPoint; var R: TRect);
begin
  if P.X < R.Left then R.Left := P.X;
  if P.Y < R.Top then R.Top := P.Y;
  if P.X >= R.Right then R.Right := P.X + 1;
  if P.Y >= R.Bottom then R.Bottom := P.Y + 1;
end;

procedure TContourRasterizer.DoRasterize(Dst: TCustomBitmap32; DstRect: TRect);
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

    PLast := GR32.Point(DstRect.Left, DstRect.Top);
    CLast := GetSample(PLast.X, PLast.Y);
    AssignColor(Dst.PixelPtr[PLast.X, PLast.Y]^, CLast);

    UpdateRect := Rect(PLast.X, PLast.Y, PLast.X + 1, PLast.Y + 1);
    while True do
    begin
      MainLoop:

      Diff := MaxInt;

      // forward
      with COORDS[Dir] do P := GR32.Point(PLast.X + X, PLast.Y + Y);
      if GR32.PtInRect(DstRect, P) and (not Visited[P.X, P.Y]) then
      begin
        C := GetSample(P.X, P.Y);
        Diff := Intensity(ColorSub(C, CLast));
        NewDir := Dir;
        AssignColor(Dst.PixelPtr[P.X, P.Y]^, C);
        Visited[P.X - DstRect.Left, P.Y - DstRect.Top] := True;
        InflateRect(P, UpdateRect);
      end;

      // left
      with COORDS[LEFT[Dir]] do P := GR32.Point(PLast.X + X, PLast.Y + Y);
      if GR32.PtInRect(DstRect, P) and (not Visited[P.X, P.Y]) then
      begin
        C := GetSample(P.X, P.Y);
        D := Intensity(ColorSub(C, CLast));
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
      with COORDS[RIGHT[Dir]] do P := GR32.Point(PLast.X + X, PLast.Y + Y);
      if GR32.PtInRect(DstRect, P) and (not Visited[P.X, P.Y]) then
      begin
        C := GetSample(P.X, P.Y);
        D := Intensity(ColorSub(C, CLast));
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
              PLast := GR32.Point(DstRect.Left + I, DstRect.Top + J);
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
      with COORDS[Dir] do PLast := GR32.Point(PLast.X + X, PLast.Y + Y);
      CLast := Dst[PLast.X, PLast.Y];
    end;

  finally
    Visited.Free;
  end;
end;


//------------------------------------------------------------------------------
//
//      TDraftRasterizer
//
//------------------------------------------------------------------------------
constructor TDraftRasterizer.Create;
begin
  inherited;
  FPixelSize := 4;
end;

procedure TDraftRasterizer.DoRasterize(Dst: TCustomBitmap32; DstRect: TRect);
var
  r: TRect;
  GetSample: TGetSampleInt;
begin
  GetSample := Sampler.GetSampleInt;

  Dst.BeginLockUpdate;
  try

    r.Top := DstRect.Top;
    r.Bottom := r.Top;
    while r.Top < DstRect.Bottom do
    begin
      Inc(r.Bottom, FPixelSize);
      if (r.Bottom > DstRect.Bottom) then
        r.Bottom := DstRect.Bottom;

      r.Left := DstRect.Left;
      r.Right := r.Left + FPixelSize;
      while r.Right < DstRect.Right do
      begin

        Dst.FillRect(r.Left, r.Top, r.Right, r.Bottom, GetSample(r.Left, r.Top));

        r.Left := r.Right;
        Inc(r.Right, FPixelSize);

      end;
      Dst.FillRect(r.Left, r.Top, DstRect.Right, r.Bottom, GetSample(r.Left, r.Top));

      r.Top := r.Bottom;
    end;

  finally
    Dst.EndLockUpdate;
  end;
  if (TCustomBitmap32Access(Dst).UpdateCount = 0) and Assigned(Dst.OnAreaChanged) then
    Dst.OnAreaChanged(Dst, DstRect, AREAINFO_RECT);
end;

procedure TDraftRasterizer.SetPixelSize(const Value: Integer);
begin
  if (FPixelSize <> Value) and (Value > 1) then
  begin
    FPixelSize := Value;
    Changed;
  end;
end;


//------------------------------------------------------------------------------
//
//      TParallelRegularRasterizer
//
//------------------------------------------------------------------------------
{$if defined(USE_PPL)}
procedure TParallelRegularRasterizer.DoRasterize(Dst: TCustomBitmap32; DstRect: TRect);
begin
  TParallel.For(DstRect.Top, DstRect.Bottom-1,
    procedure(ScanLine: integer)
    var
      i: Integer;
      p: PColor32;
    begin
      p := @Dst.Bits[DstRect.Left + ScanLine * Dst.Width];

      for i := DstRect.Left to DstRect.Right - 1 do
      begin
        AssignColor(p^, Sampler.GetSampleInt(i, ScanLine));
        Inc(p);
      end;
    end);

  Dst.Changed(DstRect);
end;
{$ifend}


//------------------------------------------------------------------------------
//
//      TTaskRegularRasterizer
//
//------------------------------------------------------------------------------
{$if defined(USE_PPL)}
type
  TScanlineProc = reference to procedure(AFromIndex, AToIndex: integer);

procedure TTaskRegularRasterizer.DoRasterize(Dst: TCustomBitmap32; DstRect: TRect);

  // Partitioning and task setup based on idea by Stefan Glienke
  // https://stackoverflow.com/a/27542557

  procedure CalcPartBounds(Low, High, Count, Index: Integer; out Min, Max: Integer);
  var
    Len: Integer;
  begin
    Len := High - Low + 1;
    Min := (Len div Count) * Index;
    if Index + 1 < Count then
      Max := Len div Count * (Index + 1) - 1
    else
      Max := Len - 1;
  end;

  function GetWorker(const ScanlineProc: TScanlineProc; Min, Max: Integer): ITask;
  begin
    Result := TTask.Run(
      procedure
      begin
        ScanlineProc(Min, Max);
      end);
  end;

var
  Workers: TArray<ITask>;
  i: integer;
  Min, Max: integer;
begin
  SetLength(Workers, NumberOfProcessors);

  for i := 0 to High(Workers) do
  begin
    CalcPartBounds(DstRect.Top, DstRect.Bottom-1, NumberOfProcessors, i, Min, Max);

    Workers[i] := GetWorker(
      procedure (AFromScanLine, AToScanLine: integer)
      var
        i: Integer;
        p: PColor32;
      begin
        while (AFromScanLine <= AToScanLine) do
        begin
          p := @Dst.Bits[DstRect.Left + AFromScanLine * Dst.Width];

          for i := DstRect.Left to DstRect.Right - 1 do
          begin
            AssignColor(p^, Sampler.GetSampleInt(i, AFromScanLine));
            Inc(p);
          end;

          Inc(AFromScanLine);
        end;
      end, Min, Max);
  end;

  TTask.WaitForAll(Workers);

  Dst.Changed(DstRect);
end;
{$ifend}


//------------------------------------------------------------------------------
//
//      TThreadRegularRasterizer
//
//------------------------------------------------------------------------------
type
  TLineRasterizerData = record
    ScanLine: Integer;
  end;
  PLineRasterizerData = ^TLineRasterizerData;

  TScanLineRasterizerThread = class(TThread)
  protected
    Data: PLineRasterizerData;
    DstRect: TRect;
    Dst: TCustomBitmap32;
    GetSample: TGetSampleInt;
    AssignColor: TAssignColor;
    procedure Execute; override;
  end;

procedure TScanLineRasterizerThread.Execute;
var
  ScanLine: Integer;
  I: Integer;
  P: PColor32;
begin
{$ifndef FPC}
  ScanLine := TInterlocked.Increment(Data^.ScanLine);
{$else}
  ScanLine := InterlockedIncrement(Data^.ScanLine);
{$endif}
  while ScanLine < DstRect.Bottom do
  begin
    P := @Dst.Bits[DstRect.Left + ScanLine * Dst.Width];

    for I := DstRect.Left to DstRect.Right - 1 do
    begin
      AssignColor(P^, GetSample(I, ScanLine));
      Inc(P);
    end;

{$ifndef FPC}
    ScanLine := TInterlocked.Increment(Data^.ScanLine);
{$else}
    ScanLine := InterlockedIncrement(Data^.ScanLine);
{$endif}
  end;
end;

procedure TThreadRegularRasterizer.DoRasterize(Dst: TCustomBitmap32; DstRect: TRect);
var
  I: Integer;
  Threads: array of TScanLineRasterizerThread;
  Data: TLineRasterizerData;

  function CreateThread: TScanLineRasterizerThread;
  begin
    Result := TScanLineRasterizerThread.Create(True);
    Result.Data := @Data;
    Result.DstRect := DstRect;
    Result.GetSample := Sampler.GetSampleInt;
    Result.AssignColor := AssignColor;
    Result.Dst := Dst;
  {$IFDEF USETHREADRESUME}
    Result.Resume;
  {$ELSE}
    Result.Start;
  {$ENDIF}
  end;

begin
  Data.ScanLine := DstRect.Top - 1;

  { Start Threads }
  SetLength(Threads, NumberOfProcessors);
  try
    for I := 0 to NumberOfProcessors - 1 do
      Threads[I] := CreateThread;

    { Wait for Threads to be ready }
    for I := 0 to High(Threads) do
    begin
      Threads[I].WaitFor;
      Threads[I].Free;
    end;

  finally
    Dst.Changed(DstRect);
  end;
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  NumberOfProcessors := GetProcessorCount;
{$IFDEF USEMULTITHREADING}
  if NumberOfProcessors > 1 then
    DefaultRasterizerClass := TMultithreadedRegularRasterizer;
{$ENDIF}


end.
