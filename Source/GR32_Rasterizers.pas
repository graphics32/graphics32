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
 * Contributor(s):
 *   Steffen Binas <steffen.binas@aquasoft.de>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  {$IFDEF Windows}
    Windows,
  {$ENDIF}
{$ELSE}
  Windows,
{$ENDIF}
  Classes, GR32, GR32_Blend, GR32_OrdinalMaps;

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
  { A base class for TCustomBitmap32-specific rasterizers. }
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

  { TRegularSamplingRasterizer }
  { This rasterizer simply picks one sample for each pixel in the output bitmap. }
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
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  public
    constructor Create; override;
  published
    property BlockSize: Integer read FBlockSize write SetBlockSize default 3;
  end;

  { TProgressiveRasterizer }
  { This class will perform rasterization in a progressive manner. It performs
    subsampling with a block size of 2^n and will successively decrease n in
    each iteration until n equals zero.  }
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

  { TTesseralRasterizer }
  { This is a recursive rasterization method. It uses a divide-and-conquer
    scheme to subdivide blocks vertically and horizontally into smaller blocks. }
  TTesseralRasterizer = class(TRasterizer)
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  end;

  { TContourRasterizer }
  TContourRasterizer = class(TRasterizer)
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  end;

  { TMultithreadedRegularRasterizer }
  TMultithreadedRegularRasterizer = class(TRasterizer)
  protected
    procedure DoRasterize(Dst: TCustomBitmap32; DstRect: TRect); override;
  end;

{ Auxiliary routines }
function CombineInfo(Bitmap: TCustomBitmap32): TCombineInfo;

const
  DEFAULT_COMBINE_INFO: TCombineInfo = (
    SrcAlpha: $FF;
    DrawMode: dmOpaque;
    CombineMode: cmBlend;
    CombineCallBack: nil;
    TransparentColor: clBlack32;
  );

var
  DefaultRasterizerClass: TRasterizerClass = TRegularRasterizer;
  NumberOfProcessors: Integer = 1;

implementation

uses
  GR32_Resamplers, GR32_Containers, GR32_System, Math, SysUtils;

type
  TThreadPersistentAccess = class(TThreadPersistent);

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
  UpdateCount := TThreadPersistentAccess(Dst).UpdateCount;
  if Assigned(FSampler) then
  begin
    FSampler.PrepareSampling;
    IntersectRect(R, DstRect, Dst.BoundsRect);
    if FSampler.HasBounds then
      IntersectRect(R, DstRect, MakeRect(FSampler.GetSampleBounds, rrOutside));
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
  finally
    EndUpdate;
    Changed;
  end;
end;

{ TRegularRasterizer }

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

{ TSwizzlingRasterizer }

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
  FSteps := 4;
  FUpdateRows := True;
end;

procedure TProgressiveRasterizer.DoRasterize(Dst: TCustomBitmap32;
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
        Dst.FillRect(I, J, I + Step, B, GetSample(I, J));
        Inc(I, Step);
      end;
      Dst.FillRect(I, J, DstRect.Right, B, GetSample(I, J));
      if DoUpdate and FUpdateRows then
        OnChanged(Dst, Rect(DstRect.Left, J, DstRect.Right, B), AREAINFO_RECT);
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
        if FUpdateRows and DoUpdate then
          OnChanged(Dst, Rect(DstRect.Left, Y, DstRect.Right, B), AREAINFO_RECT);
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

{ TTesseralRasterizer }

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


{ TContourRasterizer }

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

{ TMultithreadedRegularRasterizer }

procedure TMultithreadedRegularRasterizer.DoRasterize(Dst: TCustomBitmap32; DstRect: TRect);
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
  {$IFDEF COMPILER2010}
    Result.Start;
  {$ELSE}
    Result.Resume;
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

{ TLineRasterizerThread }

procedure TScanLineRasterizerThread.Execute;
var
  ScanLine: Integer;
  I: Integer;
  P: PColor32;
begin
  ScanLine := InterlockedIncrement(Data^.ScanLine);
  while ScanLine < DstRect.Bottom do
  begin
    P := @Dst.Bits[DstRect.Left + ScanLine * Dst.Width];

    for I := DstRect.Left to DstRect.Right - 1 do
    begin
      AssignColor(P^, GetSample(I, ScanLine));
      Inc(P);
    end;

    ScanLine := InterlockedIncrement(Data^.ScanLine);
  end;
end;

initialization
  NumberOfProcessors := GetProcessorCount;
{$IFDEF USEMULTITHREADING}
  if NumberOfProcessors > 1 then
    DefaultRasterizerClass := TMultithreadedRegularRasterizer;
{$ENDIF}


end.
