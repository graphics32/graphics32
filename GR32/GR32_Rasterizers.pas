unit GR32_Rasterizers;

interface

{$I GR32.inc}

uses
  {$IFDEF CLX}
  Qt, Types, {$IFDEF LINUX}Libc, {$ELSE}Windows, {$ENDIF}
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes, GR32, GR32_Blend;

type
  TAssignColor = procedure(var Dst: TColor32; Src: TColor32) of object;

  PCombineInfo = ^TCombineInfo;
  TCombineInfo = record
    SrcAlpha: Integer;
    DrawMode: TDrawMode;
    CombineMode: TCombineMode;
    CombineCallBack: TPixelCombineEvent;
  end;

  TGetSampleInt = function(X, Y: Integer): TColor32 of object;
  TGetSampleFloat = function(X, Y: Single): TColor32 of object;
  TGetSampleFixed = function(X, Y: TFixed): TColor32 of object;

type
  TCustomRasterizer = class(TThreadPersistent);

  { TBitmap32 specific rasterizer }
  TRasterizer = class(TCustomRasterizer)
  private
    FSampler: TCustomSampler;
    FSrcAlpha: Integer;
    FBlendMemEx: TBlendMemEx;
    FCombineCallBack: TPixelCombineEvent;
    FAssignColor: TAssignColor;
    procedure SetSampler(const Value: TCustomSampler);
    procedure AssignColorOpaque(var Dst: TColor32; Src: TColor32);
    procedure AssignColorBlend(var Dst: TColor32; Src: TColor32);
    procedure AssignColorCustom(var Dst: TColor32; Src: TColor32);
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

  TSuperSamplingRasterizer = class(TRegularRasterizer)
  private
    FSamplingX: Integer;
    FSamplingY: Integer;
    procedure SetSamplingX(const Value: Integer);
    procedure SetSamplingY(const Value: Integer);
  protected
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); override;
  public
    constructor Create; override;
  published
    property SamplingX: Integer read FSamplingX write SetSamplingX;
    property SamplingY: Integer read FSamplingY write SetSamplingY;
  end;

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

  TProgressiveRasterizer = class(TRasterizer)
  private
    FLevel: Integer;
    procedure SetLevel(const Value: Integer);
  protected
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); override;
  public
    constructor Create; override;
  published
    property Level: Integer read FLevel write SetLevel;
  end;

  TTesseralRasterizer = class(TRasterizer)
  private
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); override;
  end;

(*
  { Adaptive supersampling }
  TAdaptiveRasterizer = class(TRasterizer)
  private
    FMinOffset: Single;
    function DoRecurse(X, Y, Offset: Single; const A, B, C, D, E: TSample): TFloatColor;
    function QuadrantColor(const S1, S2: TSample; X, Y, Offset: Single;
      Proc: TRecurseProc): TFloatColor;
    function RecurseAC(X, Y, Offset: Single; const A, C: TSample): TFloatColor;
    function RecurseBD(X, Y, Offset: Single; const B, D: TSample): TFloatColor;
    function GetMaxIter: Integer;
    procedure SetMaxIter(const Value: Integer);
  protected
    function CompareSamples(S1, S2: TSample): Boolean; virtual; abstract;
  public
    constructor Create;
    procedure PerformSampling(Dst: TBitmap32; const ClipRect: TRect); override;
  published
    property MinOffset: Single read FMinOffset write FMinOffset;
    property MaxIter: Integer read GetMaxIter write SetMaxIter;
  end;
*)

  TMultiSamplingRasterizer = class(TRegularRasterizer);

  TIrregularRasterizer = class(TRasterizer);

{ Auxiliary routines }

function CombineInfo(Bitmap: TBitmap32): TCombineInfo;

implementation

uses GR32_Resamplers, Math, SysUtils;

const
  SSamplerNotAssigned = 'No sampler has been assigned to rasterizer';

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
    CombineCallBack: nil
  );
begin
  Rasterize(Dst, DstRect, DEFAULT_COMBINE_INFO);
end;

procedure TRasterizer.Rasterize(Dst: TBitmap32; const DstRect: TRect;
  const CombineInfo: TCombineInfo);
begin
  with CombineInfo do
    Rasterize(Dst, DstRect, SrcAlpha, DrawMode, CombineMode, CombineCallBack);
end;

procedure TRasterizer.Rasterize(Dst: TBitmap32; const DstRect: TRect;
  SrcAlpha: TColor32; DrawMode: TDrawMode; CombineMode: TCombineMode;
  CombineCallBack: TPixelCombineEvent);
begin
  FSrcAlpha := SrcAlpha;
  FBlendMemEx := BLEND_MEM_EX[CombineMode];
  FCombineCallBack := CombineCallBack;

  // for the sake of speed use direct mapping to avoid unnecessary checks...
  case DrawMode of
    dmOpaque: FAssignColor := AssignColorOpaque;
    dmBlend:  FAssignColor := AssignColorBlend;
  else
    if Assigned(FCombineCallback) then
      FAssignColor := AssignColorCustom
    else
      FAssignColor := AssignColorBlend;
  end;

  //if not Assigned(FSampler) then
  //  raise Exception.Create(SSamplerNotAssigned)
  //else
  if Assigned(FSampler) then
  begin
    FSampler.PrepareRasterization;
    try
      DoRasterize(Dst, DstRect);
    finally
      FSampler.FinalizeRasterization;
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
var
  R: TRect;
begin
  R := Dst.BoundsRect;
  Dec(R.Bottom);
  Dec(R.Right);
  Rasterize(Dst, R);
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
  for J := DstRect.Top to DstRect.Bottom do
  begin
    P := @Dst.Bits[DstRect.Left + J * Dst.Width];
    for I := DstRect.Left to DstRect.Right do
    begin
      AssignColor(P^, GetSample(I, J));
      Inc(P);
    end;
    Inc(UpdateCount);
    if UpdateCount = FUpdateRowCount then
    begin
      Dst.Changed(Rect(DstRect.Left, J - UpdateCount, DstRect.Right + 1, J));
      UpdateCount := 0;
    end;
  end;
  with DstRect do
    Dst.Changed(Rect(Left, Bottom - UpdateCount, Right, Bottom + 1));
end;

{ TSuperSamplingRasterizer }

constructor TSuperSamplingRasterizer.Create;
begin
  inherited;
  SamplingX := 1;
  SamplingY := 1;
end;

procedure TSuperSamplingRasterizer.DoRasterize(Dst: TBitmap32; DstRect: TRect);
var
  I, J, U, V, SX, SY, UpdateCount: Integer;
  Scale: TFixed;
  X, Y, Tx, Ty, BTx, BX, lx, ly, dx, dy: TFixed;
  Buffer: TBufferEntry;
  P: PColor32;
  GetSample: TGetSampleFixed;
begin
  if (SamplingX = 1) and (SamplingY = 1) then
  begin
    inherited;
    Exit;
  end;

  dx := Fixed(1 / FSamplingX);
  dy := Fixed(1 / FSamplingY);
  lx := Fixed(((1 / FSamplingX) - 1) / 2);
  ly := Fixed(((1 / FSamplingY) - 1) / 2);
  Scale := Fixed(1 / (FSamplingX * FSamplingY));
  GetSample := FSampler.GetSampleFixed;
  SX := (SamplingX - 1) * dx;
  SY := (SamplingY - 1) * dy;

  Y := (DstRect.Top - 1) * FixedOne + ly;
  Ty := Y + SY;

  BX := (DstRect.Left - 1) * FixedOne + lx;
  BTx := BX + SX;

  UpdateCount := 0;

  for J := DstRect.Top to DstRect.Bottom do
  begin
    P := @Dst.Bits[DstRect.Left + J * Dst.Width];
    Inc(Y, FixedOne);
    Inc(Ty, FixedOne);

    X := BX;
    Tx := BTx;

    for I := DstRect.Left to DstRect.Right do
    begin
      Buffer := EMPTY_ENTRY;
      Inc(X, FixedOne);
      Inc(Tx, FixedOne);

      V := Ty;
      repeat
        U := Tx;
        repeat
          IncBuffer(Buffer, GetSample(U, V));
          Dec(U, dx);
        until U < X;
        Dec(V, dy);
      until V < Y;

      MultiplyBuffer(Buffer, Scale);
      AssignColor(P^, BufferToColor32(Buffer, 16));
      Inc(P);
    end;
    Inc(UpdateCount);
    if UpdateCount = FUpdateRowCount then
    begin
      Dst.Changed(Rect(DstRect.Left, J - UpdateCount, DstRect.Right + 1, J));
      UpdateCount := 0;
    end;
  end;
  with DstRect do
    Dst.Changed(Rect(Left, Bottom - UpdateCount, Right, Bottom + 1));
end;

procedure TSuperSamplingRasterizer.SetSamplingX(const Value: Integer);
begin
  if (FSamplingX <> Value) and (Value > 0) then
  begin
    FSamplingX := Value;
    Changed;
  end;
end;

procedure TSuperSamplingRasterizer.SetSamplingY(const Value: Integer);
begin
  if (FSamplingY <> Value) and (Value > 0) then
  begin
    FSamplingY := Value;
    Changed;
  end;
end;


{ TSwizzlingRasterizer }

constructor TSwizzlingRasterizer.Create;
begin
  inherited;
  FBlockSize := 3;
end;

procedure TSwizzlingRasterizer.DoRasterize(Dst: TBitmap32; DstRect: TRect);
var
  I, J, L, T, W, H, Size, RowSize, D: Integer;
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
  W := DstRect.Right - DstRect.Left + 1;
  H := DstRect.Bottom - DstRect.Top + 1;
  L := DstRect.Left; T := DstRect.Top;
  Size := 1 shl (ScanReverse(Max(W, H)) + 1) + 1;

  SetLength(ForwardBuffer, Size);

  I := 2;
  while I < Size do
  begin
    ForwardBuffer[I] := ForwardBuffer[I shr 1] + 1;
    Inc(I, 2);
  end;

  Dst.Clear(clBlack32);
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
end;

procedure TProgressiveRasterizer.DoRasterize(Dst: TBitmap32;
  DstRect: TRect);
var
  I, J, Shift, L, T, W, H, Wk, Hk, X, Y: Integer;
  Step: Integer;
  GetSample: TGetSampleInt;
begin
  GetSample := FSampler.GetSampleInt;
  Step := 1 shl FLevel;

  with DstRect do
  begin
    L := Left;
    T := Top;
    W := Right - Left;
    H := Bottom - Top;
  end;
  Dst.BeginUpdate;
  J := DstRect.Top;
  while J < DstRect.Bottom do
  begin
    I := DstRect.Left;
    while I < DstRect.Right do
    begin
      Dst.FillRectS(I, J, I + Step, J + Step, GetSample(I, J));
      Inc(I, Step);
    end;
    Inc(J, Step);
  end;
  Dst.EndUpdate;
  Dst.Changed;

  Shift := FLevel;
  repeat
    Dec(Shift);
    Dst.BeginUpdate;
    Step := Step div 2;
    Wk := W div Step;
    Hk := H div Step;
    for J := 0 to Hk do
    begin
      Y := T + J shl Shift;
      if Odd(J) then
        for I := 0 to Wk do
        begin
          X := L + I shl Shift;
          Dst.FillRectS(X, Y, X + Step, Y + Step, GetSample(X, Y));
        end
      else
        for I := 0 to Wk do
          if Odd(I) then
          begin
            X := L + I shl Shift;
            Dst.FillRectS(X, Y, X + Step, Y + Step, GetSample(X, Y));
          end;
    end;
    Dst.EndUpdate;
    Dst.Changed;
  until Step = 1;
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
  W, H: Integer;
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
      for I := Y to Y + Height - 1 do
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
      for I := X to X + Width - 1 do
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
    W := Right - Left + 1;
    H := Bottom - Top + 1;
  end;

  if W > H then
    SplitVertical(DstRect.Left, DstRect.Top, W, H)
  else
    SplitHorizontal(DstRect.Left, DstRect.Top, W, H);
end;

end.
