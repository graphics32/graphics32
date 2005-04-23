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

  { TRasterizer }
  { A base class for TBitmap32-specific rasterizers. }
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

  { TSuperSamplingRasterizer }
  { This class implements ordinary supersampling, where samples are gathered
    from a regular square grid for each pixel. }
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

  { TMultiSamplingRasterizer}
  { Multisampling means that you use a predefined sampling pattern for each
    output pixel. Several output pixels may also share the same samples
    (if they are located on the edge between two pixels). }
  TSample = record
    Coord: TFloatPoint;
    Weight: Single;
  end;
  TSampleList = array of TSample;
  TSamplePattern = array of array of TSampleList;

  TFixedSample = record
    Coord: TFixedPoint;
    Shift: Integer;
  end;
  TFixedSampleList = array of TFixedSample;
  TFixedSamplePattern = array of array of TFixedSampleList;

  TSampleMode = (smSingle, smFixed);

  TMultiSamplingRasterizer = class(TRegularRasterizer)
  private
    FPattern: TSamplePattern;
    FSampleMode: TSampleMode;
    FFixedPattern: TFixedSamplePattern;
    FShiftRight: Integer;
    procedure SetPattern(const Value: TSamplePattern);
  protected
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); override;
  published
    property Pattern: TSamplePattern read FPattern write SetPattern;
  end;

const
  Quincunx: array [0..4] of TSample = (
    (Coord: (X: -0.5; Y: -0.5); Weight: 1),
    (Coord: (X:  0.5; Y: -0.5); Weight: 1),
    (Coord: (X:  0.5; Y:  0.5); Weight: 1),
    (Coord: (X: -0.5; Y:  0.5); Weight: 1),
    (Coord: (X:    0; Y:    0); Weight: 4)
  );

type
  TIrregularRasterizer = class(TRasterizer);

type
  TThreadPersistentAccess = class(TThreadPersistent);

{ Auxiliary routines }

function CombineInfo(Bitmap: TBitmap32): TCombineInfo;

implementation

uses GR32_Resamplers, Math, SysUtils;

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
var
  UpdateCount: Integer;
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

  UpdateCount := TThreadPersistentAccess(Dst).UpdateCount;
  if Assigned(FSampler) then
  begin
    FSampler.PrepareRasterization;
    try
      DoRasterize(Dst, DstRect);
    finally
      while TThreadPersistentAccess(Dst).UpdateCount > UpdateCount do
        TThreadPersistentAccess(Dst).EndUpdate;
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
      OnChanged(Dst, Rect(DstRect.Left, J, DstRect.Right, J + Step));
    Inc(J, Step);
  end;
  if DoUpdate and (not FProgressLines) then OnChanged(Dst, DstRect);

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
        OnChanged(Dst, Rect(DstRect.Left, Y, DstRect.Right, Y + Step));
    end;
    if DoUpdate and (not FProgressLines) then OnChanged(Dst, DstRect);
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
    W := Right - Left + 1;
    H := Bottom - Top + 1;
    for I := Left to Right do AssignColor(Dst.PixelPtr[I, Top]^, GetSample(I, Top));
    Dst.Changed(Rect(Left, Top, Right, Top + 1));
    for I := Top to Bottom do AssignColor(Dst.PixelPtr[Left, I]^, GetSample(Left, I));
    Dst.Changed(Rect(Left, Top, Left + 1, Bottom));
    if W > H then
      SplitVertical(DstRect.Left, DstRect.Top, W, H)
    else
      SplitHorizontal(DstRect.Left, DstRect.Top, W, H);
  end;

end;


function GetSampleMode(Pattern: TSamplePattern): TSampleMode;
begin

end;

{ TMultiSamplingRasterizer }

procedure TMultiSamplingRasterizer.DoRasterize(Dst: TBitmap32;
  DstRect: TRect);
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

procedure TMultiSamplingRasterizer.SetPattern(const Value: TSamplePattern);
begin
  FPattern := Value;
  FSampleMode := GetSampleMode(Value);
  Changed;
end;

end.
