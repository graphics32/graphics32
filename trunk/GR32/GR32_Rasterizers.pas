unit GR32_Rasterizers;

interface

uses
  Windows, Classes, GR32, GR32_Blend;

type
  TAssignColor = procedure(var Dst: TColor32; Src: TColor32) of object;

  PCombineInfo = ^TCombineInfo;
  TCombineInfo = record
    SrcAlpha: Integer;
    DrawMode: TDrawMode;
    CombineMode: TCombineMode;
    CombineCallBack: TPixelCombineEvent;
  end;

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
    procedure Rasterize(Dst: TBitmap32; const DstRect: TRect); overload;
    procedure Rasterize(Dst: TBitmap32; const DstRect: TRect; const CombineInfo: TCombineInfo); overload;
    procedure Rasterize(Dst: TBitmap32; const DstRect: TRect; Src: TBitmap32); overload;
  published
    property Sampler: TCustomSampler read FSampler write SetSampler;
  end;

  TRasterizerClass = class of TRasterizer;

  TRegularRasterizer = class(TRasterizer)
  protected
    procedure DoRasterize(Dst: TBitmap32; DstRect: TRect); override;
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

function CombineInfo(Bitmap: TBitmap32): TCombineInfo;

implementation

uses GR32_Resamplers, SysUtils;

const
  SSamplerNotAssigned = 'No sampler has been assigned to rasterizer';

type
  TGetSampleInt = function(X, Y: Integer): TColor32 of object;
  TGetSampleFloat = function(X, Y: Single): TColor32 of object;
  TGetSampleFixed = function(X, Y: TFixed): TColor32 of object;

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


procedure IncBuffer(var Buffer: TBufferEntry; Color: TColor32);
begin
  with TColor32Entry(Color) do
  begin
    Inc(Buffer.B, B);
    Inc(Buffer.G, G);
    Inc(Buffer.R, R);
    Inc(Buffer.A, A);
  end;
end;

procedure MultiplyBuffer(var Buffer: TBufferEntry; M: Integer);
begin
  Buffer.B := Buffer.B * M;
  Buffer.G := Buffer.G * M;
  Buffer.R := Buffer.R * M;
  Buffer.A := Buffer.A * M;
end;

function BufferToColor32(Buffer: TBufferEntry; Shift: Integer): TColor32; overload;
var
  Rounding: Integer;
begin
  Rounding := $7FFFFFFF shr Shift;
  with TColor32Entry(Result) do
  begin
    B := (Buffer.B + Rounding) shr Shift;
    G := (Buffer.G + Rounding) shr Shift;
    R := (Buffer.R + Rounding) shr Shift;
    A := (Buffer.A + Rounding) shr Shift;
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

  if not Assigned(FSampler) then
    raise Exception.Create(SSamplerNotAssigned)
  else
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


{ TRegularRasterizer }

procedure TRegularRasterizer.DoRasterize(Dst: TBitmap32; DstRect: TRect);
var
  I, J: Integer;
  Pixels: PColor32Array;
begin
  for J := DstRect.Top to DstRect.Bottom do
  begin
    Pixels := Dst.ScanLine[J];
    for I := DstRect.Left to DstRect.Right do
      AssignColor(Pixels[I], FSampler.GetSampleInt(I, J));
  end;
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
  I, J, U, V, SX, SY: Integer;
  Scale: Integer;
  X, Y, Tx, Ty, lx, ly, dx, dy: TFixed;
  Buffer: TBufferEntry;
  Pixels: PColor32Array;
  GetSample: TGetSampleFixed;
begin
  if (SamplingX = 1) and (SamplingY = 1) then
    inherited
  else
  begin
    dx := Fixed(1 / FSamplingX);
    dy := Fixed(1 / FSamplingY);
    lx := Fixed(((1 / FSamplingX) - 1) / 2);
    ly := Fixed(((1 / FSamplingY) - 1) / 2);
    Scale := Fixed(1 / (FSamplingX * FSamplingY));
    GetSample := FSampler.GetSampleFixed;
    SX := (SamplingX - 1) * dx;
    SY := (SamplingY - 1) * dy;
    for J := DstRect.Top to DstRect.Bottom do
    begin
      Pixels := Dst.ScanLine[J];
      Y := J * FixedOne + ly;
      Ty := Y + SY;
      for I := DstRect.Left to DstRect.Right do
      begin
        Buffer := EMPTY_ENTRY;
        X := I * FixedOne + lx;
        Tx := X + SX;

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
        AssignColor(Pixels[I], BufferToColor32(Buffer, 16));
      end;
    end;
  end;
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

end.
