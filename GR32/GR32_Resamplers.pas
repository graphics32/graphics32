unit GR32_Resamplers;

interface

uses
  Classes, Types, GR32;

procedure BlockTransfer(
  Dst: TBitmap32; DstX: Integer; DstY: Integer; DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent = nil);

procedure StretchTransfer(
  Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  Resampler: TCustomResampler;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent = nil);

function ResamplePixel(Src: TBitmap32; X, Y: Single): TColor32;

type
  TResampler = class(TCustomResampler)
  public
    function RangeCheck: Boolean; override;
    procedure Resample(
      Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
      Src: TBitmap32; SrcRect: TRect;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); override;
    procedure Transform(
      Dst: TBitmap32; DstRect: TRect;
      Src: TBitmap32; SrcRect: TRect; Transformation: TAbstractTransformation;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); override;
  end;

  { TNearestResampler }
  TNearestResampler = class(TResampler)
  public
    function Filter(Value: Single): Single; override;
    function Width: Single; override;
    procedure Resample(
      Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
      Src: TBitmap32; SrcRect: TRect;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); override;
    procedure Transform(
      Dst: TBitmap32; DstRect: TRect;
      Src: TBitmap32; SrcRect: TRect; Transformation: TAbstractTransformation;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); override;
  end;

  { TLinearResampler }
  TLinearResampler = class(TResampler)
  public
    function Filter(Value: Single): Single; override;
    function Width: Single; override;
    procedure Resample(
      Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
      Src: TBitmap32; SrcRect: TRect;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); override;
    procedure Transform(
      Dst: TBitmap32; DstRect: TRect;
      Src: TBitmap32; SrcRect: TRect; Transformation: TAbstractTransformation;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); override;
  end;

  { TDraftResampler }
  TDraftResampler = class(TLinearResampler)
  public
    function Width: Single; override;
    procedure Resample(
      Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
      Src: TBitmap32; SrcRect: TRect;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); override;
  end;

  { TCosineResampler }
  TCosineResampler = class(TResampler)
  public
    function Filter(Value: Single): Single; override;
    function Width: Single; override;
  end;

  { TSplineResampler }
  TSplineResampler = class(TResampler)
  public
    function Filter(Value: Single): Single; override;
    function Width: Single; override;
    function RangeCheck: Boolean; override;
  end;

  { TMitchellResampler }
  TMitchellResampler = class(TResampler)
  public
    function Filter(Value: Single): Single; override;
    function Width: Single; override;
    function RangeCheck: Boolean; override;
  end;

  { TCubicResampler }
  TCubicResampler = class(TResampler)
  private
    FCoeff: Single;
  public
    constructor Create; override;
    function Filter(Value: Single): Single; override;
    function Width: Single; override;
    function RangeCheck: Boolean; override;
  published
    property Coeff: Single read FCoeff write FCoeff;
  end;

  { TWindowedSincResampler }
  TWindowedSincResampler = class(TResampler)
  private
    FWidth: Single;
  public
    constructor Create; override;
    function Filter(Value: Single): Single; override;
    function Window(Value: Single): Single; virtual; abstract;
    procedure SetWidth(Value: Single);
    function Width: Single; override;
    function RangeCheck: Boolean; override;
  end;

  { TLanczosResampler }
  TLanczosResampler = class(TWindowedSincResampler)
  public
    function Window(Value: Single): Single; override;
  end;

  { TGaussianResampler }
  TGaussianResampler = class(TWindowedSincResampler)
  private
    FSigma: Single;
  public
    constructor Create; override;
    function Window(Value: Single): Single; override;
  end;

  TBlackmanResampler = class(TWindowedSincResampler)
  public
    function Window(Value: Single): Single; override;
  end;

  THannResampler = class(TWindowedSincResampler)
  public
    function Window(Value: Single): Single; override;
  end;

  THammingResampler = class(TWindowedSincResampler)
  public
    function Window(Value: Single): Single; override;
  end;

procedure RegisterResampler(ResamplerClass: TCustomResamplerClass);
function GetResamplerClassNames: TStrings;
function FindResamplerClass(ClassName: string): TCustomResamplerClass;

var
  ResamplerList: TList;

implementation

uses
  GR32_Transforms, GR32_Blend, GR32_LowLevel, GR32_System, SysUtils, Math;

var
 BlockAverage : function (Dlx, Dly, RowSrc, OffSrc: Cardinal): TColor32;
 LinearInterpolator: function(PWX_256, PWY_256: Cardinal; C11, C21: PColor32): TColor32;

const
  SDstEmpty = 'Destination bitmap is nil or empty';
  SSrcEmpty = 'Source bitmap is nil or empty';
  SSrcInvalid = 'Source rectangle is invalid';
 
type
  TTransformationAccess = class(TTransformation);
  TBitmap32Access = class(TBitmap32);

  TPointRec = record
    Pos: Integer;
    Weight: Cardinal;
  end;

  TCluster = array of TPointRec;
  TMappingTable = array of TCluster;

  PBufferEntry = ^TBufferEntry;
  TBufferEntry = record
    B, G, R, A: Integer;
  end;
  
{ procedure AccumBuffer(var Buffer: TBufferEntry; Color: TColor32; Mapping: Integer);
asm
        MOVD      MM0,EDX
        MOVD      MM1,ECX
        PXOR      MM3,MM3
        PUNPCKLBW MM0,MM3
        PUNPCKLBW MM1,MM1
        PUNPCKLWD MM1,MM1
        PUNPCKLBW MM1,MM3
        PMULLW    MM0,MM1
        MOVQ      MM1,MM0
        PUNPCKLWD MM0,MM3
        PUNPCKHWD MM1,MM3
        PADDD     MM0,[EAX]
        PADDD     MM1,[EAX+8]
        MOVQ      [EAX],MM0
        MOVQ      [EAX+8],MM1
end; }


function ResamplePixel(Src: TBitmap32; X, Y: Single): TColor32;
type
  PColorEntry = ^TColorEntry;
  TColorEntry = packed record
    B, G, R, A: Byte;
  end;
var
  clX, clY: Integer;
  fracX, fracY: Single;
  Resampler: TCustomResampler;
  W: Integer;
  Filter: TFilterMethod;
  I, J: Integer;
  C: PColorEntry;
  LoX, HiX, LoY, HiY: Integer;

  MappingX: array [-3..3] of Integer;
  MappingY: array [-3..3] of Integer;
  HorzEntry, VertEntry: TBufferEntry;
const
  EMPTY_ENTRY: TBufferEntry = (B: 0; G: 0; R: 0; A: 0);
begin
  Resampler := Src.Resampler;
  Filter := Resampler.Filter;
  W := Round(Resampler.Width);

  clX := Ceil(X);
  clY := Ceil(Y);
  fracX := clX - X;
  fracY := clY - Y;

  if clX < W then LoX := -clX else LoX := -W;
  if clY < W then LoY := -clY else LoY := -W;
  HiX := Src.Width - 1;
  HiY := Src.Height - 1;
  if clX + W >= HiX then HiX := HiX - clX else HiX := W;
  if clY + W >= HiY then HiY := HiY - clY else HiY := W;

  for I := LoX to HiX do MappingX[I] := Round(Filter(I + fracX) * 256);
  for I := LoY to HiY do MappingY[I] := Round(Filter(I + fracY) * 256);

  VertEntry := EMPTY_ENTRY;
  for I := LoY to HiY do
  begin
    HorzEntry := EMPTY_ENTRY;
    C := PColorEntry(Src.PixelPtr[LoX + clX, I + clY]);
    for J := LoX to HiX do
    begin
      Inc(HorzEntry.A, C.A * MappingX[J]);
      Inc(HorzEntry.R, C.R * MappingX[J]);
      Inc(HorzEntry.G, C.G * MappingX[J]);
      Inc(HorzEntry.B, C.B * MappingX[J]);
      Inc(C);
    end;
    Inc(VertEntry.A, HorzEntry.A * MappingY[I]);
    Inc(VertEntry.R, HorzEntry.R * MappingY[I]);
    Inc(VertEntry.G, HorzEntry.G * MappingY[I]);
    Inc(VertEntry.B, HorzEntry.B * MappingY[I]);
  end;
  if Resampler.RangeCheck then
  begin
    VertEntry.A := EnsureRange(VertEntry.A, 0, $ff0000);
    VertEntry.R := EnsureRange(VertEntry.R, 0, $ff0000);
    VertEntry.G := EnsureRange(VertEntry.G, 0, $ff0000);
    VertEntry.B := EnsureRange(VertEntry.B, 0, $ff0000);
  end;
  with TColorEntry(Result) do
  begin
    A := VertEntry.A shr 16;
    R := VertEntry.R shr 16;
    G := VertEntry.G shr 16;
    B := VertEntry.B shr 16;
  end;
end;

procedure CheckBitmaps(Dst, Src: TBitmap32);
begin
  if not Assigned(Dst) or Dst.Empty then raise ETransformError.Create(SDstEmpty);
  if not Assigned(Src) or Src.Empty then raise ETransformError.Create(SSrcEmpty);
end;

function CheckSrcRect(Src: TBitmap32; const SrcRect: TRect): Boolean;
begin
  Result := False;
  if IsRectEmpty(SrcRect) then Exit;
  if (SrcRect.Left < 0) or (SrcRect.Right > Src.Width) or
    (SrcRect.Top < 0) or (SrcRect.Bottom > Src.Height) then
    raise ETransformError.Create(SSrcInvalid);
  Result := True;
end;

procedure BlendBlock(
  Dst: TBitmap32; DstRect: TRect;
  Src: TBitmap32; SrcX, SrcY: Integer;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcP, DstP: PColor32;
  SP, DP: PColor32;
  W, I, DstY: Integer;
  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;
begin
  { Internal routine }
  W := DstRect.Right - DstRect.Left;
  SrcP := Src.PixelPtr[SrcX, SrcY];
  DstP := Dst.PixelPtr[DstRect.Left, DstRect.Top];

  case CombineOp of
    dmOpaque:
      begin
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
        begin
          //Move(SrcP^, DstP^, W shl 2); // for FastCode
          MoveLongWord(SrcP^, DstP^, W);
          Inc(SrcP, Src.Width);
          Inc(DstP, Dst.Width);
        end;
      end;
    dmBlend:
      if Src.MasterAlpha >= 255 then
      begin
        BlendLine := BLEND_LINE[Src.CombineMode];
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
        begin
          BlendLine(SrcP, DstP, W);
          Inc(SrcP, Src.Width);
          Inc(DstP, Dst.Width);
        end
      end
      else
      begin
        BlendLineEx := BLEND_LINE_EX[Src.CombineMode];
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
        begin
          BlendLineEx(SrcP, DstP, W, Src.MasterAlpha);
          Inc(SrcP, Src.Width);
          Inc(DstP, Dst.Width);
        end
      end
    else //  dmCustom:
      begin
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
        begin
          SP := SrcP;
          DP := DstP;
          for I := 0 to W - 1 do
          begin
            CombineCallBack(SP^, DP^, Src.MasterAlpha);
            Inc(SP); Inc(DP);
          end;
          Inc(SrcP, Src.Width);
          Inc(DstP, Dst.Width);
        end;
      end;
    end;
end;

procedure BlockTransfer(
  Dst: TBitmap32; DstX: Integer; DstY: Integer; DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcX, SrcY: Integer;
begin
  if Src.Empty or ((CombineOp = dmBlend) and (Src.MasterAlpha = 0)) then Exit;
  CheckBitmaps(Src, Dst);

  if not Dst.MeasuringMode then
  begin
    if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then
      CombineOp := dmOpaque;

    SrcX := SrcRect.Left;
    SrcY := SrcRect.Top;

    IntersectRect(DstClip, DstClip, Dst.BoundsRect);
    IntersectRect(SrcRect, SrcRect, Src.BoundsRect);
    OffsetRect(SrcRect, DstX - SrcX, DstY - SrcY);
    IntersectRect(SrcRect, DstClip, SrcRect);
    DstClip := SrcRect;
    OffsetRect(SrcRect, SrcX - DstX, SrcY - DstY);

    if not IsRectEmpty(SrcRect) then
    try
      BlendBlock(Dst, DstClip, Src, SrcRect.Left, SrcRect.Top, CombineOp, CombineCallBack);
    finally
      EMMS;
    end;
  end;

  Dst.Changed(MakeRect(DstX, DstY, DstX + SrcRect.Right - SrcRect.Left,
    DstY + SrcRect.Bottom - SrcRect.Top));
end;


procedure StretchNearest(
  Dst: TBitmap32; DstRect, DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  R: TRect;
  SrcW, SrcH, DstW, DstH, DstClipW, DstClipH: Integer;
  SrcY, OldSrcY: Integer;
  I, J: Integer;
  MapHorz: array of Integer;
  SrcLine, DstLine: PColor32Array;
  Buffer: TArrayOfColor32;
  Scale: Single;
  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;
begin
  IntersectRect(DstClip, DstClip, MakeRect(0, 0, Dst.Width, Dst.Height));
  IntersectRect(DstClip, DstClip, DstRect);
  if IsRectEmpty(DstClip) then Exit;
  IntersectRect(R, DstClip, DstRect);
  if IsRectEmpty(R) then Exit;
  if (SrcRect.Left < 0) or (SrcRect.Top < 0) or (SrcRect.Right > Src.Width) or
    (SrcRect.Bottom > Src.Height) then raise Exception.Create('Invalid SrcRect');

  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  DstClipW := DstClip.Right - DstClip.Left;
  DstClipH := DstClip.Bottom - DstClip.Top;
  try
    if (SrcW = DstW) and (SrcH = DstH) then
    begin
      { Copy without resampling }
      BlendBlock(Dst, DstClip, Src, SrcRect.Left + DstClip.Left - DstRect.Left,
        SrcRect.Top + DstClip.Top - DstRect.Top, CombineOp, CombineCallBack);
    end
    else
    begin
      SetLength(MapHorz, DstClipW);

      if DstW > 1 then
      begin
        if FullEdge then
        begin
          Scale := SrcW / DstW;
          for I := 0 to DstClipW - 1 do
            MapHorz[I] := Trunc(SrcRect.Left + (I + DstClip.Left - DstRect.Left) * Scale);
        end
        else
        begin
          Scale := (SrcW - 1) / (DstW - 1);
          for I := 0 to DstClipW - 1 do
            MapHorz[I] := Round(SrcRect.Left + (I + DstClip.Left - DstRect.Left) * Scale);
        end;
        Assert(MapHorz[0] >= SrcRect.Left);
        Assert(MapHorz[DstClipW - 1] < SrcRect.Right);
      end
      else
        MapHorz[0] := (SrcRect.Left + SrcRect.Right - 1) div 2;

      if DstH <= 1 then Scale := 0
      else if FullEdge then Scale := SrcH / DstH
      else Scale := (SrcH - 1) / (DstH - 1);

      if CombineOp = dmOpaque then
      begin
        DstLine := PColor32Array(Dst.PixelPtr[DstClip.Left, DstClip.Top]);
        OldSrcY := -1;
        for J := 0 to DstClipH - 1 do
        begin
          if DstH <= 1 then
            SrcY := (SrcRect.Top + SrcRect.Bottom - 1) div 2
          else if FullEdge then
            SrcY := Trunc(SrcRect.Top + (J + DstClip.Top - DstRect.Top) * Scale)
          else
            SrcY := Round(SrcRect.Top + (J + DstClip.Top - DstRect.Top) * Scale);
          if SrcY <> OldSrcY then
          begin
            SrcLine := Src.ScanLine[SrcY];
            for I := 0 to DstClipW - 1 do DstLine[I] := SrcLine[MapHorz[I]];
            OldSrcY := SrcY;
          end
          else
            MoveLongWord(DstLine[-Dst.Width], DstLine[0], DstClipW);
          Inc(DstLine, Dst.Width);
        end;
      end
      else
      begin
        SetLength(Buffer, DstClipW);
        DstLine := PColor32Array(Dst.PixelPtr[DstClip.Left, DstClip.Top]);
        OldSrcY := -1;

        if Src.MasterAlpha >= 255 then
        begin
          BlendLine := BLEND_LINE[Src.CombineMode];
          BlendLineEx := nil; // stop compiler warnings...
        end
        else
        begin
          BlendLineEx := BLEND_LINE_EX[Src.CombineMode];
          BlendLine := nil; // stop compiler warnings...
        end;

        for J := 0 to DstClipH - 1 do
        begin
          if DstH > 1 then
          begin
            EMMS;
            if FullEdge then
              SrcY := Trunc(SrcRect.Top + (J + DstClip.Top - DstRect.Top) * Scale)
            else
              SrcY := Round(SrcRect.Top + (J + DstClip.Top - DstRect.Top) * Scale);
          end
          else
            SrcY := (SrcRect.Top + SrcRect.Bottom - 1) div 2;
          if SrcY <> OldSrcY then
          begin
            SrcLine := Src.ScanLine[SrcY];
            for I := 0 to DstClipW - 1 do Buffer[I] := SrcLine[MapHorz[I]];
            OldSrcY := SrcY;
          end;

          if CombineOp = dmBlend then
          begin
            if Src.MasterAlpha >= 255 then
              BlendLine(@Buffer[0], @DstLine[0], DstClipW)
            else
              BlendLineEx(@Buffer[0], @DstLine[0], DstClipW, Src.MasterAlpha);
          end
          else
            for I := 0 to DstClipW - 1 do
              CombineCallBack(Buffer[I], DstLine[I], Src.MasterAlpha);

          Inc(DstLine, Dst.Width);
        end;
      end;
    end;
  finally
    EMMS;
  end;
end;

procedure StretchHorzStretchVertLinear(
  Dst: TBitmap32; DstRect, DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
//Assure DstRect is >= SrcRect, otherwise quality loss will occur
var
  SrcW, SrcH, DstW, DstH, DstClipW, DstClipH: Integer;
  MapHorz, MapVert: array of TPointRec;
  t2, Scale: Single;
  SrcLine, DstLine: PColor32Array;
  SrcIndex: Integer;
  I, J: Integer;
  WY: Cardinal;
  C: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  DstClipW := DstClip.Right - DstClip.Left;
  DstClipH := DstClip.Bottom - DstClip.Top;

  SetLength(MapHorz, DstClipW);
  if FullEdge then Scale := SrcW / DstW
  else Scale := (SrcW - 1) / (DstW - 1);
  for I := 0 to DstClipW - 1 do
  begin
    if FullEdge then t2 := SrcRect.Left - 0.5 + (I + DstClip.Left - DstRect.Left + 0.5) * Scale
    else t2 := SrcRect.Left + (I + DstClip.Left - DstRect.Left) * Scale;
    if t2 < 0 then t2 := 0
    else if t2 > Src.Width - 1 then t2 := Src.Width - 1;
    MapHorz[I].Pos := Floor(t2);
    MapHorz[I].Weight := 256 - Round(Frac(t2) * 256);
    //Pre-pack weights to reduce MMX Reg. setups per pixel:
    MapHorz[I].Weight:= MapHorz[I].Weight shl 16 + MapHorz[I].Weight;
  end;
  I := DstClipW - 1;
  while MapHorz[I].Pos = SrcRect.Right - 1 do
  begin
    Dec(MapHorz[I].Pos);
    MapHorz[I].Weight := 0;
    Dec(I);
  end;

  SetLength(MapVert, DstClipH);
  if FullEdge then Scale := SrcH / DstH
  else Scale := (SrcH - 1) / (DstH - 1);
  for I := 0 to DstClipH - 1 do
  begin
    if FullEdge then t2 := SrcRect.Top - 0.5 + (I + DstClip.Top - DstRect.Top + 0.5) * Scale
    else t2 := SrcRect.Top + (I + DstClip.Top - DstRect.Top) * Scale;
    if t2 < 0 then t2 := 0
    else if t2 > Src.Height - 1 then t2 := Src.Height - 1;
    MapVert[I].Pos := Floor(t2);
    MapVert[I].Weight := 256 - Round(Frac(t2) * 256);
    //Pre-pack weights to reduce MMX Reg. setups per pixel:
    MapVert[I].Weight := MapVert[I].Weight shl 16 + MapVert[I].Weight;
  end;
  I := DstClipH - 1;
  while MapVert[I].Pos = SrcRect.Bottom - 1 do
  begin
    Dec(MapVert[I].Pos);
    MapVert[I].Weight := 0;
    Dec(I);
  end;

  DstLine := PColor32Array(Dst.PixelPtr[DstClip.Left, DstClip.Top]);
  case CombineOp of
    dmOpaque:
      for J := 0 to DstClipH - 1 do
      begin
        SrcLine := Src.ScanLine[MapVert[J].Pos];
        WY := MapVert[J].Weight;
        for I := 0 to DstClipW - 1 do
        begin
          SrcIndex := MapHorz[I].Pos;
          DstLine[I] := LinearInterpolator( MapHorz[I].Weight, WY, @SrcLine[SrcIndex],
                                            @SrcLine[SrcIndex + Src.Width]);
        end;
        Inc(DstLine, Dst.Width);
      end;
    dmBlend:
      begin
        BlendMemEx := BLEND_MEM_EX[Src.CombineMode];
        for J := 0 to DstClipH - 1 do
        begin
          SrcLine := Src.ScanLine[MapVert[J].Pos];
          WY := MapVert[J].Weight;
          for I := 0 to DstClipW - 1 do
          begin
            SrcIndex := MapHorz[I].Pos;
            C := LinearInterpolator( MapHorz[I].Weight, WY, @SrcLine[SrcIndex],
                                     @SrcLine[SrcIndex + Src.Width]);
            BlendMemEx(C, DstLine[I], Src.MasterAlpha)
          end;
          Inc(DstLine, Dst.Width);
        end
      end
  else // cmCustom
    for J := 0 to DstClipH - 1 do
    begin
      SrcLine := Src.ScanLine[MapVert[J].Pos];
      WY := MapVert[J].Weight;
      for I := 0 to DstClipW - 1 do
      begin
        SrcIndex := MapHorz[I].Pos;
        C := LinearInterpolator( MapHorz[I].Weight, WY, @SrcLine[SrcIndex],
                                 @SrcLine[SrcIndex + Src.Width]);
        CombineCallBack(C, DstLine[I], Src.MasterAlpha);
      end;
      Inc(DstLine, Dst.Width);
    end;
  end;
  EMMS;
end;

function BuildMappingTable(
  DstLo, DstHi: Integer;
  ClipLo, ClipHi: Integer;
  SrcLo, SrcHi: Integer;
  Resampler: TResampler): TMappingTable;
var
  SrcW, DstW, ClipW: Integer;
  Filter: TFilterMethod;
  FilterWidth: Single;
  Scale, OldScale: Single;
  Center: Single;
  Count: Integer;
  Left, Right: Integer;
  I, J, K: Integer;
  Weight: Integer;
begin
  SrcW := SrcHi - SrcLo;
  DstW := DstHi - DstLo;
  ClipW := ClipHi - ClipLo;
  if SrcW = 0 then
  begin
    Result := nil;
    Exit;
  end
  else if SrcW = 1 then
  begin
    SetLength(Result, ClipW);
    for I := 0 to ClipW - 1 do
    begin
      SetLength(Result[I], 1);
      Result[I][0].Pos := 0;
      Result[I][0].Weight := 256;
    end;
    Exit;
  end;
  SetLength(Result, ClipW);
  if ClipW = 0 then Exit;

  if FullEdge then Scale := DstW / SrcW
  else Scale := (DstW - 1) / (SrcW - 1);

  Filter := Resampler.Filter;
  FilterWidth := Resampler.Width;
  K := 0;

  if Scale = 0 then
  begin
    Assert(Length(Result) = 1);
    SetLength(Result[0], 1);
    Result[0][0].Pos := (SrcLo + SrcHi) div 2;
    Result[0][0].Weight := 256;
  end
  else if Scale < 1 then
  begin
    OldScale := Scale;
    Scale := 1 / Scale;
    FilterWidth := FilterWidth * Scale;
    for I := 0 to ClipW - 1 do
    begin
      if FullEdge then
        Center := SrcLo - 0.5 + (I - DstLo + ClipLo + 0.5) * Scale
      else
        Center := SrcLo + (I - DstLo + ClipLo) * Scale;
      Left := Floor(Center - FilterWidth);
      Right := Ceil(Center + FilterWidth);
      Count := -256;
      for J := Left to Right do
      begin
        Weight := Round(256 * Filter((Center - J) * OldScale) * OldScale);
        if Weight <> 0 then
        begin
          Inc(Count, Weight);
          K := Length(Result[I]);
          SetLength(Result[I], K + 1);
          Result[I][K].Pos := Constrain(J, SrcLo, SrcHi - 1);
          Result[I][K].Weight := Weight;
        end;
      end;
      if Length(Result[I]) = 0 then
      begin
        SetLength(Result[I], 1);
        Result[I][0].Pos := Floor(Center);
        Result[I][0].Weight := 256;
      end
      else if Count <> 0 then
        Dec(Result[I][K div 2].Weight, Count);
    end;
  end
  else // scale > 1
  begin
    Scale := 1 / Scale;
    for I := 0 to ClipW - 1 do
    begin
      if FullEdge then
        Center := SrcLo - 0.5 + (I - DstLo + ClipLo + 0.5) * Scale
      else
        Center := SrcLo + (I - DstLo + ClipLo) * Scale;
      Left := Floor(Center - FilterWidth);
      Right := Ceil(Center + FilterWidth);
      Count := -256;
      for J := Left to Right do
      begin
        Weight := Round(256 * Filter(Center - j));
        if Weight <> 0 then
        begin
          Inc(Count, Weight);
          K := Length(Result[I]);
          SetLength(Result[I], k + 1);
          Result[I][K].Pos := Constrain(j, SrcLo, SrcHi - 1);
          Result[I][K].Weight := Weight;
        end;
      end;
      if Count <> 0 then
        Dec(Result[I][K div 2].Weight, Count);
    end;
  end;
end;

{$WARNINGS OFF}
procedure Resample(
  Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  Resampler: TResampler;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH: Single;
  DstW, DstH: Integer;
  DstClipW, DstClipH: Integer;
  t: Single;
  MapX, MapY: TMappingTable;
  I, J, X, Y, Index: Integer;
  MapXLoPos, MapXHiPos: Integer;
  HorzBuffer: array of TBufferEntry;
  ClusterX, ClusterY: TCluster;
  ClusterXSize, ClusterYSize: Integer;
  C, Wt, Cr, Cg, Cb, Ca: Integer;
  ClustYP, ClustYW, ClustXP, ClustXW: Integer;
  SrcP: PColor32;
  DstLine: PColor32Array;
  RangeCheck: Boolean;
  BlendMemEx: TBlendMemEx;
begin
  if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then
    CombineOp := dmOpaque;

  { check source and destination }
  if (CombineOp = dmBlend) and (Src.MasterAlpha = 0) then Exit;

  BlendMemEx := BLEND_MEM_EX[Src.CombineMode]; // store in local variable

  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  DstClipW := DstClip.Right - DstClip.Left;
  DstClipH := DstClip.Bottom - DstClip.Top;

  // mapping tables
  MapX := BuildMappingTable(DstRect.Left, DstRect.Right, DstClip.Left, DstClip.Right, SrcRect.Left, SrcRect.Right, Resampler);
  MapY := BuildMappingTable(DstRect.Top, DstRect.Bottom, DstClip.Top, DstClip.Bottom, SrcRect.Top, SrcRect.Bottom, Resampler);
  ClusterX := nil;
  ClusterY := nil;
  try
    RangeCheck := Resampler.RangeCheck; //StretchFilter in [sfLanczos, sfMitchell];
    if (MapX = nil) or (MapY = nil) then Exit;

    MapXLoPos := MapX[0][0].Pos;
    MapXHiPos := MapX[DstClipW - 1][High(MapX[DstClipW - 1])].Pos;
    SetLength(HorzBuffer, MapXHiPos - MapXLoPos + 1);

    { transfer pixels }
    for J := DstClip.Top to DstClip.Bottom - 1 do
    begin
      ClusterY := MapY[J - DstClip.Top];
      for X := MapXLoPos to MapXHiPos do
      begin
        Ca := 0; Cr := 0; Cg := 0; Cb := 0;
        for Y := 0 to Length(ClusterY) - 1 do
        begin
          C := Src.Bits[X + ClusterY[Y].Pos * Src.Width];
          ClustYW := ClusterY[Y].Weight;
          Inc(Ca, C shr 24 * ClustYW);
          Inc(Cr, (C and $00FF0000) shr 16 * ClustYW);
          Inc(Cg, (C and $0000FF00) shr 8 * ClustYW);
          Inc(Cb, (C and $000000FF) * ClustYW);
        end;
        with HorzBuffer[X - MapXLoPos] do
        begin
          R := Cr;
          G := Cg;
          B := Cb;
          A := Ca;
        end;
      end;

      DstLine := Dst.ScanLine[J];
      for I := DstClip.Left to DstClip.Right - 1 do
      begin
        ClusterX := MapX[I - DstClip.Left];
        Ca := 0; Cr := 0; Cg := 0; Cb := 0;
        for X := 0 to Length(ClusterX) - 1 do
        begin
          Wt := ClusterX[X].Weight;
          with HorzBuffer[ClusterX[X].Pos - MapXLoPos] do
          begin
            Inc(Ca, A * Wt);
            Inc(Cr, R * Wt);
            Inc(Cg, G * Wt);
            Inc(Cb, B * Wt);
          end;
        end;

        if RangeCheck then
        begin
          if Ca > $FF0000 then Ca := $FF0000
          else if Ca < 0 then Ca := 0
          else Ca := Ca and $00FF0000;

          if Cr > $FF0000 then Cr := $FF0000
          else if Cr < 0 then Cr := 0
          else Cr := Cr and $00FF0000;

          if Cg > $FF0000 then Cg := $FF0000
          else if Cg < 0 then Cg := 0
          else Cg := Cg and $00FF0000;

          if Cb > $FF0000 then Cb := $FF0000
          else if Cb < 0 then Cb := 0
          else Cb := Cb and $00FF0000;

          C := (Ca shl 8) or Cr or (Cg shr 8) or (Cb shr 16);
        end
        else
          C := ((Ca and $00FF0000) shl 8) or (Cr and $00FF0000) or ((Cg and $00FF0000) shr 8) or ((Cb and $00FF0000) shr 16);

        // combine it with the background
        case CombineOp of
          dmOpaque: DstLine[I] := C;
          dmBlend: BlendMemEx(C, DstLine[I], Src.MasterAlpha);
          dmCustom: CombineCallBack(C, DstLine[I], Src.MasterAlpha);
        end;
      end;
    end;
  finally
    EMMS;
    MapX := nil;
    MapY := nil;
  end;
end;
{$WARNINGS ON}

{ Draft Resample Routines }

function BlockAverage_MMX(Dlx, Dly, RowSrc, OffSrc: Cardinal): TColor32;
asm
   push       ebx
   push       esi
   push       edi

   mov        ebx, OffSrc
   mov        esi, eax
   mov        edi, edx
   sub        ecx, $04

   db $0F,$EF,$C9           /// pxor       mm1, mm1
   db $0F,$EF,$D2           /// pxor       mm2, mm2
   db $0F,$EF,$FF           /// pxor       mm7, mm7

 @@LoopY:
   mov        esi, eax
   db $0F,$EF,$C0           /// pxor       mm0, mm0
 @@LoopX:
   db $0F,$6E,$34,$B1       /// movd       mm6, [ecx + esi * 4]
   db $0F,$60,$F7           /// punpcklbw  mm6, mm7
   db $0F,$FD,$C6           /// paddw      mm0, mm6
   dec        esi
   jnz        @@LoopX

   db $0F,$6F,$F0           /// movq       mm6, mm0
   db $0F,$61,$F7           /// punpcklwd  mm6, mm7
   db $0F,$FE,$CE           /// paddd      mm1, mm6
   db $0F,$6F,$F0           /// movq       mm6, mm0
   db $0F,$69,$F7           /// punpckhwd  mm6, mm7
   db $0F,$FE,$D6           /// paddd      mm2, mm6
   add        ecx, ebx
   dec        edx
   jnz        @@LoopY

   mul        edi
   mov        ecx, eax
   mov        eax, $01000000
   div        ecx
   mov        ecx, eax

   db $0F,$7E,$C8           /// movd       eax, mm1
   mul        ecx
   shr        eax, $18
   mov        edi, eax

   db $0F,$73,$D1,$20       /// psrlq      mm1, $20
   db $0F,$7E,$C8           /// movd       eax, mm1
   mul        ecx
   shr        eax, $10
   and        eax, $0000FF00
   add        edi, eax

   db $0F,$7E,$D0           /// movd       eax, mm2
   mul        ecx
   shr        eax, $08
   and        eax, $00FF0000
   add        edi, eax

   db $0F,$73,$D2,$20       /// psrlq      mm2, $20
   db $0F,$7E,$D0           /// movd       eax, mm2
   mul        ecx
   and        eax, $FF000000
   add        eax, edi

   pop        edi
   pop        esi
   pop        ebx
end;

function BlockAverage_3dNow(Dlx, Dly, RowSrc, OffSrc: Cardinal): TColor32;
asm
   push       ebx
   push       esi
   push       edi

   mov        ebx, OffSrc
   mov        esi, eax
   mov        edi, edx

   shl        esi, $02
   sub        ebx, esi

   db $0F,$EF,$C9           /// pxor       mm1, mm1
   db $0F,$EF,$D2           /// pxor       mm2, mm2
   db $0F,$EF,$FF           /// pxor       mm7, mm7

 @@LoopY:
   mov        esi, eax
   db $0F,$EF,$C0           /// pxor       mm0, mm0
   db $0F,$0D,$34,$F1       /// prefetch   [ecx + esi * 8]
 @@LoopX:
   db $0F,$6E,$31           /// movd       mm6, [ecx]
   db $0F,$60,$F7           /// punpcklbw  mm6, mm7
   db $0F,$FD,$C6           /// paddw      mm0, mm6
   add        ecx, $04
   dec        esi

   jnz        @@LoopX

   db $0F,$6F,$F0           /// movq       mm6, mm0
   db $0F,$61,$F7           /// punpcklwd  mm6, mm7
   db $0F,$FE,$CE           /// paddd      mm1, mm6
   db $0F,$6F,$F0           /// movq       mm6, mm0
   db $0F,$69,$F7           /// punpckhwd  mm6, mm7
   db $0F,$FE,$D6           /// paddd      mm2, mm6
   add        ecx, ebx
   dec        edx

   jnz        @@LoopY

   mul        edi
   mov        ecx, eax
   mov        eax, $01000000
   div        ecx
   mov        ecx, eax

   db $0F,$7E,$C8           /// movd       eax, mm1
   mul        ecx
   shr        eax, $18
   mov        edi, eax

   db $0F,$73,$D1,$20       /// psrlq      mm1, $20
   db $0F,$7E,$C8           /// movd       eax, mm1
   mul        ecx
   shr        eax, $10
   and        eax, $0000FF00
   add        edi, eax

   db $0F,$7E,$D0           /// movd       eax, mm2
   mul        ecx
   shr        eax, $08
   and        eax, $00FF0000
   add        edi, eax

   db $0F,$73,$D2,$20       /// psrlq      mm2, $20
   db $0F,$7E,$D0           /// movd       eax, mm2
   mul        ecx
   and        eax, $FF000000
   add        eax, edi

   pop        edi
   pop        esi
   pop        ebx
end;

function BlockAverage_IA32(Dlx, Dly, RowSrc, OffSrc: Cardinal): TColor32;
type
 PCardinal = ^Cardinal;
 PRGBA = ^TRGBA;
 TRGBA = record B,G,R,A: Byte end;
var
 C: PRGBA;
 ix, iy, iA, iR, iG, iB, Area: Cardinal;
begin
  iR := 0;  iB := iR;  iG := iR;  iA := iR;
  for iy := 1 to Dly do
  begin
    C:= PRGBA(RowSrc);
    for ix := 1 to Dlx do
    begin
      inc(iB, C.B);
      inc(iG, C.G);
      inc(iR, C.R);
      inc(iA, C.A);
      inc(C);
    end;
    inc(RowSrc, OffSrc);
  end;

  Area := Dlx * Dly;
  Area := $1000000 div Area;
  Result := iA * Area and $FF000000 or
            iR * Area shr  8 and $FF0000 or
            iG * Area shr 16 and $FF00 or
            iB * Area shr 24 and $FF;
end;


procedure DraftResample(Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
                        Src: TBitmap32; SrcRect: TRect;
                        Resampler: TResampler;
                        CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH,
  DstW, DstH,
  DstClipW, DstClipH: Cardinal;
  RowSrc, OffSrc,
  dy, dx,
  c1, c2, r1, r2,
  xs, xsrc, M: Cardinal;
  C: TColor32;
  DstLine: PColor32Array;
  ScaleFactor,lx, fe: Single;
  FSrcTop,I,J,ly,
  sc, sr, cx, cy: integer;
  Y_256: TFixed;
  BlendMemEx: TBlendMemEx;
begin
 { rangechecking and rect intersection done by caller }

  SrcW := SrcRect.Right  - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;

  DstW := DstRect.Right  - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;

  DstClipW := DstClip.Right - DstClip.Left;
  DstClipH := DstClip.Bottom - DstClip.Top;

  BlendMemEx := BLEND_MEM_EX[Src.CombineMode];

  if (DstW > SrcW)or(DstH > SrcH) then begin
    if (SrcW < 2) or (SrcH < 2) then
      Resample(Dst, DstRect, DstClip, Src, SrcRect, Resampler, CombineOp,
        CombineCallBack)
    else
      StretchHorzStretchVertLinear(Dst, DstRect, DstClip, Src, SrcRect, CombineOp,
        CombineCallBack);
    end
  else
    begin //Full Scaledown, ignores Fulledge - cannot be integrated into this resampling method
      OffSrc := Src.Width * 4;

      ScaleFactor:= SrcW / DstW;
      cx := Trunc( (DstClip.Left - DstRect.Left) * ScaleFactor);
      r2 := Trunc(ScaleFactor);
      sr := Trunc( $10000 * ScaleFactor );

      ScaleFactor:= SrcH / DstH;
      cy := Trunc( (DstClip.Top - DstRect.Top) * ScaleFactor);
      c2 := Trunc(ScaleFactor);
      sc := Trunc( $10000 * ScaleFactor );

      DstLine := PColor32Array(Dst.PixelPtr[0, DstClip.Top]);
      RowSrc := Cardinal(Src.PixelPtr[ SrcRect.Left +  cx, SrcRect.Top + cy ]);

      xs := r2;
      c1 := 0;
      Dec(DstClip.Left, 2);
      Inc(DstClipW);
      Inc(DstClipH);

      for J := 2  to DstClipH do
      begin
        dy := c2 - c1;
        c1 := c2;
        c2 := J * sc shr 16;
        r1 := 0;
        r2 := xs;
        xsrc := RowSrc;

        case CombineOp of
          dmOpaque:
            for I := 2  to DstClipW do
            begin
              dx := r2 - r1;  r1 := r2;
              r2 := I * sr shr 16;
              DstLine[DstClip.Left + I]:= BlockAverage(dx, dy, xsrc, OffSrc);
              xsrc := xsrc + dx shl 2;
            end;
          dmBlend:
            for I := 2  to DstClipW do
            begin
              dx := r2 - r1;  r1 := r2;
              r2 := I * sr shr 16;
              BlendMemEx(BlockAverage(dx, dy, xsrc, OffSrc), DstLine[DstClip.Left + I], Src.MasterAlpha);
              xsrc := xsrc + dx shl 2;
            end;
          dmCustom:
            for I := 2  to DstClipW do
            begin
              dx := r2 - r1;  r1 := r2;
              r2 := I * sr shr 16;
              CombineCallBack(BlockAverage(dx, dy, xsrc, OffSrc), DstLine[DstClip.Left + I], Src.MasterAlpha);
              xsrc := xsrc + dx shl 2;
            end;
        end;

        Inc(DstLine, Dst.Width);
        Inc(RowSrc, OffSrc * dy);
      end;
    end;
  EMMS;
end;

{ Special interpolators (for sfLinear and sfDraft) }

function M_LinearInterpolator(PWX_256, PWY_256: Cardinal; C11, C21: PColor32): TColor32;
asm
        db $0F,$6F,$09           /// MOVQ      MM1,[ECX]
        MOV       ECX,C21
        db $0F,$6F,$19           /// MOVQ      MM3,[ECX]
        db $0F,$6F,$D1           /// MOVQ      MM2,MM1
        db $0F,$6F,$E3           /// MOVQ      MM4,MM3
        db $0F,$73,$D1,$20       /// PSRLQ     MM1,32
        db $0F,$73,$D3,$20       /// PSRLQ     MM3,32
        db $0F,$6E,$E8           /// MOVD      MM5,EAX
        db $0F,$62,$ED           /// PUNPCKLDQ MM5,MM5
        db $0F,$EF,$C0           /// PXOR MM0, MM0
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        db $0F,$F9,$D1           /// PSUBW     MM2,MM1
        db $0F,$D5,$D5           /// PMULLW    MM2,MM5
        db $0F,$71,$F1,$08       /// PSLLW     MM1,8
        db $0F,$FD,$D1           /// PADDW     MM2,MM1
        db $0F,$71,$D2,$08       /// PSRLW     MM2,8
        db $0F,$60,$D8           /// PUNPCKLBW MM3,MM0
        db $0F,$60,$E0           /// PUNPCKLBW MM4,MM0
        db $0F,$F9,$E3           /// PSUBW     MM4,MM3
        db $0F,$D5,$E5           /// PMULLW    MM4,MM5
        db $0F,$71,$F3,$08       /// PSLLW     MM3,8
        db $0F,$FD,$E3           /// PADDW     MM4,MM3
        db $0F,$71,$D4,$08       /// PSRLW     MM4,8
        db $0F,$6E,$EA           /// MOVD      MM5,EDX
        db $0F,$62,$ED           /// PUNPCKLDQ MM5,MM5
        db $0F,$F9,$D4           /// PSUBW     MM2,MM4
        db $0F,$D5,$D5           /// PMULLW    MM2,MM5
        db $0F,$71,$F4,$08       /// PSLLW     MM4,8
        db $0F,$FD,$D4           /// PADDW     MM2,MM4
        db $0F,$71,$D2,$08       /// PSRLW     MM2,8
        db $0F,$67,$D0           /// PACKUSWB  MM2,MM0
        db $0F,$7E,$D0           /// MOVD      EAX,MM2
end;

function _LinearInterpolator(PWX_256, PWY_256: Cardinal; C11, C21: PColor32): TColor32;
var
  C1, C3: TColor32;
begin
  PWX_256:= PWX_256 shr 16; if PWX_256 > $FF then PWX_256:= $FF;
  PWY_256:= PWY_256 shr 16; if PWY_256 > $FF then PWY_256:= $FF;
  C1 := C11^; Inc(C11);
  C3 := C21^; Inc(C21);
  Result := CombineReg(CombineReg(C1, C11^, PWX_256),
                       CombineReg(C3, C21^, PWX_256), PWY_256);
end;


{ Stretch Transfer }

{$WARNINGS OFF}
procedure StretchTransfer(
  Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  Resampler: TCustomResampler;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH: Integer;
  DstW, DstH: Integer;
  R: TRect;
begin
  if Src.Empty then Exit;
  CheckBitmaps(Dst, Src);
  if not CheckSrcRect(Src, SrcRect) then Exit;

  if not Dst.MeasuringMode then
  begin
    IntersectRect(DstClip, DstClip, MakeRect(0, 0, Dst.Width, Dst.Height));
    IntersectRect(DstClip, DstClip, DstRect);
    if IsRectEmpty(DstClip) then Exit;
    IntersectRect(R, DstClip, DstRect);
    if IsRectEmpty(R) then Exit;

    if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then CombineOp := dmOpaque;
    if (CombineOp = dmBlend) and (Src.MasterAlpha = 0) then Exit;

    SrcW := SrcRect.Right - SrcRect.Left;
    SrcH := SrcRect.Bottom - SrcRect.Top;
    DstW := DstRect.Right - DstRect.Left;
    DstH := DstRect.Bottom - DstRect.Top;

    try
      if (SrcW = DstW) and (SrcH = DstH) then
        BlendBlock(Dst, DstClip, Src, SrcRect.Left + DstClip.Left - DstRect.Left,
          SrcRect.Top + DstClip.Top - DstRect.Top, CombineOp, CombineCallBack)
      else
        Resampler.Resample(Dst, DstRect, DstClip, Src, SrcRect, CombineOp, CombineCallBack);
    finally
      EMMS;
    end;
  end;

  Dst.Changed(DstRect);
end;
{$WARNINGS ON}


{ TNearestResampler }

function TNearestResampler.Filter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then Result := 1
  else Result := 0;
end;

procedure TNearestResampler.Resample(Dst: TBitmap32; DstRect,
  DstClip: TRect; Src: TBitmap32; SrcRect: TRect; CombineOp: TDrawMode;
  CombineCallBack: TPixelCombineEvent);
begin
  StretchNearest(Dst, DstRect, DstClip, Src, SrcRect, CombineOp, CombineCallBack);
end;

procedure TNearestResampler.Transform(Dst: TBitmap32; DstRect: TRect;
  Src: TBitmap32; SrcRect: TRect; Transformation: TAbstractTransformation;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcAlpha: TColor32;
  I, J, X, Y: Integer;
  Pixels: PColor32Array;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[Src.CombineMode];
  SrcAlpha := Src.MasterAlpha;

  for J := DstRect.Top to DstRect.Bottom do
  begin
    Pixels := Dst.ScanLine[J];
    for I := DstRect.Left to DstRect.Right do
    begin
      TTransformationAccess(Transformation).ReverseTransformInt(I, J, X, Y);
      if (X >= SrcRect.Left) and (X <= SrcRect.Right) and
        (Y >= SrcRect.Top) and (Y <= SrcRect.Bottom) then
      case CombineOp of
        dmOpaque: Pixels[I] := Src.Pixel[X, Y];
        dmBlend: BlendMemEx(Src.Pixel[X, Y], Pixels[I], SrcAlpha);
      else // dmCustom:
        CombineCallBack(Src.Pixel[X, Y], Pixels[I], SrcAlpha);
      end;
    end;
  end;
end;

function TNearestResampler.Width: Single;
begin
  Result := 1;
end;

{ TResampler }

function TResampler.RangeCheck: Boolean;
begin
  Result := False;
end;

procedure TResampler.Resample(
  Dst: TBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
begin
  GR32_Resamplers.Resample(Dst, DstRect, DstClip, Src, SrcRect, Self, CombineOp, CombineCallBack);
end;

procedure TResampler.Transform(Dst: TBitmap32; DstRect: TRect;
  Src: TBitmap32; SrcRect: TRect; Transformation: TAbstractTransformation;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  C, SrcAlpha: TColor32;
  I, J: Integer;
  X, Y: Single;
  Pixels: PColor32Array;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[Src.CombineMode];
  SrcAlpha := Src.MasterAlpha;

  try
    for J := DstRect.Top to DstRect.Bottom do
    begin
      Pixels := Dst.ScanLine[J];
      for I := DstRect.Left to DstRect.Right do
      begin
        EMMS;
        TTransformationAccess(Transformation).ReverseTransformFloat(I, J, X, Y);

        if (X >= SrcRect.Left) and (X <= SrcRect.Right) and
           (Y >= SrcRect.Top) and (Y <= SrcRect.Bottom) then
        begin
          C := ResamplePixel(Src, X, Y);
          case CombineOp of
            dmOpaque: Pixels[I] := C;
            dmBlend: BlendMemEx(C, Pixels[I], SrcAlpha);
          else // dmCustom:
            CombineCallBack(C, Pixels[I], SrcAlpha);
          end;
        end;
      end;
    end;
  finally
    EMMS;
  end;
end;

{ TDraftResampler }

procedure TDraftResampler.Resample(Dst: TBitmap32; DstRect, DstClip: TRect;
  Src: TBitmap32; SrcRect: TRect; CombineOp: TDrawMode;
  CombineCallBack: TPixelCombineEvent);
begin
  DraftResample(Dst, DstRect, DstClip, Src, SrcRect, Self, CombineOp, CombineCallBack);
end;

function TDraftResampler.Width: Single;
begin
  Result := 1;
end;

{ TLinearResampler }

function TLinearResampler.Filter(Value: Single): Single;
begin
  if Value < -1 then Result := 0
  else if Value < 0 then Result := 1 + Value
  else if Value < 1 then Result := 1 - Value
  else Result := 0;
end;

procedure TLinearResampler.Resample(Dst: TBitmap32; DstRect,
  DstClip: TRect; Src: TBitmap32; SrcRect: TRect; CombineOp: TDrawMode;
  CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH: Integer;
  DstW, DstH: Integer;
begin
  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  if (DstW > SrcW) and (DstH > SrcH) and (SrcW > 1) and (SrcH > 1) then
    StretchHorzStretchVertLinear(Dst, DstRect, DstClip, Src, SrcRect, CombineOp, CombineCallBack)
  else
    GR32_Resamplers.Resample(Dst, DstRect, DstClip, Src, SrcRect, Self, CombineOp, CombineCallBack);
end;

procedure TLinearResampler.Transform(Dst: TBitmap32; DstRect: TRect;
  Src: TBitmap32; SrcRect: TRect; Transformation: TAbstractTransformation;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  C, SrcAlpha: TColor32;
  I, J: Integer;
  X, Y, X256, Y256: Integer;
  Pixels: PColor32Array;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[Src.CombineMode];
  SrcAlpha := Src.MasterAlpha;

  for J := DstRect.Top to DstRect.Bottom do
  begin
    Pixels := Dst.ScanLine[J];
    for I := DstRect.Left to DstRect.Right do
    begin
      TTransformationAccess(Transformation).ReverseTransform256(I, J, X256, Y256);
      X := SAR_8(X256);
      Y := SAR_8(Y256);

      if (X >= SrcRect.Left) and (X <= SrcRect.Right) and
         (Y >= SrcRect.Top) and (Y <= SrcRect.Bottom) then
      begin
        // everything is ok interpolate between four neighbors
        C := TBitmap32Access(Src).GET_TS256(X256, Y256);

        case CombineOp of
          dmOpaque: Pixels[I] := C;
          dmBlend: BlendMemEx(C, Pixels[I], SrcAlpha);
        else // dmCustom:
          CombineCallBack(C, Pixels[I], SrcAlpha);
        end;
      end;
    end;
  end;
end;

function TLinearResampler.Width: Single;
begin
  Result := 1;
end;

{ TCosineResampler }

function TCosineResampler.Filter(Value: Single): Single;
begin
  Result := 0;
  if Abs(Value) < 1 then
    Result := (Cos(Value * Pi) + 1) / 2;
end;

function TCosineResampler.Width: Single;
begin
  Result := 1;
end;

{ TSplineResampler }

function TSplineResampler.Filter(Value: Single): Single;
var
  tt: Single;
begin
  Value := Abs(Value);
  if Value < 1 then
  begin
    tt := Sqr(Value);
    Result := 0.5 * tt * Value - tt + 2 / 3;
  end
  else if Value < 2 then
  begin
    Value := 2 - Value;
    Result := 1 / 6 * Sqr(Value) * Value;
  end
  else Result := 0;
end;

function TSplineResampler.RangeCheck: Boolean;
begin
  Result := True;
end;

function TSplineResampler.Width: Single;
begin
  Result := 2;
end;

{ TWindowedSincResampler }

function Sinc(Value: Single): Single;
begin
  if Value <> 0 then
  begin
    Value := Value * Pi;
    Result := Sin(Value) / Value;
  end
  else Result := 1;
end;

constructor TWindowedSincResampler.Create;
begin
  inherited;
  FWidth := 3;
end;

function TWindowedSincResampler.Filter(Value: Single): Single;
begin
  Value := Abs(Value);
  if Value < FWidth then
    Result := Sinc(Value) * Window(Value)
  else
    Result := 0;
end;

function TWindowedSincResampler.RangeCheck: Boolean;
begin
  Result := True;
end;

procedure TWindowedSincResampler.SetWidth(Value: Single);
begin
  FWidth := Value;
end;

function TWindowedSincResampler.Width: Single;
begin
  Result := FWidth;
end;

{ TLanczosResampler }

function TLanczosResampler.Window(Value: Single): Single;
begin
  Result := Sinc(Value / FWidth);
end;

{ TMitchellResampler }

function TMitchellResampler.Filter(Value: Single): Single;
var
  tt, ttt: Single;
begin
  Value := Abs(Value);
  tt := Sqr(Value);
  ttt := tt * Value;
  if Value < 1 then Result := (7 * ttt + -12 * tt + 16 / 3) / 6
  else if Value < 2 then Result := (-7 / 3 * ttt + 12 * tt - 20 * Value + 32 / 3) / 6
  else Result := 0;
end;

function TMitchellResampler.RangeCheck: Boolean;
begin
  Result := True;
end;

function TMitchellResampler.Width: Single;
begin
  Result := 2;
end;

{ TCubicResampler }

constructor TCubicResampler.Create;
begin
  inherited Create;
  FCoeff := -0.5;
end;

function TCubicResampler.Filter(Value: Single): Single;
var
  tt, ttt: Single;
begin
  Value := Abs(Value);
  tt := Sqr(Value);
  ttt := tt * Value;
  if Value < 1 then
    Result := (FCoeff + 2) * ttt - (FCoeff + 3) * tt + 1
  else if Value < 2 then
    Result := FCoeff * (ttt - 5 * tt + 8 * Value - 4)
  else
    Result := 0;
end;

function TCubicResampler.RangeCheck: Boolean;
begin
  Result := True;
end;

function TCubicResampler.Width: Single;
begin
  Result := 2;
end;

{ TGaussResampler }

constructor TGaussianResampler.Create;
begin
  inherited;
  FSigma := 1.33;
end;

function TGaussianResampler.Window(Value: Single): Single;
begin
  Result := Power(2, -Sqr(Value) / FSigma);
end;

{ TBlackmanResampler }

function TBlackmanResampler.Window(Value: Single): Single;
begin
  Value := Pi * Value / FWidth;
  Result := 0.42 + 0.5 * Cos(Value) + 0.08 * Cos(2 * Value);
end;

{ THannResampler }

function THannResampler.Window(Value: Single): Single;
begin
  Result := 0.5 + 0.5 * Cos(Pi * Value / FWidth);
end;

{ THammingResampler }

function THammingResampler.Window(Value: Single): Single;
begin
  Result := 0.54 + 0.46 * Cos(Pi * Value / FWidth);
end;


{ General routines for registering resamplers and setting them up }

procedure RegisterResampler(ResamplerClass: TCustomResamplerClass);
begin
  if not Assigned(ResamplerList) then
    ResamplerList := TList.Create;
  ResamplerList.Add(ResamplerClass);
end;

function GetResamplerClassNames: TStrings;
var
  I: Integer;
begin
  if not Assigned(ResamplerList) then
    Result := nil
  else
  begin
    Result := TStringList.Create;
    for I := 0 to ResamplerList.Count - 1 do
      Result.Add(TCustomResamplerClass(ResamplerList.List[I]).ClassName);
  end;
end;

function FindResamplerClass(ClassName: string): TCustomResamplerClass;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(ResamplerList) then
    for I := 0 to ResamplerList.Count - 1 do
      if TCustomResamplerClass(ResamplerList.List[I]).ClassName = ClassName then
      begin
        Result := TCustomResamplerClass(ResamplerList.List[I]);
        Exit;
      end;
end;

procedure SetupFunctions;
var
  MMX_ACTIVE: Boolean;
  ACTIVE_3DNow: Boolean;
begin
  MMX_ACTIVE := HasMMX;
  ACTIVE_3DNow := Has3DNow;
  if ACTIVE_3DNow then
  begin
   // link 3DNow functions
   BlockAverage := BlockAverage_3DNow;
   LinearInterpolator:= M_LinearInterpolator;
  end
  else
  if MMX_ACTIVE then
  begin
   // link MMX functions
   BlockAverage:= BlockAverage_MMX;
   LinearInterpolator:= M_LinearInterpolator;
  end
  else
  begin
   // link IA32 functions
   BlockAverage:= BlockAverage_IA32;
   LinearInterpolator:= _LinearInterpolator;
  end
end;

initialization
  SetupFunctions;
  RegisterResampler(TNearestResampler);
  RegisterResampler(TDraftResampler);
  RegisterResampler(TLinearResampler);
  RegisterResampler(TCosineResampler);
  RegisterResampler(TSplineResampler);
  RegisterResampler(TCubicResampler);
  RegisterResampler(TMitchellResampler);
  RegisterResampler(TLanczosResampler);
  RegisterResampler(TGaussianResampler);
  RegisterResampler(TBlackmanResampler);
  RegisterResampler(THannResampler);
  RegisterResampler(THammingResampler);

finalization
  ResamplerList.Free;

end.
