unit GR32_Rasterizers;

interface

uses
  GR32, Windows;

type
  TGetSampleEventInt = function(X, Y: Integer): TColor32 of object;
  TGetSampleEventFixed = function(X, Y: TFixed): TColor32 of object;
  TGetSampleEventFloat = function(X, Y: Single): TColor32 of object;

  TAbstractRasterizer = class
  public
    procedure Rasterize(Dst: TBitmap32; DstRect: TRect; SrcAlpha: TColor32;
      DrawMode: TDrawMode; CombineMode: TCombineMode;
      CombineCallBack: TPixelCombineEvent); virtual; abstract;
  end;

  TRasterizer = class(TAbstractRasterizer)
  private
    FSampler: TCustomSampler;
  public
    property Sampler: TCustomSampler read FSampler write FSampler;
  end;

  TRegularRasterizer = class(TRasterizer)
  public
    procedure Rasterize(Dst: TBitmap32; DstRect: TRect; SrcAlpha: TColor32;
      DrawMode: TDrawMode; CombineMode: TCombineMode;
      CombineCallBack: TPixelCombineEvent); override;
  end;

  TIrregularRasterizer = class(TRasterizer);
  TSuperSamplingRasterizer = class(TRegularRasterizer);
  TMultiSamplingRasterizer = class(TRegularRasterizer);

implementation

uses GR32_Blend;

{ TCustomSampler }

procedure TRegularRasterizer.Rasterize(Dst: TBitmap32; DstRect: TRect;
  SrcAlpha: TColor32; DrawMode: TDrawMode; CombineMode: TCombineMode;
  CombineCallBack: TPixelCombineEvent);
var
  I, J: Integer;
  C: TColor32;
  BlendMemEx: TBlendMemEx;
  Pixels: PColor32Array;
begin
  Assert(Assigned(FSampler));
  BlendMemEx := BLEND_MEM_EX[CombineMode];
  try
    FSampler.PrepareRasterization;
    for J := DstRect.Top to DstRect.Bottom do
    begin
      Pixels := Dst.ScanLine[J];
      for I := DstRect.Left to DstRect.Right do
      begin
        EMMS;
        C := FSampler.GetSampleInt(I, J);
        case DrawMode of
          dmOpaque: Pixels[I] := C;
          dmBlend: BlendMemEx(C, Pixels[I], SrcAlpha);
        else // dmCustom:
          CombineCallBack(C, Pixels[I], SrcAlpha);
        end;
      end;
    end;
  finally
    EMMS;
    FSampler.FinalizeRasterization;
  end;
end;

end.
