unit GR32_Resamplers;

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
 * The Initial Developers of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 * (parts of this unit were taken from GR32_Transforms.pas by Alex A. Denisov)
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf,
{$ELSE}
  Windows, Types,
{$ENDIF}
  Classes, SysUtils, GR32, GR32_Transforms, GR32_Containers,
  GR32_OrdinalMaps, GR32_Blend, GR32_System, GR32_Bindings;

procedure BlockTransfer(
  Dst: TCustomBitmap32; DstX: Integer; DstY: Integer; DstClip: TRect;
  Src: TCustomBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent = nil);

procedure BlockTransferX(
  Dst: TCustomBitmap32; DstX, DstY: TFixed; 
  Src: TCustomBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent = nil);

procedure StretchTransfer(
  Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TCustomBitmap32; SrcRect: TRect;
  Resampler: TCustomResampler;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent = nil);

procedure BlendTransfer(
  Dst: TCustomBitmap32; DstX, DstY: Integer; DstClip: TRect;
  SrcF: TCustomBitmap32; SrcRectF: TRect;
  SrcB: TCustomBitmap32; SrcRectB: TRect;
  BlendCallback: TBlendReg); overload;

procedure BlendTransfer(
  Dst: TCustomBitmap32; DstX, DstY: Integer; DstClip: TRect;
  SrcF: TCustomBitmap32; SrcRectF: TRect;
  SrcB: TCustomBitmap32; SrcRectB: TRect;
  BlendCallback: TBlendRegEx; MasterAlpha: Integer); overload;

const
  MAX_KERNEL_WIDTH = 4;

type
  PKernelEntry = ^TKernelEntry;
  TKernelEntry = array [-MAX_KERNEL_WIDTH..MAX_KERNEL_WIDTH] of Integer;

  TArrayOfKernelEntry = array of TArrayOfInteger;
  PKernelEntryArray = ^TKernelEntryArray;
  TKernelEntryArray = array [0..0] of TArrayOfInteger;

  TFilterMethod = function(Value: TFloat): TFloat of object;

  EBitmapException = class(Exception);
  ESrcInvalidException = class(Exception);
  ENestedException = class(Exception);

  TGetSampleInt = function(X, Y: Integer): TColor32 of object;
  TGetSampleFloat = function(X, Y: TFloat): TColor32 of object;
  TGetSampleFixed = function(X, Y: TFixed): TColor32 of object;

  { TCustomKernel }
  TCustomKernel = class(TPersistent)
  protected
    FObserver: TNotifiablePersistent;
  protected
    procedure AssignTo(Dst: TPersistent); override;
    function RangeCheck: Boolean; virtual;
  public
    constructor Create; virtual;
    procedure Changed;
    function Filter(Value: TFloat): TFloat; virtual; abstract;
    function GetWidth: TFloat; virtual; abstract;
    property Observer: TNotifiablePersistent read FObserver;
  end;
  TCustomKernelClass = class of TCustomKernel;

  { TBoxKernel }
  TBoxKernel = class(TCustomKernel)
  public
    function Filter(Value: TFloat): TFloat; override;
    function GetWidth: TFloat; override;
  end;

  { TLinearKernel }
  TLinearKernel = class(TCustomKernel)
  public
    function Filter(Value: TFloat): TFloat; override;
    function GetWidth: TFloat; override;
  end;

  { TCosineKernel }
  TCosineKernel = class(TCustomKernel)
  public
    function Filter(Value: TFloat): TFloat; override;
    function GetWidth: TFloat; override;
  end;

  { TSplineKernel }
  TSplineKernel = class(TCustomKernel)
  protected
    function RangeCheck: Boolean; override;
  public
    function Filter(Value: TFloat): TFloat; override;
    function GetWidth: TFloat; override;
  end;

  { TMitchellKernel }
  TMitchellKernel = class(TCustomKernel)
  protected
    function RangeCheck: Boolean; override;
  public
    function Filter(Value: TFloat): TFloat; override;
    function GetWidth: TFloat; override;
  end;

  { TCubicKernel }
  TCubicKernel = class(TCustomKernel)
  private
    FCoeff: TFloat;
    procedure SetCoeff(const Value: TFloat);
  protected
    function RangeCheck: Boolean; override;
  public
    constructor Create; override;
    function Filter(Value: TFloat): TFloat; override;
    function GetWidth: TFloat; override;
  published
    property Coeff: TFloat read FCoeff write SetCoeff;
  end;

  { THermiteKernel }
  THermiteKernel = class(TCustomKernel)
  private
    FBias: TFloat;
    FTension: TFloat;
    procedure SetBias(const Value: TFloat);
    procedure SetTension(const Value: TFloat);
  protected
    function RangeCheck: Boolean; override;
  public
    constructor Create; override;
    function Filter(Value: TFloat): TFloat; override;
    function GetWidth: TFloat; override;
  published
    property Bias: TFloat read FBias write SetBias;
    property Tension: TFloat read FTension write SetTension;
  end;

  { TWindowedSincKernel }
  TWindowedSincKernel = class(TCustomKernel)
  private
    FWidth : TFloat;
    FWidthReciprocal : TFloat;
  protected
    function RangeCheck: Boolean; override;
    function Window(Value: TFloat): TFloat; virtual; abstract;
  public
    constructor Create; override;
    function Filter(Value: TFloat): TFloat; override;
    procedure SetWidth(Value: TFloat);
    function GetWidth: TFloat; override;
    property WidthReciprocal : TFloat read FWidthReciprocal;
  published
    property Width: TFloat read FWidth write SetWidth;
  end;

  { TAlbrecht-Kernel }
  TAlbrechtKernel = class(TWindowedSincKernel)
  private
    FTerms: Integer;
    FCoefPointer : Array [0..11] of Double;
    procedure SetTerms(Value : Integer);
  protected
    function Window(Value: TFloat): TFloat; override;
  public
    constructor Create; override;
  published
    property Terms: Integer read FTerms write SetTerms;
  end;

  { TLanczosKernel }
  TLanczosKernel = class(TWindowedSincKernel)
  protected
    function Window(Value: TFloat): TFloat; override;
  public
  end;

  { TGaussianKernel }
  TGaussianKernel = class(TWindowedSincKernel)
  private
    FSigma: TFloat;
    FSigmaReciprocalLn2: TFloat;
    procedure SetSigma(const Value: TFloat);
  protected
    function Window(Value: TFloat): TFloat; override;
  public
    constructor Create; override;
  published
    property Sigma: TFloat read FSigma write SetSigma;
  end;

  { TBlackmanKernel }
  TBlackmanKernel = class(TWindowedSincKernel)
  protected
    function Window(Value: TFloat): TFloat; override;
  end;

  { THannKernel }
  THannKernel = class(TWindowedSincKernel)
  protected
    function Window(Value: TFloat): TFloat; override;
  end;

  { THammingKernel }
  THammingKernel = class(TWindowedSincKernel)
  protected
    function Window(Value: TFloat): TFloat; override;
  end;

  { TSinshKernel }
  TSinshKernel = class(TCustomKernel)
  private
    FWidth: TFloat;
    FCoeff: TFloat;
    procedure SetCoeff(const Value: TFloat);
  protected
    function  RangeCheck: Boolean; override;
  public
    constructor Create; override;
    procedure SetWidth(Value: TFloat);
    function  GetWidth: TFloat; override;
    function  Filter(Value: TFloat): TFloat; override;
  published
    property Coeff: TFloat read FCoeff write SetCoeff;
    property Width: TFloat read GetWidth write SetWidth;
  end;


  { TNearestResampler }
  TNearestResampler = class(TCustomResampler)
  private
    FGetSampleInt: TGetSampleInt;
  protected
    function GetPixelTransparentEdge(X, Y: Integer): TColor32;
    function GetWidth: TFloat; override;
    procedure Resample(
      Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
      Src: TCustomBitmap32; SrcRect: TRect;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); override;
  public
    function GetSampleInt(X, Y: Integer): TColor32; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
    procedure PrepareSampling; override;
  end;

  { TLinearResampler }
  TLinearResampler = class(TCustomResampler)
  private
    FLinearKernel: TLinearKernel;
    FGetSampleFixed: TGetSampleFixed;
  protected
    function GetWidth: TFloat; override;
    function GetPixelTransparentEdge(X, Y: TFixed): TColor32;
    procedure Resample(
      Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
      Src: TCustomBitmap32; SrcRect: TRect;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
    procedure PrepareSampling; override;
  end;

  { TDraftResampler }
  TDraftResampler = class(TLinearResampler)
  protected
    procedure Resample(
      Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
      Src: TCustomBitmap32; SrcRect: TRect;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); override;
  end;

  { TKernelResampler }
  { This resampler class will perform resampling by using an arbitrary
    reconstruction kernel. By using the kmTableNearest and kmTableLinear
    kernel modes, kernel values are precomputed in a look-up table. This
    allows GetSample to execute faster for complex kernels. }

  TKernelMode = (kmDynamic, kmTableNearest, kmTableLinear);

  TKernelResampler = class(TCustomResampler)
  private
    FKernel: TCustomKernel;
    FKernelMode: TKernelMode;
    FWeightTable: TIntegerMap;
    FTableSize: Integer;
    FOuterColor: TColor32;
    procedure SetKernel(const Value: TCustomKernel);
    function GetKernelClassName: string;
    procedure SetKernelClassName(Value: string);
    procedure SetKernelMode(const Value: TKernelMode);
    procedure SetTableSize(Value: Integer);
  protected
    function GetWidth: TFloat; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
    procedure Resample(
      Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
      Src: TCustomBitmap32; SrcRect: TRect;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); override;
    procedure PrepareSampling; override;
    procedure FinalizeSampling; override;
  published
    property KernelClassName: string read GetKernelClassName write SetKernelClassName;
    property Kernel: TCustomKernel read FKernel write SetKernel;
    property KernelMode: TKernelMode read FKernelMode write SetKernelMode;
    property TableSize: Integer read FTableSize write SetTableSize;
  end;

  { TNestedSampler }
  TNestedSampler = class(TCustomSampler)
  private
    FSampler: TCustomSampler;
    FGetSampleInt: TGetSampleInt;
    FGetSampleFixed: TGetSampleFixed;
    FGetSampleFloat: TGetSampleFloat;
    procedure SetSampler(const Value: TCustomSampler);
  protected
    procedure AssignTo(Dst: TPersistent); override;
  public
    constructor Create(ASampler: TCustomSampler); reintroduce; virtual; 
    procedure PrepareSampling; override;
    procedure FinalizeSampling; override;
    function HasBounds: Boolean; override;
    function GetSampleBounds: TFloatRect; override;
  published
    property Sampler: TCustomSampler read FSampler write SetSampler;
  end;

  { TTransformer }
  TReverseTransformInt = procedure(DstX, DstY: Integer; out SrcX, SrcY: Integer) of object;
  TReverseTransformFixed = procedure(DstX, DstY: TFixed; out SrcX, SrcY: TFixed) of object;
  TReverseTransformFloat = procedure(DstX, DstY: TFloat; out SrcX, SrcY: TFloat) of object;

  TTransformer = class(TNestedSampler)
  private
    FTransformation: TTransformation;
    FTransformationReverseTransformInt: TReverseTransformInt;
    FTransformationReverseTransformFixed: TReverseTransformFixed;
    FTransformationReverseTransformFloat: TReverseTransformFloat;
    procedure SetTransformation(const Value: TTransformation);
  public
    constructor Create(ASampler: TCustomSampler; ATransformation: TTransformation); reintroduce;
    procedure PrepareSampling; override;
    function GetSampleInt(X, Y: Integer): TColor32; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
    function HasBounds: Boolean; override;
    function GetSampleBounds: TFloatRect; override;
  published
    property Transformation: TTransformation read FTransformation write SetTransformation;
  end;

  { TSuperSampler }
  TSamplingRange = 1..MaxInt;

  TSuperSampler = class(TNestedSampler)
  private
    FSamplingY: TSamplingRange;
    FSamplingX: TSamplingRange;
    FDistanceX: TFixed;
    FDistanceY: TFixed;
    FOffsetX: TFixed;
    FOffsetY: TFixed;
    FScale: TFixed;
    procedure SetSamplingX(const Value: TSamplingRange);
    procedure SetSamplingY(const Value: TSamplingRange);
  public
    constructor Create(Sampler: TCustomSampler); override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
  published
    property SamplingX: TSamplingRange read FSamplingX write SetSamplingX;
    property SamplingY: TSamplingRange read FSamplingY write SetSamplingY;
  end;

  { TAdaptiveSuperSampler }
  TRecurseProc = function(X, Y, W: TFixed; const C1, C2: TColor32): TColor32 of object;

  TAdaptiveSuperSampler = class(TNestedSampler)
  private
    FMinOffset: TFixed;
    FLevel: Integer;
    FTolerance: Integer;
    procedure SetLevel(const Value: Integer);
    function DoRecurse(X, Y, Offset: TFixed; const A, B, C, D, E: TColor32): TColor32;
    function QuadrantColor(const C1, C2: TColor32; X, Y, Offset: TFixed;
      Proc: TRecurseProc): TColor32;
    function RecurseAC(X, Y, Offset: TFixed; const A, C: TColor32): TColor32;
    function RecurseBD(X, Y, Offset: TFixed; const B, D: TColor32): TColor32;
  protected
    function CompareColors(C1, C2: TColor32): Boolean; virtual;
  public
    constructor Create(Sampler: TCustomSampler); override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
  published
    property Level: Integer read FLevel write SetLevel;
    property Tolerance: Integer read FTolerance write FTolerance;
  end;

  { TPatternSampler }
  TFloatSamplePattern = array of array of TArrayOfFloatPoint;
  TFixedSamplePattern = array of array of TArrayOfFixedPoint;

  TPatternSampler = class(TNestedSampler)
  private
    FPattern: TFixedSamplePattern;
    procedure SetPattern(const Value: TFixedSamplePattern);
  protected
    WrapProcVert: TWrapProc;
  public
    destructor Destroy; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
    property Pattern: TFixedSamplePattern read FPattern write SetPattern;
  end;

  { Auxiliary record used in accumulation routines }
  PBufferEntry = ^TBufferEntry;
  TBufferEntry = record
    B, G, R, A: Integer;
  end;

  { TKernelSampler }
  TKernelSampler = class(TNestedSampler)
  private
    FKernel: TIntegerMap;
    FStartEntry: TBufferEntry;
    FCenterX: Integer;
    FCenterY: Integer;
  protected
    procedure UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
      Weight: Integer); virtual; abstract;
    function ConvertBuffer(var Buffer: TBufferEntry): TColor32; virtual;
  public
    constructor Create(ASampler: TCustomSampler); override;
    destructor Destroy; override;
    function GetSampleInt(X, Y: Integer): TColor32; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
  published
    property Kernel: TIntegerMap read FKernel write FKernel;
    property CenterX: Integer read FCenterX write FCenterX;
    property CenterY: Integer read FCenterY write FCenterY;
  end;

  { TConvolver }
  TConvolver = class(TKernelSampler)
  protected
    procedure UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
      Weight: Integer); override;
  end;

  { TSelectiveConvolver }
  TSelectiveConvolver = class(TConvolver)
  private
    FRefColor: TColor32;
    FDelta: Integer;
    FWeightSum: TBufferEntry;
  protected
    procedure UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
      Weight: Integer); override;
    function ConvertBuffer(var Buffer: TBufferEntry): TColor32; override;
  public
    constructor Create(ASampler: TCustomSampler); override;
    function GetSampleInt(X, Y: Integer): TColor32; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
  published
    property Delta: Integer read FDelta write FDelta;
  end;

  { TMorphologicalSampler }
  TMorphologicalSampler = class(TKernelSampler)
  protected
    function ConvertBuffer(var Buffer: TBufferEntry): TColor32; override;
  end;

  { TDilater }
  TDilater = class(TMorphologicalSampler)
  protected
    procedure UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
      Weight: Integer); override;
  end;

  { TEroder }
  TEroder = class(TMorphologicalSampler)
  protected
    procedure UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
      Weight: Integer); override;
  public
    constructor Create(ASampler: TCustomSampler); override;
  end;

  { TExpander }
  TExpander = class(TKernelSampler)
  protected
    procedure UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
      Weight: Integer); override;
  end;

  { TContracter }
  TContracter = class(TExpander)
  private
    FMaxWeight: TColor32;
  protected
    procedure UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
      Weight: Integer); override;
  public
    procedure PrepareSampling; override;
    function GetSampleInt(X, Y: Integer): TColor32; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
  end;

function CreateJitteredPattern(TileWidth, TileHeight, SamplesX, SamplesY: Integer): TFixedSamplePattern;

{ Convolution and morphological routines }
procedure Convolve(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
procedure Dilate(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
procedure Erode(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
procedure Expand(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
procedure Contract(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);

{ Auxiliary routines for accumulating colors in a buffer }
procedure IncBuffer(var Buffer: TBufferEntry; Color: TColor32); {$IFDEF USEINLINING} inline; {$ENDIF}
procedure MultiplyBuffer(var Buffer: TBufferEntry; W: Integer); {$IFDEF USEINLINING} inline; {$ENDIF}
function BufferToColor32(Buffer: TBufferEntry; Shift: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure ShrBuffer(var Buffer: TBufferEntry; Shift: Integer); {$IFDEF USEINLINING} inline; {$ENDIF}

{ Registration routines }
procedure RegisterResampler(ResamplerClass: TCustomResamplerClass);
procedure RegisterKernel(KernelClass: TCustomKernelClass);

var
  KernelList: TClassList;
  ResamplerList: TClassList;

const
  EMPTY_ENTRY: TBufferEntry = (B: 0; G: 0; R: 0; A: 0);

var
  BlockAverage: function (Dlx, Dly, RowSrc, OffSrc: Cardinal): TColor32;
  Interpolator: function(WX_256, WY_256: Cardinal; C11, C21: PColor32): TColor32;

implementation

uses
  GR32_LowLevel, GR32_Rasterizers, GR32_Math, Math;

const
  SDstNil = 'Destination bitmap is nil';
  SSrcNil = 'Source bitmap is nil';
  SSrcInvalid = 'Source rectangle is invalid';
  SSamplerNil = 'Nested sampler is nil';

  cAlbrecht2    : array [0..1] of Double = (5.383553946707251e-1, 4.616446053292749e-1);
  cAlbrecht3    : array [0..2] of Double = (3.46100822018625e-1,  4.97340635096738e-1,
                                            1.56558542884637e-1);
  cAlbrecht4    : array [0..3] of Double = (2.26982412792069e-1,  4.57254070828427e-1,
                                            2.73199027957384e-1,  4.25644884221201e-2);
  cAlbrecht5    : array [0..4] of Double = (1.48942606015830e-1,  3.86001173639176e-1,
                                            3.40977403214053e-1,  1.139879604246e-1,
                                            1.00908567063414e-2);
  cAlbrecht6    : array [0..5] of Double = (9.71676200107429e-2,  3.08845222524055e-1,
                                            3.62623371437917e-1,  1.88953325525116e-1,
                                            4.02095714148751e-2,  2.20088908729420e-3);
  cAlbrecht7    : array [0..6] of Double = (6.39644241143904e-2,  2.39938645993528e-1,
                                            3.50159563238205e-1,  2.47741118970808e-1,
                                            8.54382560558580e-2,  1.23202033692932e-2,
                                            4.37788257917735e-4);
  cAlbrecht8    : array [0..7] of Double = (4.21072107042137e-2,  1.82076226633776e-1,
                                            3.17713781059942e-1,  2.84438001373442e-1,
                                            1.36762237777383e-1,  3.34038053504025e-2,
                                            3.41677216705768e-3,  8.19649337831348e-5);
  cAlbrecht9    : array [0..8] of Double = (2.76143731612611e-2,  1.35382228758844e-1,
                                            2.75287234472237e-1,  2.98843335317801e-1,
                                            1.85319330279284e-1,  6.48884482549063e-2,
                                            1.17641910285655e-2,  8.85987580106899e-4,
                                            1.48711469943406e-5);
  cAlbrecht10   : array [0..9] of Double = (1.79908225352538e-2,  9.87959586065210e-2,
                                            2.29883817001211e-1,  2.94113019095183e-1,
                                            2.24338977814325e-1,  1.03248806248099e-1,
                                            2.75674109448523e-2,  3.83958622947123e-3,
                                            2.18971708430106e-4,  2.62981665347889e-6);
  cAlbrecht11  : array [0..10] of Double = (1.18717127796602e-2,  7.19533651951142e-2,
                                            1.87887160922585e-1,  2.75808174097291e-1,
                                            2.48904243244464e-1,  1.41729867200712e-1,
                                            5.02002976228256e-2,  1.04589649084984e-2,
                                            1.13615112741660e-3,  4.96285981703436e-5,
                                            4.34303262685720e-7);
type
  TTransformationAccess = class(TTransformation);
  TCustomBitmap32Access = class(TCustomBitmap32);
  TCustomResamplerAccess = class(TCustomResampler);

  PPointRec = ^TPointRec;
  TPointRec = record
    Pos: Integer;
    Weight: Cardinal;
  end;

  TCluster = array of TPointRec;
  TMappingTable = array of TCluster;


type
  TKernelSamplerClass = class of TKernelSampler;

{ Auxiliary rasterization routine for kernel-based samplers }
procedure RasterizeKernelSampler(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap;
  CenterX, CenterY: Integer; SamplerClass: TKernelSamplerClass);
var
  Sampler: TKernelSampler;
  Rasterizer: TRasterizer;
begin
  Rasterizer := DefaultRasterizerClass.Create;
  try
    Dst.SetSizeFrom(Src);
    Sampler := SamplerClass.Create(Src.Resampler);
    Sampler.Kernel := Kernel;
    try
      Rasterizer.Sampler := Sampler;
      Rasterizer.Rasterize(Dst);
    finally
      Sampler.Free;
    end;
  finally
    Rasterizer.Free;
  end;
end;

procedure Convolve(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
begin
  RasterizeKernelSampler(Src, Dst, Kernel, CenterX, CenterY, TConvolver);
end;

procedure Dilate(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
begin
  RasterizeKernelSampler(Src, Dst, Kernel, CenterX, CenterY, TDilater);
end;

procedure Erode(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
begin
  RasterizeKernelSampler(Src, Dst, Kernel, CenterX, CenterY, TEroder);
end;

procedure Expand(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
begin
  RasterizeKernelSampler(Src, Dst, Kernel, CenterX, CenterY, TExpander);
end;

procedure Contract(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
begin
  RasterizeKernelSampler(Src, Dst, Kernel, CenterX, CenterY, TContracter);
end;

{ Auxiliary routines }

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

procedure MultiplyBuffer(var Buffer: TBufferEntry; W: Integer);
begin
  Buffer.B := Buffer.B * W;
  Buffer.G := Buffer.G * W;
  Buffer.R := Buffer.R * W;
  Buffer.A := Buffer.A * W;
end;

procedure ShrBuffer(var Buffer: TBufferEntry; Shift: Integer);
begin
  Buffer.B := Buffer.B shr Shift;
  Buffer.G := Buffer.G shr Shift;
  Buffer.R := Buffer.R shr Shift;
  Buffer.A := Buffer.A shr Shift;
end;

function BufferToColor32(Buffer: TBufferEntry; Shift: Integer): TColor32;
begin
  with TColor32Entry(Result) do
  begin
    B := Buffer.B shr Shift;
    G := Buffer.G shr Shift;
    R := Buffer.R shr Shift;
    A := Buffer.A shr Shift;
  end;
end;

procedure CheckBitmaps(Dst, Src: TCustomBitmap32); {$IFDEF USEINLINING}inline;{$ENDIF}
begin
  if not Assigned(Dst) then raise EBitmapException.Create(SDstNil);
  if not Assigned(Src) then raise EBitmapException.Create(SSrcNil);
end;

procedure BlendBlock(
  Dst: TCustomBitmap32; DstRect: TRect;
  Src: TCustomBitmap32; SrcX, SrcY: Integer;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcP, DstP: PColor32;
  SP, DP: PColor32;
  MC: TColor32;
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
        BlendLine := BLEND_LINE[Src.CombineMode]^;
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
        begin
          BlendLine(SrcP, DstP, W);
          Inc(SrcP, Src.Width);
          Inc(DstP, Dst.Width);
        end
      end
      else
      begin
        BlendLineEx := BLEND_LINE_EX[Src.CombineMode]^;
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
        begin
          BlendLineEx(SrcP, DstP, W, Src.MasterAlpha);
          Inc(SrcP, Src.Width);
          Inc(DstP, Dst.Width);
        end
      end;
    dmTransparent:
      begin
        MC := Src.OuterColor;
        for DstY := DstRect.Top to DstRect.Bottom - 1 do
        begin
          SP := SrcP;
          DP := DstP;
          { TODO: Write an optimized routine for fast masked transfers. }
          for I := 0 to W - 1 do
          begin
            if MC <> SP^ then DP^ := SP^;
            Inc(SP); Inc(DP);
          end;
          Inc(SrcP, Src.Width);
          Inc(DstP, Dst.Width);
        end;
      end;
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
  Dst: TCustomBitmap32; DstX: Integer; DstY: Integer; DstClip: TRect;
  Src: TCustomBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcX, SrcY, SrcWidth, SrcHeight: Integer;
begin
  CheckBitmaps(Dst, Src);
  if Dst.Empty or Src.Empty or ((CombineOp = dmBlend) and (Src.MasterAlpha = 0)) then Exit;

  SrcWidth := SrcRect.Right - SrcRect.Left;
  SrcHeight := SrcRect.Bottom - SrcRect.Top;

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

  Dst.Changed(MakeRect(DstX, DstY, DstX + SrcWidth, DstY + SrcHeight));
end;

{$WARNINGS OFF}
procedure BlockTransferX(
  Dst: TCustomBitmap32; DstX, DstY: TFixed;
  Src: TCustomBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent = nil);
type
  TColor32Array = array [0..1] of TColor32;
  PColor32Array = ^TColor32Array;
var
  I, Index, SrcW, SrcRectW, SrcRectH, DstW, DstH: Integer;
  FracX, FracY: Integer;
  Buffer: array [0..1] of TArrayOfColor32;
  SrcP, Buf1, Buf2: PColor32Array;
  DstP: PColor32;
  C1, C2, C3, C4: TColor32;
  LW, RW, TW, BW, MA: Integer;
  DstBounds: TRect;

  BlendLineEx: TBlendLineEx;
  BlendMemEx: TBlendMemEx;
begin
  CheckBitmaps(Dst, Src);
  if Dst.Empty or Src.Empty or ((CombineOp = dmBlend) and (Src.MasterAlpha = 0)) then Exit;

  SrcRectW := SrcRect.Right - SrcRect.Left - 1;
  SrcRectH := SrcRect.Bottom - SrcRect.Top - 1;

  if not Dst.MeasuringMode then
  begin
    FracX := (DstX and $FFFF) shr 8;
    FracY := (DstY and $FFFF) shr 8;

    DstX := DstX div $10000;
    DstY := DstY div $10000;

    DstW := Dst.Width;
    DstH := Dst.Height;

    MA := Src.MasterAlpha;

    if (DstX >= DstW) or (DstY >= DstH) or (MA = 0) then Exit;

    if (DstX + SrcRectW <= 0) or (Dsty + SrcRectH <= 0) then Exit;

    if DstX < 0 then LW := $FF else LW := FracX xor $FF;
    if DstY < 0 then TW := $FF else TW := FracY xor $FF;
    if DstX + SrcRectW >= DstW then RW := $FF else RW := FracX;
    if DstY + SrcRectH >= DstH then BW := $FF else BW := FracY;

    DstBounds := Dst.BoundsRect;
    Dec(DstBounds.Right);
    Dec(DstBounds.Bottom);
    OffsetRect(DstBounds, SrcRect.Left - DstX, SrcRect.Top - DstY);
    IntersectRect(SrcRect, SrcRect, DstBounds);

    if IsRectEmpty(SrcRect) then Exit;

    SrcW := Src.Width;

    SrcRectW := SrcRect.Right - SrcRect.Left;
    SrcRectH := SrcRect.Bottom - SrcRect.Top;

    SetLength(Buffer[0], SrcRectW + 1);
    SetLength(Buffer[1], SrcRectW + 1);

    if DstX < 0 then DstX := 0;
    if DstY < 0 then DstY := 0;

    BlendLineEx := BLEND_LINE_EX[Src.CombineMode]^;
    BlendMemEx := BLEND_MEM_EX[Src.CombineMode]^;

    try
      SrcP := PColor32Array(Src.PixelPtr[SrcRect.Left, SrcRect.Top - 1]);
      DstP := Dst.PixelPtr[DstX, DstY];

      Buf1 := @Buffer[0][0];
      Buf2 := @Buffer[1][0];

      if SrcRect.Top > 0 then
      begin
        MoveLongWord(SrcP[0], Buf1[0], SrcRectW);
        CombineLine(@Buf1[1], @Buf1[0], SrcRectW, FracX);

        if SrcRect.Left > 0 then
          C2 := CombineReg(PColor32(Integer(SrcP) - 4)^, SrcP[0], FracX xor $FF)
        else
          C2 := SrcP[0];

        if SrcRect.Right < SrcW then
          C4 := CombineReg(SrcP[SrcRectW - 1], SrcP[SrcRectW], FracX)
        else
          C4 := SrcP[SrcRectW - 1];
      end;

      Inc(PColor32(SrcP), SrcW);
      MoveLongWord(SrcP^, Buf2^, SrcRectW);
      CombineLine(@Buf2[1], @Buf2[0], SrcRectW, FracX xor $FF);

      if SrcRect.Left > 0 then
        C1 := CombineReg(PColor32(Integer(SrcP) - 4)^, SrcP[0], FracX)
      else
        C1 := SrcP[0];

      if SrcRect.Right < SrcW then
        C3 := CombineReg(SrcP[SrcRectW - 1], SrcP[SrcRectW], FracX)
      else
        C3 := SrcP[SrcRectW - 1];

      if SrcRect.Top > 0 then
      begin
        BlendMemEx(CombineReg(C1, C2, FracY), DstP^, LW * TW * MA shr 16);
        CombineLine(@Buf2[0], @Buf1[0], SrcRectW, FracY xor $FF);
      end
      else
      begin
        BlendMemEx(C1, DstP^, LW * TW * MA shr 16);
        MoveLongWord(Buf2^, Buf1^, SrcRectW);
      end;

      Inc(DstP, 1);
      BlendLineEx(@Buf1[0], DstP, SrcRectW - 1, TW * MA shr 8);

      Inc(DstP, SrcRectW - 1);

      if SrcRect.Top > 0 then
        BlendMemEx(CombineReg(C3, C4, FracY), DstP^, RW * TW * MA shr 16)
      else
        BlendMemEx(C3, DstP^, RW * TW * MA shr 16);

      Inc(DstP, DstW - SrcRectW);

      Index := 1;
      for I := SrcRect.Top to SrcRect.Bottom - 2 do
      begin
        Buf1 := @Buffer[Index][0];
        Buf2 := @Buffer[Index xor 1][0];
        Inc(PColor32(SrcP), SrcW);

        MoveLongWord(SrcP[0], Buf2^, SrcRectW);

        // Horizontal translation
        CombineLine(@Buf2[1], @Buf2[0], SrcRectW, FracX xor $FF);

        if SrcRect.Left > 0 then
          C2 := CombineReg(PColor32(Integer(SrcP) - 4)^, SrcP[0], FracX xor $FF)
        else
          C2 := SrcP[0];

        BlendMemEx(CombineReg(C1, C2, FracY), DstP^, LW * MA shr 8);
        Inc(DstP);
        C1 := C2;

        // Vertical translation
        CombineLine(@Buf2[0], @Buf1[0], SrcRectW, FracY xor $FF);

        // Blend horizontal line to Dst
        BlendLineEx(@Buf1[0], DstP, SrcRectW - 1, MA);
        Inc(DstP, SrcRectW - 1);

        if SrcRect.Right < SrcW then
          C4 := CombineReg(SrcP[SrcRectW - 1], SrcP[SrcRectW], FracX)
        else
          C4 := SrcP[SrcRectW - 1];

        BlendMemEx(CombineReg(C3, C4, FracY), DstP^, RW * MA shr 8);

        Inc(DstP, DstW - SrcRectW);
        C3 := C4;

        Index := Index xor 1;
      end;

      Buf1 := @Buffer[Index][0];
      Buf2 := @Buffer[Index xor 1][0];

      Inc(PColor32(SrcP), SrcW);

      if SrcRect.Bottom < Src.Height then
      begin
        MoveLongWord(SrcP[0], Buf2^, SrcRectW);
        CombineLine(@Buf2[1], @Buf2[0], SrcRectW, FracY xor $FF);
        CombineLine(@Buf2[0], @Buf1[0], SrcRectW, FracY xor $FF);
        if SrcRect.Left > 0 then
          C2 := CombineReg(PColor32(Integer(SrcP) - 4)^, SrcP[0], FracX xor $FF)
        else
          C2 := SrcP[0];
        BlendMemEx(CombineReg(C1, C2, FracY), DstP^, LW * BW * MA shr 16)
      end
      else
        BlendMemEx(C1, DstP^, LW * BW * MA shr 16);

      Inc(DstP);
      BlendLineEx(@Buf1[0], DstP, SrcRectW - 1, BW * MA shr 8);
      Inc(DstP, SrcRectW - 1);

      if SrcRect.Bottom < Src.Height then
      begin
        if SrcRect.Right < SrcW then
          C4 := CombineReg(SrcP[SrcRectW - 1], SrcP[SrcRectW], FracX)
        else
          C4 := SrcP[SrcRectW - 1];
        BlendMemEx(CombineReg(C3, C4, FracY), DstP^, RW * BW * MA shr 16);
      end
      else
        BlendMemEx(C3, DstP^, RW * BW * MA shr 16);

    finally
      EMMS;
      Buffer[0] := nil;
      Buffer[1] := nil;
    end;
  end;

  Dst.Changed(MakeRect(DstX, DstY, DstX + SrcRectW + 1, DstY + SrcRectH + 1));
end;
{$WARNINGS ON}

procedure BlendTransfer(
  Dst: TCustomBitmap32; DstX, DstY: Integer; DstClip: TRect;
  SrcF: TCustomBitmap32; SrcRectF: TRect;
  SrcB: TCustomBitmap32; SrcRectB: TRect;
  BlendCallback: TBlendReg);
var
  I, J, SrcFX, SrcFY, SrcBX, SrcBY: Integer;
  PSrcF, PSrcB, PDst: PColor32Array;
begin
  if not Assigned(Dst) then raise EBitmapException.Create(SDstNil);
  if not Assigned(SrcF) then raise EBitmapException.Create(SSrcNil);
  if not Assigned(SrcB) then raise EBitmapException.Create(SSrcNil);

  if Dst.Empty or SrcF.Empty or SrcB.Empty or not Assigned(BlendCallback) then Exit;

  if not Dst.MeasuringMode then
  begin
    SrcFX := SrcRectF.Left - DstX;
    SrcFY := SrcRectF.Top - DstY;
    SrcBX := SrcRectB.Left - DstX;
    SrcBY := SrcRectB.Top - DstY;

    IntersectRect(DstClip, DstClip, Dst.BoundsRect);
    IntersectRect(SrcRectF, SrcRectF, SrcF.BoundsRect);
    IntersectRect(SrcRectB, SrcRectB, SrcB.BoundsRect);

    OffsetRect(SrcRectF, -SrcFX, -SrcFY);
    OffsetRect(SrcRectB, -SrcBX, -SrcFY);

    IntersectRect(DstClip, DstClip, SrcRectF);
    IntersectRect(DstClip, DstClip, SrcRectB);

    if not IsRectEmpty(DstClip) then
    try
      for I := DstClip.Top to DstClip.Bottom - 1 do
      begin
        PSrcF := PColor32Array(SrcF.PixelPtr[SrcFX, SrcFY + I]);
        PSrcB := PColor32Array(SrcB.PixelPtr[SrcBX, SrcBY + I]);
        PDst := Dst.ScanLine[I];
        for J := DstClip.Left to DstClip.Right - 1 do
          PDst[J] := BlendCallback(PSrcF[J], PSrcB[J]);
      end;
    finally
      EMMS;
    end;
  end;
  Dst.Changed(DstClip);
end;

procedure BlendTransfer(
  Dst: TCustomBitmap32; DstX, DstY: Integer; DstClip: TRect;
  SrcF: TCustomBitmap32; SrcRectF: TRect;
  SrcB: TCustomBitmap32; SrcRectB: TRect;
  BlendCallback: TBlendRegEx; MasterAlpha: Integer);
var
  I, J, SrcFX, SrcFY, SrcBX, SrcBY: Integer;
  PSrcF, PSrcB, PDst: PColor32Array;
begin
  if not Assigned(Dst) then raise EBitmapException.Create(SDstNil);
  if not Assigned(SrcF) then raise EBitmapException.Create(SSrcNil);
  if not Assigned(SrcB) then raise EBitmapException.Create(SSrcNil);

  if Dst.Empty or SrcF.Empty or SrcB.Empty or not Assigned(BlendCallback) then Exit;

  if not Dst.MeasuringMode then
  begin
    SrcFX := SrcRectF.Left - DstX;
    SrcFY := SrcRectF.Top - DstY;
    SrcBX := SrcRectB.Left - DstX;
    SrcBY := SrcRectB.Top - DstY;

    IntersectRect(DstClip, DstClip, Dst.BoundsRect);
    IntersectRect(SrcRectF, SrcRectF, SrcF.BoundsRect);
    IntersectRect(SrcRectB, SrcRectB, SrcB.BoundsRect);

    OffsetRect(SrcRectF, -SrcFX, -SrcFY);
    OffsetRect(SrcRectB, -SrcBX, -SrcFY);

    IntersectRect(DstClip, DstClip, SrcRectF);
    IntersectRect(DstClip, DstClip, SrcRectB);

    if not IsRectEmpty(DstClip) then
    try
      for I := DstClip.Top to DstClip.Bottom - 1 do
      begin
        PSrcF := PColor32Array(SrcF.PixelPtr[SrcFX, SrcFY + I]);
        PSrcB := PColor32Array(SrcB.PixelPtr[SrcBX, SrcBY + I]);
        PDst := Dst.ScanLine[I];
        for J := DstClip.Left to DstClip.Right - 1 do
          PDst[J] := BlendCallback(PSrcF[J], PSrcB[J], MasterAlpha);
      end;
    finally
      EMMS;
    end;
  end;
  Dst.Changed(DstClip);
end;

procedure StretchNearest(
  Dst: TCustomBitmap32; DstRect, DstClip: TRect;
  Src: TCustomBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  R: TRect;
  SrcW, SrcH, DstW, DstH, DstClipW, DstClipH: Integer;
  SrcY, OldSrcY: Integer;
  I, J: Integer;
  MapHorz: array of Integer;
  SrcLine, DstLine: PColor32Array;
  Buffer: TArrayOfColor32;
  Scale: TFloat;
  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;
  DstLinePtr, MapPtr: PColor32;
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
            DstLinePtr := @DstLine[0];
            MapPtr := @MapHorz[0];
            for I := 0 to DstClipW - 1 do
            begin
              DstLinePtr^ := SrcLine[MapPtr^];
              Inc(DstLinePtr);
              Inc(MapPtr);
            end;
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
          BlendLine := BLEND_LINE[Src.CombineMode]^;
          BlendLineEx := nil; // stop compiler warnings...
        end
        else
        begin
          BlendLineEx := BLEND_LINE_EX[Src.CombineMode]^;
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
            DstLinePtr := @Buffer[0];
            MapPtr := @MapHorz[0];
            for I := 0 to DstClipW - 1 do
            begin
              DstLinePtr^ := SrcLine[MapPtr^];
              Inc(DstLinePtr);
              Inc(MapPtr);
            end;
            OldSrcY := SrcY;
          end;

          case CombineOp of
            dmBlend:
              if Src.MasterAlpha >= 255 then
                BlendLine(@Buffer[0], @DstLine[0], DstClipW)
              else
                BlendLineEx(@Buffer[0], @DstLine[0], DstClipW, Src.MasterAlpha);
            dmTransparent:
              for I := 0 to DstClipW - 1 do
                if Buffer[I] <> Src.OuterColor then DstLine[I] := Buffer[I];
            dmCustom:
              for I := 0 to DstClipW - 1 do
                CombineCallBack(Buffer[I], DstLine[I], Src.MasterAlpha);
          end;

          Inc(DstLine, Dst.Width);
        end;
      end;
    end;
  finally
    EMMS;
  end;
end;

procedure StretchHorzStretchVertLinear(
  Dst: TCustomBitmap32; DstRect, DstClip: TRect;
  Src: TCustomBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
//Assure DstRect is >= SrcRect, otherwise quality loss will occur
var
  SrcW, SrcH, DstW, DstH, DstClipW, DstClipH: Integer;
  MapHorz, MapVert: array of TPointRec;
  t2, Scale: TFloat;
  SrcLine, DstLine: PColor32Array;
  SrcIndex: Integer;
  SrcPtr1, SrcPtr2: PColor32;
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
    //MapHorz[I].Weight:= MapHorz[I].Weight shl 16 + MapHorz[I].Weight;
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
    //MapVert[I].Weight := MapVert[I].Weight shl 16 + MapVert[I].Weight;
  end;
  I := DstClipH - 1;
  while MapVert[I].Pos = SrcRect.Bottom - 1 do
  begin
    Dec(MapVert[I].Pos);
    MapVert[I].Weight := 0;
    Dec(I);
  end;

  DstLine := PColor32Array(Dst.PixelPtr[DstClip.Left, DstClip.Top]);
  SrcW := Src.Width;
  DstW := Dst.Width;
  case CombineOp of
    dmOpaque:
      for J := 0 to DstClipH - 1 do
      begin
        SrcLine := Src.ScanLine[MapVert[J].Pos];
        WY := MapVert[J].Weight;

        SrcIndex := MapHorz[0].Pos;
        SrcPtr1 := @SrcLine[SrcIndex];
        SrcPtr2 := @SrcLine[SrcIndex + SrcW];
        for I := 0 to DstClipW - 1 do
        begin
          if SrcIndex <> MapHorz[I].Pos then
          begin
            SrcIndex := MapHorz[I].Pos;
            SrcPtr1 := @SrcLine[SrcIndex];
            SrcPtr2 := @SrcLine[SrcIndex + SrcW];
          end;
          DstLine[I] := Interpolator(MapHorz[I].Weight, WY, SrcPtr1, SrcPtr2);
        end;
        Inc(DstLine, DstW);
      end;
    dmBlend:
      begin
        BlendMemEx := BLEND_MEM_EX[Src.CombineMode]^;
        for J := 0 to DstClipH - 1 do
        begin
          SrcLine := Src.ScanLine[MapVert[J].Pos];
          WY := MapVert[J].Weight;
          SrcIndex := MapHorz[0].Pos;
          SrcPtr1 := @SrcLine[SrcIndex];
          SrcPtr2 := @SrcLine[SrcIndex + SrcW];
          for I := 0 to DstClipW - 1 do
          begin
            if SrcIndex <> MapHorz[I].Pos then
            begin
              SrcIndex := MapHorz[I].Pos;
              SrcPtr1 := @SrcLine[SrcIndex];
              SrcPtr2 := @SrcLine[SrcIndex + SrcW];
            end;
            C := Interpolator(MapHorz[I].Weight, WY, SrcPtr1, SrcPtr2);
            BlendMemEx(C, DstLine[I], Src.MasterAlpha)
          end;
          Inc(DstLine, Dst.Width);
        end
      end;
    dmTransparent:
      begin
        for J := 0 to DstClipH - 1 do
        begin
          SrcLine := Src.ScanLine[MapVert[J].Pos];
          WY := MapVert[J].Weight;
          SrcIndex := MapHorz[0].Pos;
          SrcPtr1 := @SrcLine[SrcIndex];
          SrcPtr2 := @SrcLine[SrcIndex + SrcW];
          for I := 0 to DstClipW - 1 do
          begin
            if SrcIndex <> MapHorz[I].Pos then
            begin
              SrcIndex := MapHorz[I].Pos;
              SrcPtr1 := @SrcLine[SrcIndex];
              SrcPtr2 := @SrcLine[SrcIndex + SrcW];
            end;
            C := Interpolator(MapHorz[I].Weight, WY, SrcPtr1, SrcPtr2);
            if C <> Src.OuterColor then DstLine[I] := C;
          end;
          Inc(DstLine, Dst.Width);
        end
      end;
  else // cmCustom
    for J := 0 to DstClipH - 1 do
    begin
      SrcLine := Src.ScanLine[MapVert[J].Pos];
      WY := MapVert[J].Weight;
      SrcIndex := MapHorz[0].Pos;    
      SrcPtr1 := @SrcLine[SrcIndex];    
      SrcPtr2 := @SrcLine[SrcIndex + SrcW];    
      for I := 0 to DstClipW - 1 do    
      begin    
        if SrcIndex <> MapHorz[I].Pos then    
        begin    
          SrcIndex := MapHorz[I].Pos;    
          SrcPtr1 := @SrcLine[SrcIndex];    
          SrcPtr2 := @SrcLine[SrcIndex + SrcW];    
        end;    
        C := Interpolator(MapHorz[I].Weight, WY, SrcPtr1, SrcPtr2);
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
  Kernel: TCustomKernel): TMappingTable;
var
  SrcW, DstW, ClipW: Integer;
  Filter: TFilterMethod;
  FilterWidth: TFloat;
  Scale, OldScale: TFloat;
  Center: TFloat;
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

  Filter := Kernel.Filter;
  FilterWidth := Kernel.GetWidth;
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
  Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TCustomBitmap32; SrcRect: TRect;
  Kernel: TCustomKernel;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  DstClipW: Integer;
  MapX, MapY: TMappingTable;
  I, J, X, Y: Integer;
  MapXLoPos, MapXHiPos: Integer;
  HorzBuffer: array of TBufferEntry;
  ClusterX, ClusterY: TCluster;
  Wt, Cr, Cg, Cb, Ca: Integer;
  C: Cardinal;
  ClustYW: Integer;
  DstLine: PColor32Array;
  RangeCheck: Boolean;
  BlendMemEx: TBlendMemEx;
begin
  if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then
    CombineOp := dmOpaque;

  { check source and destination }
  if (CombineOp = dmBlend) and (Src.MasterAlpha = 0) then Exit;

  BlendMemEx := BLEND_MEM_EX[Src.CombineMode]^; // store in local variable

  DstClipW := DstClip.Right - DstClip.Left;

  // mapping tables
  MapX := BuildMappingTable(DstRect.Left, DstRect.Right, DstClip.Left, DstClip.Right, SrcRect.Left, SrcRect.Right, Kernel);
  MapY := BuildMappingTable(DstRect.Top, DstRect.Bottom, DstClip.Top, DstClip.Bottom, SrcRect.Top, SrcRect.Bottom, Kernel);
  ClusterX := nil;
  ClusterY := nil;
  try
    RangeCheck := Kernel.RangeCheck; //StretchFilter in [sfLanczos, sfMitchell];
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
          dmTransparent: if C <> Src.OuterColor then DstLine[I] := C;
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

function BlockAverage_Pas(Dlx, Dly, RowSrc, OffSrc: Cardinal): TColor32;
type
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

{$IFDEF TARGET_x86}
function BlockAverage_MMX(Dlx, Dly, RowSrc, OffSrc: Cardinal): TColor32;
asm
   push       ebx
   push       esi
   push       edi

   mov        ebx,OffSrc
   mov        esi,eax
   mov        edi,edx

   shl        esi,$02
   sub        ebx,esi

   pxor       mm1, mm1
   pxor       mm2, mm2
   pxor       mm7, mm7

 @@LoopY:
   mov        esi,eax
   pxor       mm0, mm0
   lea        ecx,[ecx+esi*4]
   neg        esi
 @@LoopX:
   movd       mm6,[ecx+esi*4]
   punpcklbw  mm6, mm7
   paddw      mm0, mm6
   inc        esi
   jnz        @@LoopX

   movq       mm6, mm0
   punpcklwd  mm6, mm7
   paddd      mm1, mm6
   movq       mm6, mm0
   punpckhwd  mm6, mm7
   paddd      mm2, mm6
   add        ecx,ebx
   dec        edx
   jnz        @@LoopY

   mul        edi
   mov        ecx,eax
   mov        eax,$01000000
   div        ecx
   mov        ecx,eax

   movd       eax, mm1
   mul        ecx
   shr        eax,$18
   mov        edi,eax

   psrlq      mm1, $20
   movd       eax, mm1
   mul        ecx
   shr        eax,$10
   and        eax,$0000FF00
   add        edi,eax

   movd       eax, mm2
   mul        ecx
   shr        eax,$08
   and        eax,$00FF0000
   add        edi,eax

   psrlq      mm2, $20
   movd       eax, mm2
   mul        ecx
   and        eax,$FF000000
   add        eax,edi

   pop        edi
   pop        esi
   pop        ebx
end;

function BlockAverage_3dNow(Dlx, Dly, RowSrc, OffSrc: Cardinal): TColor32;
asm
   push       ebx
   push       esi
   push       edi

   mov        ebx,OffSrc
   mov        esi,eax
   mov        edi,edx

   shl        esi,$02
   sub        ebx,esi

   pxor       mm1, mm1
   pxor       mm2, mm2
   pxor       mm7, mm7

 @@LoopY:
   mov        esi,eax
   pxor       mm0, mm0
   lea        ecx,[ecx+esi*4]
   neg        esi
   db $0F,$0D,$84,$B1,$00,$02,$00,$00 // PREFETCH [ecx + esi * 4 + 512]
 @@LoopX:
   movd       mm6, [ecx + esi * 4]
   punpcklbw  mm6, mm7
   paddw      mm0, mm6
   inc        esi

   jnz        @@LoopX

   movq       mm6, mm0
   punpcklwd  mm6, mm7
   paddd      mm1, mm6
   movq       mm6, mm0
   punpckhwd  mm6, mm7
   paddd      mm2, mm6
   add        ecx,ebx
   dec        edx

   jnz        @@LoopY

   mul        edi
   mov        ecx,eax
   mov        eax,$01000000
   div        ecx
   mov        ecx,eax

   movd       eax, mm1
   mul        ecx
   shr        eax,$18
   mov        edi,eax

   psrlq      mm1, $20
   movd       eax, mm1
   mul        ecx
   shr        eax,$10
   and        eax,$0000FF00
   add        edi,eax

   movd       eax, mm2
   mul        ecx
   shr        eax,$08
   and        eax,$00FF0000
   add        edi,eax

   psrlq      mm2, $20
   movd       eax, mm2
   mul        ecx
   and        eax,$FF000000
   add        eax,edi

   pop        edi
   pop        esi
   pop        ebx
end;

{$ENDIF}


procedure DraftResample(Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TCustomBitmap32; SrcRect: TRect; Kernel: TCustomKernel;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH,
  DstW, DstH,
  DstClipW, DstClipH: Cardinal;
  RowSrc, OffSrc,
  dy, dx,
  c1, c2, r1, r2,
  xs, xsrc: Cardinal;
  C: TColor32;
  DstLine: PColor32Array;
  ScaleFactor: TFloat;
  I,J, sc, sr, cx, cy: Integer;
  BlendMemEx: TBlendMemEx;
begin
 { rangechecking and rect intersection done by caller }

  SrcW := SrcRect.Right  - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;

  DstW := DstRect.Right  - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;

  DstClipW := DstClip.Right - DstClip.Left;
  DstClipH := DstClip.Bottom - DstClip.Top;

  BlendMemEx := BLEND_MEM_EX[Src.CombineMode]^;

  if (DstW > SrcW)or(DstH > SrcH) then begin
    if (SrcW < 2) or (SrcH < 2) then
      Resample(Dst, DstRect, DstClip, Src, SrcRect, Kernel, CombineOp,
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
        c2 := FixedMul(J, sc);
        r1 := 0;
        r2 := xs;
        xsrc := RowSrc;

        case CombineOp of
          dmOpaque:
            for I := 2  to DstClipW do
            begin
              dx := r2 - r1;  r1 := r2;
              r2 := FixedMul(I, sr);
              DstLine[DstClip.Left + I] := BlockAverage(dx, dy, xsrc, OffSrc);
              xsrc := xsrc + dx shl 2;
            end;
          dmBlend:
            for I := 2  to DstClipW do
            begin
              dx := r2 - r1;  r1 := r2;
              r2 := FixedMul(I, sr);
              BlendMemEx(BlockAverage(dx, dy, xsrc, OffSrc), DstLine[DstClip.Left + I], Src.MasterAlpha);
              xsrc := xsrc + dx shl 2;
            end;
          dmTransparent:
            for I := 2  to DstClipW do
            begin
              dx := r2 - r1;  r1 := r2;
              r2 := FixedMul(I, sr);
              C := BlockAverage(dx, dy, xsrc, OffSrc);
              if C <> Src.OuterColor then DstLine[DstClip.Left + I] := C;
              xsrc := xsrc + dx shl 2;
            end;
          dmCustom:
            for I := 2  to DstClipW do
            begin
              dx := r2 - r1;  r1 := r2;
              r2 := FixedMul(I, sr);
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

function Interpolator_Pas(WX_256, WY_256: Cardinal; C11, C21: PColor32): TColor32;
var
  C1, C3: TColor32;
begin
  if WX_256 > $FF then WX_256:= $FF;
  if WY_256 > $FF then WY_256:= $FF;
  C1 := C11^; Inc(C11);
  C3 := C21^; Inc(C21);
  Result := CombineReg(CombineReg(C1, C11^, WX_256),
                       CombineReg(C3, C21^, WX_256), WY_256);
end;

{$IFDEF TARGET_x86}
function Interpolator_MMX(WX_256, WY_256: Cardinal; C11, C21: PColor32): TColor32;
asm
        MOVQ      MM1,[ECX]
        MOVQ      MM2,MM1
        MOV       ECX,C21
        MOVQ      MM3,[ECX]
        PSRLQ     MM1,32
        MOVQ      MM4,MM3
        PSRLQ     MM3,32
        MOVD      MM5,EAX
        PSHUFW    MM5,MM5,0
        PXOR      MM0,MM0
        PUNPCKLBW MM1,MM0
        PUNPCKLBW MM2,MM0
        PSUBW     MM2,MM1
        PMULLW    MM2,MM5
        PSLLW     MM1,8
        PADDW     MM2,MM1
        PSRLW     MM2,8
        PUNPCKLBW MM3,MM0
        PUNPCKLBW MM4,MM0
        PSUBW     MM4,MM3
        PSLLW     MM3,8
        PMULLW    MM4,MM5
        PADDW     MM4,MM3
        PSRLW     MM4,8
        MOVD      MM5,EDX
        PSHUFW    MM5,MM5,0
        PSUBW     MM2,MM4
        PMULLW    MM2,MM5
        PSLLW     MM4,8
        PADDW     MM2,MM4
        PSRLW     MM2,8
        PACKUSWB  MM2,MM0
        MOVD      EAX,MM2
end;
{$ENDIF}

{ Stretch Transfer }

{$WARNINGS OFF}
procedure StretchTransfer(
  Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TCustomBitmap32; SrcRect: TRect;
  Resampler: TCustomResampler;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH: Integer;
  DstW, DstH: Integer;
  R: TRect;
  RatioX, RatioY: Single;
begin
  CheckBitmaps(Dst, Src);

  // transform dest rect when the src rect is out of the src bitmap's bounds
  if (SrcRect.Left < 0) or (SrcRect.Right > Src.Width) or
    (SrcRect.Top < 0) or (SrcRect.Bottom > Src.Height) then
  begin
    RatioX := (DstRect.Right - DstRect.Left) / (SrcRect.Right - SrcRect.Left);
    RatioY := (DstRect.Bottom - DstRect.Top) / (SrcRect.Bottom - SrcRect.Top);

    if SrcRect.Left < 0 then
    begin
      DstRect.Left := DstRect.Left + Ceil(-SrcRect.Left * RatioX);
      SrcRect.Left := 0;
    end;

    if SrcRect.Top < 0 then
    begin
      DstRect.Top := DstRect.Top + Ceil(-SrcRect.Top * RatioY);
      SrcRect.Top := 0;
    end;

    if SrcRect.Right > Src.Width then
    begin
      DstRect.Right := DstRect.Right - Floor((SrcRect.Right - Src.Width) * RatioX);
      SrcRect.Right := Src.Width;
    end;

    if SrcRect.Bottom > Src.Height then
    begin
      DstRect.Bottom := DstRect.Bottom - Floor((SrcRect.Bottom - Src.Height) * RatioY);
      SrcRect.Bottom := Src.Height;
    end;
  end;

  if Src.Empty or Dst.Empty or
    ((CombineOp = dmBlend) and (Src.MasterAlpha = 0)) or IsRectEmpty(SrcRect) then Exit;

  if not Dst.MeasuringMode then
  begin
    IntersectRect(DstClip, DstClip, Dst.BoundsRect);
    IntersectRect(DstClip, DstClip, DstRect);
    if IsRectEmpty(DstClip) then Exit;
    IntersectRect(R, DstClip, DstRect);
    if IsRectEmpty(R) then Exit;

    if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then CombineOp := dmOpaque;

    SrcW := SrcRect.Right - SrcRect.Left;
    SrcH := SrcRect.Bottom - SrcRect.Top;
    DstW := DstRect.Right - DstRect.Left;
    DstH := DstRect.Bottom - DstRect.Top;

    try
      if (SrcW = DstW) and (SrcH = DstH) then
        BlendBlock(Dst, DstClip, Src, SrcRect.Left + DstClip.Left - DstRect.Left,
          SrcRect.Top + DstClip.Top - DstRect.Top, CombineOp, CombineCallBack)
      else
        TCustomResamplerAccess(Resampler).Resample(
          Dst, DstRect, DstClip, Src, SrcRect, CombineOp, CombineCallBack);
    finally
      EMMS;
    end;
  end;

  Dst.Changed(DstRect);
end;
{$WARNINGS ON}



{ TCustomKernel }

procedure TCustomKernel.AssignTo(Dst: TPersistent);
begin
  if Dst is TCustomKernel then
    SmartAssign(Self, Dst)
  else
    inherited;
end;

procedure TCustomKernel.Changed;
begin
  if Assigned(FObserver) then FObserver.Changed;
end;

constructor TCustomKernel.Create;
begin
end;

function TCustomKernel.RangeCheck: Boolean;
begin
  Result := False;
end;


{ TBoxKernel }

function TBoxKernel.Filter(Value: TFloat): TFloat;
begin
  if (Value >= -0.5) and (Value <= 0.5) then Result := 1.0
  else Result := 0;
end;

function TBoxKernel.GetWidth: TFloat;
begin
  Result := 1;
end;

{ TLinearKernel }

function TLinearKernel.Filter(Value: TFloat): TFloat;
begin
  if Value < -1 then Result := 0
  else if Value < 0 then Result := 1 + Value
  else if Value < 1 then Result := 1 - Value
  else Result := 0;
end;

function TLinearKernel.GetWidth: TFloat;
begin
  Result := 1;
end;

{ TCosineKernel }

function TCosineKernel.Filter(Value: TFloat): TFloat;
begin
  Result := 0;
  if Abs(Value) < 1 then
    Result := (Cos(Value * Pi) + 1) * 0.5;
end;

function TCosineKernel.GetWidth: TFloat;
begin
  Result := 1;
end;

{ TSplineKernel }

function TSplineKernel.Filter(Value: TFloat): TFloat;
var
  tt: TFloat;
const
  TwoThirds = 2 / 3;
  OneSixth = 1 / 6;
begin
  Value := Abs(Value);
  if Value < 1 then
  begin
    tt := Sqr(Value);
    Result := 0.5 * tt * Value - tt + TwoThirds;
  end
  else if Value < 2 then
  begin
    Value := 2 - Value;
    Result := OneSixth * Sqr(Value) * Value;
  end
  else Result := 0;
end;

function TSplineKernel.RangeCheck: Boolean;
begin
  Result := True;
end;

function TSplineKernel.GetWidth: TFloat;
begin
  Result := 2;
end;

{ TWindowedSincKernel }

function Sinc(Value: TFloat): TFloat;
begin
  if Value <> 0 then
  begin
    Value := Value * Pi;
    Result := Sin(Value) / Value;
  end
  else Result := 1;
end;

constructor TWindowedSincKernel.Create;
begin
  FWidth := 3;
  FWidthReciprocal := 1 / FWidth;
end;

function TWindowedSincKernel.Filter(Value: TFloat): TFloat;
begin
  Value := Abs(Value);
  if Value < FWidth then
    Result := Sinc(Value) * Window(Value)
  else
    Result := 0;
end;

function TWindowedSincKernel.RangeCheck: Boolean;
begin
  Result := True;
end;

procedure TWindowedSincKernel.SetWidth(Value: TFloat);
begin
  Value := Min(MAX_KERNEL_WIDTH, Value);
  if Value <> FWidth then
  begin
    FWidth := Value;
    FWidthReciprocal := 1 / FWidth;
    Changed;
  end;
end;

function TWindowedSincKernel.GetWidth: TFloat;
begin
  Result := FWidth;
end;

{ TAlbrechtKernel }

constructor TAlbrechtKernel.Create;
begin
  inherited;
  Terms := 11;
end;

procedure TAlbrechtKernel.SetTerms(Value: Integer);
begin
  if (Value < 2) then Value := 2;
  if (Value > 11) then Value := 11;
  if FTerms <> Value then
  begin
    FTerms := Value;
    case Value of
      2 : Move(cAlbrecht2 [0], fCoefPointer[0], Value * SizeOf(Double));
      3 : Move(cAlbrecht3 [0], fCoefPointer[0], Value * SizeOf(Double));
      4 : Move(cAlbrecht4 [0], fCoefPointer[0], Value * SizeOf(Double));
      5 : Move(cAlbrecht5 [0], fCoefPointer[0], Value * SizeOf(Double));
      6 : Move(cAlbrecht6 [0], fCoefPointer[0], Value * SizeOf(Double));
      7 : Move(cAlbrecht7 [0], fCoefPointer[0], Value * SizeOf(Double));
      8 : Move(cAlbrecht8 [0], fCoefPointer[0], Value * SizeOf(Double));
      9 : Move(cAlbrecht9 [0], fCoefPointer[0], Value * SizeOf(Double));
     10 : Move(cAlbrecht10[0], fCoefPointer[0], Value * SizeOf(Double));
     11 : Move(cAlbrecht11[0], fCoefPointer[0], Value * SizeOf(Double));
    end;
  end;
end;

function TAlbrechtKernel.Window(Value: TFloat): TFloat;
var
  cs : Double;
  i  : Integer;
begin
  cs := cos(Pi * Value * FWidthReciprocal);
  i := fTerms - 1;
  result := fCoefPointer[i];
  while i>0 do
  begin
    dec(i);
    result := result * cs + fCoefPointer[i];
  end;
end;

{ TLanczosKernel }

function TLanczosKernel.Window(Value: TFloat): TFloat;
begin
  Result := Sinc(Value * FWidthReciprocal); // Get rid of division
end;

{ TMitchellKernel }

function TMitchellKernel.Filter(Value: TFloat): TFloat;
var
  tt, ttt: TFloat;
const OneEighteenth = 1 / 18;
begin
  Value := Abs(Value);
  tt := Sqr(Value);
  ttt := tt * Value;
  if Value < 1 then Result := (21 * ttt - 36 * tt + 16 ) * OneEighteenth  // get rid of divisions
  else if Value < 2 then Result := (- 7 * ttt + 36 * tt - 60 * Value + 32) * OneEighteenth // "
  else Result := 0;
end;

function TMitchellKernel.RangeCheck: Boolean;
begin
  Result := True;
end;

function TMitchellKernel.GetWidth: TFloat;
begin
  Result := 2;
end;

{ TCubicKernel }

constructor TCubicKernel.Create;
begin
  FCoeff := -0.5;
end;

function TCubicKernel.Filter(Value: TFloat): TFloat;
var
  tt, ttt: TFloat;
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

function TCubicKernel.RangeCheck: Boolean;
begin
  Result := True;
end;

function TCubicKernel.GetWidth: TFloat;
begin
  Result := 2;
end;

{ TGaussKernel }

constructor TGaussianKernel.Create;
begin
  inherited;
  FSigma := 1.33;
  FSigmaReciprocalLn2 := -Ln(2) / FSigma;
end;

procedure TGaussianKernel.SetSigma(const Value: TFloat);
begin
  if (FSigma <> Value) and (FSigma <> 0) then
  begin
    FSigma := Value;
    FSigmaReciprocalLn2 := -Ln(2) / FSigma;
    Changed;
  end;
end;

function TGaussianKernel.Window(Value: TFloat): TFloat;
begin
  Result := Exp(Sqr(Value) * FSigmaReciprocalLn2);       // get rid of nasty LN2 and divition
end;

procedure TCubicKernel.SetCoeff(const Value: TFloat);
begin
  if Value <> FCoeff then
  begin
    FCoeff := Value;
    Changed;
  end
end;

{ TBlackmanKernel }

function TBlackmanKernel.Window(Value: TFloat): TFloat;
begin
  Value := Cos(Pi * Value * FWidthReciprocal);                // get rid of division
  Result := 0.34 + 0.5 * Value + 0.16 * sqr(Value);
end;

{ THannKernel }

function THannKernel.Window(Value: TFloat): TFloat;
begin
  Result := 0.5 + 0.5 * Cos(Pi * Value * FWidthReciprocal);   // get rid of division
end;

{ THammingKernel }

function THammingKernel.Window(Value: TFloat): TFloat;
begin
  Result := 0.54 + 0.46 * Cos(Pi * Value * FWidthReciprocal); // get rid of division
end;

{ TSinshKernel }

constructor TSinshKernel.Create;
begin
  FWidth := 3;
  FCoeff := 0.5;
end;

function TSinshKernel.Filter(Value: TFloat): TFloat;
begin
  if Value = 0 then
    Result := 1
  else
    Result := FCoeff * Sin(Pi * Value) / Sinh(Pi * FCoeff * Value);
end;

function TSinshKernel.RangeCheck: Boolean;
begin
  Result := True;
end;

procedure TSinshKernel.SetWidth(Value: TFloat);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

function TSinshKernel.GetWidth: TFloat;
begin
  Result := FWidth;
end;

procedure TSinshKernel.SetCoeff(const Value: TFloat);
begin
  if (FCoeff <> Value) and (FCoeff <> 0) then
  begin
    FCoeff := Value;
    Changed;
  end;
end;

{ THermiteKernel }

constructor THermiteKernel.Create;
begin
  FBias := 0;
  FTension := 0;
end;

function THermiteKernel.Filter(Value: TFloat): TFloat;
var
  Z: Integer;
  t, t2, t3, m0, m1, a0, a1, a2, a3: TFloat;
begin
  t := (1 - FTension) * 0.5;
  m0 := (1 + FBias) * t;
  m1 := (1 - FBias) * t;

  Z := Floor(Value);
  t := Abs(Z - Value);
  t2 := t * t;
  t3 := t2 * t;

  a1 := t3 - 2 * t2 + t;
  a2 := t3 - t2;
  a3 := -2 * t3 + 3 * t2;
  a0 := -a3 + 1;

  case Z of
    -2: Result := a2 * m1;
    -1: Result := a3 + a1 * m1 + a2 * (m0 - m1);
     0: Result := a0 + a1 * (m0 - m1) - a2 * m0;
     1: Result := -a1 * m0;
  else
    Result := 0;
  end;
end;

function THermiteKernel.GetWidth: TFloat;
begin
  Result := 2;
end;

function THermiteKernel.RangeCheck: Boolean;
begin
  Result := True;
end;

procedure THermiteKernel.SetBias(const Value: TFloat);
begin
  if FBias <> Value then
  begin
    FBias := Value;
    Changed;
  end;
end;

procedure THermiteKernel.SetTension(const Value: TFloat);
begin
  if FTension <> Value then
  begin
    FTension := Value;
    Changed;
  end;
end;



{ TKernelResampler }

constructor TKernelResampler.Create;
begin
  inherited;
  Kernel := TBoxKernel.Create;
  FTableSize := 32;
end;

destructor TKernelResampler.Destroy;
begin
  FKernel.Free;
  inherited;
end;

function TKernelResampler.GetKernelClassName: string;
begin
  Result := FKernel.ClassName;
end;

procedure TKernelResampler.SetKernelClassName(Value: string);
var
  KernelClass: TCustomKernelClass;
begin
  if (Value <> '') and (FKernel.ClassName <> Value) and Assigned(KernelList) then
  begin
    KernelClass := TCustomKernelClass(KernelList.Find(Value));
    if Assigned(KernelClass) then
    begin
      FKernel.Free;
      FKernel := KernelClass.Create;
      Changed;
    end;
  end;
end;

procedure TKernelResampler.SetKernel(const Value: TCustomKernel);
begin
  if Assigned(Value) and (FKernel <> Value) then
  begin
    FKernel.Free;
    FKernel := Value;
    Changed;
  end;
end;

procedure TKernelResampler.Resample(Dst: TCustomBitmap32; DstRect,
  DstClip: TRect; Src: TCustomBitmap32; SrcRect: TRect; CombineOp: TDrawMode;
  CombineCallBack: TPixelCombineEvent);
begin
  GR32_Resamplers.Resample(Dst, DstRect, DstClip, Src, SrcRect, FKernel, CombineOp, CombineCallBack);
end;

{$WARNINGS OFF}
function TKernelResampler.GetSampleFloat(X, Y: TFloat): TColor32;
var
  clX, clY: Integer;
  fracX, fracY: Integer;
  fracXS: TFloat absolute fracX;
  fracYS: TFloat absolute fracY;

  Filter: TFilterMethod;
  WrapProcVert: TWrapProcEx absolute Filter;
  WrapProcHorz: TWrapProcEx;
  Colors: PColor32EntryArray;
  Width, W, Wv, I, J, P, Incr, Dev: Integer;
  SrcP: PColor32Entry;
  C: TColor32Entry absolute SrcP;
  LoX, HiX, LoY, HiY, MappingY: Integer;

  HorzKernel, VertKernel: TKernelEntry;
  PHorzKernel, PVertKernel, FloorKernel, CeilKernel: PKernelEntry;

  HorzEntry, VertEntry: TBufferEntry;
  MappingX: TKernelEntry;
  Edge: Boolean;
begin
  Width := Ceil(FKernel.GetWidth);

  clX := Ceil(X);
  clY := Ceil(Y);

  case PixelAccessMode of
    pamUnsafe, pamWrap:
      begin
        LoX := -Width; HiX := Width;
        LoY := -Width; HiY := Width;
      end;

    pamSafe, pamTransparentEdge:
      begin
        with ClipRect do
        begin
          if not ((clX < Left) or (clX > Right) or (clY < Top) or (clY > Bottom)) then
          begin
            Edge := False;

            if clX - Width < Left then
            begin
              LoX := Left - clX;
              Edge := True;
            end
            else
              LoX := -Width;

            if clX + Width >= Right then
            begin
              HiX := Right - clX - 1;
              Edge := True;
            end
            else
              HiX := Width;

            if clY - Width < Top then
            begin
              LoY := Top - clY;
              Edge := True;
            end
            else
              LoY := -Width;

            if clY + Width >= Bottom then
            begin
              HiY := Bottom - clY - 1;
              Edge := True;
            end
            else
              HiY := Width;

          end
          else
          begin
            if PixelAccessMode = pamTransparentEdge then
              Result := 0
            else
              Result := FOuterColor;
            Exit;
          end;

        end;
      end;
  end;

  case FKernelMode of
    kmDynamic:
      begin
        Filter := FKernel.Filter;
        fracXS := clX - X;
        fracYS := clY - Y;

        PHorzKernel := @HorzKernel;
        PVertKernel := @VertKernel;

        Dev := -256;
        for I := -Width to Width do
        begin
          W := Round(Filter(I + fracXS) * 256);
          HorzKernel[I] := W;
          Inc(Dev, W);
        end;
        Dec(HorzKernel[0], Dev);

        Dev := -256;
        for I := -Width to Width do
        begin
          W := Round(Filter(I + fracYS) * 256);
          VertKernel[I] := W;
          Inc(Dev, W);
        end;
        Dec(VertKernel[0], Dev);

      end;
    kmTableNearest:
      begin
        W := FWeightTable.Height - 2;
        PHorzKernel := @FWeightTable.ValPtr[Width - MAX_KERNEL_WIDTH, Round((clX - X) * W)]^;
        PVertKernel := @FWeightTable.ValPtr[Width - MAX_KERNEL_WIDTH, Round((clY - Y) * W)]^;
      end;
    kmTableLinear:
      begin
        W := (FWeightTable.Height - 2) * $10000;
        J := FWeightTable.Width * 4;

        with TFixedRec(FracX) do
        begin
          Fixed := Round((clX - X) * W);
          PHorzKernel := @HorzKernel;
          FloorKernel := @FWeightTable.ValPtr[Width - MAX_KERNEL_WIDTH, Int]^;
          CeilKernel := PKernelEntry(Integer(FloorKernel) + J);
          Dev := -256;
          for I := -Width to Width do
          begin
            Wv :=  FloorKernel[I] + ((CeilKernel[I] - FloorKernel[I]) * Frac + $7FFF) div FixedOne;
            HorzKernel[I] := Wv;
            Inc(Dev, Wv);
          end;
          Dec(HorzKernel[0], Dev);
        end;

        with TFixedRec(FracY) do
        begin
          Fixed := Round((clY - Y) * W);
          PVertKernel := @VertKernel;
          FloorKernel := @FWeightTable.ValPtr[Width - MAX_KERNEL_WIDTH, Int]^;
          CeilKernel := PKernelEntry(Integer(FloorKernel) + J);
          Dev := -256;
          for I := -Width to Width do
          begin
            Wv := FloorKernel[I] + ((CeilKernel[I] - FloorKernel[I]) * Frac + $7FFF) div FixedOne;
            VertKernel[I] := Wv;
            Inc(Dev, Wv);
          end;
          Dec(VertKernel[0], Dev);
        end;
      end;

  end;

  VertEntry := EMPTY_ENTRY;
  case PixelAccessMode of
    pamUnsafe, pamSafe, pamTransparentEdge:
      begin
        SrcP := PColor32Entry(Bitmap.PixelPtr[LoX + clX, LoY + clY]);
        Incr := Bitmap.Width - (HiX - LoX) - 1;
        for I := LoY to HiY do
        begin
          Wv := PVertKernel[I];
          if Wv <> 0 then
          begin
            HorzEntry := EMPTY_ENTRY;
            for J := LoX to HiX do
            begin
              W := PHorzKernel[J];
              Inc(HorzEntry.A, SrcP.A * W);
              Inc(HorzEntry.R, SrcP.R * W);
              Inc(HorzEntry.G, SrcP.G * W);
              Inc(HorzEntry.B, SrcP.B * W);
              Inc(SrcP);
            end;
            Inc(VertEntry.A, HorzEntry.A * Wv);
            Inc(VertEntry.R, HorzEntry.R * Wv);
            Inc(VertEntry.G, HorzEntry.G * Wv);
            Inc(VertEntry.B, HorzEntry.B * Wv);
          end else Inc(SrcP, HiX - LoX + 1);
          Inc(SrcP, Incr);
        end;

        if (PixelAccessMode <> pamUnsafe) and Edge then
        begin
          if PixelAccessMode = pamSafe then
           for I := -Width to Width do
           begin
             Wv := PVertKernel[I];
             if Wv <> 0 then
             begin
               HorzEntry := EMPTY_ENTRY;
               for J := -Width to Width do
               if (J < LoX) or (J > HiX) or (I < LoY) or (I > HiY) then
               begin
                 W := PHorzKernel[J];
                 Inc(HorzEntry.A, TColor32Entry(FOuterColor).A * W);
                 Inc(HorzEntry.R, TColor32Entry(FOuterColor).R * W);
                 Inc(HorzEntry.G, TColor32Entry(FOuterColor).G * W);
                 Inc(HorzEntry.B, TColor32Entry(FOuterColor).B * W);
               end;
               Inc(VertEntry.A, HorzEntry.A * Wv);
               Inc(VertEntry.R, HorzEntry.R * Wv);
               Inc(VertEntry.G, HorzEntry.G * Wv);
               Inc(VertEntry.B, HorzEntry.B * Wv);
             end;
           end
          else
           for I := -Width to Width do
           begin
             Wv := PVertKernel[I];
             if Wv <> 0 then
             begin
               HorzEntry := EMPTY_ENTRY;
               P := Clamp(clY + I, Bitmap.Height - 1);
               for J := -Width to Width do
               if (J < LoX) or (J > HiX) or (I < LoY) or (I > HiY) then
               with TColor32Entry(Bitmap.Pixel[Clamp(clX + J, Bitmap.Width - 1), P]) do
               begin
                 W := PHorzKernel[J];
                 //exclude alpha, implicit transparent edge
                 Inc(HorzEntry.R, R * W);
                 Inc(HorzEntry.G, G * W);
                 Inc(HorzEntry.B, B * W);
               end;
               Inc(VertEntry.R, HorzEntry.R * Wv);
               Inc(VertEntry.G, HorzEntry.G * Wv);
               Inc(VertEntry.B, HorzEntry.B * Wv);
             end;
           end;
        end;
      end;

    pamWrap:
      begin
        WrapProcHorz := GetWrapProcEx(Bitmap.WrapMode, ClipRect.Left, ClipRect.Right - 1);
        WrapProcVert := GetWrapProcEx(Bitmap.WrapMode, ClipRect.Top, ClipRect.Bottom - 1);

        for I := -Width to Width do
          MappingX[I] := WrapProcHorz(clX + I, ClipRect.Left, ClipRect.Right - 1);

        for I := -Width to Width do
        begin
          Wv := PVertKernel[I];
          if Wv <> 0 then
          begin
            MappingY := WrapProcVert(clY + I, ClipRect.Top, ClipRect.Bottom - 1);
            Colors := PColor32EntryArray(Bitmap.ScanLine[MappingY]);
            HorzEntry := EMPTY_ENTRY;
            for J := -Width to Width do
            begin
              W := PHorzKernel[J];
              C := Colors[MappingX[J]];
              Inc(HorzEntry.A, C.A * W);
              Inc(HorzEntry.R, C.R * W);
              Inc(HorzEntry.G, C.G * W);
              Inc(HorzEntry.B, C.B * W);
            end;
            Inc(VertEntry.A, HorzEntry.A * Wv);
            Inc(VertEntry.R, HorzEntry.R * Wv);
            Inc(VertEntry.G, HorzEntry.G * Wv);
            Inc(VertEntry.B, HorzEntry.B * Wv);
          end;
        end;
      end;
  end;

  with TColor32Entry(Result) do
  begin
  if FKernel.RangeCheck then
  begin
      A := Clamp(TFixedRec(VertEntry.A).Int);
      R := Clamp(TFixedRec(VertEntry.R).Int);
      G := Clamp(TFixedRec(VertEntry.G).Int);
      B := Clamp(TFixedRec(VertEntry.B).Int);
    end
    else
  begin
      A := TFixedRec(VertEntry.A).Int;
      R := TFixedRec(VertEntry.R).Int;
      G := TFixedRec(VertEntry.G).Int;
      B := TFixedRec(VertEntry.B).Int;
    end;
  end;
end;
{$WARNINGS ON}

function TKernelResampler.GetWidth: TFloat;
begin
  Result := Kernel.GetWidth;
end;

procedure TKernelResampler.SetKernelMode(const Value: TKernelMode);
begin
  if FKernelMode <> Value then
  begin
    FKernelMode := Value;
    Changed;
  end;
end;

procedure TKernelResampler.SetTableSize(Value: Integer);
begin
  if Value < 2 then Value := 2;
  if FTableSize <> Value then
  begin
    FTableSize := Value;
    Changed;
  end;
end;

procedure TKernelResampler.FinalizeSampling;
begin
  if FKernelMode in [kmTableNearest, kmTableLinear] then
    FWeightTable.Free;
  inherited;
end;

procedure TKernelResampler.PrepareSampling;
var
  I, J, W, Weight, Dev: Integer;
  Fraction: TFloat;
  KernelPtr: PKernelEntry;
begin
  inherited;
  FOuterColor := Bitmap.OuterColor;
  W := Ceil(FKernel.GetWidth);
  if FKernelMode in [kmTableNearest, kmTableLinear] then
  begin
    FWeightTable := TIntegerMap.Create;
    FWeightTable.SetSize(W * 2 + 1, FTableSize + 1);
    for I := 0 to FTableSize do
    begin
      Fraction := I / (FTableSize - 1);
      KernelPtr :=  @FWeightTable.ValPtr[W - MAX_KERNEL_WIDTH, I]^;
      Dev := - 256;
      for J := -W to W do
      begin
        Weight := Round(FKernel.Filter(J + Fraction) * 256);
        KernelPtr[J] := Weight;
        Inc(Dev, Weight);
      end;
      Dec(KernelPtr[0], Dev);
    end;
  end;
end;

{ TCustomBitmap32NearestResampler }

function TNearestResampler.GetSampleInt(X, Y: Integer): TColor32;
begin
  Result := FGetSampleInt(X, Y);
end;

function TNearestResampler.GetSampleFixed(X, Y: TFixed): TColor32;
begin
  Result := FGetSampleInt(FixedRound(X), FixedRound(Y));
end;

function TNearestResampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Result := FGetSampleInt(Round(X), Round(Y));
end;

function TNearestResampler.GetWidth: TFloat;
begin
  Result := 1;
end;

function TNearestResampler.GetPixelTransparentEdge(X,Y: Integer): TColor32;
var
  I, J: Integer;
begin
  with Bitmap, Bitmap.ClipRect do
  begin
    I := Clamp(X, Left, Right - 1);
    J := Clamp(Y, Top, Bottom - 1);
    Result := Pixel[I, J];
    if (I <> X) or (J <> Y) then
      Result := Result and $00FFFFFF;
  end;
end;

procedure TNearestResampler.PrepareSampling;
begin
  inherited;
  case PixelAccessMode of
    pamUnsafe: FGetSampleInt := TCustomBitmap32Access(Bitmap).GetPixel;
    pamSafe: FGetSampleInt := TCustomBitmap32Access(Bitmap).GetPixelS;
    pamWrap: FGetSampleInt := TCustomBitmap32Access(Bitmap).GetPixelW;
    pamTransparentEdge: FGetSampleInt := GetPixelTransparentEdge;
  end;
end;

procedure TNearestResampler.Resample(
  Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TCustomBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
begin
  StretchNearest(Dst, DstRect, DstClip, Src, SrcRect, CombineOp, CombineCallBack)
end;


{ TCustomBitmap32LinearResampler }

constructor TLinearResampler.Create;
begin
  inherited;
  FLinearKernel := TLinearKernel.Create;
end;

destructor TLinearResampler.Destroy;
begin
  FLinearKernel.Free;
  inherited Destroy;
end;

function TLinearResampler.GetSampleFixed(X, Y: TFixed): TColor32;
begin
  Result := FGetSampleFixed(X, Y);
end;

function TLinearResampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Result := FGetSampleFixed(Round(X * FixedOne), Round(Y * FixedOne));
end;

function TLinearResampler.GetPixelTransparentEdge(X, Y: TFixed): TColor32;
var
  I, J, X1, X2, Y1, Y2, WX, R, B: TFixed;
  C1, C2, C3, C4: TColor32;
  PSrc: PColor32Array;
begin
  with TCustomBitmap32Access(Bitmap), Bitmap.ClipRect do
  begin
    R := Right - 1;
    B := Bottom - 1;

    I := TFixedRec(X).Int;
    J := TFixedRec(Y).Int;

    if (I >= Left) and (J >= Top) and (I < R) and (J < B) then
    begin //Safe
      Result := GET_T256(X shr 8, Y shr 8);
      EMMS;
    end
    else
    if (I >= Left - 1) and (J >= Top - 1) and (I <= R) and (J <= B) then
    begin //Near edge, on edge or outside

      X1 := Clamp(I, R);
      X2 := Clamp(I + Sign(X), R);
      Y1 := Clamp(J, B) * Width;
      Y2 := Clamp(J + Sign(Y), B) * Width;

      PSrc := @Bits[0];
      C1 := PSrc[X1 + Y1];
      C2 := PSrc[X2 + Y1];
      C3 := PSrc[X1 + Y2];
      C4 := PSrc[X2 + Y2];

      if X <= Fixed(Left) then
      begin
        C1 := C1 and $00FFFFFF;
        C3 := C3 and $00FFFFFF;
      end
      else if I = R then
      begin
        C2 := C2 and $00FFFFFF;
        C4 := C4 and $00FFFFFF;
      end;

      if Y <= Fixed(Top) then
      begin
        C1 := C1 and $00FFFFFF;
        C2 := C2 and $00FFFFFF;
      end
      else if J = B then
      begin
        C3 := C3 and $00FFFFFF;
        C4 := C4 and $00FFFFFF;
      end;

      WX := GAMMA_TABLE[((X shr 8) and $FF) xor $FF];
      Result := CombineReg(CombineReg(C1, C2, WX),
                           CombineReg(C3, C4, WX),
                           GAMMA_TABLE[((Y shr 8) and $FF) xor $FF]);
      EMMS;  
    end  
    else  
      Result := 0; //Nothing really makes sense here, return zero
  end;
end;

procedure TLinearResampler.PrepareSampling;
begin
  inherited;
  case PixelAccessMode of
    pamUnsafe: FGetSampleFixed := TCustomBitmap32Access(Bitmap).GetPixelX;
    pamSafe: FGetSampleFixed := TCustomBitmap32Access(Bitmap).GetPixelXS;
    pamWrap: FGetSampleFixed := TCustomBitmap32Access(Bitmap).GetPixelXW;
    pamTransparentEdge: FGetSampleFixed := GetPixelTransparentEdge;
  end;
end;

function TLinearResampler.GetWidth: TFloat;
begin
  Result := 1;
end;

procedure TLinearResampler.Resample(
  Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TCustomBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH: TFloat;
  DstW, DstH: Integer;
begin
  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  if (DstW > SrcW) and (DstH > SrcH) and (SrcW > 1) and (SrcH > 1) then
    StretchHorzStretchVertLinear(Dst, DstRect, DstClip, Src, SrcRect, CombineOp, CombineCallBack)
  else
    GR32_Resamplers.Resample(Dst, DstRect, DstClip, Src, SrcRect, FLinearKernel, CombineOp, CombineCallBack);
end;

procedure TDraftResampler.Resample(
  Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
  Src: TCustomBitmap32; SrcRect: TRect;
  CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent);
begin
  DraftResample(Dst, DstRect, DstClip, Src, SrcRect, FLinearKernel, CombineOp, CombineCallBack)
end;

{ TTransformer }

function TTransformer.GetSampleInt(X, Y: Integer): TColor32;
var
  U, V: TFixed;
begin
  FTransformationReverseTransformFixed(X * FixedOne, Y * FixedOne, U, V);
  Result := FGetSampleFixed(U, V);
end;

function TTransformer.GetSampleFixed(X, Y: TFixed): TColor32;
var
  U, V: TFixed;
begin
  FTransformationReverseTransformFixed(X, Y, U, V);
  Result := FGetSampleFixed(U, V);
end;

function TTransformer.GetSampleFloat(X, Y: TFloat): TColor32;
var
  U, V: TFloat;
begin
  FTransformationReverseTransformFloat(X, Y, U, V);
  Result := FGetSampleFloat(U, V);
end;

procedure TTransformer.SetTransformation(const Value: TTransformation);
begin
  FTransformation := Value;
  if Assigned(Value) then
  begin
    FTransformationReverseTransformInt := TTransformationAccess(FTransformation).ReverseTransformInt;
    FTransformationReverseTransformFixed := TTransformationAccess(FTransformation).ReverseTransformFixed;
    FTransformationReverseTransformFloat := TTransformationAccess(FTransformation).ReverseTransformFloat;
  end;
end;

constructor TTransformer.Create(ASampler: TCustomSampler; ATransformation: TTransformation);
begin
  inherited Create(ASampler);
  Transformation := ATransformation;
end;

procedure TTransformer.PrepareSampling;
begin
  inherited;
  with TTransformationAccess(FTransformation) do
    if not TransformValid then
      PrepareTransform;
end;

function TTransformer.GetSampleBounds: TFloatRect;
begin
  IntersectRect(Result, inherited GetSampleBounds, FTransformation.SrcRect);
  Result := FTransformation.GetTransformedBounds(Result);
end;

function TTransformer.HasBounds: Boolean;
begin
  Result := FTransformation.HasTransformedBounds and inherited HasBounds;
end;


{ TSuperSampler }

constructor TSuperSampler.Create(Sampler: TCustomSampler);
begin
  inherited Create(Sampler);
  FSamplingX := 4;
  FSamplingY := 4;
  SamplingX := 4;
  SamplingY := 4;
end;

function TSuperSampler.GetSampleFixed(X, Y: TFixed): TColor32;
var
  I, J: Integer;
  dX, dY, tX: TFixed;
  Buffer: TBufferEntry;
begin
  Buffer := EMPTY_ENTRY;
  tX := X + FOffsetX;
  Inc(Y, FOffsetY);
  dX := FDistanceX;
  dY := FDistanceY;
  for J := 1 to FSamplingY do
  begin
    X := tX;
    for I := 1 to FSamplingX do
    begin
      IncBuffer(Buffer, FGetSampleFixed(X, Y));
      Inc(X, dX);
    end;
    Inc(Y, dY);
  end;
  MultiplyBuffer(Buffer, FScale);
  Result := BufferToColor32(Buffer, 16);
end;

procedure TSuperSampler.SetSamplingX(const Value: TSamplingRange);
begin
  FSamplingX := Value;
  FDistanceX := Fixed(1 / Value);
  FOffsetX := Fixed(((1 / Value) - 1) * 0.5);     // replaced "/2" by "*0.5"
  FScale := Fixed(1 / (FSamplingX * FSamplingY));
end;

procedure TSuperSampler.SetSamplingY(const Value: TSamplingRange);
begin
  FSamplingY := Value;
  FDistanceY := Fixed(1 / Value);
  FOffsetY := Fixed(((1 / Value) - 1) * 0.5);     // replaced "/2" by "*0.5"
  FScale := Fixed(1 / (FSamplingX * FSamplingY));
end;

{ TAdaptiveSuperSampler }

function TAdaptiveSuperSampler.CompareColors(C1, C2: TColor32): Boolean;
var
  Diff: TColor32Entry;
begin
  Diff.ARGB := ColorDifference(C1, C2);
  Result := FTolerance < Diff.R + Diff.G + Diff.B;
end;

constructor TAdaptiveSuperSampler.Create(Sampler: TCustomSampler);
begin
  inherited Create(Sampler);
  Level := 4;
  Tolerance := 256;
end;

function TAdaptiveSuperSampler.DoRecurse(X, Y, Offset: TFixed; const A, B,
  C, D, E: TColor32): TColor32;
var
  C1, C2, C3, C4: TColor32;
begin
  C1 := QuadrantColor(A, E, X - Offset, Y - Offset, Offset, RecurseAC);
  C2 := QuadrantColor(B, E, X + Offset, Y - Offset, Offset, RecurseBD);
  C3 := QuadrantColor(E, C, X + Offset, Y + Offset, Offset, RecurseAC);
  C4 := QuadrantColor(E, D, X - Offset, Y + Offset, Offset, RecurseBD);
  Result := ColorAverage(ColorAverage(C1, C2), ColorAverage(C3, C4));
end;

function TAdaptiveSuperSampler.GetSampleFixed(X, Y: TFixed): TColor32;
var
  A, B, C, D, E: TColor32;
const
  FIXED_HALF = 32768;
begin
  A := FGetSampleFixed(X - FIXED_HALF, Y - FIXED_HALF);
  B := FGetSampleFixed(X + FIXED_HALF, Y - FIXED_HALF);
  C := FGetSampleFixed(X + FIXED_HALF, Y + FIXED_HALF);
  D := FGetSampleFixed(X - FIXED_HALF, Y + FIXED_HALF);
  E := FGetSampleFixed(X, Y);
  Result := Self.DoRecurse(X, Y, 16384, A, B, C, D, E);
  EMMS;
end;

function TAdaptiveSuperSampler.QuadrantColor(const C1, C2: TColor32; X, Y,
  Offset: TFixed; Proc: TRecurseProc): TColor32;
begin
  if CompareColors(C1, C2) and (Offset >= FMinOffset) then
    Result := Proc(X, Y, Offset, C1, C2)
  else
    Result := ColorAverage(C1, C2);
end;

function TAdaptiveSuperSampler.RecurseAC(X, Y, Offset: TFixed; const A,
  C: TColor32): TColor32;
var
  B, D, E: TColor32;
begin
  EMMS;
  B := FGetSampleFixed(X + Offset, Y - Offset);
  D := FGetSampleFixed(X - Offset, Y + Offset);
  E := FGetSampleFixed(X, Y);
  Result := DoRecurse(X, Y, Offset shr 1, A, B, C, D, E);
end;

function TAdaptiveSuperSampler.RecurseBD(X, Y, Offset: TFixed; const B,
  D: TColor32): TColor32;
var
  A, C, E: TColor32;
begin
  EMMS;
  A := FGetSampleFixed(X - Offset, Y - Offset);
  C := FGetSampleFixed(X + Offset, Y + Offset);
  E := FGetSampleFixed(X, Y);
  Result := DoRecurse(X, Y, Offset shr 1, A, B, C, D, E);
end;

procedure TAdaptiveSuperSampler.SetLevel(const Value: Integer);
begin
  FLevel := Value;
  FMinOffset := Fixed(1 / (1 shl Value));
end;

{ TPatternSampler }

destructor TPatternSampler.Destroy;
begin
  if Assigned(FPattern) then FPattern := nil;
  inherited;
end;

function TPatternSampler.GetSampleFixed(X, Y: TFixed): TColor32;
var
  Points: TArrayOfFixedPoint;
  P: PFixedPoint;
  I, PY: Integer;
  Buffer: TBufferEntry;
  GetSample: TGetSampleFixed;
  WrapProcHorz: TWrapProc;
begin
  GetSample := FSampler.GetSampleFixed;
  PY := WrapProcVert(TFixedRec(Y).Int, High(FPattern));
  I := High(FPattern[PY]);
  WrapProcHorz := GetOptimalWrap(I);
  Points := FPattern[PY][WrapProcHorz(TFixedRec(X).Int, I)];
  Buffer := EMPTY_ENTRY;
  P := @Points[0];
  for I := 0 to High(Points) do
  begin
    IncBuffer(Buffer, GetSample(P.X + X, P.Y + Y));
    Inc(P);
  end;
  MultiplyBuffer(Buffer, FixedOne div Length(Points));
  Result := BufferToColor32(Buffer, 16);
end;

procedure TPatternSampler.SetPattern(const Value: TFixedSamplePattern);
begin
  if Assigned(Value) then
  begin
    FPattern := nil;
    FPattern := Value;
    WrapProcVert := GetOptimalWrap(High(FPattern));
  end;
end;

function JitteredPattern(XRes, YRes: Integer): TArrayOfFixedPoint;
var
  I, J: Integer;
begin
  SetLength(Result, XRes * YRes);
  for I := 0 to XRes - 1 do
    for J := 0 to YRes - 1 do
      with Result[I + J * XRes] do
        begin
          X := (Random(65536) + I * 65536) div XRes - 32768;
          Y := (Random(65536) + J * 65536) div YRes - 32768;
        end;
end;

function CreateJitteredPattern(TileWidth, TileHeight, SamplesX, SamplesY: Integer): TFixedSamplePattern;
var
  I, J: Integer;
begin
  SetLength(Result, TileHeight, TileWidth);
  for I := 0 to TileWidth - 1 do
    for J := 0 to TileHeight - 1 do
      Result[J][I] := JitteredPattern(SamplesX, SamplesY);
end;

procedure RegisterResampler(ResamplerClass: TCustomResamplerClass);
begin
  if not Assigned(ResamplerList) then ResamplerList := TClassList.Create;
  ResamplerList.Add(ResamplerClass);
end;

procedure RegisterKernel(KernelClass: TCustomKernelClass);
begin
  if not Assigned(KernelList) then KernelList := TClassList.Create;
  KernelList.Add(KernelClass);
end;

{ TNestedSampler }

procedure TNestedSampler.AssignTo(Dst: TPersistent);
begin
  if Dst is TNestedSampler then
    SmartAssign(Self, Dst)
  else
    inherited;
end;

constructor TNestedSampler.Create(ASampler: TCustomSampler);
begin
  inherited Create;
  Sampler := ASampler;
end;

procedure TNestedSampler.FinalizeSampling;
begin
  if not Assigned(FSampler) then
    raise ENestedException.Create(SSamplerNil)
  else
    FSampler.FinalizeSampling;
end;

{$WARNINGS OFF}
function TNestedSampler.GetSampleBounds: TFloatRect;
begin
  if not Assigned(FSampler) then
    raise ENestedException.Create(SSamplerNil)
  else
    Result := FSampler.GetSampleBounds;
end;

function TNestedSampler.HasBounds: Boolean;
begin
  if not Assigned(FSampler) then
    raise ENestedException.Create(SSamplerNil)
  else
    Result := FSampler.HasBounds;
end;
{$WARNINGS ON}

procedure TNestedSampler.PrepareSampling;
begin
  if not Assigned(FSampler) then
    raise ENestedException.Create(SSamplerNil)
  else
    FSampler.PrepareSampling;
end;

procedure TNestedSampler.SetSampler(const Value: TCustomSampler);
begin
  FSampler := Value;
  if Assigned(Value) then
  begin
    FGetSampleInt := FSampler.GetSampleInt;
    FGetSampleFixed := FSampler.GetSampleFixed;
    FGetSampleFloat := FSampler.GetSampleFloat;
  end;
end;


{ TKernelSampler }

function TKernelSampler.ConvertBuffer(var Buffer: TBufferEntry): TColor32;
begin
  Buffer.A := Constrain(Buffer.A, 0, $FFFF);
  Buffer.R := Constrain(Buffer.R, 0, $FFFF);
  Buffer.G := Constrain(Buffer.G, 0, $FFFF);
  Buffer.B := Constrain(Buffer.B, 0, $FFFF);

  Result := BufferToColor32(Buffer, 8);
end;

constructor TKernelSampler.Create(ASampler: TCustomSampler);
begin
  inherited;
  FKernel := TIntegerMap.Create;
  FStartEntry := EMPTY_ENTRY;
end;

destructor TKernelSampler.Destroy;
begin
  FKernel.Free;
  inherited;
end;

function TKernelSampler.GetSampleFixed(X, Y: TFixed): TColor32;
var
  I, J: Integer;
  Buffer: TBufferEntry;
begin
  X := X + FCenterX shl 16;
  Y := Y + FCenterY shl 16;
  Buffer := FStartEntry;
  for I := 0 to FKernel.Width - 1 do
    for J := 0 to FKernel.Height - 1 do
      UpdateBuffer(Buffer, FGetSampleFixed(X - I shl 16, Y - J shl 16), FKernel[I, J]);

  Result := ConvertBuffer(Buffer);
end;

function TKernelSampler.GetSampleInt(X, Y: Integer): TColor32;
var
  I, J: Integer;
  Buffer: TBufferEntry;
begin
  X := X + FCenterX;
  Y := Y + FCenterY;
  Buffer := FStartEntry;
  for I := 0 to FKernel.Width - 1 do
    for J := 0 to FKernel.Height - 1 do
      UpdateBuffer(Buffer, FGetSampleInt(X - I, Y - J), FKernel[I, J]);

  Result := ConvertBuffer(Buffer);
end;

{ TConvolver }

procedure TConvolver.UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
  Weight: Integer);
begin
  with TColor32Entry(Color) do
  begin
    Inc(Buffer.A, A * Weight);
    Inc(Buffer.R, R * Weight);
    Inc(Buffer.G, G * Weight);
    Inc(Buffer.B, B * Weight);
  end;
end;

{ TDilater }

procedure TDilater.UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
  Weight: Integer);
begin
  with TColor32Entry(Color) do
  begin
    Buffer.A := Max(Buffer.A, A + Weight);
    Buffer.R := Max(Buffer.R, R + Weight);
    Buffer.G := Max(Buffer.G, G + Weight);
    Buffer.B := Max(Buffer.B, B + Weight);
  end;
end;

{ TEroder }

constructor TEroder.Create(ASampler: TCustomSampler);
const
  START_ENTRY: TBufferEntry = (B: $FFFF; G: $FFFF; R: $FFFF; A: $FFFF);
begin
  inherited;
  FStartEntry := START_ENTRY;
end;

procedure TEroder.UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
  Weight: Integer);
begin
  with TColor32Entry(Color) do
  begin
    Buffer.A := Min(Buffer.A, A - Weight);
    Buffer.R := Min(Buffer.R, R - Weight);
    Buffer.G := Min(Buffer.G, G - Weight);
    Buffer.B := Min(Buffer.B, B - Weight);
  end;
end;

{ TExpander }

procedure TExpander.UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
  Weight: Integer);
begin
  with TColor32Entry(Color) do
  begin
    Buffer.A := Max(Buffer.A, A * Weight);
    Buffer.R := Max(Buffer.R, R * Weight);
    Buffer.G := Max(Buffer.G, G * Weight);
    Buffer.B := Max(Buffer.B, B * Weight);
  end;
end;

{ TContracter }

function TContracter.GetSampleFixed(X, Y: TFixed): TColor32;
begin
  Result := ColorSub(FMaxWeight, inherited GetSampleFixed(X, Y));
end;

function TContracter.GetSampleInt(X, Y: Integer): TColor32;
begin
  Result := ColorSub(FMaxWeight, inherited GetSampleInt(X, Y));
end;

procedure TContracter.PrepareSampling;
var
  I, J, W: Integer;
begin
  W := Low(Integer);
  for I := 0 to FKernel.Width - 1 do
    for J := 0 to FKernel.Height - 1 do
      W := Max(W, FKernel[I, J]);
  if W > 255 then W := 255;
  FMaxWeight := Gray32(W, W);
end;

procedure TContracter.UpdateBuffer(var Buffer: TBufferEntry; Color: TColor32;
  Weight: Integer);
begin
  inherited UpdateBuffer(Buffer, Color xor $FFFFFFFF, Weight);
end;

{ TMorphologicalSampler }

function TMorphologicalSampler.ConvertBuffer(
  var Buffer: TBufferEntry): TColor32;
begin
  Buffer.A := Constrain(Buffer.A, 0, $FF);
  Buffer.R := Constrain(Buffer.R, 0, $FF);
  Buffer.G := Constrain(Buffer.G, 0, $FF);
  Buffer.B := Constrain(Buffer.B, 0, $FF);

  with TColor32Entry(Result) do
  begin
    A := Buffer.A;
    R := Buffer.R;
    G := Buffer.G;
    B := Buffer.B;
  end;
end;

{ TSelectiveConvolver }

function TSelectiveConvolver.ConvertBuffer(var Buffer: TBufferEntry): TColor32;
begin
  with TColor32Entry(Result) do
  begin
    A := Buffer.A div FWeightSum.A;
    R := Buffer.R div FWeightSum.R;
    G := Buffer.G div FWeightSum.G;
    B := Buffer.B div FWeightSum.B;
  end;
end;

constructor TSelectiveConvolver.Create(ASampler: TCustomSampler);
begin
  inherited;
  FDelta := 30;
end;

function TSelectiveConvolver.GetSampleFixed(X, Y: TFixed): TColor32;
begin
  FRefColor := FGetSampleFixed(X, Y);
  FWeightSum := EMPTY_ENTRY;
  Result := inherited GetSampleFixed(X, Y);
end;

function TSelectiveConvolver.GetSampleInt(X, Y: Integer): TColor32;
begin
  FRefColor := FGetSampleInt(X, Y);
  FWeightSum := EMPTY_ENTRY;
  Result := inherited GetSampleInt(X, Y);
end;

procedure TSelectiveConvolver.UpdateBuffer(var Buffer: TBufferEntry;
  Color: TColor32; Weight: Integer);
begin
  with TColor32Entry(Color) do
  begin
    if Abs(TColor32Entry(FRefColor).A - A) <= FDelta then
    begin
      Inc(Buffer.A, A * Weight);
      Inc(FWeightSum.A, Weight);
    end;
    if Abs(TColor32Entry(FRefColor).R - R) <= FDelta then
    begin
      Inc(Buffer.R, R * Weight);
      Inc(FWeightSum.R, Weight);
    end;
    if Abs(TColor32Entry(FRefColor).G - G) <= FDelta then
    begin
      Inc(Buffer.G, G * Weight);
      Inc(FWeightSum.G, Weight);
    end;
    if Abs(TColor32Entry(FRefColor).B - B) <= FDelta then
    begin
      Inc(Buffer.B, B * Weight);
      Inc(FWeightSum.B, Weight);
    end;
  end;
end;

{CPU target and feature Function templates}

const
  FID_BLOCKAVERAGE = 0;
  FID_INTERPOLATOR = 1;

var
  Registry: TFunctionRegistry;

procedure RegisterBindings;
begin
  Registry := NewRegistry('GR32_Resamplers bindings');
  Registry.RegisterBinding(FID_BLOCKAVERAGE, @@BlockAverage);
  Registry.RegisterBinding(FID_INTERPOLATOR, @@Interpolator);

  Registry.Add(FID_BLOCKAVERAGE, @BlockAverage_Pas);
  Registry.Add(FID_INTERPOLATOR, @Interpolator_Pas);
{$IFDEF TARGET_x86}
  Registry.Add(FID_BLOCKAVERAGE, @BlockAverage_MMX, [ciMMX]);
  Registry.Add(FID_BLOCKAVERAGE, @BlockAverage_3DNow, [ci3DNow]);
  Registry.Add(FID_INTERPOLATOR, @Interpolator_MMX, [ciSSE]);
{$ENDIF}
  Registry.RebindAll;
end;

initialization
  RegisterBindings;

  { Register resamplers }
  RegisterResampler(TNearestResampler);
  RegisterResampler(TLinearResampler);
  RegisterResampler(TDraftResampler);
  RegisterResampler(TKernelResampler);

  { Register kernels }
  RegisterKernel(TBoxKernel);
  RegisterKernel(TLinearKernel);
  RegisterKernel(TCosineKernel);
  RegisterKernel(TSplineKernel);
  RegisterKernel(TCubicKernel);
  RegisterKernel(TMitchellKernel);
  RegisterKernel(TAlbrechtKernel);
  RegisterKernel(TLanczosKernel);
  RegisterKernel(TGaussianKernel);
  RegisterKernel(TBlackmanKernel);
  RegisterKernel(THannKernel);
  RegisterKernel(THammingKernel);
  RegisterKernel(TSinshKernel);
  RegisterKernel(THermiteKernel);

finalization
  ResamplerList.Free;
  KernelList.Free;

end.
