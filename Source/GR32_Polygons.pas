unit GR32_Polygons;

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
 * The Original Code is Vectorial Polygon Rasterizer for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2012
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
{$if not defined(FPC)}
  System.Types,
{$else}
  Types,
{$ifend}
  GR32,
  GR32_Bindings,
  GR32_Containers,
  GR32_VPR,
  GR32_Transforms,
  GR32_Resamplers;

//------------------------------------------------------------------------------
//
//      TJoinStyle
//
//------------------------------------------------------------------------------
// Polygon join style
//------------------------------------------------------------------------------
type
  TJoinStyle = (
    jsMiter,            // jsMiter: Edges are offset and extended to intersect with neighboring
                        // edges. If an intersection is too far away from its vertice, e.g. if
                        // the angle of the corner is small, then the corner is beveled instead.
                        // The MiterLimit parameter specifies the maximum ratio between the
                        // offset value and the distance from the vertice to the corner.

    jsBevel,            // jsBevel: Cut corners so the point at the end of an edge is perpendicular
                        // to the vertex that produced the corner.

    jsRound,            // jsRound: Rounds convex joins.

    jsRoundEx,          // jsRoundEx: Rounds both convex and concave joins unlike jsRound which
                        // only rounds convex joins. The depth of convex join rounding is controlled
                        // by Grow's MiterLimit parameter.

    jsSquare            // jsSquare: Cut corners so the distance from the vertice producing the
                        // corner to the midpoint of the corner is the same as the offset distrance.
  );
  TJoinStyles = set of TJoinStyle;


//------------------------------------------------------------------------------
//
//      TEndStyle
//
//------------------------------------------------------------------------------
// Polyline end style
//------------------------------------------------------------------------------
type
  TEndStyle = (esButt, esSquare, esRound);
  TEndStyles = set of TEndStyle;


//------------------------------------------------------------------------------
//
//      TPolyFillMode
//
//------------------------------------------------------------------------------
// Polygon fill mode
//------------------------------------------------------------------------------
type
  TPolyFillMode = (
    pfAlternate,        // Alternate; aka EvenOdd
    pfWinding,          // Winding; aka NonZero
    pfEvenOdd = 0,
    pfNonZero
  );


//------------------------------------------------------------------------------
//
//      TCustomPolygonRenderer
//
//------------------------------------------------------------------------------
type
  TCustomPolygonRenderer = class abstract(TThreadPersistent)
  public
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect); overload; virtual; abstract;
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect; Transformation: TTransformation); overload; virtual;

    procedure PolygonFS(const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect; Transformation: TTransformation); overload; virtual;
    procedure PolygonFS(const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect); overload; virtual;

    // procedure PolyPolygonXS(const Points: TArrayOfArrayOfFixedPoint; const ClipRect: TFixedRect; Transformation: TTransformation); virtual; overload;
    // procedure PolyPolygonXS(const Points: TArrayOfArrayOfFixedPoint; const ClipRect: TFixedRect); virtual; overload;
  end;

  TCustomPolygonRendererClass = class of TCustomPolygonRenderer;


//------------------------------------------------------------------------------
//
//      TPolygonRenderer32
//
//------------------------------------------------------------------------------
  TCustomPolygonFiller = class;

  TPolygonRenderer32 = class abstract(TCustomPolygonRenderer)
  private
    FBitmap: TCustomBitmap32;
    FFillMode: TPolyFillMode;
    FColor: TColor32;
    FFiller: TCustomPolygonFiller;
  protected
    procedure SetBitmap(const Value: TCustomBitmap32); virtual;
    procedure SetColor(const Value: TColor32); virtual;
    procedure SetFillMode(const Value: TPolyFillMode); virtual;
    procedure SetFiller(const Value: TCustomPolygonFiller); virtual;
  public
    constructor Create(Bitmap: TCustomBitmap32; Fillmode: TPolyFillMode = pfWinding); reintroduce; overload;

    procedure PolygonFS(const Points: TArrayOfFloatPoint); overload; virtual;
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint); overload; virtual;

    property Bitmap: TCustomBitmap32 read FBitmap write SetBitmap;
    property FillMode: TPolyFillMode read FFillMode write SetFillMode;
    property Color: TColor32 read FColor write SetColor;
    property Filler: TCustomPolygonFiller read FFiller write SetFiller;
  end;

  TPolygonRenderer32Class = class of TPolygonRenderer32;


  // IPolygonRendererBatching: A polygon renderer can implement this interface
  // if it supports batching.
  // Graphics32 itself does not take advantage of the interface but applications
  // can do so. See the Benchmark example application for usage.
  IPolygonRendererBatching = interface
    ['{84DE8135-D134-4A4A-B015-C194FA2469F6}']
    procedure BeginDraw;
    procedure EndDraw;
  end;


//------------------------------------------------------------------------------
//
//      TPolygonRenderer32VPR
//
//------------------------------------------------------------------------------
// Polygon renderer based on VPR. Computes exact coverages for optimal anti-aliasing.
//------------------------------------------------------------------------------
  TFillProc = procedure(Coverage: PSingleArray; AlphaValues: PColor32Array; Count: Integer; Color: TColor32);

  TPolygonRenderer32VPR = class(TPolygonRenderer32)
  private
    FFillProc: TFillProc;
  protected
    procedure UpdateFillProc;
    procedure GetFillProc(var AFillProc: TFillProc); virtual;
    procedure RenderSpan(const Span: TValueSpan; DstY: Integer); virtual;
    procedure FillSpan(const Span: TValueSpan; DstY: Integer); virtual;
    function GetRenderSpan: TRenderSpanEvent; virtual;
    property FillProc: TFillProc read FFillProc;
  public
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
  end;

//------------------------------------------------------------------------------
// TPolygonRenderer32LCD
//------------------------------------------------------------------------------
  TPolygonRenderer32LCD = class(TPolygonRenderer32VPR)
  protected
    procedure RenderSpan(const Span: TValueSpan; DstY: Integer); override;
  public
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
  end;

//------------------------------------------------------------------------------
// TPolygonRenderer32LCD2
//------------------------------------------------------------------------------
  TPolygonRenderer32LCD2 = class(TPolygonRenderer32LCD)
  public
    procedure RenderSpan(const Span: TValueSpan; DstY: Integer); override;
  end;


//------------------------------------------------------------------------------
//
//      TCustomPolygonFiller
//
//------------------------------------------------------------------------------
  TFillLineEvent = procedure(Dst: PColor32; DstX, DstY, Length: Integer;
    AlphaValues: PColor32; CombineMode: TCombineMode) of object;

  TCustomPolygonFiller = class abstract
  protected
    function GetFillLine: TFillLineEvent; virtual; abstract;
  public
    procedure BeginRendering; virtual;
    procedure EndRendering; virtual;

    property FillLine: TFillLineEvent read GetFillLine;
  end;

//------------------------------------------------------------------------------
// TCallbackPolygonFiller
//------------------------------------------------------------------------------
  TCallbackPolygonFiller = class(TCustomPolygonFiller)
  private
    FFillLineEvent: TFillLineEvent;
  protected
    function GetFillLine: TFillLineEvent; override;
  public
    procedure BeginRendering; override;
    property FillLineEvent: TFillLineEvent read FFillLineEvent write FFillLineEvent;
  end;

//------------------------------------------------------------------------------
// TInvertPolygonFiller
//------------------------------------------------------------------------------
  TInvertPolygonFiller = class(TCustomPolygonFiller)
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineBlend(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  end;

//------------------------------------------------------------------------------
// TClearPolygonFiller
//------------------------------------------------------------------------------
  TClearPolygonFiller = class(TCustomPolygonFiller)
  private
    FColor: TColor32;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineClear(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    constructor Create(Color: TColor32 = $00808080); reintroduce; virtual;

    property Color: TColor32 read FColor write FColor;
  end;

//------------------------------------------------------------------------------
// TBitmapPolygonFiller
//------------------------------------------------------------------------------
  TBitmapPolygonFiller = class(TCustomPolygonFiller)
  private
    FPattern: TCustomBitmap32;
    FOffsetY: Integer;
    FOffsetX: Integer;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineOpaque(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineBlend(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineBlendMasterAlpha(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineCustomCombine(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    procedure BeginRendering; override;
    property Pattern: TCustomBitmap32 read FPattern write FPattern;
    property OffsetX: Integer read FOffsetX write FOffsetX;
    property OffsetY: Integer read FOffsetY write FOffsetY;
  end;

//------------------------------------------------------------------------------
// TSamplerFiller
//------------------------------------------------------------------------------
  TSamplerFiller = class(TCustomPolygonFiller)
  private
    FSampler: TCustomSampler;
    FGetSample: TGetSampleInt;
    procedure SetSampler(const Value: TCustomSampler);
  protected
    procedure SamplerChanged; virtual;
    function GetFillLine: TFillLineEvent; override;
    procedure SampleLineOpaque(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    constructor Create(Sampler: TCustomSampler = nil); reintroduce; virtual;
    procedure BeginRendering; override;
    procedure EndRendering; override;
    property Sampler: TCustomSampler read FSampler write SetSampler;
  end;


//------------------------------------------------------------------------------
//
//      PolyPolygon and Polygon wrappers
//
//------------------------------------------------------------------------------
// Float, unclipped versions
//------------------------------------------------------------------------------
procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;

procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;

//------------------------------------------------------------------------------
// Float, clipped versions
//------------------------------------------------------------------------------
procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;

procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;

//------------------------------------------------------------------------------
// Fixed, unclipped versions
//------------------------------------------------------------------------------
procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonXS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonXS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;

procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonXS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil);
procedure PolygonXS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil);


//------------------------------------------------------------------------------
//
//      PolyPolyline and Polyline wrappers
//
//------------------------------------------------------------------------------
// Float, PolyPolyline
//------------------------------------------------------------------------------
procedure PolyPolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
procedure PolyPolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;

//------------------------------------------------------------------------------
// Float, Polyline
//------------------------------------------------------------------------------
procedure PolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
procedure PolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;

//------------------------------------------------------------------------------
// Fixed, PolyPolyline
//------------------------------------------------------------------------------
procedure PolyPolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;
procedure PolyPolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;

//------------------------------------------------------------------------------
// Fixed, Polyline
//------------------------------------------------------------------------------
procedure PolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;
procedure PolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;



//------------------------------------------------------------------------------
//
//      Dashed lines
//
//------------------------------------------------------------------------------
// Filled only Dashes ...
//------------------------------------------------------------------------------
// Float
//------------------------------------------------------------------------------
procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Color: TColor32;
  Closed: Boolean = False; Width: TFloat = 1.0); overload;
procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;
//------------------------------------------------------------------------------
// Fixed
//------------------------------------------------------------------------------
procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Color: TColor32;
  Closed: Boolean = False; Width: TFixed = $10000); overload;
procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000); overload;

//------------------------------------------------------------------------------
// Filled and stroked Dashes ...
//------------------------------------------------------------------------------
// Float
//------------------------------------------------------------------------------
procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFloat = 1.0); overload;
procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;
//------------------------------------------------------------------------------
// Fixed
//------------------------------------------------------------------------------
procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFixed = $10000); overload;
procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000); overload;


//------------------------------------------------------------------------------
//
//      TCustomPolygonFiller wrapper
//
//------------------------------------------------------------------------------
// Fill entire bitmap with a given polygon filler
//------------------------------------------------------------------------------
procedure FillBitmap(Bitmap: TCustomBitmap32; Filler: TCustomPolygonFiller);


//------------------------------------------------------------------------------
//
//      Polygon Renderer registration routines
//
//------------------------------------------------------------------------------
procedure RegisterPolygonRenderer(PolygonRendererClass: TCustomPolygonRendererClass);
procedure UnregisterPolygonRenderer(PolygonRendererClass: TCustomPolygonRendererClass);

type
{$if defined(NO_GENERIC_METACLASS_LISTS)}
  TCustomPolygonRendererList = class(TClassList)
  public
    function Find(const AClassName: string): TCustomPolygonRendererClass;
  end;

  TPolygonRendererList = class(TClassList)
  public
    function Find(const AClassName: string): TPolygonRenderer32Class;
  end;
{$else}
  TCustomPolygonRendererList = TCustomClassList<TCustomPolygonRendererClass>;
  TPolygonRendererList = TCustomClassList<TPolygonRenderer32Class>;
{$ifend}

var
  // CustomPolygonRendererList contains all registered renderers.
  // It corresponds to the old PolygonRendererList prior to that
  // being changed to only contain TPolygonRenderer32 classes.
  CustomPolygonRendererList: TCustomPolygonRendererList;

  // PolygonRendererList contains only renderers that inherit from TPolygonRenderer32
  PolygonRendererList: TPolygonRendererList;
  DefaultPolygonRendererClass: TPolygonRenderer32Class = TPolygonRenderer32VPR;


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
function PolygonsRegistry: TFunctionRegistry;

var
  // Coverage builders used internally by TPolygonRenderer32VPR
  MakeAlphaEvenOddUP: TFillProc;
  MakeAlphaNonZeroUP: TFillProc;
  MakeAlphaEvenOddUPF: TFillProc;
  MakeAlphaNonZeroUPF: TFillProc;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$if not defined(FPC)}
  System.Math,
  System.SysUtils,
{$else}
  Math,
  SysUtils,
{$ifend}
  GR32_Math,
  GR32_LowLevel,
  GR32_Blend,
  GR32_VectorUtils,
  GR32.Types.SIMD;

resourcestring
  RCStrNoSamplerSpecified = 'No sampler specified!';

type
  TBitmap32Access = class(TCustomBitmap32);

//------------------------------------------------------------------------------
//
//      Polygon Renderer registration routines
//
//------------------------------------------------------------------------------
procedure RegisterPolygonRenderer(PolygonRendererClass: TCustomPolygonRendererClass);
begin
  if (CustomPolygonRendererList = nil) then
    CustomPolygonRendererList := TCustomPolygonRendererList.Create;

  if (PolygonRendererList = nil) then
    PolygonRendererList := TPolygonRendererList.Create;

  CustomPolygonRendererList.Add(PolygonRendererClass);

  if (PolygonRendererClass.InheritsFrom(TPolygonRenderer32)) then
    PolygonRendererList.Add(TPolygonRenderer32Class(PolygonRendererClass));
end;

procedure UnregisterPolygonRenderer(PolygonRendererClass: TCustomPolygonRendererClass);
begin
  if (CustomPolygonRendererList <> nil) then
    CustomPolygonRendererList.Remove(PolygonRendererClass);

  if (PolygonRendererList <> nil) and (PolygonRendererClass.InheritsFrom(TPolygonRenderer32)) then
    PolygonRendererList.Remove(TPolygonRenderer32Class(PolygonRendererClass));
end;


//------------------------------------------------------------------------------
//
//      Make Alpha NonZero UP
//
//------------------------------------------------------------------------------
// Coverage builders used internally by TPolygonRenderer32VPR.
// For use in pfWinding/pfNonZero fill mode with a static color.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// MakeAlphaNonZeroUP_Pas
//------------------------------------------------------------------------------
procedure MakeAlphaNonZeroUP_Pas(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V: Cardinal;
  Last: TFloat;
  C: TColor32Entry absolute Color;
begin
  (* Mattias Andersson (from gm4iqo$87i$1@news.graphics32.org):
  **
  ** What is passed in the Coverage[] array is *not* the actual coverages and I
  ** agree that using this terminology is ambiguous. The array contains the
  ** "winding numbers" which are then processed according to either the even-odd
  ** or non-zero rule.
  ** An example of how this works can be seen here:
  **   http://www.w3.org/TR/SVG11/painting.html#FillProperties
  *)


  (*

    Compute V = Alpha (A) scaled with coverage value (M).
    The range of all values are [0..255].

      V = A * M / 255

    Since we're operating in integers this becomes:

      V = A * M div 255

    Divisions are expensive and shifts are cheap, so a normal approximation is:

      V = (A * M) div 256 ->
      V = (A * M) shr 8

    If we use the range [0..256] for the coverage value M instead, this can be
    improved with the more precise:

      V = (A * M * 257) shr 16

    Since the coverage is really a floating point value [0..+/-1] the actual
    calculation is this:

      M = Abs([Coverage * 256])
      V = (A * M * 257) shr 16

    We can improve the precision even more by calculating M in 9:8 fixed point
    format instead of 9:0

      M = Abs([Coverage * 256 * 256])
      V = (A * M * 257) shr 24

  *)
  M := C.A * $101; // $101 = 257
  Last := Infinity;
  for I := 0 to Count - 1 do
  begin
    // Reuse last computed value if coverage is the same
    // Note: Cast to integer to avoid slower floating point comparison
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(Round(Last * $10000)); // $10000 = 256 * 256
      if V > $10000 then
        V := $10000;
      V := V * M shr 24;
      C.A := V;
    end;
    AlphaValues[I] := Color;
  end;
end;

(*
procedure MakeAlphaNonZeroUP(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V, C: Cardinal;
begin
  M := Color shr 24 * $101;
  C := Color and $00ffffff;
  for I := 0 to Count - 1 do
  begin
    V := Abs(Round(Coverage[I] * $10000));
    if V > $10000 then V := $10000;
    AlphaValues[I] := (V * M and $ff000000) or C;
  end;
end;
*)


//------------------------------------------------------------------------------
// MakeAlphaNonZeroUP_SSE2
//------------------------------------------------------------------------------
// Contributed by Kadaif
//------------------------------------------------------------------------------
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}

procedure MakeAlphaNonZeroUP_SSE2(Coverage: PSingleArray; AlphaValues: PColor32Array; Count: integer; Color: TColor32);
// Note: Don't bother aligning the SSE_FloatOne data so we can use
// MOVAPS; It gives zero performance improvement (and might be slower
// due to instruction size).
{$if defined(TARGET_x64) and defined(FPC)}begin{$ifend}
asm
{$if defined(TARGET_x86)}

  // Parameters (x86):
  // EAX <- Coverage
  // EDX <- AlphaValues
  // ECX <- Count
  // Stack[0] <- Color

  // SSE register usage:
  //   XMM0: work
  //   XMM1:
  //   XMM2: [Alpha * 1.0] x 4
  //   XMM3: [Color without alpha] x 4
  //   XMM4: [$7FFFFFFF] x 4
  //   XMM5: [1.0] x 4

        TEST        ECX, ECX
        JLE         @EXIT

        PUSH        EBX
        PUSH        ESI
        PUSH        EDI

        MOV         EDI, Color          // save ARGB
        MOV         EBX, EDI

         // Prepare 0RGB: mask off alpha from Color and replicate into XMM3
        AND         EBX, $00FFFFFF
        MOVD        XMM3, EBX
        PSHUFD      XMM3, XMM3, $0      // save 0RGB

        // Load constant 1.0 into XMM5
        MOVUPS      XMM5, DQWORD PTR [SSE_FloatOne]

        // Prepare alpha multiplier: extract alpha from Color, replicate and convert to float
        SHR         EDI, 24             // alpha
        MOVD        XMM2, EDI
        PSHUFD      XMM2, XMM2, $0      // alphas
        CVTDQ2PS    XMM2, XMM2          // to float

        // Prepare mask for absolute value (0x7FFFFFFF) in XMM4
        PCMPEQD     XMM4, XMM4
        PSRLD       XMM4, 1

        CMP         ECX, 4
        JL          @remainder

        // Main loop: process 4 elements per iteration
        MOV         ESI, ECX
        SAR         ESI, 2

@Loop:
        MOVUPS      XMM0, [EAX]         // Load coverage
        ANDPS       XMM0, XMM4          // abs
        MINPS       XMM0, XMM5          // min (1)
        MULPS       XMM0, XMM2          // multiply with alpha
        CVTPS2DQ    XMM0, XMM0          // 4xsingle -> 4xinteger
        PSLLD       XMM0, 24            // A -> A000
        POR         XMM0, XMM3          // A000 or 0RGB -> ARGB
        MOVDQU      [EDX], XMM0         // Save ARGB
        ADD         EDX, 16
        ADD         EAX, 16
        DEC         ESI
        JNZ         @Loop
        AND         ECX, 3              // get remainder
        JZ          @END

@remainder:
        // Same as above, just on 1 dword/single at a time instead of 4
        MOVSS       XMM0, [EAX]
        ANDPS       XMM0, XMM4
        MINSS       XMM0, XMM5
        MULSS       XMM0, XMM2
        CVTPS2DQ    XMM0, XMM0
        PSLLD       XMM0, 24
        POR         XMM0, XMM3
        MOVD        [EDX], XMM0
        ADD         EDX, 4
        ADD         EAX, 4
        DEC         ECX
        JNZ         @remainder

@END:
        POP         EDI
        POP         ESI
        POP         EBX
@EXIT:

{$elseif defined(TARGET_x64)}

  // Parameters (x64):
  // RCX <- Coverage
  // RDX <- AlphaValues
  // R8D <- Count
  // R9D <- Color

  // SSE register usage:
  //   XMM0: work
  //   XMM1:
  //   XMM2: [Alpha * 1.0] x 4
  //   XMM3: [Color without alpha] x 4
  //   XMM4: [$7FFFFFFF] x 4
  //   XMM5: [1.0] x 4

{$IFNDEF FPC}
  .SAVENV XMM4
  .SAVENV XMM5
{$ENDIF}

        TEST        R8D, R8D
        JLE         @Exit

        // Prepare 0RGB: mask off alpha from Color and replicate into XMM3
        MOV         EAX, R9D
        AND         EAX, $00FFFFFF
        MOVD        XMM3, EAX
        PSHUFD      XMM3, XMM3, 0       // save 0RGB

        // Load constant 1.0 into XMM6
{$if (not defined(FPC))}
        MOVUPS      XMM5, DQWORD PTR [SSE_FloatOne]
{$else}
        MOVUPS      XMM5, DQWORD PTR [rip+SSE_FloatOne]
{$ifend}

        // Prepare alpha multiplier: extract alpha from Color, replicate and convert to float
        SHR         R9D, 24
        MOVD        XMM2, R9D
        PSHUFD      XMM2, XMM2, 0
        CVTDQ2PS    XMM2, XMM2

        // Prepare mask for absolute value (0x7FFFFFFF) in XMM4
        PCMPEQD     XMM4, XMM4
        PSRLD       XMM4, 1

        CMP         R8D, 4
        JL          @Remainder

        // Main loop: process 4 elements per iteration
        MOV         R10, R8
        SHR         R10, 2

@Loop:
        MOVUPS      XMM0, [RCX]
        ANDPS       XMM0, XMM4
        MINPS       XMM0, XMM5
        MULPS       XMM0, XMM2
        CVTPS2DQ    XMM0, XMM0
        PSLLD       XMM0, 24
        POR         XMM0, XMM3
        MOVDQU      [RDX], XMM0
        ADD         RCX, 16
        ADD         RDX, 16
        DEC         R10
        JNZ         @Loop

        AND         R8D, 3
        JZ          @Exit

@Remainder:
        MOVSS       XMM0, [RCX]
        ANDPS       XMM0, XMM4
        MINSS       XMM0, XMM5
        MULSS       XMM0, XMM2
        CVTSS2SI    EAX, XMM0
        SHL         EAX, 24
        MOVD        R11D, XMM3
        OR          EAX, R11D
        MOV         [RDX], EAX
        ADD         RCX, 4
        ADD         RDX, 4
        DEC         R8D
        JNZ         @Remainder

@Exit:

{$if defined(FPC)}end['XMM4', 'XMM5'];{$ifend}

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ifend}


//------------------------------------------------------------------------------
//
//      Make Alpha EvenOdd UP
//
//------------------------------------------------------------------------------
// Coverage builders used internally by TPolygonRenderer32VPR.
// For use in pfAlternate/pfEvenOdd fill mode with a static color.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// MakeAlphaEvenOddUP_Pas
//------------------------------------------------------------------------------
procedure MakeAlphaEvenOddUP_Pas(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V: Cardinal;
  Last: TFloat;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  Last := Infinity;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(Round(Coverage[I] * $10000));
      V := V and $01ffff;
      if V >= $10000 then
        V := V xor $1ffff;
      V := V * M shr 24;
      C.A := V;
    end;
    AlphaValues[I] := Color;
  end;
end;


//------------------------------------------------------------------------------
// MakeAlphaEvenOddUP_SSE41
//------------------------------------------------------------------------------
// Contributed by Kadaif
//------------------------------------------------------------------------------
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
procedure MakeAlphaEvenOddUP_SSE2(Coverage: PSingleArray; AlphaValues: PColor32Array; Count: integer; Color: TColor32);
{$if defined(TARGET_x64) and defined(FPC)}begin{$ifend}
asm
{$if defined(TARGET_x86)}

  // Parameters (x86):
  // EAX <- Coverage
  // EDX <- AlphaValues
  // ECX <- Count
  // Stack[0] <- Color

  // SSE register usage:
  //   XMM0: work
  //   XMM1: work
  //   XMM2: [Alpha * 257] x 4
  //   XMM3: [Color without alpha] x 4
  //   XMM4: [$7FFFFFFF] x 4
  //   XMM5: [256x256] x 4
  //   XMM6: [$0001FFFF] x 4
  //   XMM7: work

        TEST        ECX, ECX
        JLE         @EXIT

        PUSH        EBX
        PUSH        ESI
        PUSH        EDI

        MOV         EDI, Color
        MOV         EBX, EDI
        AND         EBX, $00FFFFFF
        MOVD        XMM3, EBX
        PSHUFD      XMM3, XMM3, $0      // save 0RGB

        PCMPEQD     XMM6, XMM6
        PSRLD       XMM6, 15            // 4 x $0001FFFF
        MOVUPS      XMM5, DQWORD PTR [SSE_Float256x256]

        SHR         EDI, 24             // alpha
        MOVD        XMM2, EDI
        PUNPCKLBW   XMM2, XMM2          // alpha * 257
        PSHUFD      XMM2, XMM2, $0      // alphas

        PCMPEQD     XMM4, XMM4          // for abs
        PSRLD       XMM4, 1

        CMP         ECX, 4
        JL          @remainder

        MOV         ESI, ECX
        SAR         ESI, 2

@Loop:
        MOVUPS      XMM0, [EAX]         // Load overage
        ANDPS       XMM0, XMM4          // abs
        MULPS       XMM0, XMM5
        CVTPS2DQ    XMM0, XMM0
        PAND        XMM0, XMM6          // and with $0001FFFF
        MOVDQA      XMM7, XMM0
        PXOR        XMM7, XMM6

        // PMINUD (SSE4.1) for SSE2
        MOVDQA      XMM1, XMM7
        PCMPGTD     XMM1, XMM0
        PAND        XMM0, XMM1
        PANDN       XMM1, XMM7
        POR         XMM0, XMM1

        // alpha * 257 * Coverage
        PMULHUW     XMM0, XMM2
        PSRLW       XMM0, 8
        PSLLD       XMM0, 24
        POR         XMM0, XMM3
        MOVDQU      [EDX], XMM0         // Save ARGB
        ADD         EAX, 16
        ADD         EDX, 16
        DEC         ESI
        JNZ         @Loop
        AND         ECX, 3
        JZ          @END

@remainder:
        MOVSS       XMM0, [EAX]         // coverage
        ANDPS       XMM0, XMM4          // abs
        MULSS       XMM0, XMM5
        CVTPS2DQ    XMM0, XMM0
        PAND        XMM0, XMM6          // and with $1FF
        MOVDQA      XMM7, XMM6
        PSUBD       XMM7, XMM0
        MOVDQA      XMM1, XMM7
        PCMPGTD     XMM1, XMM0
        PAND        XMM0, XMM1
        PANDN       XMM1, XMM7
        POR         XMM0, XMM1
        PMULHUW     XMM0, XMM2
        PSRLW       XMM0, 8
        PSLLD       XMM0, 24
        POR         XMM0, XMM3
        MOVD        [EDX], XMM0
        ADD         EDX, 4
        ADD         EAX, 4
        DEC         ECX
        JNZ         @remainder

@END:
        POP         EDI
        POP         ESI
        POP         EBX
@EXIT:

{$elseif defined(TARGET_x64)}

  // Parameters (x64):
  // RCX <- Coverage
  // RDX <- AlphaValues
  // R8D <- Count
  // R9D <- Color

  // SSE register usage:
  //   XMM0: work
  //   XMM1: work
  //   XMM2: [Alpha * 257] x 4
  //   XMM3: [Color without alpha] x 4
  //   XMM4: [$7FFFFFFF] x 4
  //   XMM5: [256x256] x 4
  //   XMM6: [$0001FFFF] x 4
  //   XMM7: work

{$IFNDEF FPC}
  .SAVENV XMM4
  .SAVENV XMM5
  .SAVENV XMM6
  .SAVENV XMM7
{$ENDIF}

        TEST        R8D, R8D
        JLE         @EXIT

(*
        SUB         RSP, 32
        MOVDQU      [RSP], XMM6
        MOVDQU      [RSP + 16], XMM7
*)

        MOV         R10D, R9D
        AND         R10D, $00FFFFFF
        MOVD        XMM3, R10D
        PSHUFD      XMM3, XMM3,$0       // save 0RGB

        PCMPEQD     XMM6, XMM6
        PSRLD       XMM6, 15            // $0001FFFF

{$if (not defined(FPC))}
        MOVUPS      XMM5, DQWORD PTR [SSE_Float256x256]
{$else}
        MOVUPS      XMM5, DQWORD PTR [rip+SSE_Float256x256]
{$ifend}

        SHR         R9D, 24
        MOVD        XMM2, R9D
        PUNPCKLBW   XMM2, XMM2          // alpha * 257
        PSHUFD      XMM2, XMM2, $0      // alphas

        PCMPEQD     XMM4, XMM4          // for abs
        PSRLD       XMM4, 1

        CMP         R8D, 4
        JL          @remainder

        MOV         EAX, R8D
        SAR         EAX, 2

@Loop:
        MOVUPS      XMM0, [RCX]         // coverage
        ANDPS       XMM0, XMM4          // abs
        MULPS       XMM0, XMM5          // multiply
        CVTPS2DQ    XMM0, XMM0
        PAND        XMM0, XMM6          // and with $0001FFFF
        MOVDQA      XMM7, XMM0
        PXOR        XMM7, XMM6

        // PMINUD for SSE2
        MOVDQA      XMM1, XMM7
        PCMPGTD     XMM1, XMM0
        PAND        XMM0, XMM1
        PANDN       XMM1, XMM7
        POR         XMM0, XMM1

        PMULHUW     XMM0, XMM2
        PSRLW       XMM0, 8
        PSLLD       XMM0, 24
        POR         XMM0, XMM3
        MOVDQU      [RDX], XMM0
        ADD         RCX, 16
        ADD         RDX, 16
        DEC         EAX
        JNZ         @Loop
        AND         R8D, 3
        JZ          @END

@remainder:
        MOVSS       XMM0, [RCX]         // coverage
        ANDPS       XMM0, XMM4          // abs
        MULSS       XMM0, XMM5
        CVTPS2DQ    XMM0, XMM0
        PAND        XMM0, XMM6
        MOVDQA      XMM7, XMM6
        PSUBD       XMM7, XMM0

        // PMINUD for SSE2
        MOVDQA      XMM1, XMM7
        PCMPGTD     XMM1, XMM0
        PAND        XMM0, XMM1
        PANDN       XMM1, XMM7
        POR         XMM0, XMM1

        PMULHUW     XMM0, XMM2
        PSRLW       XMM0, 8
        PSLLD       XMM0, 24
        POR         XMM0, XMM3
        MOVD        [RDX], XMM0
        ADD         RDX,4
        ADD         RCX,4
        DEC         R8D
        JNZ         @remainder

@END:
(*
        MOVDQU      XMM7, [RSP + 16]
        MOVDQU      XMM6, [RSP]
        ADD         RSP, 32
*)
@EXIT:

{$if defined(FPC)}end['XMM4', 'XMM5', 'XMM6', 'XMM7'];{$ifend}

{$else}
{$error 'Missing target'}
{$ifend}
end;

//------------------------------------------------------------------------------
// MakeAlphaEvenOddUP_SSE41
//------------------------------------------------------------------------------
// Contributed by Kadaif
//------------------------------------------------------------------------------
procedure MakeAlphaEvenOddUP_SSE41(Coverage: PSingleArray; AlphaValues: PColor32Array; Count: integer; Color: TColor32);
{$if defined(TARGET_x64) and defined(FPC)}begin{$ifend}
asm
{$if defined(TARGET_x86)}

  // Parameters (x86):
  // EAX <- Coverage
  // EDX <- AlphaValues
  // ECX <- Count
  // Stack[0] <- Color

  // SSE register usage:
  //   XMM0: work
  //   XMM1: work
  //   XMM2: [Alpha * 257] x 4
  //   XMM3: [Color without alpha] x 4
  //   XMM4: [$7FFFFFFF] x 4
  //   XMM5: [256x256] x 4
  //   XMM6: [$0001FFFF] x 4
  //   XMM7: work

        TEST        ECX,ECX
        JLE         @EXIT

        PUSH        EBX
        PUSH        ESI
        PUSH        EDI

        MOV         EDI, Color
        MOV         EBX, EDI
        AND         EBX, $00FFFFFF
        MOVD        XMM3, EBX
        PSHUFD      XMM3, XMM3, $0      // save 0RGB
        PCMPEQD     XMM6, XMM6
        PSRLD       XMM6, 15            // $0001FFFF
        MOVUPS      XMM5, DQWORD PTR [SSE_Float256x256]

        SHR         EDI, 24
        MOVD        XMM2, EDI
        PUNPCKLBW   XMM2, XMM2          // alpha * 257
        PSHUFD      XMM2, XMM2, $0      // alphas

        PCMPEQD     XMM4, XMM4          // for abs
        PSRLD       XMM4, 1

        CMP         ECX, 4
        JL          @remainder

        MOV         ESI, ECX
        SAR         ESI, 2

@Loop:
        MOVUPS      XMM0, [EAX]         // Load coverage
        ANDPS       XMM0, XMM4          // abs
        MULPS       XMM0, XMM5
        CVTPS2DQ    XMM0, XMM0
        PAND        XMM0, XMM6          // and with $0001FFFF
        MOVDQA      XMM7, XMM0
        PXOR        XMM7, XMM6
        PMINUD      XMM0, XMM7
        PMULHUW     XMM0, XMM2
        PSRLW       XMM0, 8
        PSLLD       XMM0, 24
        POR         XMM0, XMM3
        MOVDQU      [EDX], XMM0         // Save ARGB
        ADD         EAX, 16
        ADD         EDX, 16
        DEC         ESI
        JNZ         @Loop
        AND         ECX,3
        JZ          @END

@remainder:
        MOVSS       XMM0, [EAX]         // coverage
        ANDPS       XMM0, XMM4          // abs
        MULSS       XMM0, XMM5
        CVTPS2DQ    XMM0, XMM0
        PAND        XMM0, XMM6          // and with $1FF
        MOVDQA      XMM7, XMM6
        PSUBD       XMM7, XMM0
        PMINUD      XMM0, XMM7
        PMULHUW     XMM0, XMM2
        PSRLW       XMM0, 8
        PSLLD       XMM0, 24
        POR         XMM0, XMM3
        MOVD        [EDX], XMM0
        ADD         EDX, 4
        ADD         EAX, 4
        DEC         ECX
        JNZ         @remainder

@END:
        POP         EDI
        POP         ESI
        POP         EBX
@EXIT:

{$elseif defined(TARGET_x64)}

  // Parameters (x64):
  // RCX <- Coverage
  // RDX <- AlphaValues
  // R8D <- Count
  // R9D <- Color

  // SSE register usage:
  //   XMM0: work
  //   XMM1: work
  //   XMM2: [Alpha * 257] x 4
  //   XMM3: [Color without alpha] x 4
  //   XMM4: [$7FFFFFFF] x 4
  //   XMM5: [256x256] x 4
  //   XMM6: [$0001FFFF] x 4
  //   XMM7: work

{$IFNDEF FPC}
  .SAVENV XMM4
  .SAVENV XMM5
  .SAVENV XMM6
  .SAVENV XMM7
{$ENDIF}

        TEST        R8D, R8D
        JLE         @EXIT

(*
        SUB         RSP,32
        MOVDQU      [RSP],XMM6
        MOVDQU      [RSP + 16],XMM7
*)

        MOV         R10D, R9D
        AND         R10D, $00FFFFFF
        MOVD        XMM3, R10D
        PSHUFD      XMM3, XMM3,$0       // save 0RGB
        PCMPEQD     XMM6, XMM6
        PSRLD       XMM6, 15            // $0001FFFF

{$if (not defined(FPC))}
        MOVUPS      XMM5, DQWORD PTR [SSE_Float256x256]
{$else}
        MOVUPS      XMM5, DQWORD PTR [rip+SSE_Float256x256]
{$ifend}

        SHR         R9D, 24
        MOVD        XMM2, R9D
        PUNPCKLBW   XMM2, XMM2          // alpha * 257
        PSHUFD      XMM2, XMM2, $0      // alphas

        PCMPEQD     XMM4, XMM4          // for abs
        PSRLD       XMM4, 1

        CMP         R8D, 4
        JL          @remainder


        MOV         EAX, R8D
        SAR         EAX, 2

@Loop:
        MOVUPS      XMM0, [RCX]         // Load coverage
        ANDPS       XMM0, XMM4          // abs
        MULPS       XMM0, XMM5
        CVTPS2DQ    XMM0, XMM0
        PAND        XMM0, XMM6          // and with $0001FFFF
        MOVDQA      XMM7, XMM0
        PXOR        XMM7, XMM6
        PMINUD      XMM0, XMM7
        PMULHUW     XMM0, XMM2
        PSRLW       XMM0, 8
        PSLLD       XMM0, 24
        POR         XMM0, XMM3
        MOVDQU      [RDX], XMM0         // Save ARGB
        ADD         RCX, 16
        ADD         RDX, 16
        DEC         EAX
        JNZ         @Loop
        AND         R8D, 3
        JZ          @END

@remainder:
        MOVSS       XMM0, [RCX]         // Load coverage
        ANDPS       XMM0, XMM4          // abs
        MULSS       XMM0, XMM5
        CVTPS2DQ    XMM0, XMM0
        PAND        XMM0, XMM6          // and with $1FF
        MOVDQA      XMM7, XMM6
        PSUBD       XMM7, XMM0
        PMINUD      XMM0, XMM7
        PMULHUW     XMM0, XMM2
        PSRLW       XMM0, 8
        PSLLD       XMM0, 24
        POR         XMM0, XMM3
        MOVD        [RDX], XMM0         // Save ARGB
        ADD         RDX, 4
        ADD         RCX, 4
        DEC         R8D
        JNZ         @remainder

@END:

(*
        MOVDQU      XMM7,[RSP + 16]
        MOVDQU      XMM6,[RSP]
        ADD         RSP,32
*)
@EXIT:

{$if defined(FPC)}end['XMM4', 'XMM5', 'XMM6', 'XMM7'];{$ifend}

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ifend}


//------------------------------------------------------------------------------
//
//      Unused Make Alpha * P stuff
//
//------------------------------------------------------------------------------
procedure MakeAlphaNonZeroP(Value: Single; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  M, V: Cardinal;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  V := Abs(Round(Value * $10000));
  if V > $10000 then
    V := $10000;
  V := V * M shr 24;
  C.A := V;
  FillLongWord(AlphaValues[0], Count, Color);
end;

procedure MakeAlphaEvenOddP(Value: Single; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  M, V: Cardinal;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  V := Abs(Round(Value * $10000));
  V := V and $01ffff;
  if V > $10000 then
    V := V xor $1ffff;
  V := V * M shr 24;
  C.A := V;
  FillLongWord(AlphaValues[0], Count, Color);
end;



//------------------------------------------------------------------------------
//
//      Make Alpha NonZero UPF
//
//------------------------------------------------------------------------------
// Coverage builders used internally by TPolygonRenderer32VPR.
// Only extracts alpha.
// For use in pfWinding/pfNonZero fill mode with a filler.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// MakeAlphaNonZeroUPF_Pas
//------------------------------------------------------------------------------
procedure MakeAlphaNonZeroUPF_Pas(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  V: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    V := Clamp(Round(Abs(Coverage[I]) * 256));
    AlphaValues[I] := V;
  end;
end;


//------------------------------------------------------------------------------
//
//      Make Alpha EvenOdd UPF
//
//------------------------------------------------------------------------------
// Coverage builders used internally by TPolygonRenderer32VPR.
// Only extracts alpha.
// For use in pfAlternate/pfEvenOdd  fill mode with a filler.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// MakeAlphaEvenOddUPF_Pas
//------------------------------------------------------------------------------
procedure MakeAlphaEvenOddUPF_Pas(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  V: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    V := Round(Abs(Coverage[I]) * 256);
    V := V and $000001ff;
    if V >= $100 then
      V := V xor $1ff;
    AlphaValues[I] := V;
  end;
end;


//------------------------------------------------------------------------------
//
//      Unused MakeAlpha * PF stuff
//
//------------------------------------------------------------------------------
procedure MakeAlphaNonZeroPF(Value: Single; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  V: Integer;
begin
  V := Clamp(Round(Abs(Value) * 256));
  FillLongWord(AlphaValues[0], Count, V);
end;

procedure MakeAlphaEvenOddPF(Value: Single; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  V: Integer;
begin
  V := Round(Abs(Value) * 256);
  V := V and $000001ff;
  if V >= $100 then
    V := V xor $1ff;
  FillLongWord(AlphaValues[0], Count, V);
end;


//------------------------------------------------------------------------------
//
//      PolyPolygon and Polygon wrappers
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Float, unclipped versions
//------------------------------------------------------------------------------

procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    Renderer.PolyPolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    Renderer.PolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  if (Filler = nil) then
    Exit;
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    Renderer.PolyPolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  if (Filler = nil) then
    Exit;
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    Renderer.PolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolyPolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolyPolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolyPolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolyPolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;


//------------------------------------------------------------------------------
// Float, clipped versions
//------------------------------------------------------------------------------

procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolyPolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
  IntersectedClipRect: TRect;
begin
  if (Filler = nil) then
    Exit;
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolyPolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
  IntersectedClipRect: TRect;
begin
  if (Filler = nil) then
    Exit;
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolyPolygonFS_LCD(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32;
  FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolyPolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure PolyPolygonFS_LCD2(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32;
  FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolyPolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;


//------------------------------------------------------------------------------
// Fixed, unclipped versions
//------------------------------------------------------------------------------

procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    Renderer.PolyPolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    Renderer.PolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    Renderer.PolyPolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    Renderer.PolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonXS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonXS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD;
begin
  Renderer := TPolygonRenderer32LCD.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolyPolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonXS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonXS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32LCD2;
begin
  Renderer := TPolygonRenderer32LCD2.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.FillMode := FillMode;
    Renderer.Color := Color;
    Renderer.PolyPolygonFS(FixedPointToFloatPoint(Points),
      FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

//------------------------------------------------------------------------------
//
//      PolyPolyline and Polyline wrappers
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Float, PolyPolyline
//------------------------------------------------------------------------------
procedure PolyPolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFloat; Transformation: TTransformation);
var
  Dst: TArrayOfArrayOfFloatPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  PolyPolygonFS(Bitmap, Dst, Color, pfWinding, Transformation);
end;

//------------------------------------------------------------------------------

procedure PolyPolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: TFloat;
  Transformation: TTransformation);
var
  Dst: TArrayOfArrayOfFloatPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  PolyPolygonFS(Bitmap, Dst, Filler, pfWinding, Transformation);
end;

//------------------------------------------------------------------------------
// Float, Polyline
//------------------------------------------------------------------------------

procedure PolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFloat; JoinStyle: TJoinStyle;
  EndStyle: TEndStyle; MiterLimit: TFloat; Transformation: TTransformation);
begin
  PolyPolylineFS(Bitmap, PolyPolygon(Points), Color, Closed, StrokeWidth,
    JoinStyle, EndStyle, MiterLimit, Transformation);
end;

//------------------------------------------------------------------------------

procedure PolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: TFloat;
  Transformation: TTransformation);
begin
  PolyPolylineFS(Bitmap, PolyPolygon(Points), Filler, Closed, StrokeWidth,
    JoinStyle, EndStyle, MiterLimit, Transformation);
end;

//------------------------------------------------------------------------------
// Fixed, PolyPolyline
//------------------------------------------------------------------------------
procedure PolyPolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFixed; JoinStyle: TJoinStyle;
  EndStyle: TEndStyle; MiterLimit: TFixed; Transformation: TTransformation);
var
  Dst: TArrayOfArrayOfFixedPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle,
    MiterLimit);
  PolyPolygonXS(Bitmap, Dst, Color, pfWinding, Transformation);
end;

//------------------------------------------------------------------------------

procedure PolyPolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean; StrokeWidth: TFixed;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: TFixed;
  Transformation: TTransformation);
var
  Dst: TArrayOfArrayOfFixedPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle,
    MiterLimit);
  PolyPolygonXS(Bitmap, Dst, Filler, pfWinding, Transformation);
end;

//------------------------------------------------------------------------------
// Fixed, Polyline
//------------------------------------------------------------------------------
procedure PolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFixed;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFixed; Transformation: TTransformation);
begin
  PolyPolylineXS(Bitmap, PolyPolygon(Points), Color,
    Closed, StrokeWidth, JoinStyle, EndStyle,
    MiterLimit, Transformation);
end;

//------------------------------------------------------------------------------

procedure PolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean; StrokeWidth: TFixed;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle; MiterLimit: TFixed;
  Transformation: TTransformation);
begin
  PolyPolylineXS(Bitmap, PolyPolygon(Points), Filler, Closed, StrokeWidth,
    JoinStyle, EndStyle, MiterLimit, Transformation);
end;


//------------------------------------------------------------------------------
//
//      Dashed lines
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Filled only Dashes ...
//------------------------------------------------------------------------------
// Float
//------------------------------------------------------------------------------
procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Color: TColor32; Closed: Boolean; Width: TFloat);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineFS(Bitmap, MultiPoly, Color, False, Width);
end;

//------------------------------------------------------------------------------

procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolygonFS(Bitmap, MultiPoly, FillColor);
  PolyPolylineFS(Bitmap, MultiPoly, StrokeColor, True, StrokeWidth);
end;

//------------------------------------------------------------------------------
// Fixed
//------------------------------------------------------------------------------

procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Color: TColor32; Closed: Boolean; Width: TFixed);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, Color, False, Width);
end;

procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, FillColor, False, Width);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolylineXS(Bitmap, MultiPoly, StrokeColor, True, strokeWidth);
end;

//------------------------------------------------------------------------------
// Filled and stroked Dashes ...
//------------------------------------------------------------------------------
// Float
//------------------------------------------------------------------------------

procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller;
  Closed: Boolean; Width: TFloat);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineFS(Bitmap, MultiPoly, Filler, False, Width);
end;

//------------------------------------------------------------------------------

procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolygonFS(Bitmap, MultiPoly, Filler);
  PolyPolylineFS(Bitmap, MultiPoly, StrokeColor, True, StrokeWidth);
end;

//------------------------------------------------------------------------------
// Fixed
//------------------------------------------------------------------------------

procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller;
  Closed: Boolean; Width: TFixed);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, Filler, False, Width);
end;

//------------------------------------------------------------------------------

procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, Filler, False, Width);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolylineXS(Bitmap, MultiPoly, StrokeColor, True, StrokeWidth);
end;


//------------------------------------------------------------------------------
//
//      TCustomPolygonFiller wrapper
//
//------------------------------------------------------------------------------
procedure FillBitmap(Bitmap: TCustomBitmap32; Filler: TCustomPolygonFiller);
var
  AlphaValues: PColor32;
  Y: Integer;
begin
{$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc(Bitmap.Width * SizeOf(TColor32));
{$ELSE}
  GetMem(AlphaValues, Bitmap.Width * SizeOf(TColor32));
{$ENDIF}
  FillLongword(AlphaValues^, Bitmap.Width, $FF);
  Filler.BeginRendering;
  for Y := 0 to Bitmap.Height - 1 do
    Filler.FillLine(PColor32(Bitmap.ScanLine[y]), 0, y, Bitmap.Width,
      AlphaValues, Bitmap.CombineMode);
  Filler.EndRendering;
{$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
{$ELSE}
  FreeMem(AlphaValues);
{$ENDIF}
end;



//------------------------------------------------------------------------------
//
//      LCD sub-pixel rendering
//
//------------------------------------------------------------------------------
// References:
// - https://en.wikipedia.org/wiki/Subpixel_rendering
// - https://www.grc.com/cleartype.htm
// - https://en.wikipedia.org/wiki/ClearType
// - https://en.wikipedia.org/wiki/CoolType
//------------------------------------------------------------------------------

type
{$if not defined(FPC)}
  PByteArray = System.SysUtils.PByteArray;
{$else}
  PByteArray = SysUtils.PByteArray;
{$ifend}

type
  TRGBTriple = packed record
    B, G, R: Byte;
  end;

  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0..0] of TRGBTriple;

  TMakeAlphaProcLCD = procedure(Coverage: PSingleArray; AlphaValues: PByteArray; Count: Integer; Color: TColor32);

//------------------------------------------------------------------------------
//
//      Make Alpha NonZero LCD
//
//------------------------------------------------------------------------------
// Coverage builders used internally by TPolygonRenderer32LCD.
// Uses subpixel anti-aliasing.
// For use in pfWinding/pfNonZero fill mode with a static color.
//------------------------------------------------------------------------------
procedure MakeAlphaNonZeroLCD(Coverage: PSingleArray; AlphaValues: PByteArray;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V: Cardinal;
  Last: TFloat;
  C: TColor32Entry absolute Color;
begin
  M := C.A * 86;  // 86 = 258 / 3

  Last := Infinity;
  V := 0;
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(Round(Last * $10000));
      if V > $10000 then
        V := $10000;
      V := V * M shr 24;
    end;
    Inc(AlphaValues[I], V);
    Inc(AlphaValues[I + 1], V);
    AlphaValues[I + 2] := V;
  end;
  AlphaValues[Count + 2] := 0;
  AlphaValues[Count + 3] := 0;
end;


//------------------------------------------------------------------------------
//
//      Make Alpha EvenOdd LCD
//
//------------------------------------------------------------------------------
// Coverage builders used internally by TPolygonRenderer32LCD.
// Uses subpixel anti-aliasing.
// For use in pfAlternate/pfEvenOdd fill mode with a static color.
//------------------------------------------------------------------------------
procedure MakeAlphaEvenOddLCD(Coverage: PSingleArray; AlphaValues: PByteArray;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  M, V: Cardinal;
  Last: TFloat;
begin
  M := Color shr 24 * 86;  // 86 = 258 / 3

  Last := Infinity;
  V := 0;
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  for I := 0 to Count - 1 do
  begin
    if PInteger(@Last)^ <> PInteger(@Coverage[I])^ then
    begin
      Last := Coverage[I];
      V := Abs(Round(Coverage[I] * $10000));
      V := V and $01ffff;
      if V >= $10000 then
        V := V xor $1ffff;
      V := V * M shr 24;
    end;
    Inc(AlphaValues[I], V);
    Inc(AlphaValues[I + 1], V);
    AlphaValues[I + 2] := V;
  end;
  AlphaValues[Count + 2] := 0;
  AlphaValues[Count + 3] := 0;
end;


//------------------------------------------------------------------------------
//
//      Make Alpha NonZero LCD2
//
//------------------------------------------------------------------------------
// Coverage builders used internally by TPolygonRenderer32LCD2.
// Uses subpixel anti-aliasing. Slightly softer AA transitions.
// For use in pfWinding/pfNonZero fill mode with a static color.
//------------------------------------------------------------------------------
procedure MakeAlphaNonZeroLCD2(Coverage: PSingleArray; AlphaValues: PByteArray;
  Count: Integer; Color: TColor32);
var
  I: Integer;
begin
  MakeAlphaNonZeroLCD(Coverage, AlphaValues, Count, Color);
  AlphaValues[Count + 2] := (AlphaValues[Count] + AlphaValues[Count + 1]) div 3;
  AlphaValues[Count + 3] := AlphaValues[Count + 1] div 3;
  for I := Count + 1 downto 2 do
  begin
    AlphaValues[I] := (AlphaValues[I] + AlphaValues[I - 1] + AlphaValues[I - 2]) div 3;
  end;
  AlphaValues[1] := (AlphaValues[0] + AlphaValues[1]) div 3;
  AlphaValues[0] := AlphaValues[0] div 3;
end;


//------------------------------------------------------------------------------
//
//      Make Alpha EvenOdd LCD2
//
//------------------------------------------------------------------------------
// Coverage builders used internally by TPolygonRenderer32LCD2.
// Uses subpixel anti-aliasing. Slightly softer AA transitions.
// For use in pfAlternate/pfEvenOdd fill mode with a static color.
//------------------------------------------------------------------------------
procedure MakeAlphaEvenOddLCD2(Coverage: PSingleArray; AlphaValues: PByteArray;
  Count: Integer; Color: TColor32);
var
  I: Integer;
begin
  MakeAlphaEvenOddLCD(Coverage, AlphaValues, Count, Color);
  AlphaValues[Count + 2] := (AlphaValues[Count] + AlphaValues[Count + 1]) div 3;
  AlphaValues[Count + 3] := AlphaValues[Count + 1] div 3;
  for I := Count + 1 downto 2 do
  begin
    AlphaValues[I] := (AlphaValues[I] + AlphaValues[I - 1] + AlphaValues[I - 2]) div 3;
  end;
  AlphaValues[1] := (AlphaValues[0] + AlphaValues[1]) div 3;
  AlphaValues[0] := AlphaValues[0] div 3;
end;


//------------------------------------------------------------------------------
// CombineLineLCD
//------------------------------------------------------------------------------
procedure CombineLineLCD(Weights: PRGBTripleArray; Dst: PColor32Array; Color: TColor32; Count: Integer);
var
  I: Integer;
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  Weights64: UInt64;
{$ENDIF}
begin
  I := 0;
  while Count <> 0 do
{$IFDEF TEST_BLENDMEMRGB128SSE4}
    if (Count shr 1) = 0 then
{$ENDIF}
    begin
      if PColor32(@Weights[I])^ = $FFFFFFFF then
        Dst[I] := Color
      else
        BlendMemRGB(Color, Dst[I], PColor32(@Weights[I])^);
      Dec(Count);
      Inc(I);
    end
{$IFDEF TEST_BLENDMEMRGB128SSE4}
    else
    begin
      Weights64 := (UInt64(PColor32(@Weights[I + 1])^) shl 32) or
        PColor32(@Weights[I])^;
      if Weights64 = $FFFFFFFFFFFFFFFF then
      begin
        Dst[I] := Color;
        Dst[I + 1] := Color;
      end
      else
        BlendMemRGB128(Color, Dst[I], Weights64);
      Dec(Count, 2);
      Inc(I, 2);
    end
{$ENDIF};
end;


//------------------------------------------------------------------------------
//
//      TCustomPolygonFiller
//
//------------------------------------------------------------------------------
procedure TCustomPolygonFiller.BeginRendering;
begin
  // implemented by descendants
end;

procedure TCustomPolygonFiller.EndRendering;
begin
  // implemented by descendants
end;


//------------------------------------------------------------------------------
//
//      TCallbackPolygonFiller
//
//------------------------------------------------------------------------------
procedure TCallbackPolygonFiller.BeginRendering;
begin
  inherited;

  if (not Assigned(FFillLineEvent)) then
    raise Exception.Create('Missing polygon filler delegate');
end;

function TCallbackPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FFillLineEvent;
end;


//------------------------------------------------------------------------------
//
//      TInvertPolygonFiller
//
//------------------------------------------------------------------------------
procedure TInvertPolygonFiller.FillLineBlend(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(InvertColor(Dst^), Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

function TInvertPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FillLineBlend;
end;


//------------------------------------------------------------------------------
//
//      TClearPolygonFiller
//
//------------------------------------------------------------------------------
constructor TClearPolygonFiller.Create(Color: TColor32);
begin
  inherited Create;
  FColor := Color;
end;

procedure TClearPolygonFiller.FillLineClear(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
begin
  FillLongword(Dst^, Length, FColor);
end;

function TClearPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FillLineClear;
end;


//------------------------------------------------------------------------------
//
//      TBitmapPolygonFiller
//
//------------------------------------------------------------------------------
procedure TBitmapPolygonFiller.FillLineOpaque(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  OpaqueAlpha: TColor32;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then
    PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then
    PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if (AlphaValues <> nil) then
  begin
    OpaqueAlpha := TColor32($FF shl 24);
    BlendMemEx := BLEND_MEM_EX[FPattern.CombineMode]^;
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^ and $00FFFFFF or OpaqueAlpha, Dst^, AlphaValues^);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end;
  end
  else
    for X := DstX to DstX + Length - 1 do
    begin
      Dst^ := Src^;
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
end;

//------------------------------------------------------------------------------

procedure TBitmapPolygonFiller.BeginRendering;
begin
  inherited;

  if (FPattern = nil) or (FPattern.DrawMode = dmTransparent) or
    ((FPattern.DrawMode = dmCustom) and (not Assigned(FPattern.OnPixelCombine))) then
    raise Exception.Create('Missing or invalid polygon filler pattern');
end;

//------------------------------------------------------------------------------

procedure TBitmapPolygonFiller.FillLineBlend(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
  BlendMem: TBlendMem;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then
    PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then
    PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if (AlphaValues <> nil) then
  begin
    BlendMemEx := BLEND_MEM_EX[FPattern.CombineMode]^;
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^, Dst^, AlphaValues^);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end;
  end
  else
  begin
    BlendMem := BLEND_MEM[FPattern.CombineMode]^;
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMem(Src^, Dst^);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitmapPolygonFiller.FillLineBlendMasterAlpha(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then
    PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then
    PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  BlendMemEx := BLEND_MEM_EX[FPattern.CombineMode]^;

  if (AlphaValues <> nil) then
  begin
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^, Dst^, Div255(AlphaValues^ * FPattern.MasterAlpha));
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end;
  end else
  begin
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^, Dst^, FPattern.MasterAlpha);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitmapPolygonFiller.FillLineCustomCombine(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then
    PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then
    PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if (AlphaValues <> nil) then
  begin
    for X := DstX to DstX + Length - 1 do
    begin
      FPattern.OnPixelCombine(Src^, Dst^, Div255(AlphaValues^ * FPattern.MasterAlpha));
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end;
  end else
  begin
    for X := DstX to DstX + Length - 1 do
    begin
      FPattern.OnPixelCombine(Src^, Dst^, FPattern.MasterAlpha);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TBitmapPolygonFiller.GetFillLine: TFillLineEvent;
begin
  if (FPattern = nil) then
    Result := nil
  else
  if FPattern.DrawMode = dmOpaque then
    Result := FillLineOpaque
  else
  if FPattern.DrawMode = dmBlend then
  begin
    if FPattern.MasterAlpha = 255 then
      Result := FillLineBlend
    else
      Result := FillLineBlendMasterAlpha;
  end else
  if (FPattern.DrawMode = dmCustom) and Assigned(FPattern.OnPixelCombine) then
    Result := FillLineCustomCombine
  else
    Result := nil;
end;


//------------------------------------------------------------------------------
//
//      TSamplerFiller
//
//------------------------------------------------------------------------------
constructor TSamplerFiller.Create(Sampler: TCustomSampler = nil);
begin
  inherited Create;
  FSampler := Sampler;
  SamplerChanged;
end;

procedure TSamplerFiller.EndRendering;
begin
  if (FSampler = nil) then
    raise Exception.Create(RCStrNoSamplerSpecified);
  FSampler.FinalizeSampling;
  inherited;
end;

procedure TSamplerFiller.SampleLineOpaque(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(FGetSample(X, DstY) and $00FFFFFF or $FF000000, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TSamplerFiller.SamplerChanged;
begin
  if (FSampler <> nil) then
    FGetSample := FSampler.GetSampleInt;
end;

procedure TSamplerFiller.BeginRendering;
begin
  inherited;
  if (FSampler = nil) then
    raise Exception.Create(RCStrNoSamplerSpecified);
  FSampler.PrepareSampling;
end;

function TSamplerFiller.GetFillLine: TFillLineEvent;
begin
  Result := SampleLineOpaque;
end;

procedure TSamplerFiller.SetSampler(const Value: TCustomSampler);
begin
  if FSampler <> Value then
  begin
    FSampler := Value;
    SamplerChanged;
  end;
end;


//------------------------------------------------------------------------------
//
//      TCustomPolygonRenderer
//
//------------------------------------------------------------------------------
procedure TCustomPolygonRenderer.PolygonFS(
  const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect;
  Transformation: TTransformation);
begin
  PolyPolygonFS(PolyPolygon(Points), ClipRect, Transformation);
end;

procedure TCustomPolygonRenderer.PolygonFS(
  const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect);
begin
  PolyPolygonFS(PolyPolygon(Points), ClipRect);
end;

procedure TCustomPolygonRenderer.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect;
  Transformation: TTransformation);
var
  APoints: TArrayOfArrayOfFloatPoint;
begin
  if (Transformation <> nil) then
    APoints := TransformPolyPolygon(Points, Transformation)
  else
    APoints := Points;
  PolyPolygonFS(APoints, ClipRect);
end;


//------------------------------------------------------------------------------
//
//      TPolygonRenderer32
//
//------------------------------------------------------------------------------
constructor TPolygonRenderer32.Create(Bitmap: TCustomBitmap32; Fillmode: TPolyFillMode);
begin
  inherited Create;
  SetBitmap(Bitmap);
  SetFillMode(Fillmode);
end;

procedure TPolygonRenderer32.PolygonFS(const Points: TArrayOfFloatPoint);
begin
  PolyPolygonFS(PolyPolygon(Points), FloatRect(FBitmap.ClipRect));
end;

procedure TPolygonRenderer32.PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint);
begin
  PolyPolygonFS(Points, FloatRect(FBitmap.ClipRect));
end;

procedure TPolygonRenderer32.SetBitmap(const Value: TCustomBitmap32);
begin
  if FBitmap <> Value then
  begin
    FBitmap := Value;
    Changed;
  end;
end;

procedure TPolygonRenderer32.SetColor(const Value: TColor32);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TPolygonRenderer32.SetFiller(const Value: TCustomPolygonFiller);
begin
  if FFiller <> Value then
  begin
    FFiller := Value;
    Changed;
  end;
end;

procedure TPolygonRenderer32.SetFillMode(const Value: TPolyFillMode);
begin
  if FFillMode <> Value then
  begin
    FFillMode := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
//
//      TPolygonRenderer32VPR
//
//------------------------------------------------------------------------------
{$IFDEF USESTACKALLOC}
{$W+}
{$ENDIF}
procedure TPolygonRenderer32VPR.FillSpan(const Span: TValueSpan; DstY: Integer);
var
  AlphaValues: PColor32Array;
  Count: Integer;
begin
  Count := Span.HighX - Span.LowX + 1;
{$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc(Count * SizeOf(TColor32));
{$ELSE}
  GetMem(AlphaValues, Count * SizeOf(TColor32));
{$ENDIF}
  FFillProc(Span.Values, AlphaValues, Count, FColor);
  FFiller.FillLine(@Bitmap.ScanLine[DstY][Span.LowX], Span.LowX, DstY, Count, PColor32(AlphaValues), Bitmap.CombineMode);
{$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
{$ELSE}
  FreeMem(AlphaValues);
{$ENDIF}
end;
{$IFDEF USESTACKALLOC}
{$W-}
{$ENDIF}

//------------------------------------------------------------------------------

function TPolygonRenderer32VPR.GetRenderSpan: TRenderSpanEvent;
begin
  if (FFiller <> nil) then
    Result := FillSpan
  else
    Result := RenderSpan;
end;

//------------------------------------------------------------------------------

procedure TPolygonRenderer32VPR.PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
{$IFDEF CHANGENOTIFICATIONS}
var
  i: Integer;
  ChangeRect: TRect;
{$ENDIF}
begin
  if (not Bitmap.MeasuringMode) then
  begin

    UpdateFillProc;

    if (FFiller <> nil) then
    begin
      FFiller.BeginRendering;
      RenderPolyPolygon(Points, ClipRect, GetRenderSpan());
      FFiller.EndRendering;
    end else
      RenderPolyPolygon(Points, ClipRect, GetRenderSpan());

  end;

{$IFDEF CHANGENOTIFICATIONS}
  if (TBitmap32Access(Bitmap).LockUpdateCount = 0) and
    ((Bitmap.MeasuringMode) or (TBitmap32Access(Bitmap).UpdateCount = 0)) then
  begin
    for i := 0 to High(Points) do
      if (Length(Points[i]) > 0) then
      begin
        if (GR32.IntersectRect(ChangeRect, MakeRect(ClipRect, rrOutside), MakeRect(PolygonBounds(Points[i])))) then
          Bitmap.Changed(ChangeRect);
      end;
  end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$W+}
procedure TPolygonRenderer32VPR.RenderSpan(const Span: TValueSpan; DstY: Integer);
var
  AlphaValues: PColor32Array;
  Count: Integer;
begin
  Count := Span.HighX - Span.LowX + 1;
{$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc(Count * SizeOf(TColor32));
{$ELSE}
  GetMem(AlphaValues, Count * SizeOf(TColor32));
{$ENDIF}
  FFillProc(Span.Values, AlphaValues, Count, FColor);
  if Bitmap.CombineMode = cmMerge then
    MergeLine(@AlphaValues[0], @Bitmap.ScanLine[DstY][Span.LowX], Count)
  else
    BlendLine(@AlphaValues[0], @Bitmap.ScanLine[DstY][Span.LowX], Count);
{$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
{$ELSE}
  FreeMem(AlphaValues);
{$ENDIF}
end;
{$W-}

//------------------------------------------------------------------------------

procedure TPolygonRenderer32VPR.GetFillProc(var AFillProc: TFillProc);
type
  PFillProc = ^TFillProc;
const
  FillProcs: array [Boolean, TPolyFillMode] of PFillProc = (
    (@@MakeAlphaEvenOddUP, @@MakeAlphaNonZeroUP),
    (@@MakeAlphaEvenOddUPF, @@MakeAlphaNonZeroUPF)
  );
begin
  AFillProc := FillProcs[(FFiller <> nil), FillMode]^;
end;

//------------------------------------------------------------------------------

procedure TPolygonRenderer32VPR.UpdateFillProc;
begin
  GetFillProc(FFillProc);
end;


//------------------------------------------------------------------------------
//
//      TPolygonRenderer32LCD
//
//------------------------------------------------------------------------------
procedure TPolygonRenderer32LCD.PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
var
  R: TFloatRect;
  APoints: TArrayOfArrayOfFloatPoint;
{$IFDEF CHANGENOTIFICATIONS}
  i: Integer;
  ChangeRect: TRect;
{$ENDIF}
begin
  if (not Bitmap.MeasuringMode) then
  begin
    APoints := ScalePolyPolygon(Points, 3, 1);

    R.Top := ClipRect.Top;
    R.Bottom := ClipRect.Bottom;
    R.Left := ClipRect.Left * 3;
    R.Right := ClipRect.Right * 3;

    RenderPolyPolygon(APoints, R, RenderSpan);
  end;

{$IFDEF CHANGENOTIFICATIONS}
  if (TBitmap32Access(Bitmap).LockUpdateCount = 0) and
    ((Bitmap.MeasuringMode) or (TBitmap32Access(Bitmap).UpdateCount = 0)) then
  begin
    for i := 0 to High(Points) do
      if (Length(Points[i]) > 0) then
      begin
        if (GR32.IntersectRect(ChangeRect, MakeRect(ClipRect, rrOutside), MakeRect(PolygonBounds(Points[i])))) then
          Bitmap.Changed(ChangeRect);
      end;
  end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$W+}
procedure TPolygonRenderer32LCD.RenderSpan(const Span: TValueSpan;
  DstY: Integer);
const
  PADDING = 5;
var
  AlphaValues: PByteArray;
  Count: Integer;
  X, Offset: Integer;
const
  MakeAlpha: array [TPolyFillMode] of TMakeAlphaProcLCD = (MakeAlphaEvenOddLCD, MakeAlphaNonZeroLCD);
begin
  Count := Span.HighX - Span.LowX + 1;
  X := DivMod(Span.LowX, 3, Offset);

  // Left Padding + Right Padding + Filter Width = 2 + 2 + 2 = 6
{$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc((Count + 6 + PADDING) * SizeOf(Byte));
{$ELSE}
  GetMem(AlphaValues, (Count + 6 + PADDING) * SizeOf(Byte));
{$ENDIF}
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  if (X > 0) then
  begin
    Dec(X);
    Inc(Offset, 3);
    AlphaValues[2] := 0;
    AlphaValues[3] := 0;
    AlphaValues[4] := 0;
  end;

  MakeAlpha[FFillMode](Span.Values, PByteArray(@AlphaValues[PADDING]), Count, FColor);
  CombineLineLCD(@AlphaValues[PADDING - Offset], PColor32Array(@Bitmap.ScanLine[DstY][X]), FColor, (Count + Offset + 2) div 3);

{$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
{$ELSE}
  FreeMem(AlphaValues);
{$ENDIF}
end;
{$W-}


//------------------------------------------------------------------------------
//
//      TPolygonRenderer32LCD2
//
//------------------------------------------------------------------------------
{$W+}
procedure TPolygonRenderer32LCD2.RenderSpan(const Span: TValueSpan; DstY: Integer);
const
  PADDING = 5;
var
  AlphaValues: PByteArray;
  Count: Integer;
  X, Offset: Integer;
const
  MakeAlpha: array [TPolyFillMode] of TMakeAlphaProcLCD = (MakeAlphaEvenOddLCD2, MakeAlphaNonZeroLCD2);
begin
  Count := Span.HighX - Span.LowX + 1;
  X := DivMod(Span.LowX, 3, Offset);

  // Left Padding + Right Padding + Filter Width = 2 + 2 + 2 = 6
{$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc((Count + 6 + PADDING) * SizeOf(Byte));
{$ELSE}
  GetMem(AlphaValues, (Count + 6 + PADDING) * SizeOf(Byte));
{$ENDIF}
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  if (X > 0) then
  begin
    Dec(X);
    Inc(Offset, 3);
    AlphaValues[2] := 0;
    AlphaValues[3] := 0;
    AlphaValues[4] := 0;
  end;

  Dec(Offset, 1);
  MakeAlpha[FFillMode](Span.Values, PByteArray(@AlphaValues[PADDING]), Count, FColor);
  Inc(Count);
  CombineLineLCD(@AlphaValues[PADDING - Offset], PColor32Array(@Bitmap.ScanLine[DstY][X]), FColor, (Count + Offset + 2) div 3);

{$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
{$ELSE}
  FreeMem(AlphaValues);
{$ENDIF}
end;
{$W-}


//------------------------------------------------------------------------------
//
//      NO_GENERIC_METACLASS_LISTS
//
//------------------------------------------------------------------------------
{$if defined(NO_GENERIC_METACLASS_LISTS)}
function TCustomPolygonRendererList.Find(const AClassName: string): TCustomPolygonRendererClass;
begin
  Result := TCustomPolygonRendererClass(inherited Find(AClassName));
end;

function TPolygonRendererList.Find(const AClassName: string): TPolygonRenderer32Class;
begin
  Result := TPolygonRenderer32Class(inherited Find(AClassName));
end;
{$ifend}


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
procedure RegisterBindings;
begin
  PolygonsRegistry.RegisterBinding(@@MakeAlphaEvenOddUP, 'MakeAlphaEvenOddUP');
  PolygonsRegistry.RegisterBinding(@@MakeAlphaNonZeroUP, 'MakeAlphaNonZeroUP');
  PolygonsRegistry.RegisterBinding(@@MakeAlphaEvenOddUPF, 'MakeAlphaEvenOddUPF');
  PolygonsRegistry.RegisterBinding(@@MakeAlphaNonZeroUPF, 'MakeAlphaNonZeroUPF');
end;

var
  FPolygonsRegistry: TFunctionRegistry = nil;

function PolygonsRegistry: TFunctionRegistry;
begin
  if (FPolygonsRegistry = nil) then
  begin
    FPolygonsRegistry := NewRegistry('GR32_Polygons bindings');
    RegisterBindings;
  end;
  Result := FPolygonsRegistry;
end;


//------------------------------------------------------------------------------
//
//      Function bindings
//
//------------------------------------------------------------------------------
procedure RegisterBindingFunctions;
begin
  // EvenOddUP
  PolygonsRegistry[@@MakeAlphaEvenOddUP].Add(   @MakeAlphaEvenOddUP_Pas,        [isPascal]).Name := 'MakeAlphaEvenOddUP_Pas';
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
  PolygonsRegistry[@@MakeAlphaEvenOddUP].Add(   @MakeAlphaEvenOddUP_SSE2,       [isSSE2]).Name := 'MakeAlphaEvenOddUP_SSE2';
  PolygonsRegistry[@@MakeAlphaEvenOddUP].Add(   @MakeAlphaEvenOddUP_SSE41,      [isSSE2]).Name := 'MakeAlphaEvenOddUP_SSE41';
{$ifend}

  // NonZeroUP
  PolygonsRegistry[@@MakeAlphaNonZeroUP].Add(   @MakeAlphaNonZeroUP_Pas,        [isPascal]).Name := 'MakeAlphaNonZeroUP_Pas';
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
  PolygonsRegistry[@@MakeAlphaNonZeroUP].Add(   @MakeAlphaNonZeroUP_SSE2,       [isSSE2]).Name := 'MakeAlphaNonZeroUP_SSE2';
{$ifend}

  // EvenOddUPF
  PolygonsRegistry[@@MakeAlphaEvenOddUPF].Add(  @MakeAlphaEvenOddUPF_Pas,       [isPascal]).Name := 'MakeAlphaEvenOddUPF_Pas';

  // NonZeroUPF
  PolygonsRegistry[@@MakeAlphaNonZeroUPF].Add(  @MakeAlphaNonZeroUPF_Pas,       [isPascal]).Name := 'MakeAlphaNonZeroUPF_Pas';
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


initialization

  RegisterBindingFunctions;
  PolygonsRegistry.RebindAll;

  RegisterPolygonRenderer(TPolygonRenderer32VPR);
  RegisterPolygonRenderer(TPolygonRenderer32LCD);
  RegisterPolygonRenderer(TPolygonRenderer32LCD2);

finalization

  CustomPolygonRendererList.Free;
  PolygonRendererList.Free;

end.

