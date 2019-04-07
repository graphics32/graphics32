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
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Types, GR32, GR32_Containers, GR32_VPR, GR32_Transforms, GR32_Resamplers;

type
  { Polygon join style - used by GR32_VectorUtils.Grow(). }
  { nb: jsRoundEx rounds both convex and concave joins unlike jsRound which
    only rounds convex joins. The depth of convex join rounding is controlled
    by Grow's MiterLimit parameter }
  TJoinStyle = (jsMiter, jsBevel, jsRound, jsRoundEx);

  { Polygon end style }
  TEndStyle = (esButt, esSquare, esRound);

  { Polygon fill mode }
  TPolyFillMode = (pfAlternate, pfWinding, pfEvenOdd = 0, pfNonZero);

  { TCustomPolygonRenderer }
  TCustomPolygonRenderer = class(TThreadPersistent)
  public
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation); overload; virtual;
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); overload; virtual;
    procedure PolygonFS(const Points: TArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation); overload; virtual;
    procedure PolygonFS(const Points: TArrayOfFloatPoint;
      const ClipRect: TFloatRect); overload; virtual;

    // procedure PolyPolygonXS(const Points: TArrayOfArrayOfFixedPoint; const ClipRect: TFixedRect; Transformation: TTransformation); virtual; overload;
    // procedure PolyPolygonXS(const Points: TArrayOfArrayOfFixedPoint; const ClipRect: TFixedRect); virtual; overload;
  end;
  TCustomPolygonRendererClass = class of TCustomPolygonRenderer;

  TCustomPolygonFiller = class;

  { TPolygonRenderer32 }
  TPolygonRenderer32 = class(TCustomPolygonRenderer)
  private
    FBitmap: TBitmap32;
    FFillMode: TPolyFillMode;
    FColor: TColor32;
    FFiller: TCustomPolygonFiller;
    procedure SetColor(const Value: TColor32);
    procedure SetFillMode(const Value: TPolyFillMode);
    procedure SetFiller(const Value: TCustomPolygonFiller);
  protected
    procedure SetBitmap(const Value: TBitmap32); virtual;
  public
    constructor Create(Bitmap: TBitmap32; Fillmode: TPolyFillMode = pfWinding); reintroduce; overload;
    procedure PolygonFS(const Points: TArrayOfFloatPoint); overload; virtual;
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint); overload; virtual;

    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
    property FillMode: TPolyFillMode read FFillMode write SetFillMode;
    property Color: TColor32 read FColor write SetColor;
    property Filler: TCustomPolygonFiller read FFiller write SetFiller;
  end;
  TPolygonRenderer32Class = class of TPolygonRenderer32;

  { TPolygonRenderer32VPR }
  { Polygon renderer based on VPR. Computes exact coverages for optimal anti-aliasing. }
  TFillProc = procedure(Coverage: PSingleArray; AlphaValues: PColor32Array; Count: Integer; Color: TColor32);

  TPolygonRenderer32VPR = class(TPolygonRenderer32)
  private
    FFillProc: TFillProc;
    procedure UpdateFillProcs;
  protected
    procedure RenderSpan(const Span: TValueSpan; DstY: Integer); virtual;
    procedure FillSpan(const Span: TValueSpan; DstY: Integer); virtual;
    function GetRenderSpan: TRenderSpanEvent; virtual;
  public
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
  end;

  { TPolygonRenderer32LCD }
  TPolygonRenderer32LCD = class(TPolygonRenderer32VPR)
  protected
    procedure RenderSpan(const Span: TValueSpan; DstY: Integer); override;
  public
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
  end;

  { TPolygonRenderer32LCD2 }
  TPolygonRenderer32LCD2 = class(TPolygonRenderer32LCD)
  public
    procedure RenderSpan(const Span: TValueSpan; DstY: Integer); override;
  end;

  { TCustomPolygonFiller }

  TFillLineEvent = procedure(Dst: PColor32; DstX, DstY, Length: Integer;
    AlphaValues: PColor32; CombineMode: TCombineMode) of object;

  TCustomPolygonFiller = class
  protected
    function GetFillLine: TFillLineEvent; virtual; abstract;
  public
    procedure BeginRendering; virtual;
    procedure EndRendering; virtual;

    property FillLine: TFillLineEvent read GetFillLine;
  end;

  { TCallbackPolygonFiller }
  TCallbackPolygonFiller = class(TCustomPolygonFiller)
  private
    FFillLineEvent: TFillLineEvent;
  protected
    function GetFillLine: TFillLineEvent; override;
  public
    property FillLineEvent: TFillLineEvent read FFillLineEvent write FFillLineEvent;
  end;

  { TInvertPolygonFiller }
  TInvertPolygonFiller = class(TCustomPolygonFiller)
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineBlend(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  end;

  { TClearPolygonFiller }
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

  { TBitmapPolygonFiller }
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
    property Pattern: TCustomBitmap32 read FPattern write FPattern;
    property OffsetX: Integer read FOffsetX write FOffsetX;
    property OffsetY: Integer read FOffsetY write FOffsetY;
  end;

  { TSamplerFiller }
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

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;


procedure PolyPolylineFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
procedure PolyPolylineFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;

procedure PolylineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
procedure PolylineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;

//Filled only Dashes ...
procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Color: TColor32;
  Closed: Boolean = False; Width: TFloat = 1.0); overload;
procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;
//Filled and stroked Dashes ...
procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFloat = 1.0); overload;
procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;

procedure PolyPolygonXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonXS_LCD(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonXS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil);
procedure PolyPolygonXS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonXS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil);

procedure PolyPolylineXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;
procedure PolyPolylineXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;

procedure PolylineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;
procedure PolylineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFixed = $10000;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;

//Filled only Dashes ...
procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Color: TColor32;
  Closed: Boolean = False; Width: TFixed = $10000); overload;
procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000); overload;
//Filled and stroked Dashes ...
procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFixed = $10000); overload;
procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000); overload;

// fill entire bitmap with a given polygon filler
procedure FillBitmap(Bitmap: TBitmap32; Filler: TCustomPolygonFiller);

{ Registration routines }
procedure RegisterPolygonRenderer(PolygonRendererClass: TCustomPolygonRendererClass);

var
  PolygonRendererList: TClassList;
  DefaultPolygonRendererClass: TPolygonRenderer32Class = TPolygonRenderer32VPR;

implementation

uses
  Math, SysUtils, GR32_Math, GR32_LowLevel, GR32_Blend, GR32_Gamma,
  GR32_VectorUtils;

resourcestring
  RCStrNoSamplerSpecified = 'No sampler specified!';

type
  TBitmap32Access = class(TBitmap32);

procedure RegisterPolygonRenderer(PolygonRendererClass: TCustomPolygonRendererClass);
begin
  if not Assigned(PolygonRendererList) then PolygonRendererList := TClassList.Create;
  PolygonRendererList.Add(PolygonRendererClass);
end;

// routines for color filling:

procedure MakeAlphaNonZeroUP(Coverage: PSingleArray; AlphaValues: PColor32Array;
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
      V := Abs(Round(Last * $10000));
      if V > $10000 then V := $10000;
      V := V * M shr 24;
{$IFDEF USEGR32GAMMA}
      V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
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
{$IFDEF USEGR32GAMMA}
    V := GAMMA_ENCODING_TABLE[V * M shr 24];
    AlphaValues[I] := (V shl 24) or C;
{$ELSE}
    AlphaValues[I] := (V * M and $ff000000) or C;
{$ENDIF}
  end;
end;
*)

procedure MakeAlphaEvenOddUP(Coverage: PSingleArray; AlphaValues: PColor32Array;
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
{$IFDEF USEGR32GAMMA}
      V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
      C.A := V;
    end;
    AlphaValues[I] := Color;
  end;
end;

procedure MakeAlphaNonZeroP(Value: Single; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  M, V: Cardinal;
  C: TColor32Entry absolute Color;
begin
  M := C.A * $101;
  V := Abs(Round(Value * $10000));
  if V > $10000 then V := $10000;
  V := V * M shr 24;
{$IFDEF USEGR32GAMMA}
  V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
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
  if V > $10000 then V := V xor $1ffff;
  V := V * M shr 24;
{$IFDEF USEGR32GAMMA}
  V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
  C.A := V;
  FillLongWord(AlphaValues[0], Count, Color);
end;


// polygon filler routines (extract alpha only):

procedure MakeAlphaNonZeroUPF(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  V: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    V := Clamp(Round(Abs(Coverage[I]) * 256));
{$IFDEF USEGR32GAMMA}
    V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
    AlphaValues[I] := V;
  end;
end;

procedure MakeAlphaEvenOddUPF(Coverage: PSingleArray; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  I: Integer;
  V: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    V := Round(Abs(Coverage[I]) * 256);
    V := V and $000001ff;
    if V >= $100 then V := V xor $1ff;
{$IFDEF USEGR32GAMMA}
    V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
    AlphaValues[I] := V;
  end;
end;

procedure MakeAlphaNonZeroPF(Value: Single; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  V: Integer;
begin
  V := Clamp(Round(Abs(Value) * 256));
{$IFDEF USEGR32GAMMA}
    V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
  FillLongWord(AlphaValues[0], Count, V);
end;

procedure MakeAlphaEvenOddPF(Value: Single; AlphaValues: PColor32Array;
  Count: Integer; Color: TColor32);
var
  V: Integer;
begin
  V := Round(Abs(Value) * 256);
  V := V and $000001ff;
  if V >= $100 then V := V xor $1ff;
{$IFDEF USEGR32GAMMA}
    V := GAMMA_ENCODING_TABLE[V];
{$ENDIF}
  FillLongWord(AlphaValues[0], Count, V);
end;

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
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

procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
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

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  if not Assigned(Filler) then Exit;
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

procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
begin
  if not Assigned(Filler) then Exit;
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

procedure PolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
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

procedure PolyPolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
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

procedure PolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
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

procedure PolyPolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
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

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
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

procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
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

procedure PolyPolygonFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
  IntersectedClipRect: TRect;
begin
  if not Assigned(Filler) then Exit;
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

procedure PolygonFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32VPR;
  IntersectedClipRect: TRect;
begin
  if not Assigned(Filler) then Exit;
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

procedure PolygonFS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
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

procedure PolyPolygonFS_LCD(Bitmap: TBitmap32;
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

procedure PolygonFS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
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

procedure PolyPolygonFS_LCD2(Bitmap: TBitmap32;
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

procedure PolyPolylineFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFloat; Transformation: TTransformation);
var
  Dst: TArrayOfArrayOfFloatPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  PolyPolygonFS(Bitmap, Dst, Color, pfWinding, Transformation);
end;

procedure PolyPolylineFS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil);
var
  Dst: TArrayOfArrayOfFloatPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  PolyPolygonFS(Bitmap, Dst, Filler, pfWinding, Transformation);
end;

procedure PolylineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFloat; Transformation: TTransformation);
begin
  PolyPolylineFS(Bitmap, PolyPolygon(Points), Color, Closed, StrokeWidth,
    JoinStyle, EndStyle, MiterLimit, Transformation);
end;

procedure PolylineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil);
begin
  PolyPolylineFS(Bitmap, PolyPolygon(Points), Filler, Closed, StrokeWidth,
    JoinStyle, EndStyle, MiterLimit, Transformation);
end;

procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Color: TColor32;
  Closed: Boolean = False; Width: TFloat = 1.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineFS(Bitmap, MultiPoly, Color, False, Width);
end;

procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolygonFS(Bitmap, MultiPoly, FillColor);
  PolyPolylineFS(Bitmap, MultiPoly, StrokeColor, True, StrokeWidth);
end;

procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFloat = 1.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineFS(Bitmap, MultiPoly, Filler, False, Width);
end;

procedure DashLineFS(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolygonFS(Bitmap, MultiPoly, Filler);
  PolyPolylineFS(Bitmap, MultiPoly, StrokeColor, True, StrokeWidth);
end;


procedure PolyPolygonXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
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

procedure PolygonXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
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

procedure PolyPolygonXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
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

procedure PolygonXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
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

procedure PolygonXS_LCD(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
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

procedure PolyPolygonXS_LCD(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
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

procedure PolygonXS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
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

procedure PolyPolygonXS_LCD2(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
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

procedure PolyPolylineXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFixed;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFixed; Transformation: TTransformation);
var
  Dst: TArrayOfArrayOfFixedPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle,
    MiterLimit);
  PolyPolygonXS(Bitmap, Dst, Color, pfWinding, Transformation);
end;

procedure PolyPolylineXS(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False;
  StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter;
  EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000;
  Transformation: TTransformation = nil);
var
  Dst: TArrayOfArrayOfFixedPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle,
    MiterLimit);
  PolyPolygonXS(Bitmap, Dst, Filler, pfWinding, Transformation);
end;

procedure PolylineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFixed;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFixed; Transformation: TTransformation);
begin
  PolyPolylineXS(Bitmap, PolyPolygon(Points), Color,
    Closed, StrokeWidth, JoinStyle, EndStyle,
    MiterLimit, Transformation);
end;

procedure PolylineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False;
  StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter;
  EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000;
  Transformation: TTransformation = nil);
begin
  PolyPolylineXS(Bitmap, PolyPolygon(Points), Filler, Closed, StrokeWidth,
    JoinStyle, EndStyle, MiterLimit, Transformation);
end;

procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Color: TColor32;
  Closed: Boolean = False; Width: TFixed = $10000);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, Color, False, Width);
end;

procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, FillColor, False, Width);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolylineXS(Bitmap, MultiPoly, StrokeColor, True, strokeWidth);
end;

procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFixed = $10000);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, Filler, False, Width);
end;

procedure DashLineXS(Bitmap: TBitmap32; const Points: TArrayOfFixedPoint;
  const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000);
var
  MultiPoly: TArrayOfArrayOfFixedPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineXS(Bitmap, MultiPoly, Filler, False, Width);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolylineXS(Bitmap, MultiPoly, StrokeColor, True, StrokeWidth);
end;

procedure FillBitmap(Bitmap: TBitmap32; Filler: TCustomPolygonFiller);
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


{ LCD sub-pixel rendering (see http://www.grc.com/cttech.htm) }

type
  PRGBTriple = ^TRGBTriple;
  TRGBTriple = packed record
    B, G, R: Byte;
  end;

  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0..0] of TRGBTriple;

  TMakeAlphaProcLCD = procedure(Coverage: PSingleArray; AlphaValues: SysUtils.PByteArray;
    Count: Integer; Color: TColor32);

procedure MakeAlphaNonZeroLCD(Coverage: PSingleArray; AlphaValues: SysUtils.PByteArray;
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
      if V > $10000 then V := $10000;
      V := V * M shr 24;
    end;
    Inc(AlphaValues[I], V);
{$IFDEF USEGR32GAMMA}
    AlphaValues[I] := GAMMA_ENCODING_TABLE[AlphaValues[I]];
{$ENDIF}
    Inc(AlphaValues[I + 1], V);
    AlphaValues[I + 2] := V;
  end;
  AlphaValues[Count + 2] := 0;
  AlphaValues[Count + 3] := 0;
end;

procedure MakeAlphaEvenOddLCD(Coverage: PSingleArray; AlphaValues: SysUtils.PByteArray;
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
      if V >= $10000 then V := V xor $1ffff;
      V := V * M shr 24;
    end;
    Inc(AlphaValues[I], V);
{$IFDEF USEGR32GAMMA}
    AlphaValues[I] := GAMMA_ENCODING_TABLE[AlphaValues[I]];
{$ENDIF}
    Inc(AlphaValues[I + 1], V);
    AlphaValues[I + 2] := V;
  end;
  AlphaValues[Count + 2] := 0;
  AlphaValues[Count + 3] := 0;
end;

procedure MakeAlphaNonZeroLCD2(Coverage: PSingleArray; AlphaValues: SysUtils.PByteArray;
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

procedure MakeAlphaEvenOddLCD2(Coverage: PSingleArray; AlphaValues: SysUtils.PByteArray;
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
  EMMS;
end;

{ TCustomPolygonFiller }

procedure TCustomPolygonFiller.BeginRendering;
begin
  // implemented by descendants
end;

procedure TCustomPolygonFiller.EndRendering;
begin
  // implemented by descendants
end;

{ TCallbackPolygonFiller }

function TCallbackPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FFillLineEvent;
end;


{ TInvertPolygonFiller }

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
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

function TInvertPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FillLineBlend;
end;


{ TClearPolygonFiller }

constructor TClearPolygonFiller.Create(Color: TColor32 = $00808080);
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


{ TBitmapPolygonFiller }

procedure TBitmapPolygonFiller.FillLineOpaque(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  OpaqueAlpha: TColor32;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if Assigned(AlphaValues) then
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
    end
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

procedure TBitmapPolygonFiller.FillLineBlend(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
  BlendMem: TBlendMem;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if Assigned(AlphaValues) then
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
    end
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

procedure TBitmapPolygonFiller.FillLineBlendMasterAlpha(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  BlendMemEx := BLEND_MEM_EX[FPattern.CombineMode]^;

  if Assigned(AlphaValues) then
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
    end
  else
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

procedure TBitmapPolygonFiller.FillLineCustomCombine(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if Assigned(AlphaValues) then
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
    end
  else
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

function TBitmapPolygonFiller.GetFillLine: TFillLineEvent;
begin
  if not Assigned(FPattern) then
  begin
    Result := nil;
  end
  else if FPattern.DrawMode = dmOpaque then
    Result := FillLineOpaque
  else if FPattern.DrawMode = dmBlend then
  begin
    if FPattern.MasterAlpha = 255 then
      Result := FillLineBlend
    else
      Result := FillLineBlendMasterAlpha;
  end
  else if (FPattern.DrawMode = dmCustom) and Assigned(FPattern.OnPixelCombine) then
  begin
    Result := FillLineCustomCombine;
  end
  else
    Result := nil;
end;

{ TSamplerFiller }

constructor TSamplerFiller.Create(Sampler: TCustomSampler = nil);
begin
  inherited Create;
  FSampler := Sampler;
  SamplerChanged;
end;

procedure TSamplerFiller.EndRendering;
begin
  if Assigned(FSampler) then
    FSampler.FinalizeSampling
  else
    raise Exception.Create(RCStrNoSamplerSpecified);
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
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TSamplerFiller.SamplerChanged;
begin
  if Assigned(FSampler) then
    FGetSample := FSampler.GetSampleInt;
end;

procedure TSamplerFiller.BeginRendering;
begin
  if Assigned(FSampler) then
    FSampler.PrepareSampling
  else
    raise Exception.Create(RCStrNoSamplerSpecified);
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


{ TCustomPolygonRenderer }

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
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
begin
  // implemented by descendants
end;

procedure TCustomPolygonRenderer.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect;
  Transformation: TTransformation);
var
  APoints: TArrayOfArrayOfFloatPoint;
begin
  if Assigned(Transformation) then
    APoints := TransformPolyPolygon(Points, Transformation)
  else
    APoints := Points;
  PolyPolygonFS(APoints, ClipRect);
end;

{ TPolygonRenderer32 }

constructor TPolygonRenderer32.Create(Bitmap: TBitmap32;
  Fillmode: TPolyFillMode);
begin
  inherited Create;
  FBitmap := Bitmap;
  FFillMode := Fillmode;
end;

procedure TPolygonRenderer32.PolygonFS(const Points: TArrayOfFloatPoint);
begin
  PolyPolygonFS(PolyPolygon(Points), FloatRect(FBitmap.ClipRect));
end;

procedure TPolygonRenderer32.PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint);
begin
  PolyPolygonFS(Points, FloatRect(FBitmap.ClipRect));
end;

procedure TPolygonRenderer32.SetBitmap(const Value: TBitmap32);
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

{ TPolygonRenderer32VPR }

{$IFDEF USESTACKALLOC}
{$W+}
{$ENDIF}
procedure TPolygonRenderer32VPR.FillSpan(const Span: TValueSpan; DstY: Integer);
var
  AlphaValues: PColor32Array;
  Count: Integer;
begin
  Count := Span.X2 - Span.X1 + 1;
  {$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc(Count * SizeOf(TColor32));
  {$ELSE}
  GetMem(AlphaValues, Count * SizeOf(TColor32));
  {$ENDIF}
  FFillProc(Span.Values, AlphaValues, Count, FColor);
  FFiller.FillLine(@Bitmap.ScanLine[DstY][Span.X1], Span.X1, DstY, Count,
    PColor32(AlphaValues), Bitmap.CombineMode);
  EMMS;
  {$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
  {$ELSE}
  FreeMem(AlphaValues);
  {$ENDIF}
end;
{$IFDEF USESTACKALLOC}
{$W-}
{$ENDIF}

function TPolygonRenderer32VPR.GetRenderSpan: TRenderSpanEvent;
begin
  if Assigned(FFiller) then
    Result := FillSpan
  else
    Result := RenderSpan;
end;

procedure TPolygonRenderer32VPR.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
{$IFDEF CHANGENOTIFICATIONS}
var
  I: Integer;
{$ENDIF}
begin
  UpdateFillProcs;
  if Assigned(FFiller) then
  begin
    FFiller.BeginRendering;
    RenderPolyPolygon(Points, ClipRect, GetRenderSpan());
    FFiller.EndRendering;
  end
  else
    RenderPolyPolygon(Points, ClipRect, GetRenderSpan());

{$IFDEF CHANGENOTIFICATIONS}
  if TBitmap32Access(Bitmap).UpdateCount = 0 then
    for I := 0 to High(Points) do
      if Length(Points[I]) > 0 then
        Bitmap.Changed(MakeRect(PolygonBounds(Points[I])));
{$ENDIF}
end;

{$W+}
procedure TPolygonRenderer32VPR.RenderSpan(const Span: TValueSpan;
  DstY: Integer);
var
  AlphaValues: PColor32Array;
  Count: Integer;
begin
  Count := Span.X2 - Span.X1 + 1;
  {$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc(Count * SizeOf(TColor32));
  {$ELSE}
  GetMem(AlphaValues, Count * SizeOf(TColor32));
  {$ENDIF}
  FFillProc(Span.Values, AlphaValues, Count, FColor);
  if Bitmap.CombineMode = cmMerge then
    MergeLine(@AlphaValues[0], @Bitmap.ScanLine[DstY][Span.X1], Count)
  else
    BlendLine(@AlphaValues[0], @Bitmap.ScanLine[DstY][Span.X1], Count);
  EMMS;
  {$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
  {$ELSE}
  FreeMem(AlphaValues);
  {$ENDIF}
end;
{$W-}

procedure TPolygonRenderer32VPR.UpdateFillProcs;
const
  FillProcs: array [Boolean, TPolyFillMode] of TFillProc = (
    (MakeAlphaEvenOddUP, MakeAlphaNonZeroUP),
    (MakeAlphaEvenOddUPF, MakeAlphaNonZeroUPF)
  );
begin
  FFillProc := FillProcs[Assigned(FFiller), FillMode];
end;

{ TPolygonRenderer32LCD }

procedure TPolygonRenderer32LCD.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
var
  R: TFloatRect;
  APoints: TArrayOfArrayOfFloatPoint;
{$IFDEF CHANGENOTIFICATIONS}
  I: Integer;
{$ENDIF}
begin
  APoints := ScalePolyPolygon(Points, 3, 1);
  R.Top := ClipRect.Top;
  R.Bottom := ClipRect.Bottom;
  R.Left := ClipRect.Left * 3;
  R.Right := ClipRect.Right * 3;
  RenderPolyPolygon(APoints, R, RenderSpan);
{$IFDEF CHANGENOTIFICATIONS}
  if TBitmap32Access(Bitmap).UpdateCount = 0 then
    for I := 0 to High(Points) do
      if length(Points[I]) > 0 then
        Bitmap.Changed(MakeRect(PolygonBounds(Points[I])));
{$ENDIF}
end;

{$W+}
procedure TPolygonRenderer32LCD.RenderSpan(const Span: TValueSpan;
  DstY: Integer);
const
  PADDING = 5;
var
  AlphaValues: SysUtils.PByteArray;
  Count: Integer;
  X1, Offset: Integer;
const
  MakeAlpha: array [TPolyFillMode] of TMakeAlphaProcLCD = (MakeAlphaEvenOddLCD, MakeAlphaNonZeroLCD);
begin
  Count := Span.X2 - Span.X1 + 1;
  X1 := DivMod(Span.X1, 3, Offset);

  // Left Padding + Right Padding + Filter Width = 2 + 2 + 2 = 6
  {$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc((Count + 6 + PADDING) * SizeOf(Byte));
  {$ELSE}
  GetMem(AlphaValues, (Count + 6 + PADDING) * SizeOf(Byte));
  {$ENDIF}
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  if (X1 > 0) then
  begin
    Dec(X1);
    Inc(Offset, 3);
    AlphaValues[2] := 0;
    AlphaValues[3] := 0;
    AlphaValues[4] := 0;
  end;

  MakeAlpha[FFillMode](Span.Values, PByteArray(@AlphaValues[PADDING]), Count, FColor);
  CombineLineLCD(@AlphaValues[PADDING - Offset], PColor32Array(@Bitmap.ScanLine[DstY][X1]), FColor, (Count + Offset + 2) div 3);

  {$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
  {$ELSE}
  FreeMem(AlphaValues);
  {$ENDIF}
end;
{$W-}


{ TPolygonRenderer32LCD2 }

{$W+}
procedure TPolygonRenderer32LCD2.RenderSpan(const Span: TValueSpan;
  DstY: Integer);
const
  PADDING = 5;
var
  AlphaValues: SysUtils.PByteArray;
  Count: Integer;
  X1, Offset: Integer;
const
  MakeAlpha: array [TPolyFillMode] of TMakeAlphaProcLCD = (MakeAlphaEvenOddLCD2, MakeAlphaNonZeroLCD2);
begin
  Count := Span.X2 - Span.X1 + 1;
  X1 := DivMod(Span.X1, 3, Offset);

  // Left Padding + Right Padding + Filter Width = 2 + 2 + 2 = 6
  {$IFDEF USESTACKALLOC}
  AlphaValues := StackAlloc((Count + 6 + PADDING) * SizeOf(Byte));
  {$ELSE}
  GetMem(AlphaValues, (Count + 6 + PADDING) * SizeOf(Byte));
  {$ENDIF}
  AlphaValues[0] := 0;
  AlphaValues[1] := 0;
  if (X1 > 0) then
  begin
    Dec(X1);
    Inc(Offset, 3);
    AlphaValues[2] := 0;
    AlphaValues[3] := 0;
    AlphaValues[4] := 0;
  end;

  Dec(Offset, 1);
  MakeAlpha[FFillMode](Span.Values, PByteArray(@AlphaValues[PADDING]), Count, FColor);
  Inc(Count);
  CombineLineLCD(@AlphaValues[PADDING - Offset], PColor32Array(@Bitmap.ScanLine[DstY][X1]), FColor, (Count + Offset + 2) div 3);

  {$IFDEF USESTACKALLOC}
  StackFree(AlphaValues);
  {$ELSE}
  FreeMem(AlphaValues);
  {$ENDIF}
end;
{$W-}

initialization
  RegisterPolygonRenderer(TPolygonRenderer32VPR);
  RegisterPolygonRenderer(TPolygonRenderer32LCD);
  RegisterPolygonRenderer(TPolygonRenderer32LCD2);

finalization
  PolygonRendererList.Free;

end.
