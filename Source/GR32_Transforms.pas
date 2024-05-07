unit GR32_Transforms;

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
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF DEBUG}
  Windows, // In interface section so we don't override TFixed
{$ENDIF}
  SysUtils,
  Classes,
  Types,
  GR32,
  GR32_VectorMaps,
  GR32_Rasterizers;

type
  ETransformError = class(Exception);
  ETransformNotImplemented = class(Exception);

type
  TFloatMatrix = array [0..2, 0..2] of TFloat;     // 3x3 TFloat precision
  TFixedMatrix = array [0..2, 0..2] of TFixed;     // 3x3 fixed precision

const
  IdentityMatrix: TFloatMatrix = (
    (1, 0, 0),
    (0, 1, 0),
    (0, 0, 1));

type
  TVector3f = array [0..2] of TFloat;
  TVector3i = array [0..2] of Integer;

// Matrix conversion routines
function FixedMatrix(const FloatMatrix: TFloatMatrix): TFixedMatrix; overload;
function FloatMatrix(const FixedMatrix: TFixedMatrix): TFloatMatrix; overload;

procedure Adjoint(var M: TFloatMatrix);
function Determinant(const M: TFloatMatrix): TFloat;
procedure Scale(var M: TFloatMatrix; Factor: TFloat);
procedure Invert(var M: TFloatMatrix);
function Mult(const M1, M2: TFloatMatrix): TFloatMatrix;
function VectorTransform(const M: TFloatMatrix; const V: TVector3f): TVector3f;


//------------------------------------------------------------------------------
//
//      TTransformation
//
//------------------------------------------------------------------------------
type
  TTransformation = class(TNotifiablePersistent)
  private
    FSrcRect: TFloatRect;
    procedure SetSrcRect(const Value: TFloatRect);
  protected
    TransformValid: Boolean;
    procedure PrepareTransform; virtual;
    procedure ReverseTransformInt(DstX, DstY: Integer; out SrcX, SrcY: Integer); virtual;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); virtual;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); virtual;
    procedure TransformInt(SrcX, SrcY: Integer; out DstX, DstY: Integer); virtual;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); virtual;
    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); virtual;
  public
    constructor Create; virtual;
    procedure Changed; override;
    function HasTransformedBounds: Boolean; virtual;
    function GetTransformedBounds: TFloatRect; overload;
    function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; overload; virtual;
    function ReverseTransform(const P: TPoint): TPoint; overload; virtual;
    function ReverseTransform(const P: TFixedPoint): TFixedPoint; overload; virtual;
    function ReverseTransform(const P: TFloatPoint): TFloatPoint; overload; virtual;
    function Transform(const P: TPoint): TPoint; overload; virtual;
    function Transform(const P: TFixedPoint): TFixedPoint; overload; virtual;
    function Transform(const P: TFloatPoint): TFloatPoint; overload; virtual;
    property SrcRect: TFloatRect read FSrcRect write SetSrcRect;
  end;

  TTransformationClass = class of TTransformation;


//------------------------------------------------------------------------------
//
//      TNestedTransformation
//
//------------------------------------------------------------------------------
type
  TNestedTransformation = class(TTransformation)
  private
    FItems: TList;
    FOwner: TPersistent;
    function GetCount: Integer;
    function GetItem(Index: Integer): TTransformation;
    procedure SetItem(Index: Integer; const Value: TTransformation);
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); override;
    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Add(ItemClass: TTransformationClass): TTransformation;
    procedure Clear;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer; ItemClass: TTransformationClass): TTransformation;

    property Owner: TPersistent read FOwner;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TTransformation read GetItem write SetItem; default;
  end;


//------------------------------------------------------------------------------
//
//      T3x3Transformation
//
//------------------------------------------------------------------------------
type
  T3x3Transformation = class(TTransformation)
  protected
    FMatrix, FInverseMatrix: TFloatMatrix;
    FFixedMatrix, FInverseFixedMatrix: TFixedMatrix;
    procedure PrepareTransform; override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); override;
    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); override;
  public
    property Matrix: TFloatMatrix read FMatrix;
  end;


//------------------------------------------------------------------------------
//
//      TAffineTransformation
//
//------------------------------------------------------------------------------
type
  TAffineTransformation = class(T3x3Transformation)
  private
    FStack: ^TFloatMatrix;
    FStackLevel: Integer;
  public
    constructor Create; override;

    function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; override;
    procedure Push;
    procedure Pop;
    procedure Clear; overload;
    procedure Clear(BaseMatrix: TFloatMatrix); overload;
    procedure Rotate(Alpha: TFloat); overload; // degrees
    procedure Rotate(Cx, Cy, Alpha: TFloat); overload; // degrees
    procedure Skew(Fx, Fy: TFloat);
    procedure Scale(Sx, Sy: TFloat); overload;
    procedure Scale(Value: TFloat); overload;
    procedure Translate(Dx, Dy: TFloat);
  end;


//------------------------------------------------------------------------------
//
//      TProjectiveTransformation
//
//------------------------------------------------------------------------------
type
  TProjectiveTransformation = class(T3x3Transformation)
  private
    FQuadX: array [0..3] of TFloat;
    FQuadY: array [0..3] of TFloat;
    procedure SetX(Index: Integer; const Value: TFloat); {$IFDEF UseInlining} inline; {$ENDIF}
    procedure SetY(Index: Integer; const Value: TFloat); {$IFDEF UseInlining} inline; {$ENDIF}
    function GetX(Index: Integer): TFloat; {$IFDEF UseInlining} inline; {$ENDIF}
    function GetY(Index: Integer): TFloat; {$IFDEF UseInlining} inline; {$ENDIF}
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); override;
    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); override;
  public
    function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; override;
    property X[Index: Integer]: TFloat read GetX write SetX;
    property Y[index: Integer]: TFloat read GetX write SetY;
  published
    property X0: TFloat index 0 read GetX write SetX;
    property X1: TFloat index 1 read GetX write SetX;
    property X2: TFloat index 2 read GetX write SetX;
    property X3: TFloat index 3 read GetX write SetX;
    property Y0: TFloat index 0 read GetY write SetY;
    property Y1: TFloat index 1 read GetY write SetY;
    property Y2: TFloat index 2 read GetY write SetY;
    property Y3: TFloat index 3 read GetY write SetY;
  end;


//------------------------------------------------------------------------------
//
//      TProjectiveTransformationEx
//
//------------------------------------------------------------------------------
// Performs projective transformation between two convex quadrilaterals.
//------------------------------------------------------------------------------
// References:
//
// - "Fundamentals of Texture Mapping and Image Warping"
//   Paul S. Heckbert
//   www.cs.cmu.edu/~ph/texfund/texfund.pdf
//
// - "Projective Mappings for ImageWarping"
//   Paul S. Heckbert
//   http://graphics.cs.cmu.edu/courses/15-463/2008_fall/Papers/proj.pdf
//
// - "Geometric Tools for Computer Graphics"
//   David H. Eberly
//   https://www.amazon.com/Geometric-Computer-Graphics-Morgan-Kaufmann/dp/1558605940
//
// - "Perspective Mappings"
//   David H. Eberly
//   https://geometrictools.com/Documentation/PerspectiveMappings.pdf
//
//------------------------------------------------------------------------------
type
  TQuadrilateral = array[0..3] of TPoint;
  TFloatQuadrilateral = array[0..3] of TFloatPoint;

  TProjectiveTransformationEx = class(T3x3Transformation)
  private
    FExtrapolate: boolean;
    FSourceQuad: TFloatQuadrilateral;
    FDestQuad: TFloatQuadrilateral;
    procedure SetSourceQuad(const Value: TFloatQuadrilateral); {$IFDEF UseInlining} inline; {$ENDIF}
    procedure SetDestQuad(const Value: TFloatQuadrilateral); {$IFDEF UseInlining} inline; {$ENDIF}
    procedure SetSource(Index: Integer; const Value: TFloatPoint); {$IFDEF UseInlining} inline; {$ENDIF}
    procedure SetSourceX(Index: Integer; const Value: TFloat); {$IFDEF UseInlining} inline; {$ENDIF}
    procedure SetSourceY(Index: Integer; const Value: TFloat); {$IFDEF UseInlining} inline; {$ENDIF}
    procedure SetDest(Index: Integer; const Value: TFloatPoint); {$IFDEF UseInlining} inline; {$ENDIF}
    procedure SetDestX(Index: Integer; const Value: TFloat); {$IFDEF UseInlining} inline; {$ENDIF}
    procedure SetDestY(Index: Integer; const Value: TFloat); {$IFDEF UseInlining} inline; {$ENDIF}
    function GetSource(Index: Integer): TFloatPoint; {$IFDEF UseInlining} inline; {$ENDIF}
    function GetSourceX(Index: Integer): TFloat; {$IFDEF UseInlining} inline; {$ENDIF}
    function GetSourceY(Index: Integer): TFloat; {$IFDEF UseInlining} inline; {$ENDIF}
    function GetDest(Index: Integer): TFloatPoint; {$IFDEF UseInlining} inline; {$ENDIF}
    function GetDestX(Index: Integer): TFloat; {$IFDEF UseInlining} inline; {$ENDIF}
    function GetDestY(Index: Integer): TFloat; {$IFDEF UseInlining} inline; {$ENDIF}
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); override;
    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); override;
  public
    function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; override;

    property SourceQuad: TFloatQuadrilateral read FSourceQuad write SetSourceQuad;
    property Source[Index: Integer]: TFloatPoint read GetSource write SetSource;
    property SourceX[Index: Integer]: TFloat read GetSourceX write SetSourceX;
    property SourceY[Index: Integer]: TFloat read GetSourceY write SetSourceY;
    property DestQuad: TFloatQuadrilateral read FDestQuad write SetDestQuad;
    property Dest[Index: Integer]: TFloatPoint read GetDest write SetDest;
    property DestX[Index: Integer]: TFloat read GetDestX write SetDestX;
    property DestY[index: Integer]: TFloat read GetDestX write SetDestY;
  published
    // Set Extrapolate=True to have pixels beyond the destination be transformed.
    // This is done by having GetTransformedBounds return the passed source rect,
    // which in turn causes the rasterizer to process all pixels of the source
    // image instead of just the pixels covered by the target quad.
    property Extrapolate: boolean read FExtrapolate write FExtrapolate;

    property DestX0: TFloat index 0 read GetDestX write SetDestX;
    property DestX1: TFloat index 1 read GetDestX write SetDestX;
    property DestX2: TFloat index 2 read GetDestX write SetDestX;
    property DestX3: TFloat index 3 read GetDestX write SetDestX;
    property DestY0: TFloat index 0 read GetDestY write SetDestY;
    property DestY1: TFloat index 1 read GetDestY write SetDestY;
    property DestY2: TFloat index 2 read GetDestY write SetDestY;
    property DestY3: TFloat index 3 read GetDestY write SetDestY;

    property SourceX0: TFloat index 0 read GetSourceX write SetSourceX;
    property SourceX1: TFloat index 1 read GetSourceX write SetSourceX;
    property SourceX2: TFloat index 2 read GetSourceX write SetSourceX;
    property SourceX3: TFloat index 3 read GetSourceX write SetSourceX;
    property SourceY0: TFloat index 0 read GetSourceY write SetSourceY;
    property SourceY1: TFloat index 1 read GetSourceY write SetSourceY;
    property SourceY2: TFloat index 2 read GetSourceY write SetSourceY;
    property SourceY3: TFloat index 3 read GetSourceY write SetSourceY;
  end;


//------------------------------------------------------------------------------
//
//      TTwirlTransformation
//
//------------------------------------------------------------------------------
type
  TTwirlTransformation = class(TTransformation)
  private
    Frx, Fry: TFloat;
    FTwirl: TFloat;
    procedure SetTwirl(const Value: TFloat);
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
  public
    constructor Create; override;
    function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; override;
  published
    property Twirl: TFloat read FTwirl write SetTwirl;
  end;


//------------------------------------------------------------------------------
//
//      TBloatTransformation
//
//------------------------------------------------------------------------------
type
  TBloatTransformation = class(TTransformation)
  private
    FBloatPower: TFloat;
    FBP: TFloat;
    FPiW, FPiH: TFloat;
    procedure SetBloatPower(const Value: TFloat);
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
    procedure TransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
  public
    constructor Create; override;
  published
    property BloatPower: TFloat read FBloatPower write SetBloatPower;
  end;


//------------------------------------------------------------------------------
//
//      TDisturbanceTransformation
//
//------------------------------------------------------------------------------
type
  TDisturbanceTransformation = class(TTransformation)
  private
    FDisturbance: TFloat;
    procedure SetDisturbance(const Value: TFloat);
  protected
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
  public
    function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; override;
  published
    property Disturbance: TFloat read FDisturbance write SetDisturbance;
  end;


//------------------------------------------------------------------------------
//
//      TFishEyeTransformation
//
//------------------------------------------------------------------------------
type
  TFishEyeTransformation = class(TTransformation)
  private
    Frx, Fry: TFloat;
    Faw, Fsr: TFloat;
    Sx, Sy: TFloat;
    FMinR: TFloat;
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
  end;


//------------------------------------------------------------------------------
//
//      TPolarTransformation
//
//------------------------------------------------------------------------------
type
  TPolarTransformation = class(TTransformation)
  private
    FDstRect: TFloatRect;
    FPhase: TFloat;
    Sx, Sy, Cx, Cy, Dx, Dy, Rt, Rt2, Rr, Rcx, Rcy: TFloat;
    procedure SetDstRect(const Value: TFloatRect);
    procedure SetPhase(const Value: TFloat);
  protected
    procedure PrepareTransform; override;
    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
  public
    property DstRect: TFloatRect read FDstRect write SetDstRect;
    property Phase: TFloat read FPhase write SetPhase;
  end;


//------------------------------------------------------------------------------
//
//      TPathTransformation
//
//------------------------------------------------------------------------------
type
  TPathTransformation = class(TTransformation)
  private
    FTopLength: TFloat;
    FBottomLength: TFloat;
    FBottomCurve: TArrayOfFloatPoint;
    FTopCurve: TArrayOfFloatPoint;
    FTopHypot, FBottomHypot: array of record Dist, RecDist: TFloat end;
    procedure SetBottomCurve(const Value: TArrayOfFloatPoint);
    procedure SetTopCurve(const Value: TArrayOfFloatPoint);
  protected
    rdx, rdy: TFloat;
    procedure PrepareTransform; override;
    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); override;
  public
    destructor Destroy; override;
    property TopCurve: TArrayOfFloatPoint read FTopCurve write SetTopCurve;
    property BottomCurve: TArrayOfFloatPoint read FBottomCurve write SetBottomCurve;
  end;


//------------------------------------------------------------------------------
//
//      TRadialDistortionTransformation
//
//------------------------------------------------------------------------------
type
  TRadialDistortionTransformation = class(TTransformation)
  protected
    FCoefficient1, FCoefficient2, FScale: TFloat;
    FFocalPoint: TFloatPoint;
    r_0, r_tgt_max, r_tgt_min: Single;
    FMapElements: Integer;
    Map: Array of TFloat;
    function LookUpReverseMap(const r_tgt: TFloat): TFloat;
    procedure SetCoefficient1(const Value: TFloat);
    procedure SetCoefficient2(const Value: TFloat);
    procedure SetScale(const Value: TFloat);
    procedure SetMapElements(const Value: Integer);
    procedure PrepareReverseMap;
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); override;
  public
    constructor Create; override;
    function HasTransformedBounds: Boolean; override;
  published
    property Coefficient1: TFloat read FCoefficient1 write SetCoefficient1;
    property Coefficient2: TFloat read FCoefficient2 write SetCoefficient2;
    property Scale: TFloat read FScale write SetScale;
    property MapElements: Integer read FMapElements write SetMapElements;
  end;


//------------------------------------------------------------------------------
//
//      TRemapTransformation
//
//------------------------------------------------------------------------------
type
  TRemapTransformation = class(TTransformation)
  private
    FVectorMap: TVectorMap;
    FScalingFixed: TFixedVector;
    FScalingFloat: TFloatVector;
    FCombinedScalingFixed: TFixedVector;
    FCombinedScalingFloat: TFloatVector;
    FSrcTranslationFixed: TFixedVector;
    FSrcScaleFixed: TFixedVector;
    FDstTranslationFixed: TFixedVector;
    FDstScaleFixed: TFixedVector;
    FSrcTranslationFloat: TFloatVector;
    FSrcScaleFloat: TFloatVector;
    FDstTranslationFloat: TFloatVector;
    FDstScaleFloat: TFloatVector;
    FOffsetFixed : TFixedVector;
    FOffsetInt : TPoint;
    FMappingRect: TFloatRect;
    FOffset: TFloatVector;
    procedure SetMappingRect(Rect: TFloatRect);
    procedure SetOffset(const Value: TFloatVector);
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformInt(DstX, DstY: Integer; out SrcX, SrcY: Integer); override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function HasTransformedBounds: Boolean; override;
    function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; override;
    procedure Scale(Sx, Sy: TFloat);
    property MappingRect: TFloatRect read FMappingRect write SetMappingRect;
    property Offset: TFloatVector read FOffset write SetOffset;
    property VectorMap: TVectorMap read FVectorMap write FVectorMap;
  end;


//------------------------------------------------------------------------------
//
//      TSphereTransformation
//
//------------------------------------------------------------------------------
// Transform a map (planisphere) into a Spherical projection.
// By Marc LAFON (marc.lafon AT free.fr), 01 nov 2005
//------------------------------------------------------------------------------
type
  TSphereTransformation = class(TTransformation)
  private
    FMapWidth, FMapHeight: TFloat;
    FSquareRadius: TFloat;
    FCenter: TFloatPoint;
    FRadius: TFloat;
    FLongitude: TFloat;
    FLattitude: TFloat;
    FLattitudeSin: TFloat;
    FLattitudeCos: TFloat;
    FLattitudeSinInvRadius: TFloat;
    FLattitudeCosInvRadius: TFloat;
    FSrcRectTop: TFloat;
    FSrcRectLeft: TFloat;
    procedure SetCenter(const Value: TFloatPoint);
    procedure SetLattitude(const Value: TFloat);
    procedure SetLongitude(const Value: TFloat);
    procedure SetRadius(const Value: TFloat);
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
  public
    constructor Create; override;

    function HasTransformedBounds: Boolean; override;
    function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; override;

    // Return True if the (X,Y) point is in the Sphere projection
    function IsInSphere(CartesianX, CartesianY: TFloat):boolean;
    // Transform (X,Y) coordinate as Lattitude and Longitude coordinates in the Sphere
    function SphericalCoordinate(CartesianX, CartesianY: TFloat):TFloatPoint;
    // Transform Longitude and Lattitude coordinates (X,Y) into their screen projection.
    // Returns False if this point is on visible face.
    function ScreenCoordinate(var X, Y: TFloat):boolean;
  published
    // Center of the Sphere in the Destination Bitmap
    property Center: TFloatPoint read FCenter write SetCenter;
    // Radius of the Sphere in the Destination Bitmap
    property Radius: TFloat read FRadius write SetRadius;
    // Rotation of the Sphere (Y-axe rotation angle)
    property Lattitude: TFloat read FLattitude write SetLattitude;
    // Rotation of the Sphere (X-axe rotation angle)
    property Longitude: TFloat read FLongitude write SetLongitude;
  end;


//------------------------------------------------------------------------------
//
//      Utilities
//
//------------------------------------------------------------------------------
function TransformPoints(Points: TArrayOfArrayOfFixedPoint; Transformation: TTransformation): TArrayOfArrayOfFixedPoint;

procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; Reverse: boolean = True); overload;
procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; const DstClip: TRect; Reverse: boolean = True); overload;
procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; Rasterizer: TRasterizer; Reverse: boolean = True); overload;
procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; Rasterizer: TRasterizer; const DstClip: TRect; Reverse: boolean = True); overload;

procedure RasterizeTransformation(Vectormap: TVectormap;
  Transformation: TTransformation; DstRect: TRect;
  CombineMode: TVectorCombineMode = vcmAdd;
  CombineCallback: TVectorCombineEvent = nil);

procedure SetBorderTransparent(ABitmap: TCustomBitmap32; ARect: TRect);

//------------------------------------------------------------------------------

{ FullEdge controls how the bitmap is resampled }
var
  FullEdge: Boolean = True;

resourcestring
  RCStrReverseTransformationNotImplemented = 'Reverse transformation is not implemented in %s.';
  RCStrForwardTransformationNotImplemented = 'Forward transformation is not implemented in %s.';
  RCStrTopBottomCurveNil = 'Top or bottom curve is nil';


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  GR32_Blend,
  GR32_LowLevel,
  GR32_Math,
  GR32_Bindings,
  GR32_Resamplers,
  GR32_Geometry;

resourcestring
  RCStrSrcRectIsEmpty = 'SrcRect is empty!';
  RCStrMappingRectIsEmpty = 'MappingRect is empty!';
  RStrStackEmpty = 'Stack empty';

type
  {provides access to proctected members of TTransformation by typecasting}
  TTransformationAccess = class(TTransformation);

//------------------------------------------------------------------------------
//
//      A bit of linear algebra
//
//------------------------------------------------------------------------------

var
  DET_2x2_32: function(a1, a2, b1, b2: TFloat): TFloat;
  DET_3x3_32: function(a1, a2, a3, b1, b2, b3, c1, c2, c3: TFloat): TFloat;
  DET_2x2_64: function(a1, a2, b1, b2: Double): Double;


function DET_2x2_32_Pas(a1, a2, b1, b2: TFloat): TFloat; overload;
begin
  Result := a1 * b2 - a2 * b1;
end;

function DET_2x2_64_Pas(a1, a2, b1, b2: Double): Double; overload;
begin
  Result := a1 * b2 - a2 * b1;
end;

{$IFNDEF PUREPASCAL}
function DET_2x2_32_ASM(a1, a2, b1, b2: TFloat): TFloat; overload; {$IFDEF FPC}assembler; {$IFDEF CPU64}nostackframe;{$ENDIF}{$ENDIF}
asm
{$IFDEF CPU64}
        MULSS   XMM0, XMM3
        MULSS   XMM1, XMM2
        ADDSS   XMM0, XMM1
{$ELSE}
        FLD     A1.Single
        FMUL    B2.Single
        FLD     A2.Single
        FMUL    B1.Single
        FSUBP
{$ENDIF}
end;

function DET_2x2_64_ASM(a1, a2, b1, b2: Double): Double; overload;
asm
{$IFDEF CPU64}
        MULSD   XMM0, XMM3
        MULSD   XMM1, XMM2
        ADDSD   XMM0, XMM1
{$ELSE}
        FLD     A1.Double
        FMUL    B2.Double
        FLD     A2.Double
        FMUL    B1.Double
        FSUBP
{$ENDIF}
end;
{$ENDIF}

function DET_3x3_32_Pas(a1, a2, a3, b1, b2, b3, c1, c2, c3: TFloat): TFloat; overload; {$IFDEF UseInlining} inline; {$ENDIF}
begin
  Result :=
    a1 * (b2 * c3 - b3 * c2) -
    b1 * (a2 * c3 - a3 * c2) +
    c1 * (a2 * b3 - a3 * b2);
end;


//------------------------------------------------------------------------------
//
//      Utilities
//
//------------------------------------------------------------------------------
procedure Adjoint(var M: TFloatMatrix);
var
  Tmp: TFloatMatrix;
begin
  Tmp := M;

  M[0,0] :=  DET_2x2_32(Tmp[1,1], Tmp[1,2], Tmp[2,1], Tmp[2,2]);
  M[0,1] := -DET_2x2_32(Tmp[0,1], Tmp[0,2], Tmp[2,1], Tmp[2,2]);
  M[0,2] :=  DET_2x2_32(Tmp[0,1], Tmp[0,2], Tmp[1,1], Tmp[1,2]);

  M[1,0] := -DET_2x2_32(Tmp[1,0], Tmp[1,2], Tmp[2,0], Tmp[2,2]);
  M[1,1] :=  DET_2x2_32(Tmp[0,0], Tmp[0,2], Tmp[2,0], Tmp[2,2]);
  M[1,2] := -DET_2x2_32(Tmp[0,0], Tmp[0,2], Tmp[1,0], Tmp[1,2]);

  M[2,0] :=  DET_2x2_32(Tmp[1,0], Tmp[1,1], Tmp[2,0], Tmp[2,1]);
  M[2,1] := -DET_2x2_32(Tmp[0,0], Tmp[0,1], Tmp[2,0], Tmp[2,1]);
  M[2,2] :=  DET_2x2_32(Tmp[0,0], Tmp[0,1], Tmp[1,0], Tmp[1,1]);
end;

//------------------------------------------------------------------------------

function Determinant(const M: TFloatMatrix): TFloat;
begin
  Result := DET_3x3_32_Pas(
    M[0,0], M[1,0], M[2,0],
    M[0,1], M[1,1], M[2,1],
    M[0,2], M[1,2], M[2,2]);
end;

//------------------------------------------------------------------------------

procedure Scale(var M: TFloatMatrix; Factor: TFloat);
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      M[i,j] := M[i,j] * Factor;
end;

//------------------------------------------------------------------------------

procedure Invert(var M: TFloatMatrix);
var
  Det: TFloat;
begin
  Det := Determinant(M);
  if Abs(Det) < 1E-5 then
    M := IdentityMatrix
  else
  begin
    Adjoint(M);
    Scale(M, 1 / Det);
  end;
end;

//------------------------------------------------------------------------------

function Mult(const M1, M2: TFloatMatrix): TFloatMatrix;
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] :=
        M1[0, j] * M2[i, 0] +
        M1[1, j] * M2[i, 1] +
        M1[2, j] * M2[i, 2];
end;

//------------------------------------------------------------------------------

function VectorTransform(const M: TFloatMatrix; const V: TVector3f): TVector3f;
begin
  Result[0] := M[0,0] * V[0] + M[1,0] * V[1] + M[2,0] * V[2];
  Result[1] := M[0,1] * V[0] + M[1,1] * V[1] + M[2,1] * V[2];
  Result[2] := M[0,2] * V[0] + M[1,2] * V[1] + M[2,2] * V[2];
end;

//------------------------------------------------------------------------------

procedure SetBorderTransparent(ABitmap: TCustomBitmap32; ARect: TRect);
var
  I: Integer;
begin
  GR32.IntersectRect(ARect, ARect, ABitmap.BoundsRect);
  with ARect, ABitmap do
  if (Right > Left) and (Bottom > Top) and
    (Left < ClipRect.Right) and (Top < ClipRect.Bottom) and
    (Right > ClipRect.Left) and (Bottom > ClipRect.Top) then
  begin
    Dec(Right);
    Dec(Bottom);
    for I := Left to Right do
      begin
      ABitmap[I, Top] := ABitmap[I, Top] and $00FFFFFF;
      ABitmap[I, Bottom] := ABitmap[I, Bottom] and $00FFFFFF;
      end;
    for I := Top to Bottom do
    begin
      ABitmap[Left, I] := ABitmap[Left, I] and $00FFFFFF;
      ABitmap[Right, I] := ABitmap[Right, I] and $00FFFFFF;
    end;
    Changed;
  end;
end;

//------------------------------------------------------------------------------
// Transformation functions
//------------------------------------------------------------------------------
function TransformPoints(Points: TArrayOfArrayOfFixedPoint; Transformation: TTransformation): TArrayOfArrayOfFixedPoint;
var
  I, J: Integer;
begin
  if Points = nil then
    Result := nil
  else
  begin
    SetLength(Result, Length(Points));
    Transformation.PrepareTransform;

    for I := 0 to High(Result) do
    begin
      SetLength(Result[I], Length(Points[I]));
      if Length(Result[I]) > 0 then
        for J := 0 to High(Result[I]) do
          Transformation.TransformFixed(Points[I][J].X, Points[I][J].Y, Result[I][J].X, Result[I][J].Y);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; Reverse: boolean);
var
  Rasterizer: TRasterizer;
begin
  Rasterizer := DefaultRasterizerClass.Create;
  try
    Transform(Dst, Src, Transformation, Rasterizer, Reverse);
  finally
    Rasterizer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; const DstClip: TRect; Reverse: boolean);
var
  Rasterizer: TRasterizer;
begin
  Rasterizer := DefaultRasterizerClass.Create;
  try
    Transform(Dst, Src, Transformation, Rasterizer, DstClip, Reverse);
  finally
    Rasterizer.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation;
  Rasterizer: TRasterizer; Reverse: boolean);
begin
  Transform(Dst, Src, Transformation, Rasterizer, Dst.BoundsRect, Reverse);
end;

//------------------------------------------------------------------------------

procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation;
  Rasterizer: TRasterizer; const DstClip: TRect; Reverse: boolean);
var
  DstRect: TRect;
  Transformer: TTransformer;
begin
  GR32.IntersectRect(DstRect, DstClip, Dst.ClipRect);

  if (DstRect.Right < DstRect.Left) or (DstRect.Bottom < DstRect.Top) then
    Exit;

  if not Dst.MeasuringMode then
  begin
    Transformer := TTransformer.Create(Src.Resampler, Transformation, Reverse);
    try
      Rasterizer.Sampler := Transformer;
      Rasterizer.Rasterize(Dst, DstRect, Src);
    finally
      EMMS;
      Transformer.Free;
    end;
  end;
  Dst.Changed(DstRect);
end;

//------------------------------------------------------------------------------

procedure RasterizeTransformation(Vectormap: TVectormap;
  Transformation: TTransformation; DstRect: TRect;
  CombineMode: TVectorCombineMode = vcmAdd;
  CombineCallback: TVectorCombineEvent = nil);
var
  I, J: Integer;
  P, Q, Progression: TFixedVector;
  ProgressionX, ProgressionY: TFixed;
  MapPtr: PFixedPointArray;
begin
  GR32.IntersectRect(DstRect, VectorMap.BoundsRect, DstRect);
  if GR32.IsRectEmpty(DstRect) then
    Exit;

  if not TTransformationAccess(Transformation).TransformValid then
    TTransformationAccess(Transformation).PrepareTransform;

  case CombineMode of
    vcmAdd:
      begin
        with DstRect do
        for I := Top to Bottom - 1 do
        begin
          MapPtr := @VectorMap.Vectors[I * VectorMap.Width];
          for J := Left to Right - 1 do
          begin
            P := FixedPoint(Integer(J - Left), Integer(I - Top));
            Q := Transformation.ReverseTransform(P);
            Inc(MapPtr[J].X, Q.X - P.X);
            Inc(MapPtr[J].Y, Q.Y - P.Y);
          end;
        end;
      end;
    vcmReplace:
      begin
        with DstRect do
        for I := Top to Bottom - 1 do
        begin
          MapPtr := @VectorMap.Vectors[I * VectorMap.Width];
          for J := Left to Right - 1 do
          begin
            P := FixedPoint(Integer(J - Left), Integer(I - Top));
            Q := Transformation.ReverseTransform(P);
            MapPtr[J].X := Q.X - P.X;
            MapPtr[J].Y := Q.Y - P.Y;
          end;
        end;
      end;
  else // vcmCustom
    ProgressionX := Fixed(1 / (DstRect.Right - DstRect.Left - 1));
    ProgressionY := Fixed(1 / (DstRect.Bottom - DstRect.Top - 1));
    Progression.Y := 0;
    with DstRect do for I := Top to Bottom - 1 do
    begin
      Progression.X := 0;
      MapPtr := @VectorMap.Vectors[I * VectorMap.Width];
      for J := Left to Right - 1 do
      begin
        P := FixedPoint(Integer(J - Left), Integer(I - Top));
        Q := Transformation.ReverseTransform(P);
        Q.X := Q.X - P.X;
        Q.Y := Q.Y - P.Y;
        CombineCallback(Q, Progression, MapPtr[J]);

        Inc(Progression.X, ProgressionX);
      end;
     Inc(Progression.Y, ProgressionY);
    end;
  end;
end;

//------------------------------------------------------------------------------
// Matrix conversion routines
//------------------------------------------------------------------------------
function FixedMatrix(const FloatMatrix: TFloatMatrix): TFixedMatrix;
begin
  Result[0,0] := Round(FloatMatrix[0,0] * FixedOne);
  Result[0,1] := Round(FloatMatrix[0,1] * FixedOne);
  Result[0,2] := Round(FloatMatrix[0,2] * FixedOne);
  Result[1,0] := Round(FloatMatrix[1,0] * FixedOne);
  Result[1,1] := Round(FloatMatrix[1,1] * FixedOne);
  Result[1,2] := Round(FloatMatrix[1,2] * FixedOne);
  Result[2,0] := Round(FloatMatrix[2,0] * FixedOne);
  Result[2,1] := Round(FloatMatrix[2,1] * FixedOne);
  Result[2,2] := Round(FloatMatrix[2,2] * FixedOne);
end;

function FloatMatrix(const FixedMatrix: TFixedMatrix): TFloatMatrix;
begin
  Result[0,0] := FixedMatrix[0,0] * FixedToFloat;
  Result[0,1] := FixedMatrix[0,1] * FixedToFloat;
  Result[0,2] := FixedMatrix[0,2] * FixedToFloat;
  Result[1,0] := FixedMatrix[1,0] * FixedToFloat;
  Result[1,1] := FixedMatrix[1,1] * FixedToFloat;
  Result[1,2] := FixedMatrix[1,2] * FixedToFloat;
  Result[2,0] := FixedMatrix[2,0] * FixedToFloat;
  Result[2,1] := FixedMatrix[2,1] * FixedToFloat;
  Result[2,2] := FixedMatrix[2,2] * FixedToFloat;
end;


//------------------------------------------------------------------------------
//
//      TTransformation
//
//------------------------------------------------------------------------------
function TTransformation.GetTransformedBounds: TFloatRect;
begin
  Result := GetTransformedBounds(FSrcRect);
end;

procedure TTransformation.Changed;
begin
  TransformValid := False;
  inherited;
end;

constructor TTransformation.Create;
begin
  // virtual constructor to be overriden in derived classes
end;

function TTransformation.GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect;
begin
  Result := ASrcRect;
end;

function TTransformation.HasTransformedBounds: Boolean;
begin
  Result := True;
end;

procedure TTransformation.PrepareTransform;
begin
  // Dummy
end;

function TTransformation.ReverseTransform(const P: TFloatPoint): TFloatPoint;
begin
  if not TransformValid then
    PrepareTransform;
  ReverseTransformFloat(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.ReverseTransform(const P: TFixedPoint): TFixedPoint;
begin
  if not TransformValid then
    PrepareTransform;
  ReverseTransformFixed(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.ReverseTransform(const P: TPoint): TPoint;
begin
  if not TransformValid then
    PrepareTransform;
  ReverseTransformInt(P.X, P.Y, Result.X, Result.Y);
end;

procedure TTransformation.ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed);
var
  X, Y: TFloat;
begin
  ReverseTransformFloat(DstX * FixedToFloat, DstY * FixedToFloat, X, Y);
  SrcX := Fixed(X);
  SrcY := Fixed(Y);
end;

procedure TTransformation.ReverseTransformFloat(DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
begin
  // ReverseTransformFloat is the top precisionlevel, all descendants must override at least this level!
  raise ETransformNotImplemented.CreateFmt(RCStrReverseTransformationNotImplemented, [Self.Classname]);
end;

procedure TTransformation.ReverseTransformInt(DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
var
  X, Y: TFixed;
begin
  ReverseTransformFixed(DstX shl 16, DstY shl 16, X, Y);
  SrcX := FixedRound(X);
  SrcY := FixedRound(Y);
end;

procedure TTransformation.SetSrcRect(const Value: TFloatRect);
begin
  FSrcRect := Value;
  Changed;
end;

function TTransformation.Transform(const P: TFloatPoint): TFloatPoint;
begin
  if not TransformValid then
    PrepareTransform;
  TransformFloat(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.Transform(const P: TFixedPoint): TFixedPoint;
begin
  if not TransformValid then
    PrepareTransform;
  TransformFixed(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.Transform(const P: TPoint): TPoint;
begin
  if not TransformValid then
    PrepareTransform;
  TransformInt(P.X, P.Y, Result.X, Result.Y);
end;

procedure TTransformation.TransformFixed(SrcX, SrcY: TFixed; out DstX,
  DstY: TFixed);
var
  X, Y: TFloat;
begin
  TransformFloat(SrcX * FixedToFloat, SrcY * FixedToFloat, X, Y);
  DstX := Fixed(X);
  DstY := Fixed(Y);
end;

procedure TTransformation.TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat);
begin
  // TransformFloat is the top precisionlevel, all descendants must override at least this level!
  raise ETransformNotImplemented.CreateFmt(RCStrForwardTransformationNotImplemented, [Self.Classname]);
end;

procedure TTransformation.TransformInt(SrcX, SrcY: Integer; out DstX, DstY: Integer);
var
  X, Y: TFixed;
begin
  TransformFixed(SrcX shl 16, SrcY shl 16, X, Y);
  DstX := FixedRound(X);
  DstY := FixedRound(Y);
end;


//------------------------------------------------------------------------------
//
//      TNestedTransformation
//
//------------------------------------------------------------------------------
constructor TNestedTransformation.Create;
begin
  FItems := TList.Create;
end;

destructor TNestedTransformation.Destroy;
begin
  if Assigned(FItems) then
    Clear;
  FItems.Free;
  inherited;
end;

function TNestedTransformation.Add(
  ItemClass: TTransformationClass): TTransformation;
begin
  Result := ItemClass.Create;
  {$IFDEF NEXTGEN}
  Result.__ObjAddRef;
  {$ENDIF}
  FItems.Add(Result);
end;

procedure TNestedTransformation.Clear;
begin
  BeginUpdate;
  try
    while FItems.Count > 0 do
      Delete(FItems.Count - 1);
  finally
    EndUpdate;
  end;
end;

procedure TNestedTransformation.Delete(Index: Integer);
begin
  TTransformation(FItems[Index]).Free;
  FItems.Delete(Index);
end;

function TNestedTransformation.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TNestedTransformation.GetItem(Index: Integer): TTransformation;
begin
  Result := FItems[Index];
end;

function TNestedTransformation.Insert(Index: Integer;
  ItemClass: TTransformationClass): TTransformation;
begin
  BeginUpdate;
  try
    Result := Add(ItemClass);
  finally
    EndUpdate;
  end;
end;

procedure TNestedTransformation.PrepareTransform;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
    TTransformation(FItems[Index]).PrepareTransform;
end;

procedure TNestedTransformation.ReverseTransformFixed(DstX, DstY: TFixed;
  out SrcX, SrcY: TFixed);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    TTransformation(FItems[Index]).ReverseTransformFixed(DstX, DstY, SrcX,
      SrcY);
    DstX := SrcX;
    DstY := SrcY;
  end;
end;

procedure TNestedTransformation.ReverseTransformFloat(DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    TTransformation(FItems[Index]).ReverseTransformFloat(DstX, DstY, SrcX,
      SrcY);
    DstX := SrcX;
    DstY := SrcY;
  end;
end;

procedure TNestedTransformation.SetItem(Index: Integer;
  const Value: TTransformation);
begin
  TCollectionItem(FItems[Index]).Assign(Value);
end;

procedure TNestedTransformation.TransformFixed(SrcX, SrcY: TFixed; out DstX,
  DstY: TFixed);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    TTransformation(FItems[Index]).TransformFixed(SrcX, SrcY, DstX, DstY);
    SrcX := DstX;
    SrcY := DstY;
  end;
end;

procedure TNestedTransformation.TransformFloat(SrcX, SrcY: TFloat; out DstX,
  DstY: TFloat);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    TTransformation(FItems[Index]).TransformFloat(SrcX, SrcY, DstX, DstY);
    SrcX := DstX;
    SrcY := DstY;
  end;
end;


//------------------------------------------------------------------------------
//
//      T3x3Transformation
//
//------------------------------------------------------------------------------
procedure T3x3Transformation.PrepareTransform;
begin
  FInverseMatrix := Matrix;
  Invert(FInverseMatrix);

  // calculate a fixed point (65536) factors
  FInverseFixedMatrix := FixedMatrix(FInverseMatrix);
  FFixedMatrix := FixedMatrix(Matrix);

  TransformValid := True;
end;

procedure T3x3Transformation.ReverseTransformFixed(DstX, DstY: TFixed; out SrcX,
  SrcY: TFixed);
begin
  SrcX := FixedMul(DstX, FInverseFixedMatrix[0, 0]) +
    FixedMul(DstY, FInverseFixedMatrix[1, 0]) + FInverseFixedMatrix[2, 0];
  SrcY := FixedMul(DstX, FInverseFixedMatrix[0, 1]) +
    FixedMul(DstY, FInverseFixedMatrix[1, 1]) + FInverseFixedMatrix[2, 1];
end;

procedure T3x3Transformation.ReverseTransformFloat(DstX, DstY: TFloat; out SrcX,
  SrcY: TFloat);
begin
  SrcX := DstX * FInverseMatrix[0, 0] + DstY * FInverseMatrix[1, 0] +
    FInverseMatrix[2, 0];
  SrcY := DstX * FInverseMatrix[0, 1] + DstY * FInverseMatrix[1, 1] +
    FInverseMatrix[2, 1];
end;

procedure T3x3Transformation.TransformFixed(SrcX, SrcY: TFixed; out DstX,
  DstY: TFixed);
begin
  DstX := FixedMul(SrcX, FFixedMatrix[0, 0]) +
    FixedMul(SrcY, FFixedMatrix[1, 0]) + FFixedMatrix[2, 0];
  DstY := FixedMul(SrcX, FFixedMatrix[0, 1]) +
    FixedMul(SrcY, FFixedMatrix[1, 1]) + FFixedMatrix[2, 1];
end;

procedure T3x3Transformation.TransformFloat(SrcX, SrcY: TFloat; out DstX,
  DstY: TFloat);
begin
  DstX := SrcX * Matrix[0, 0] + SrcY * Matrix[1, 0] + Matrix[2, 0];
  DstY := SrcX * Matrix[0, 1] + SrcY * Matrix[1, 1] + Matrix[2, 1];
end;


//------------------------------------------------------------------------------
//
//      TAffineTransformation
//
//------------------------------------------------------------------------------
constructor TAffineTransformation.Create;
begin
  FStackLevel := 0;
  FStack := nil;
  Clear;
end;

procedure TAffineTransformation.Clear;
begin
  FMatrix := IdentityMatrix;
  Changed;
end;

procedure TAffineTransformation.Clear(BaseMatrix: TFloatMatrix);
begin
  FMatrix := BaseMatrix;
  Changed;
end;

function TAffineTransformation.GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect;
var
  V1, V2, V3, V4: TVector3f;
begin
  V1[0] := ASrcRect.Left;  V1[1] := ASrcRect.Top;    V1[2] := 1;
  V2[0] := ASrcRect.Right; V2[1] := V1[1];           V2[2] := 1;
  V3[0] := V1[0];          V3[1] := ASrcRect.Bottom; V3[2] := 1;
  V4[0] := V2[0];          V4[1] := V3[1];           V4[2] := 1;
  V1 := VectorTransform(Matrix, V1);
  V2 := VectorTransform(Matrix, V2);
  V3 := VectorTransform(Matrix, V3);
  V4 := VectorTransform(Matrix, V4);
  Result.Left   := Min(Min(V1[0], V2[0]), Min(V3[0], V4[0]));
  Result.Right  := Max(Max(V1[0], V2[0]), Max(V3[0], V4[0]));
  Result.Top    := Min(Min(V1[1], V2[1]), Min(V3[1], V4[1]));
  Result.Bottom := Max(Max(V1[1], V2[1]), Max(V3[1], V4[1]));
end;

procedure TAffineTransformation.Push;
begin
  Inc(FStackLevel);
  ReallocMem(FStack, FStackLevel * SizeOf(TFloatMatrix));
  Move(FMatrix, FStack^[FStackLevel - 1], SizeOf(TFloatMatrix));
end;

procedure TAffineTransformation.Pop;
begin
  if FStackLevel <= 0 then
    raise Exception.Create(RStrStackEmpty);

  Move(FStack^[FStackLevel - 1], FMatrix, SizeOf(TFloatMatrix));
  Dec(FStackLevel);
  Changed;
end;

procedure TAffineTransformation.Rotate(Alpha: TFloat);
var
  S, C: TFloat;
  M: TFloatMatrix;
begin
  Alpha := DegToRad(Alpha);
  GR32_Math.SinCos(Alpha, S, C);
  M := IdentityMatrix;
  M[0, 0] := C;   M[1, 0] := S;
  M[0, 1] := -S;  M[1, 1] := C;
  FMatrix := Mult(M, Matrix);
  Changed;
end;

procedure TAffineTransformation.Rotate(Cx, Cy, Alpha: TFloat);
var
  S, C: TFloat;
  M: TFloatMatrix;
begin
  if (Cx <> 0) or (Cy <> 0) then
    Translate(-Cx, -Cy);
  Alpha := DegToRad(Alpha);
  GR32_Math.SinCos(Alpha, S, C);
  M := IdentityMatrix;
  M[0, 0] := C;   M[1, 0] := S;
  M[0, 1] := -S;  M[1, 1] := C;
  FMatrix := Mult(M, Matrix);
  if (Cx <> 0) or (Cy <> 0) then
    Translate(Cx, Cy);
  Changed;
end;

procedure TAffineTransformation.Scale(Sx, Sy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[0, 0] := Sx;
  M[1, 1] := Sy;
  FMatrix := Mult(M, Matrix);
  Changed;
end;

procedure TAffineTransformation.Scale(Value: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[0, 0] := Value;
  M[1, 1] := Value;
  FMatrix := Mult(M, Matrix);
  Changed;
end;

procedure TAffineTransformation.Skew(Fx, Fy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[1, 0] := Fx;
  M[0, 1] := Fy;
  FMatrix := Mult(M, Matrix);
  Changed;
end;

procedure TAffineTransformation.Translate(Dx, Dy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[2, 0] := Dx;
  M[2, 1] := Dy;
  FMatrix := Mult(M, Matrix);

  Changed;
end;


//------------------------------------------------------------------------------
//
//      TProjectiveTransformation
//
//------------------------------------------------------------------------------
function TProjectiveTransformation.GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect;
begin
  Result.Left   := Min(Min(FQuadX[0], FQuadX[1]), Min(FQuadX[2], FQuadX[3]));
  Result.Right  := Max(Max(FQuadX[0], FQuadX[1]), Max(FQuadX[2], FQuadX[3]));
  Result.Top    := Min(Min(FQuadY[0], FQuadY[1]), Min(FQuadY[2], FQuadY[3]));
  Result.Bottom := Max(Max(FQuadY[0], FQuadY[1]), Max(FQuadY[2], FQuadY[3]));
end;

function TProjectiveTransformation.GetX(Index: Integer): TFloat;
begin
  Result := FQuadX[Index];
end;

function TProjectiveTransformation.GetY(Index: Integer): TFloat;
begin
  Result := FQuadY[Index];
end;

procedure TProjectiveTransformation.PrepareTransform;
var
  dx1, dx2, px, dy1, dy2, py: TFloat;
  g, h, k: TFloat;
  R: TFloatMatrix;
begin
  px  := FQuadX[0] - FQuadX[1] + FQuadX[2] - FQuadX[3];
  py  := FQuadY[0] - FQuadY[1] + FQuadY[2] - FQuadY[3];

  if (px = 0) and (py = 0) then
  begin
    // affine mapping
    FMatrix[0, 0] := FQuadX[1] - FQuadX[0];
    FMatrix[1, 0] := FQuadX[2] - FQuadX[1];
    FMatrix[2, 0] := FQuadX[0];

    FMatrix[0, 1] := FQuadY[1] - FQuadY[0];
    FMatrix[1, 1] := FQuadY[2] - FQuadY[1];
    FMatrix[2, 1] := FQuadY[0];

    FMatrix[0, 2] := 0;
    FMatrix[1, 2] := 0;
    FMatrix[2, 2] := 1;
  end
  else
  begin
    // projective mapping
    dx1 := FQuadX[1] - FQuadX[2];
    dx2 := FQuadX[3] - FQuadX[2];
    dy1 := FQuadY[1] - FQuadY[2];
    dy2 := FQuadY[3] - FQuadY[2];
    k := dx1 * dy2 - dx2 * dy1;
    if k <> 0 then
    begin
      k := 1 / k;
      g := (px * dy2 - py * dx2) * k;
      h := (dx1 * py - dy1 * px) * k;

      FMatrix[0, 0] := FQuadX[1] - FQuadX[0] + g * FQuadX[1];
      FMatrix[1, 0] := FQuadX[3] - FQuadX[0] + h * FQuadX[3];
      FMatrix[2, 0] := FQuadX[0];

      FMatrix[0, 1] := FQuadY[1] - FQuadY[0] + g * FQuadY[1];
      FMatrix[1, 1] := FQuadY[3] - FQuadY[0] + h * FQuadY[3];
      FMatrix[2, 1] := FQuadY[0];

      FMatrix[0, 2] := g;
      FMatrix[1, 2] := h;
      FMatrix[2, 2] := 1;
    end
    else
    begin
      FillChar(FMatrix, SizeOf(FMatrix), 0);
    end;
  end;

  // denormalize texture space (u, v)
  R := IdentityMatrix;
  R[0, 0] := 1 / (SrcRect.Right - SrcRect.Left);
  R[1, 1] := 1 / (SrcRect.Bottom - SrcRect.Top);
  FMatrix := Mult(FMatrix, R);

  R := IdentityMatrix;
  R[2, 0] := -SrcRect.Left;
  R[2, 1] := -SrcRect.Top;
  FMatrix := Mult(FMatrix, R);

  inherited;
end;

procedure TProjectiveTransformation.SetX(Index: Integer; const Value: TFloat);
begin
  FQuadX[Index] := Value;
  Changed;
end;

procedure TProjectiveTransformation.SetY(Index: Integer; const Value: TFloat);
begin
  FQuadY[Index] := Value;
  Changed;
end;

procedure TProjectiveTransformation.ReverseTransformFixed(DstX, DstY: TFixed;
  out SrcX, SrcY: TFixed);
var
  Z: TFixed;
  Zf: TFloat;
begin
  Z := FixedMul(FInverseFixedMatrix[0, 2], DstX) +
    FixedMul(FInverseFixedMatrix[1, 2], DstY) + FInverseFixedMatrix[2, 2];

  if Z = 0 then
    Exit;

  {$IFDEF UseInlining}
  SrcX := FixedMul(DstX, FInverseFixedMatrix[0, 0]) +
    FixedMul(DstY, FInverseFixedMatrix[1, 0]) + FInverseFixedMatrix[2, 0];
  SrcY := FixedMul(DstX, FInverseFixedMatrix[0,1]) +
    FixedMul(DstY, FInverseFixedMatrix[1, 1]) + FInverseFixedMatrix[2, 1];
  {$ELSE}
  inherited;
  {$ENDIF}

  if Z <> FixedOne then
  begin
    EMMS;
    Zf := FixedOne / Z;
    SrcX := Round(SrcX * Zf);
    SrcY := Round(SrcY * Zf);
  end;
end;

procedure TProjectiveTransformation.ReverseTransformFloat(
  DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
var
  Z: TFloat;
begin
  EMMS;
  Z := FInverseMatrix[0, 2] * DstX + FInverseMatrix[1, 2] * DstY +
    FInverseMatrix[2, 2];

  if Z = 0 then
    Exit;

  {$IFDEF UseInlining}
  SrcX := DstX * FInverseMatrix[0, 0] + DstY * FInverseMatrix[1, 0] +
    FInverseMatrix[2, 0];
  SrcY := DstX * FInverseMatrix[0, 1] + DstY * FInverseMatrix[1, 1] +
    FInverseMatrix[2, 1];
  {$ELSE}
  inherited;
  {$ENDIF}

  if Z <> 1 then
  begin
    Z := 1 / Z;
    SrcX := SrcX * Z;
    SrcY := SrcY * Z;
  end;
end;

procedure TProjectiveTransformation.TransformFixed(SrcX, SrcY: TFixed;
  out DstX, DstY: TFixed);
var
  Z: TFixed;
  Zf: TFloat;
begin
  Z := FixedMul(FFixedMatrix[0, 2], SrcX) +
    FixedMul(FFixedMatrix[1, 2], SrcY) + FFixedMatrix[2, 2];

  if Z = 0 then
    Exit;

  {$IFDEF UseInlining}
  DstX := FixedMul(SrcX, FFixedMatrix[0, 0]) +
    FixedMul(SrcY, FFixedMatrix[1, 0]) + FFixedMatrix[2, 0];
  DstY := FixedMul(SrcX, FFixedMatrix[0, 1]) +
    FixedMul(SrcY, FFixedMatrix[1, 1]) + FFixedMatrix[2, 1];
  {$ELSE}
  inherited;
  {$ENDIF}

  if Z <> FixedOne then
  begin
    EMMS;
    Zf := FixedOne / Z;
    DstX := Round(DstX * Zf);
    DstY := Round(DstY * Zf);
  end;
end;

procedure TProjectiveTransformation.TransformFloat(SrcX, SrcY: TFloat;
  out DstX, DstY: TFloat);
var
  Z: TFloat;
begin
  EMMS;
  Z := FMatrix[0, 2] * SrcX + FMatrix[1, 2] * SrcY + FMatrix[2, 2];

  if Z = 0 then Exit;

  {$IFDEF UseInlining}
  DstX := SrcX * Matrix[0, 0] + SrcY * Matrix[1, 0] + Matrix[2, 0];
  DstY := SrcX * Matrix[0, 1] + SrcY * Matrix[1, 1] + Matrix[2, 1];
  {$ELSE}
  inherited;
  {$ENDIF}

  if Z <> 1 then
  begin
    Z := 1 / Z;
    DstX := DstX * Z;
    DstY := DstY * Z;
  end;
end;


//------------------------------------------------------------------------------
//
//      TProjectiveTransformationEx
//
//------------------------------------------------------------------------------
// Based on amBitmapEditorToolForwardProjectiveTransform by Anders Melander
//------------------------------------------------------------------------------

function TProjectiveTransformationEx.GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect;
var
  i: integer;
begin
  if (FExtrapolate) then
    Exit(ASrcRect);

  // Transform the coords of the source rect to find the coords of
  // the corresponding target quad. Then return the boinding box of
  // this quad.
  var Bounds: TFloatQuadrilateral;
  for i := 0 to High(Bounds) do
    ReverseTransformFloat(FSourceQuad[i].X, FSourceQuad[i].Y, Bounds[i].X, Bounds[i].Y);

  Result.Left   := Min(Min(Bounds[0].X, Bounds[1].X), Min(Bounds[2].X, Bounds[3].X));
  Result.Right  := Max(Max(Bounds[0].X, Bounds[1].X), Max(Bounds[2].X, Bounds[3].X));
  Result.Top    := Min(Min(Bounds[0].Y, Bounds[1].Y), Min(Bounds[2].Y, Bounds[3].Y));
  Result.Bottom := Max(Max(Bounds[0].Y, Bounds[1].Y), Max(Bounds[2].Y, Bounds[3].Y));

(* Naive; Does not take projection to DestQuad into account.
  Result.Left   := Min(Min(FDestQuad[0].X, FDestQuad[1].X), Min(FDestQuad[2].X, FDestQuad[3].X));
  Result.Right  := Max(Max(FDestQuad[0].X, FDestQuad[1].X), Max(FDestQuad[2].X, FDestQuad[3].X));
  Result.Top    := Min(Min(FDestQuad[0].Y, FDestQuad[1].Y), Min(FDestQuad[2].Y, FDestQuad[3].Y));
  Result.Bottom := Max(Max(FDestQuad[0].Y, FDestQuad[1].Y), Max(FDestQuad[2].Y, FDestQuad[3].Y));
*)
end;

function TProjectiveTransformationEx.GetSource(Index: Integer): TFloatPoint;
begin
  Result := FSourceQuad[Index];
end;

function TProjectiveTransformationEx.GetSourceX(Index: Integer): TFloat;
begin
  Result := FSourceQuad[Index].X;
end;

function TProjectiveTransformationEx.GetSourceY(Index: Integer): TFloat;
begin
  Result := FSourceQuad[Index].Y;
end;

function TProjectiveTransformationEx.GetDest(Index: Integer): TFloatPoint;
begin
  Result := FDestQuad[Index];
end;

function TProjectiveTransformationEx.GetDestX(Index: Integer): TFloat;
begin
  Result := FDestQuad[Index].X;
end;

function TProjectiveTransformationEx.GetDestY(Index: Integer): TFloat;
begin
  Result := FDestQuad[Index].Y;
end;

procedure TProjectiveTransformationEx.SetSource(Index: Integer; const Value: TFloatPoint);
begin
  FSourceQuad[Index] := Value;
  Changed;
end;

procedure TProjectiveTransformationEx.SetSourceQuad(const Value: TFloatQuadrilateral);
begin
  FSourceQuad := Value;
  Changed;
end;

procedure TProjectiveTransformationEx.SetSourceX(Index: Integer; const Value: TFloat);
begin
  FSourceQuad[Index].Y := Value;
  Changed;
end;

procedure TProjectiveTransformationEx.SetSourceY(Index: Integer; const Value: TFloat);
begin
  FSourceQuad[Index].X := Value;
  Changed;
end;

procedure TProjectiveTransformationEx.SetDest(Index: Integer; const Value: TFloatPoint);
begin
  FDestQuad[Index] := Value;
  Changed;
end;

procedure TProjectiveTransformationEx.SetDestQuad(const Value: TFloatQuadrilateral);
begin
  FDestQuad := Value;
  Changed;
end;

procedure TProjectiveTransformationEx.SetDestX(Index: Integer; const Value: TFloat);
begin
  FDestQuad[Index].X := Value;
  Changed;
end;

procedure TProjectiveTransformationEx.SetDestY(Index: Integer; const Value: TFloat);
begin
  FDestQuad[Index].Y := Value;
  Changed;
end;

procedure TProjectiveTransformationEx.PrepareTransform;
//------------------------------------------------------------------------------
// From "Fundamentals of Texture Mapping and Image Warping" by Paul S. Heckbert:
//------------------------------------------------------------------------------
//
// The general form of a projective mapping is a rational linear mapping:
//
//        au + bv + c            du + ev + f
//   x = -------------  ,   y = -------------                                                                   [1]
//        gu + hv + i            gu + hv + i
//
// Manipulation of projective mappings is much easier in the homogeneous matrix notation:
//
//   Pd = Ps * Msd
//
//        ┌    ┐   ┌         ┐┌    ┐
//        │ x' │   │ a  d  g ││ u' │
//      = │ y' │ = │ b  e  h ││ v' │
//        │ w  │   │ c  f  i ││ q  │
//        └    ┘   └         ┘└    ┘
//
//            T               T                     T              T
// where (x,y) = (x'/ w, y'/w) for w ≠ 0, and (u,v) = (u'/q, v'/q) for q ≠ 0.
//
// Although there are 9 coefficients in the matrix above, these mappings are homogeneous, so
// any nonzero scalar multiple of these matrices gives an equivalent mapping. Hence there are only
// 8 degrees of freedom in a 2-D projective mapping. We can assume without loss of generality that
// i=1 except in the special case that source point (0, 0)T maps to a point at infinity. A projective
// mapping is affine when g=h=0.
//
// ...
//
// Projective mappings may be composed by concatenating their matrices.
//
// Another remarkable property is that the inverse of a projective mapping is a projective mapping.
// This is intuitively explained by reversing the plane-to-plane mapping by which a projective mapping
// is defined. The matrix for the inverse mapping is the inverse or adjoint of the forward mapping. (The
// adjoint of a matrix is the transpose of the matrix of cofactors; M^-1 = adj(M)/det(M)).
// In homogeneous algebra, the adjoint matrix can be used in place of the inverse matrix whenever an
// inverse transform is needed, since the two are scalar multiples of each other, and the adjoint always
// exists, while the inverse does not if the matrix is singular. The inverse transformation is thus:
//
//   Ps = Msd * Pd
//
//        ┌    ┐   ┌         ┐┌    ┐
//        │ u' │   │ A  D  G ││ x' │
//      = │ v' │ = │ B  E  H ││ y' │
//        │ q  │   │ C  F  I ││ w  │
//        └    ┘   └         ┘└    ┘
//
//        ┌                     ┐┌    ┐
//        │ ei-fh  ch-bi  bf-ce ││ x' │
//      = │ fg-di  ai-cg  cd-af ││ y' │
//        │ dh-eg  bg-ah  ae-bd ││ w  │
//        └                     ┘└    ┘
//                                                                T           T
// When mapping a point by the inverse transform we compute (u, v) from (x, y). If w ≠ 0 and
// q ≠ 0 then we can choose w = 1 and calculate:
//
//        Ax + By + C            Dx + Ey + F
//   u = -------------  ,   y = -------------                                                                   [2]
//        Gx + Hy + I            Gx + Hy + I
//
// ...
//
// In an interactive image warper one might specify the four corners of source and destination quadrilaterals
// with a tablet or mouse, and wish to warp one area to the other. This sort of task is an ideal
// application of projective mappings, but how do we find the mapping matrix?
//
// A projective mapping has 8 degrees of freedom which can be determined from the source and
// destination coordinates of the four corners of a quadrilateral. Let the correspondence map (uk, vk)^T
// to (xk, yk)^T for vertices numbered cyclically k = 0,1,2,3. All coordinates are assumed to be real
// (finite). To compute the forward mapping matrix Msd, assuming that i= 1, we have eight equations
// in the eight unknowns a-h:
//
//         auk + bvk + c
//   xk = ---------------  =>  uka + vkb + c - ukxkg - vkxkh = xk
//         guk + hvk + 1
//
//         duk + evk + f
//   yk = ---------------  =>  ukd + vke + f - ukykg - vkykh = yk
//         guk + hvk + 1
//
// for k = 0,1,2,3. This can be rewritten as an 8 × 8 system:
//
//        ┌                                    ┐┌   ┐   ┌    ┐
//        │ u0  v0   1   0   0   0 -u0x0 -v0x0 ││ a │   │ x0 │
//        │ u1  v1   1   0   0   0 -u1x1 -v1x1 ││ b │   │ x1 │
//        │ u2  v2   1   0   0   0 -u2x2 -v2x2 ││ c │   │ x2 │
//        │ u3  v3   1   0   0   0 -u3x3 -v3x3 ││ d │ = │ x3 │
//        │  0   0   0  u0  v0   1 -u0y0 -v0y0 ││ e │   │ y0 │
//        │  0   0   0  u1  v1   1 -u1y1 -v1y1 ││ f │   │ y1 │
//        │  0   0   0  u2  v2   1 -u2y2 -v2y2 ││ g │   │ y2 │
//        │  0   0   0  u3  v3   1 -u3y3 -v3y3 ││ h │   │ y3 │
//        └                                    ┘└   ┘   └    ┘
//
// This linear system can be solved using Gaussian elimination or other methods for the forward mapping
// coefficients a-h. If the inverse mapping is desired instead, then either we compute the adjoint
// of Msd or we follow the same procedure, starting from equation [2] instead of [1], and solve an
// 8 × 8 system for coefficients A-H.
//
// In speed-critical special cases, there are more efficient formulas for computing the mapping
// matrix. The formula above handles the case where the polygon is a general quadrilateral in both
// source and destination spaces. We will consider three additional cases: square-to-quadrilateral,
// quadrilateral-to-square, and (again) the general quadrilateral-to-quadrilateral mapping.
//
// Case 1. The system is easily solved symbolically in the special case where the uv quadrilateral
// is a unit square. If the vertex correspondence is as follows:
//
//     x  y  u  v
//   -------------
//    x0 y0  0  0
//    x1 y1  1  0
//    x2 y2  1  1
//    x3 y3  0  1
//
// then the eight equations reduce to
//
//                       c = x0
//             a + c - gx1 = x1
//       a + c - gx2 - hx2 = x2
//             b + c - hx3 = x3
//                       f = y0
//             d + f - gy1 = y1
//   d + e + f - gy2 - hy2 = y2
//             e + f - hy3 = y3
//
// If we define
//
//   ∆x1 = x1 - x2,  ∆x2 = x3 - x2,  ∑x = x0 - x1 + x2 - x3                                                     [a.1]
//   ∆y1 = y1 - y2,  ∆y2 = y3 - y2,  ∑y = y0 - y1 + y2 - y3                                                     [a.2]
//
// then the solution splits into two sub-cases:
//
//   (a) ∑x = 0 and ∑y = 0. This implies that the xy polygon is a parallelogram, so the mapping is
//       affine, and a = x1 - x0,  b = x2 - x1,  c = x0,  d = y1 - y0,  e = y2 - y1,  f = y0,  g = 0,  h = 0.   [b]
//
//   (b) ∑x ≠ 0 or ∑y ≠ 0 gives a projective mapping:
//           │ ∑x  ∆x2 │   │ ∆x1 ∆x2 │
//       g = │         │ / │         │                                                                          [c.1]
//           │ ∑y  ∆y2 │   │ ∆y1 ∆y2 │
//
//
//           │ ∆x1  ∑x │   │ ∆x1 ∆x2 │
//       h = │         │ / │         │                                                                          [c.2]
//           │ ∆y1  ∑y │   │ ∆y1 ∆y2 │
//
//       a = x1 - x0 + gx1                                                                                      [c.3]
//       b = x3 - x0 + hx3
//       c = x0
//       d = y1 - y0 + gy1
//       e = y3 - y0 + hy3
//       f = y0
//
// This computation is much faster than a straightforward 8 × 8 system solver. The mapping above is
// easily generalized to map a rectangle to a quadrilateral by pre-multiplying with a scale and translation
// matrix.
//
// Case 2. The inverse mapping, a quadrilateral to a square, can also be optimized. It turns out
// that the most efficient algorithm for computing this is not purely symbolic, as in the previous case,
// but numerical. We use the square-to-quadrilateral formulas just described to find the inverse of the
// desired mapping, and then take its adjoint to compute the quadrilateral-to-square mapping.
//
// Case 3. Since we can compute quadrilateral-to-square and square-to-quadrilateral mappings
// quickly, the two mappings can easily be composed to yield a general quadrilateral-to-
// mapping. This solution method is faster than a general 8 × 8 system solver.
//
  procedure CreateProjectiveMapping(const Quad: TFloatQuadrilateral; var Matrix: TFloatMatrix);
  var
    ∑x, ∑y: TFloat;
    ∆x1, ∆x2, ∆y1, ∆y2: TFloat;
    g, h, k: TFloat;
  begin
    ∑x := Quad[0].X - Quad[1].X + Quad[2].X - Quad[3].X;                        // See [a]
    ∑y := Quad[0].Y - Quad[1].Y + Quad[2].Y - Quad[3].Y;

    if (IsZero(∑x)) and (IsZero(∑y)) then                                       // See [b]
    begin
      // Quadrilateral is a parallelogram - Mapping is affine
      //   ┌         ┐
      //   │ a  d  0 │
      //   │ b  e  0 │
      //   │ c  f  1 │
      //   └         ┘
      Matrix[0, 0] := Quad[1].X - Quad[0].X;    // a
      Matrix[1, 0] := Quad[2].X - Quad[1].X;    // b
      Matrix[2, 0] := Quad[0].X;                // c

      Matrix[0, 1] := Quad[1].Y - Quad[0].Y;    // d
      Matrix[1, 1] := Quad[2].Y - Quad[1].Y;    // e
      Matrix[2, 1] := Quad[0].Y;                // f

      Matrix[0, 2] := 0;                        // g
      Matrix[1, 2] := 0;                        // h
      Matrix[2, 2] := 1;                        // i
    end else
    begin
      // Projective mapping
      //   ┌         ┐
      //   │ a  d  g │
      //   │ b  e  h │
      //   │ c  f  1 │
      //   └         ┘
      ∆x1 := Quad[1].X - Quad[2].X;                                             // See [a]
      ∆x2 := Quad[3].X - Quad[2].X;
      ∆y1 := Quad[1].Y - Quad[2].Y;
      ∆y2 := Quad[3].Y - Quad[2].Y;

      k := ∆x1 * ∆y2 - ∆x2 * ∆y1;

      if (not IsZero(k)) then
      begin
        k := 1 / k; // Avoid (one) costly divisions below
        g := (∑x * ∆y2 - ∑y * ∆x2) * k;                                         // See [c]
        h := (∆x1 * ∑y - ∆y1 * ∑x) * k;

        Matrix[0, 0] := Quad[1].X - Quad[0].X + g * Quad[1].X;  // a
        Matrix[1, 0] := Quad[3].X - Quad[0].X + h * Quad[3].X;  // b
        Matrix[2, 0] := Quad[0].X;                              // c

        Matrix[0, 1] := Quad[1].Y - Quad[0].Y + g * Quad[1].Y;  // d
        Matrix[1, 1] := Quad[3].Y - Quad[0].Y + h * Quad[3].Y;  // e
        Matrix[2, 1] := Quad[0].Y;                              // f

        Matrix[0, 2] := g;                                      // g
        Matrix[1, 2] := h;                                      // h
        Matrix[2, 2] := 1;                                      // i
      end else
        Matrix := Default(TFloatMatrix);
    end;
  end;

var
  SourceMatrix: TFloatMatrix;
  DestMatrix: TFloatMatrix;
//  R: TFloatMatrix;
begin
  CreateProjectiveMapping(FSourceQuad, SourceMatrix);

  CreateProjectiveMapping(FDestQuad, DestMatrix);
  Invert(DestMatrix);

  FMatrix := Mult(SourceMatrix, DestMatrix);

(*
  // Denormalize texture space (u, v)
  // Scale
  R := IdentityMatrix;
  R[0, 0] := 1 / (SrcRect.Right - SrcRect.Left);
  R[1, 1] := 1 / (SrcRect.Bottom - SrcRect.Top);
  FMatrix := Mult(FMatrix, R);

  // Translate
  R := IdentityMatrix;
  R[2, 0] := -SrcRect.Left;
  R[2, 1] := -SrcRect.Top;
  FMatrix := Mult(FMatrix, R);
*)

  inherited;
end;

procedure TProjectiveTransformationEx.ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed);
var
  Z: TFixed;
  Zf: TFloat;
begin
  Z := FixedMul(FInverseFixedMatrix[0, 2], DstX) +
       FixedMul(FInverseFixedMatrix[1, 2], DstY) +
                FInverseFixedMatrix[2, 2];

  if Z = 0 then
    Exit;

{$IFDEF UseInlining}
  SrcX := FixedMul(FInverseFixedMatrix[0, 0], DstX) +
          FixedMul(FInverseFixedMatrix[1, 0], DstY) +
                   FInverseFixedMatrix[2, 0];

  SrcY := FixedMul(FInverseFixedMatrix[0, 1], DstX) +
          FixedMul(FInverseFixedMatrix[1, 1], DstY) +
                   FInverseFixedMatrix[2, 1];
{$ELSE}
  inherited;
{$ENDIF}

  if Z <> FixedOne then
  begin
    EMMS;
    Zf := FixedOne / Z;
    SrcX := Round(SrcX * Zf);
    SrcY := Round(SrcY * Zf);
  end;
end;

procedure TProjectiveTransformationEx.ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat);
var
  Z: TFloat;
begin
  EMMS;
  Z := FInverseMatrix[0, 2] * DstX +
       FInverseMatrix[1, 2] * DstY +
       FInverseMatrix[2, 2];

  if IsZero(Z) then
    Exit;

{$IFDEF UseInlining}
  SrcX := FInverseMatrix[0, 0] * DstX +
          FInverseMatrix[1, 0] * DstY +
          FInverseMatrix[2, 0];

  SrcY := FInverseMatrix[0, 1] * DstX +
          FInverseMatrix[1, 1] * DstY +
          FInverseMatrix[2, 1];
{$ELSE}
  inherited;
{$ENDIF}

  if Z <> 1 then
  begin
    Z := 1 / Z;
    SrcX := SrcX * Z;
    SrcY := SrcY * Z;
  end;
end;

procedure TProjectiveTransformationEx.TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed);
var
  Z: TFixed;
  Zf: TFloat;
begin
  Z := FixedMul(FFixedMatrix[0, 2], SrcX) +
       FixedMul(FFixedMatrix[1, 2], SrcY) +
                FFixedMatrix[2, 2];

  if Z = 0 then
    Exit;

{$IFDEF UseInlining}
  DstX := FixedMul(FFixedMatrix[0, 0], SrcX) +
          FixedMul(FFixedMatrix[1, 0], SrcY) +
                   FFixedMatrix[2, 0];
  DstY := FixedMul(FFixedMatrix[0, 1], SrcX) +
          FixedMul(FFixedMatrix[1, 1], SrcY) +
                   FFixedMatrix[2, 1];
{$ELSE}
  inherited;
{$ENDIF}

  if Z <> FixedOne then
  begin
    EMMS;
    Zf := FixedOne / Z;
    DstX := Round(DstX * Zf);
    DstY := Round(DstY * Zf);
  end;
end;

procedure TProjectiveTransformationEx.TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat);
var
  Z: TFloat;
begin
  EMMS;
  Z := FMatrix[0, 2] * SrcX +
       FMatrix[1, 2] * SrcY +
       FMatrix[2, 2];

  if IsZero(Z) then
    Exit;

{$IFDEF UseInlining}
  DstX := FMatrix[0, 0] * SrcX +
          FMatrix[1, 0] * SrcY +
          FMatrix[2, 0];
  DstY := FMatrix[0, 1] * SrcX +
          FMatrix[1, 1] * SrcY +
          FMatrix[2, 1];
{$ELSE}
  inherited;
{$ENDIF}

  if Z <> 1 then
  begin
    Z := 1 / Z;
    DstX := DstX * Z;
    DstY := DstY * Z;
  end;
end;


//------------------------------------------------------------------------------
//
//      TTwirlTransformation
//
//------------------------------------------------------------------------------
constructor TTwirlTransformation.Create;
begin
  FTwirl := 0.03;
end;

function TTwirlTransformation.GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect;
var
  Cx, Cy, R: TFloat;
const
  CPiHalf: TFloat = 0.5 * Pi;
begin
  Cx := (ASrcRect.Left + ASrcRect.Right) * 0.5;
  Cy := (ASrcRect.Top + ASrcRect.Bottom) * 0.5;
  R := Max(Cx - ASrcRect.Left, Cy - ASrcRect.Top);
  Result.Left := Cx - R * CPiHalf;
  Result.Right := Cx + R * CPiHalf;
  Result.Top := Cy - R * CPiHalf;
  Result.Bottom := Cy + R * CPiHalf;
end;

procedure TTwirlTransformation.PrepareTransform;
begin
  with FSrcRect do
  begin
    Frx := (Right - Left) * 0.5;
    Fry := (Bottom - Top) * 0.5;
  end;
  TransformValid := True;
end;

procedure TTwirlTransformation.ReverseTransformFloat(DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
var
  xf, yf, r, t: Single;
begin
  xf := DstX - Frx;
  yf := DstY - Fry;

  r := GR32_Math.Hypot(xf, yf);
  t := ArcTan2(yf, xf) + r * FTwirl;
  GR32_Math.SinCos(t, yf, xf);

  SrcX := Frx + r * xf;
  SrcY := Fry + r * yf;
end;

procedure TTwirlTransformation.SetTwirl(const Value: TFloat);
begin
  FTwirl := Value;
  Changed;
end;


//------------------------------------------------------------------------------
//
//      TBloatTransformation
//
//------------------------------------------------------------------------------
constructor TBloatTransformation.Create;
begin
  FBloatPower := 0.3;
end;

procedure TBloatTransformation.PrepareTransform;
begin
  FPiW := (Pi / (FSrcRect.Right - FSrcRect.Left));
  FPiH := (Pi / (FSrcRect.Bottom - FSrcRect.Top));
  FBP := FBloatPower * Max(FSrcRect.Right - FSrcRect.Left, FSrcRect.Bottom - FSrcRect.Top);
  TransformValid := True;  
end;

procedure TBloatTransformation.ReverseTransformFloat(DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
var
  SinY, CosY, SinX, CosX, t: Single;
begin
  GR32_Math.SinCos(FPiH * DstY, SinY, CosY);
  GR32_Math.SinCos(FPiW * DstX, SinX, CosX);
  t := FBP * SinY * SinX;
  SrcX := DstX + t * CosX;
  SrcY := DstY + t * CosY;
end;

procedure TBloatTransformation.TransformFloat(DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
var
  SinY, CosY, SinX, CosX, t: Single;
begin
  GR32_Math.SinCos(-FPiH * DstY, SinY, CosY);
  GR32_Math.SinCos(-FPiW * DstX, SinX, CosX);
  t := FBP * SinY * SinX;
  SrcX := DstX + t * CosX;
  SrcY := DstY + t * CosY;
end;

procedure TBloatTransformation.SetBloatPower(const Value: TFloat);
begin
  FBloatPower := Value;
  Changed;
end;


//------------------------------------------------------------------------------
//
//      TFishEyeTransformation
//
//------------------------------------------------------------------------------
procedure TFishEyeTransformation.PrepareTransform;
begin
  with FSrcRect do
  begin
    Frx := (Right - Left) * 0.5;
    Fry := (Bottom - Top) * 0.5;
    if Frx <= Fry then
    begin
      FMinR := Frx;
      Sx := 1;
      Sy:= Frx / Fry;
    end
    else
    begin
      FMinR := Fry;
      Sx:= Fry / Frx;
      Sy := 1;
    end;
    Fsr := 1 / FMinR;
    Faw := ArcSin(Constrain(FMinR * Fsr, -1, 1));
    if Faw <> 0 then
      Faw := 1 / Faw;
    Faw := Faw * FMinR
  end;
  TransformValid := True;  
end;

procedure TFishEyeTransformation.ReverseTransformFloat(DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
var
  d, Xrx, Yry: TFloat;
begin
  Yry := (DstY - Fry) * sy;
  Xrx := (DstX - Frx) * sx;
  d := GR32_Math.Hypot(Xrx, Yry);
  if (d < FMinR) and (d > 0) then
  begin
    d := ArcSin(d * Fsr) * Faw / d;
    SrcX := Frx + Xrx * d;
    SrcY := Fry + Yry * d;
  end
  else
  begin
    SrcX := DstX;
    SrcY := DstY;
  end;
end;


//------------------------------------------------------------------------------
//
//      TPolarTransformation
//
//------------------------------------------------------------------------------
procedure TPolarTransformation.PrepareTransform;
begin
  Sx := SrcRect.Right - SrcRect.Left;
  Sy := SrcRect.Bottom - SrcRect.Top;
  Cx := (DstRect.Left + DstRect.Right) * 0.5;
  Cy := (DstRect.Top + DstRect.Bottom) * 0.5;
  Dx := DstRect.Right - Cx;
  Dy := DstRect.Bottom - Cy;

  Rt := (1 / (PI * 2)) * Sx;

  Rt2 := Sx;
  if Rt2 <> 0 then
    Rt2 := 1 / Sx
  else
    Rt2 := 0.00000001;
  Rt2 := Rt2 * 2 * Pi;

  Rr := Sy;
  if Rr <> 0 then
    Rr := 1 / Rr
  else
    Rr := 0.00000001;

  Rcx := Cx;
  if Rcx <> 0 then
    Rcx := 1 / Rcx
  else
    Rcx := 0.00000001;

  Rcy := Cy;
  if Rcy <> 0 then
    Rcy := 1 / Rcy
   else
    Rcy := 0.00000001;

  TransformValid := True;
end;

procedure TPolarTransformation.SetDstRect(const Value: TFloatRect);
begin
  FDstRect := Value;
  Changed;
end;

procedure TPolarTransformation.TransformFloat(SrcX, SrcY: TFloat; out DstX,
  DstY: TFloat);
var
  R, Theta, S, C: TFloat;
begin
  Theta := (SrcX - SrcRect.Left) * Rt2 + Phase;
  R := (SrcY - SrcRect.Bottom) * Rr;
  GR32_Math.SinCos(Theta, S, C);

  DstX := Dx * R * C + Cx;
  DstY := Dy * R * S + Cy;
end;

procedure TPolarTransformation.ReverseTransformFloat(DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
const
  PI2 = 2 * PI;
var
  Dcx, Dcy, Theta: TFloat;
begin
  Dcx := (DstX - Cx) * Rcx;
  Dcy := (DstY - Cy) * Rcy;

  Theta := ArcTan2(Dcy, Dcx) + Pi - Phase;
  if Theta < 0 then
    Theta := Theta + PI2;

  SrcX := SrcRect.Left + Theta * Rt;
  SrcY := SrcRect.Bottom - GR32_Math.Hypot(Dcx, Dcy) * Sy;
end;


procedure TPolarTransformation.SetPhase(const Value: TFloat);
begin
  FPhase := Value;
  Changed;
end;


//------------------------------------------------------------------------------
//
//      TPathTransformation
//
//------------------------------------------------------------------------------
destructor TPathTransformation.Destroy;
begin
  FTopHypot := nil;
  FBottomHypot := nil;
  inherited;
end;

procedure TPathTransformation.PrepareTransform;
var
  I: Integer;
  L, DDist: TFloat;
begin
  if not (Assigned(FTopCurve) and Assigned(FBottomCurve)) then
    raise ETransformError.Create(RCStrTopBottomCurveNil);

  SetLength(FTopHypot, Length(FTopCurve));
  SetLength(FBottomHypot, Length(FBottomCurve));

  L := 0;
  for I := 0 to High(FTopCurve) - 1 do
  begin
    FTopHypot[I].Dist := L;
    with FTopCurve[I + 1] do
      L := L + GR32_Math.Hypot(FTopCurve[I].X - X, FTopCurve[I].Y - Y);
  end;
  FTopLength := L;

  for I := 1 to High(FTopCurve) do
    with FTopHypot[I] do
    begin
      DDist := Dist - FTopHypot[I - 1].Dist;
      if DDist <> 0 then
        RecDist := 1 / DDist
      else
      if I > 1 then
        RecDist := FTopHypot[I - 1].RecDist
      else
        RecDist := 0;
    end;

  L := 0;
  for I := 0 to High(FBottomCurve) - 1 do
  begin
    FBottomHypot[I].Dist := L;
    with FBottomCurve[I + 1] do
      L := L + GR32_Math.Hypot(FBottomCurve[I].X - X, FBottomCurve[I].Y - Y);
  end;
  FBottomLength := L;

  for I := 1 to High(FBottomCurve) do
    with FBottomHypot[I] do
    begin
      DDist := Dist - FBottomHypot[I - 1].Dist;
      if DDist <> 0 then
        RecDist := 1 / DDist
      else
      if I > 1 then
        RecDist := FBottomHypot[I - 1].RecDist
      else
        RecDist := 0;
    end;

  rdx := 1 / (SrcRect.Right - SrcRect.Left);
  rdy := 1 / (SrcRect.Bottom - SrcRect.Top);

  TransformValid := True;
end;

procedure TPathTransformation.SetBottomCurve(const Value: TArrayOfFloatPoint);
begin
  FBottomCurve := Value;
  Changed;
end;

procedure TPathTransformation.SetTopCurve(const Value: TArrayOfFloatPoint);
begin
  FTopCurve := Value;
  Changed;
end;

procedure TPathTransformation.TransformFloat(SrcX, SrcY: TFloat; out DstX,
  DstY: TFloat);
var
  I, H: Integer;
  X, Y, fx, dx, dy, r, Tx, Ty, Bx, By: TFloat;
begin
  X := (SrcX - SrcRect.Left) * rdx;
  Y := (SrcY - SrcRect.Top) * rdy;

  fx := X * FTopLength;
  I := 1;
  H := High(FTopHypot);
  while (FTopHypot[I].Dist < fx) and (I < H) do
    Inc(I);


  with FTopHypot[I] do
    r := (Dist - fx) * RecDist;

  dx := (FTopCurve[I - 1].X - FTopCurve[I].X);
  dy := (FTopCurve[I - 1].Y - FTopCurve[I].Y);
  Tx := FTopCurve[I].X + r * dx;
  Ty := FTopCurve[I].Y + r * dy;

  fx := X * FBottomLength;
  I := 1;
  H := High(FBottomHypot);
  while (FBottomHypot[I].Dist < fx) and (I < H) do
    Inc(I);

  with FBottomHypot[I] do
    r := (Dist - fx) * RecDist;

  dx := (FBottomCurve[I - 1].X - FBottomCurve[I].X);
  dy := (FBottomCurve[I - 1].Y - FBottomCurve[I].Y);
  Bx := FBottomCurve[I].X + r * dx;
  By := FBottomCurve[I].Y + r * dy;

  DstX := Tx + Y * (Bx - Tx);
  DstY := Ty + Y * (By - Ty);
end;


//------------------------------------------------------------------------------
//
//      TDisturbanceTransformation
//
//------------------------------------------------------------------------------
function TDisturbanceTransformation.GetTransformedBounds(
  const ASrcRect: TFloatRect): TFloatRect;
begin
  Result := ASrcRect;
  GR32.InflateRect(Result, 0.5 * FDisturbance, 0.5 * FDisturbance);
end;

procedure TDisturbanceTransformation.ReverseTransformFloat(DstX,
  DstY: TFloat; out SrcX, SrcY: TFloat);
begin
  SrcX := DstX + (Random - 0.5) * FDisturbance;
  SrcY := DstY + (Random - 0.5) * FDisturbance;
end;

procedure TDisturbanceTransformation.SetDisturbance(const Value: TFloat);
begin
  FDisturbance := Value;
  Changed;  
end;


//------------------------------------------------------------------------------
//
//      TRadialDistortionTransformation
//
//------------------------------------------------------------------------------
constructor TRadialDistortionTransformation.Create;
begin
  FCoefficient1 := 0;
  FCoefficient2 := 0;
  FScale := 1;
  FMapElements := 0;
end;

function TRadialDistortionTransformation.HasTransformedBounds: Boolean;
begin
  Result := False;
end;

procedure TRadialDistortionTransformation.PrepareReverseMap;
var
  i, j, LowerI, UpperI, jmax: Integer;
  r_src, r_tgt, LowerValue, UpperValue: TFloat;
{$IFDEF DEBUG}
  // some counters to evaluate the mapping
  interpolated, unset, mapToSameIndex, IndexOutOfRange: Integer;
{$ENDIF}
begin
  if MapElements <= 1 then
    MapElements := Trunc(r_0);

  r_tgt_max := 2;
  r_tgt_min := -0.5;

  SetLength(Map, MapElements);
  for i := 0 to High(Map) do
    Map[i] := -1;

  jmax := 1000;
{$IFDEF DEBUG}
  mapToSameIndex := 0;
  IndexOutOfRange := 0;
{$ENDIF}
  for j := 0 to jmax do
  begin
    r_src := j/jmax*2;
    r_tgt := Scale*(1 + FCoefficient1 * Sqr(r_src) + FCoefficient2 * Power(r_src, 4));
    Assert(InRange(r_tgt, r_tgt_min, r_tgt_max));
    i := Trunc((r_tgt*r_src-r_tgt_min)/(r_tgt_max-r_tgt_min)*(High(Map)-1));
    if not InRange(i, 0, High(Map)) then
    begin
{$IFDEF DEBUG}
      Inc(IndexOutOfRange);
      // OutputDebugString(PChar(Format('PrepareReverseMap: i=%d out of range (0, MapElements=%d), r_tgt=%f', [ i, MapElements, r_tgt ])))
{$ENDIF}
    end
    else
    if Map[i]<>-1 then
    begin
{$IFDEF DEBUG}
      Inc(mapToSameIndex);
      // OutputDebugString(PChar(Format('PrepareReverseMap: Map[i=%d] already has value %f (wanted to put %f there)', [ i, Map[i], r_tgt ])))
{$ENDIF}
    end
    else
      Map[i] := r_tgt;
  end;

{$IFDEF DEBUG}
  unset := 0;
  for i := 0 to High(Map) do
  begin
    if Map[i] = -1 then
      Inc(unset);
  end;
{$ENDIF}

  // linear interpolation where Map[i] == -1 (but no extrapolation)
  i := 0;
  LowerI := -1;
  LowerValue := -1;
{$IFDEF DEBUG}
  interpolated := 0;
{$ENDIF}
  repeat
    if Map[i] = -1 then
    begin
      if LowerI <> -1 then
      begin
        UpperI := i+1;
        while (UpperI<=High(Map)) and (Map[UpperI] = -1) do
          Inc(UpperI);
        if UpperI<=High(Map) then
        begin
          UpperValue := Map[UpperI];
          for j := LowerI+1 to UpperI-1 do
          begin
            Map[j] := LowerValue + (UpperValue-LowerValue) * (j-LowerI) / (UpperI - LowerI);
{$IFDEF DEBUG}
            Inc(interpolated);
{$ENDIF}
          end;
        end;
      end;
    end
    else
    begin
      LowerI := i;
      LowerValue := Map[i];
    end;
    Inc(i);
  until i > High(Map);
{$IFDEF DEBUG}
  OutputDebugString(PChar(Format(
    'TRadialDistortionTransformation.PrepareReverseMap: mapToSameIndex=%d. IndexOutOfRange=%d. %d out of %d map elements were uninitialized, %d of these were interpolated',
    [ mapToSameIndex, IndexOutOfRange, unset, High(Map), interpolated ])));
{$ENDIF}

  for i := 0 to High(Map) do
  begin
    if Map[i] = -1 then
      Map[i] := 1;
  end;
{$IFDEF DEBUG}
  OutputDebugString(PChar(Format('TRadialDistortionTransformation.PrepareReverseMap: MinValue(Map)=%f MaxValue(Map)=%f', [ MinValue(Map), MaxValue(Map) ])));
{$ENDIF}
end;

procedure TRadialDistortionTransformation.PrepareTransform;
var
  r: TRect;
begin
  if GR32.IsRectEmpty(SrcRect) then
    raise Exception.Create(RCStrSrcRectIsEmpty);
  TransformValid := not GR32.IsRectEmpty(SrcRect);
  if Not TransformValid then
    Exit;

  // center / focal point relative to which all (un)distortions are calculated
  FFocalPoint.x := (SrcRect.Right + SrcRect.Left) / 2;
  FFocalPoint.y := (SrcRect.Bottom + SrcRect.Top) / 2;

  r := MakeRect(SrcRect);
  r_0 := Sqrt(2*Sqr(Min(r.Right - r.Left, r.Bottom - r.Top)))/2;

  PrepareReverseMap;
end;

function TRadialDistortionTransformation.LookUpReverseMap(const r_tgt: TFloat): TFloat;
var
  index: Integer;
begin
  index := Trunc((r_tgt-r_tgt_min) / (r_tgt_max-r_tgt_min) * High(Map));
  if not InRange(index, 0, High(Map)) then
    raise Exception.Create(Format('TRadialDistortionTransformation.LookUpReverseMap: Index %d out of range (0..%d)', [ index, MapElements ]));
  Result := Map[index];
end;

procedure TRadialDistortionTransformation.ReverseTransformFloat(DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
var
  r_tgt, r_src: Single;
  d: TFloatPoint;
begin
  d.x := DstX;
  d.y := DstY;
  r_tgt := Distance(FFocalPoint, d)/r_0;

  r_src := LookUpReverseMap(r_tgt);

  SrcX := FFocalPoint.X + (d.X-FFocalPoint.X) / r_src;
  SrcY := FFocalPoint.Y + (d.Y-FFocalPoint.Y) / r_src;
end;

procedure TRadialDistortionTransformation.SetCoefficient1(const Value: TFloat);
begin
  FCoefficient1 := Value;
  Changed;
end;

procedure TRadialDistortionTransformation.SetCoefficient2(const Value: TFloat);
begin
  FCoefficient2 := Value;
  Changed;
end;

procedure TRadialDistortionTransformation.SetScale(const Value: TFloat);
begin
  FScale := Value;
  Changed;
end;

procedure TRadialDistortionTransformation.SetMapElements(const Value: Integer);
begin
  FMapElements := Value;
  Changed;
end;

procedure TRadialDistortionTransformation.TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat);
var
  r_tgt, r_src: Single;
  d: TFloatPoint;
begin
  d.x := SrcX;
  d.y := SrcY;
  r_src := Distance(FFocalPoint, d)/r_0;
  r_tgt := Scale*(1 + FCoefficient1 * Sqr(r_src) + FCoefficient2 * Power(r_src, 4));
  DstX := FFocalPoint.X + (d.X-FFocalPoint.X) * r_tgt;
  DstY := FFocalPoint.Y + (d.Y-FFocalPoint.Y) * r_tgt;
end;


//------------------------------------------------------------------------------
//
//      TRemapTransformation
//
//------------------------------------------------------------------------------
constructor TRemapTransformation.Create;
begin
  inherited;
  FScalingFixed := FixedPoint(1, 1);
  FScalingFloat := FloatPoint(1, 1);
  FOffset := FloatPoint(0,0);
  FVectorMap := TVectorMap.Create;
  // Ensuring initial setup to avoid exceptions
  FVectorMap.SetSize(1, 1);
end;

destructor TRemapTransformation.Destroy;
begin
  FVectorMap.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TRemapTransformation.GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect;
const
  InfRect: TFloatRect = (Left: -Infinity; Top: -Infinity; Right: Infinity; Bottom: Infinity);
begin
  // We can't predict the ultimate bounds without transforming each vector in
  // the vector map, return the absolute biggest possible transformation bounds
  Result := InfRect;
end;

function TRemapTransformation.HasTransformedBounds: Boolean;
begin
  Result := False;
end;

procedure TRemapTransformation.PrepareTransform;
begin
  if GR32.IsRectEmpty(SrcRect) then
    raise Exception.Create(RCStrSrcRectIsEmpty);
  if GR32.IsRectEmpty(FMappingRect) then
    raise Exception.Create(RCStrMappingRectIsEmpty);
  with SrcRect do
  begin
    FSrcTranslationFloat.X := Left;
    FSrcTranslationFloat.Y := Top;
    FSrcScaleFloat.X := (Right - Left) / (FVectorMap.Width - 1);
    FSrcScaleFloat.Y := (Bottom - Top) / (FVectorMap.Height - 1);
    FSrcTranslationFixed := FixedPoint(FSrcTranslationFloat);
    FSrcScaleFixed := FixedPoint(FSrcScaleFloat);
  end;

  with FMappingRect do
  begin
    FDstTranslationFloat.X := Left;
    FDstTranslationFloat.Y := Top;
    FDstScaleFloat.X := (FVectorMap.Width - 1) / (Right - Left);
    FDstScaleFloat.Y := (FVectorMap.Height - 1) / (Bottom - Top);
    FCombinedScalingFloat.X := FDstScaleFloat.X * FScalingFloat.X;
    FCombinedScalingFloat.Y := FDstScaleFloat.Y * FScalingFloat.Y;
    FCombinedScalingFixed := FixedPoint(FCombinedScalingFloat);
    FDstTranslationFixed := FixedPoint(FDstTranslationFloat);
    FDstScaleFixed := FixedPoint(FDstScaleFloat);
  end;
  TransformValid := True;
end;

procedure TRemapTransformation.ReverseTransformFixed(DstX, DstY: TFixed;
  out SrcX, SrcY: TFixed);
begin
  with FVectorMap.FixedVectorX[DstX - FOffsetFixed.X, DstY - FOffsetFixed.Y] do
  begin
    DstX := DstX - FDstTranslationFixed.X;
    DstX := FixedMul(DstX , FDstScaleFixed.X);
    DstX := DstX + FixedMul(X, FCombinedScalingFixed.X);
    DstX := FixedMul(DstX, FSrcScaleFixed.X);
    SrcX := DstX + FSrcTranslationFixed.X;

    DstY := DstY - FDstTranslationFixed.Y;
    DstY := FixedMul(DstY, FDstScaleFixed.Y);
    DstY := DstY + FixedMul(Y, FCombinedScalingFixed.Y);
    DstY := FixedMul(DstY, FSrcScaleFixed.Y);
    SrcY := DstY + FSrcTranslationFixed.Y;
  end;
end;

procedure TRemapTransformation.ReverseTransformFloat(DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
begin
  with FVectorMap.FloatVectorF[DstX - FOffset.X, DstY - FOffset.Y] do
  begin
    DstX := DstX - FDstTranslationFloat.X;
    DstY := DstY - FDstTranslationFloat.Y;
    DstX := DstX * FDstScaleFloat.X;
    DstY := DstY * FDstScaleFloat.Y;

    DstX := DstX + X * FCombinedScalingFloat.X;
    DstY := DstY + Y * FCombinedScalingFloat.Y;

    DstX := DstX * FSrcScaleFloat.X;
    DstY := DstY * FSrcScaleFloat.Y;
    SrcX := DstX + FSrcTranslationFloat.X;
    SrcY := DstY + FSrcTranslationFloat.Y;
  end;
end;

procedure TRemapTransformation.ReverseTransformInt(DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
begin
  with FVectorMap.FixedVector[DstX - FOffsetInt.X, DstY - FOffsetInt.Y] do
  begin
    DstX := DstX * FixedOne - FDstTranslationFixed.X;
    DstY := DstY * FixedOne - FDstTranslationFixed.Y;
    DstX := FixedMul(DstX, FDstScaleFixed.X);
    DstY := FixedMul(DstY, FDstScaleFixed.Y);

    DstX := DstX + FixedMul(X, FCombinedScalingFixed.X);
    DstY := DstY + FixedMul(Y, FCombinedScalingFixed.Y);

    DstX := FixedMul(DstX, FSrcScaleFixed.X);
    DstY := FixedMul(DstY, FSrcScaleFixed.Y);
    SrcX := FixedRound(DstX + FSrcTranslationFixed.X);
    SrcY := FixedRound(DstY + FSrcTranslationFixed.Y);
  end;
end;

procedure TRemapTransformation.Scale(Sx, Sy: TFloat);
begin
  FScalingFixed.X := Fixed(Sx);
  FScalingFixed.Y := Fixed(Sy);
  FScalingFloat.X := Sx;
  FScalingFloat.Y := Sy;
  Changed;  
end;

procedure TRemapTransformation.SetMappingRect(Rect: TFloatRect);
begin
  FMappingRect := Rect;
  Changed;
end;

procedure TRemapTransformation.SetOffset(const Value: TFloatVector);
begin
  FOffset := Value;
  FOffsetInt := Point(Value);
  FOffsetFixed := FixedPoint(Value);
  Changed;
end;


//------------------------------------------------------------------------------
//
//      TSphereTransformation
//
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Utilities
//------------------------------------------------------------------------------
procedure Modulo2Pi(var Angle: TFloat); {Result is between 0 and 2PI }
{$if defined(PUREPASCAL) or (not defined(TARGET_x86))}
begin
  Angle := GR32_Math.FloatMod(Angle, PI*2);
end;
{$else}
asm
        FLDPI
        FADD    ST,ST               // 2PI
        FLD     DWORD ptr [Angle]
        FPREM                       // calc Modulo
        FLDZ
        FCOMIP  ST,ST(1)            // Compare 0 and Modulo (+pop 0)
        JNB     @@1                 // if Modulo >= 0 then
        FSTP    DWORD ptr [Angle]   //  return Modulo...
        FSTP    ST(0)               //  POP the rest (2PI)
        JMP     @@2
@@1:    FADDP                       // add Modulo and Rest (2PI)
        FSTP    DWORD ptr [Angle]   // Modulo+2PI...
@@2:    FWAIT
end;
{$ifend}


constructor TSphereTransformation.Create;
begin
  inherited;
  FRadius := 1;
end;

//------------------------------------------------------------------------------

function TSphereTransformation.GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect;
begin
  // There is not direct relation between SourceRect and DestRect !
  // During transformation process this rect will be clipped.
  Result.Left := FCenter.X - FRadius;
  Result.Top := FCenter.Y - FRadius;
  Result.Bottom := FCenter.Y + FRadius;
  Result.Right := FCenter.X + FRadius;
end;

function TSphereTransformation.HasTransformedBounds: Boolean;
begin
  Result := False;
end;

function TSphereTransformation.IsInSphere(CartesianX, CartesianY: TFloat): boolean;
begin
  if not TransformValid then
    PrepareTransform;

  CartesianX := CartesianX - FCenter.X;
  CartesianY := CartesianY - FCenter.Y;

  Result := (FSquareRadius >= (CartesianX * CartesianX + CartesianY * CartesianY));
end;

procedure TSphereTransformation.PrepareTransform;
begin
  // Invariants during transformation
  FMapWidth := (SrcRect.Width - 1) / (2 * PI);
  FMapHeight := (SrcRect.Height - 1) / PI;
  FSquareRadius := Sqr(FRadius);
  GR32_Math.SinCos(FLattitude, FLattitudeSin, FLattitudeCos);
  FLattitudeSinInvRadius := -FLattitudeSin / FRadius;
  FLattitudeCosInvRadius := FLattitudeCos / FRadius;
  FSrcRectTop := SrcRect.Top;
  FSrcRectLeft := SrcRect.Left;

  TransformValid := True;
end;

procedure TSphereTransformation.ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat);
{$if defined(PUREPASCAL) or (not defined(TARGET_x86))}
var
  Dist: TFloat;
begin
  // Screen projection on sphere
  DstX := DstX - FCenter.X; // = Y
  DstY := FCenter.Y - DstY; // = Z
  Dist := DstX * DstX + DstY * DstY;

  if (Dist > FSquareRadius) then // Not projectable on the sphere
  begin
    SrcX := -1;
    SrcY := -1;
    Exit;
  end;

  Dist := Sqrt(FSquareRadius - Dist);

  // Apply rotations
  DstX := Arctan2(DstX, Dist * FLattitudeCos + DstY * FLattitudeSin) + FLongitude;
  Modulo2Pi(DstX);

  // Map projection
  SrcX := SrcRect.Left + DstX * FMapWidth;
  SrcY := SrcRect.Top + ArcCos(DstY * FLattitudeCosInvRadius + Dist * FLattitudeSinInvRadius) * FMapHeight;
end;
{$else}
{Assembler version (FPU) ... 4% faster on a P4 }
asm
// screen projection on sphere
//  DstX := DstX - FCenterX; // = Y
    fld   DstX               // DstX
    fsub  [eax].FCenter.X // DstX'
//  DstY := FCenterY - DstY; // = Z
    fld   [eax].FCenter.Y // FCenterY | DstX'
    fsub  DstY               // DstY'    | DstX'
//  x := DstX * DstX + DstY * DstY;
    fld   st(0)                    // Z    | Z    | Y
    fmul  st(0),st(1)              // ZZ   | Z    | Y
    fld   st(2)                    // Y    | ZZ   | Z   | Y
    fmul  st(0),st(3)
    faddp                          // X'   | Z    | Y
//  if (FSquareRadius < x) then // not projetable in the sphere.
    fld [eax].FSquareRadius
    fcomi st(0),st(1) // st(0) < st(1)
    jnbe   @@1
    fstp  st(0)
    fstp  st(0)
    fstp  st(0)
    fstp  st(0)
//    SrcX := -1;
    mov   [SrcX],$bf800000
//    SrcY := -1;
    mov   [SrcY],$bf800000
//    Exit;
    jmp @@fin
@@1:
//  x := sqrt(FSquareRadius - x);
    fsubrp
    fsqrt                          // X    | Z    | Y
// apply rotations
//  DstX := Arctan2(Y,X * FLattitudeCos + Z * FLattitudeSin) + FLongitude; // Lon
    fxch  st(2)                   // Y     | Z    | Y
    fld   st(2)                   // X     | Y    | Z    | X
    fmul  [eax].FLattitudeCos
    fld   st(2)                   // Z     | Xx.  | Y    | Z    | X
    fmul  [eax].FLattitudeSin     // Zx.   | Xx.  | Y    | Z    | X
    faddp                         // Xx+Zx | Y    | Z    | X
    fpatan
    fadd  [eax].FLongitude  // DstX  | Z    | X
//  if DstX > PI2S then
    fldpi
    fadd  st(0),st(0)       // 2PI   | DstX | Z    | X
    fcomi st(0),st(1) // st(0) < st(1)
    jnb   @@test2
//    DstX := DstX - PI2S
    fsubp st(1),st(0)
    jmp   @@testfin
//  else if DstX < 0 then
@@test2:
    fldz
    fcomip st(0),st(2) // st(0) < st(2)
    jb @@test3
//    DstX := DstX + PI2S;
    faddp
    jmp   @@testfin
@@test3:
    fstp  st(0)
@@testfin:
// Map projection
//  SrcX := DstX * FMapWidth;
    fmul  [eax].FMapWidth
    FADD  [eax].FSrcRectLeft
    fstp  dword ptr [SrcX]// Z    | X
//  SrcY := ArcCos(Z * FLattitudeCosInvRadius + x * FLattitudeSinInvRadius) * FMapHeight;
    fmul  [eax].FLattitudeCosInvRadius
    fxch
    fmul  [eax].FLattitudeSinInvRadius
    faddp
    FLD1                 // 1      | X
    FLD   ST(1)          // X      | 1      | X
    FMUL  ST(0),ST(0)    // X²     | 1      | X
    FSUBP ST(1),ST(0)    // 1 - X² | X
    FABS                 //<- avoid rounding errors...
    FSQRT                // sqrt(.)| X
    FXCH  st(1)
    FPATAN               // result |
    fmul  [eax].FMapHeight
    FADD  [eax].FSrcRectTop
    fstp  dword ptr [SrcY]
@@fin:
    fwait
end;
{$ifend}

function TSphereTransformation.ScreenCoordinate(var X, Y: TFloat): boolean;
var
  SinLong, CosLong, SinLat, CosLat: TFloat;
begin
  if not TransformValid then
    PrepareTransform;

  GR32_Math.SinCos(X - FLongitude, SinLong, CosLong);
  GR32_Math.SinCos(Y, SinLat, CosLat);

  Result := (SinLat * CosLong * FLattitudeCos >= CosLat * FLattitudeSin);

  if Result then
  begin
    X := FCenter.X + FRadius * SinLat * SinLong;
    Y := FCenter.Y - FRadius * (SinLat * CosLong * FLattitudeSin + CosLat * FLattitudeCos);
  end;
end;

procedure TSphereTransformation.SetCenter(const Value: TFloatPoint);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    Changed;
  end;
end;

procedure TSphereTransformation.SetLattitude(const Value: TFloat);
begin
  if FLattitude <> Value then
  begin
    FLattitude := Value;
    Modulo2Pi(FLattitude);
    Changed;
  end;
end;

procedure TSphereTransformation.SetLongitude(const Value: TFloat);
begin
  if FLongitude <> Value then
  begin
    FLongitude := Value;
    Modulo2Pi(FLongitude);
    Changed;
  end;
end;

procedure TSphereTransformation.SetRadius(const Value: TFloat);
begin
  if (Value > 0) and (FRadius <> Value) then
  begin
    FRadius := Value;
    Changed;
  end;
end;

function TSphereTransformation.SphericalCoordinate(CartesianX, CartesianY: TFloat): TFloatPoint;
var
  Dist: TFloat;
begin
  if not TransformValid then
    PrepareTransform;

  // Screen projection on sphere
  CartesianX := CartesianX - FCenter.X; // = Y
  CartesianY := FCenter.Y - CartesianY; // = Z
  Dist := CartesianX * CartesianX + CartesianY * CartesianY;

  if (Dist > FSquareRadius) then // Not projectable in the sphere.
  begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  end;

  Dist := Sqrt(FSquareRadius - Dist);

  // Apply rotations
  Result.X := Arctan2(CartesianX, Dist * FLattitudeCos + CartesianY * FLattitudeSin) + FLongitude;
  Modulo2Pi(Result.X);
  Result.Y := ArcCos(CartesianY * FLattitudeCosInvRadius + Dist * FLattitudeSinInvRadius) - (PI / 2);
end;


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------

var
  TransformsRegistry: TFunctionRegistry;

procedure RegisterBindings;
begin
  TransformsRegistry := NewRegistry('GR32_Transforms bindings');
  TransformsRegistry.RegisterBinding(@@DET_2x2_32);
  TransformsRegistry.RegisterBinding(@@DET_3x3_32);
  TransformsRegistry.RegisterBinding(@@DET_2x2_64);

  // DET_2x2_32
  TransformsRegistry.Add(@@DET_2x2_32, @DET_2x2_32_Pas);
{$IFNDEF PUREPASCAL}
  TransformsRegistry.Add(@@DET_2x2_32, @DET_2x2_32_ASM);
//  TransformsRegistry.Add(@@DET_2x2_32, @DET_2x2_32_SSE2, [isSSE2]);
{$ENDIF}

  // DET_2x2_64
  TransformsRegistry.Add(@@DET_2x2_64, @DET_2x2_64_Pas);
{$IFNDEF PUREPASCAL}
  TransformsRegistry.Add(@@DET_2x2_64, @DET_2x2_64_ASM);
//  TransformsRegistry.Add(@@DET_2x2_64, @DET_2x2_64_SSE2, [isSSE2]);
{$ENDIF}

  // DET_3x3_32
  TransformsRegistry.Add(@@DET_3x3_32, @DET_3x3_32_Pas);

  TransformsRegistry.RebindAll;
end;

//------------------------------------------------------------------------------

initialization
  RegisterBindings;

end.
