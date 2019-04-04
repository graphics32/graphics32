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
 * Contributor(s):
 *   Andre Beckedorf <Andre@metaException.de>
 *   Mattias Andersson <Mattias@Centaurix.com>
 *   J. Tulach <tulach@position.cz>
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Peter Larson
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Classes, Types, GR32, GR32_VectorMaps, GR32_Rasterizers;

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

  TRemapTransformation = class(TTransformation)
  private
    FVectorMap : TVectorMap;
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

function TransformPoints(Points: TArrayOfArrayOfFixedPoint; Transformation: TTransformation): TArrayOfArrayOfFixedPoint;

procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation); overload;
procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation;
  const DstClip: TRect); overload;
procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation;
  Rasterizer: TRasterizer); overload;
procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation;
  Rasterizer: TRasterizer; const DstClip: TRect); overload;

procedure RasterizeTransformation(Vectormap: TVectormap;
  Transformation: TTransformation; DstRect: TRect;
  CombineMode: TVectorCombineMode = vcmAdd;
  CombineCallback: TVectorCombineEvent = nil);

procedure SetBorderTransparent(ABitmap: TCustomBitmap32; ARect: TRect);

{ FullEdge controls how the bitmap is resampled }
var
  FullEdge: Boolean = True;

resourcestring
  RCStrReverseTransformationNotImplemented = 'Reverse transformation is not implemented in %s.';
  RCStrForwardTransformationNotImplemented = 'Forward transformation is not implemented in %s.';
  RCStrTopBottomCurveNil = 'Top or bottom curve is nil';

implementation

uses
  Math, GR32_Blend, GR32_LowLevel, GR32_Math, GR32_Bindings,
  GR32_Resamplers;

resourcestring
  RCStrSrcRectIsEmpty = 'SrcRect is empty!';
  RCStrMappingRectIsEmpty = 'MappingRect is empty!';
  RStrStackEmpty = 'Stack empty';

type
  {provides access to proctected members of TCustomBitmap32 by typecasting}
  TTransformationAccess = class(TTransformation);

var
  DET32: function(a1, a2, b1, b2: Single): Single;
  DET64: function(a1, a2, b1, b2: Double): Double;


{ A bit of linear algebra }

function DET32_Pas(a1, a2, b1, b2: Single): Single; overload;
begin
  Result := a1 * b2 - a2 * b1;
end;

function DET64_Pas(a1, a2, b1, b2: Double): Double; overload;
begin
  Result := a1 * b2 - a2 * b1;
end;

{$IFNDEF PUREPASCAL}
function DET32_ASM(a1, a2, b1, b2: Single): Single; overload;
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

function DET64_ASM(a1, a2, b1, b2: Double): Double; overload;
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

{ implementation of detereminant for TFloat precision }

function _DET(a1, a2, b1, b2: TFloat): TFloat; overload; {$IFDEF UseInlining} inline; {$ENDIF}
begin
  Result := a1 * b2 - a2 * b1;
end;

function _DET(a1, a2, a3, b1, b2, b3, c1, c2, c3: TFloat): TFloat; overload; {$IFDEF UseInlining} inline; {$ENDIF}
begin
  Result :=
    a1 * (b2 * c3 - b3 * c2) -
    b1 * (a2 * c3 - a3 * c2) +
    c1 * (a2 * b3 - a3 * b2);
end;

procedure Adjoint(var M: TFloatMatrix);
var
  Tmp: TFloatMatrix;
begin
  Tmp := M;

  M[0,0] :=  _DET(Tmp[1,1], Tmp[1,2], Tmp[2,1], Tmp[2,2]);
  M[0,1] := -_DET(Tmp[0,1], Tmp[0,2], Tmp[2,1], Tmp[2,2]);
  M[0,2] :=  _DET(Tmp[0,1], Tmp[0,2], Tmp[1,1], Tmp[1,2]);

  M[1,0] := -_DET(Tmp[1,0], Tmp[1,2], Tmp[2,0], Tmp[2,2]);
  M[1,1] :=  _DET(Tmp[0,0], Tmp[0,2], Tmp[2,0], Tmp[2,2]);
  M[1,2] := -_DET(Tmp[0,0], Tmp[0,2], Tmp[1,0], Tmp[1,2]);

  M[2,0] :=  _DET(Tmp[1,0], Tmp[1,1], Tmp[2,0], Tmp[2,1]);
  M[2,1] := -_DET(Tmp[0,0], Tmp[0,1], Tmp[2,0], Tmp[2,1]);
  M[2,2] :=  _DET(Tmp[0,0], Tmp[0,1], Tmp[1,0], Tmp[1,1]);
end;

function Determinant(const M: TFloatMatrix): TFloat;
begin
  Result := _DET(M[0,0], M[1,0], M[2,0],
                 M[0,1], M[1,1], M[2,1],
                 M[0,2], M[1,2], M[2,2]);
end;

procedure Scale(var M: TFloatMatrix; Factor: TFloat);
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      M[i,j] := M[i,j] * Factor;
end;

procedure Invert(var M: TFloatMatrix);
var
  Det: TFloat;
begin
  Det := Determinant(M);
  if Abs(Det) < 1E-5 then M := IdentityMatrix
  else
  begin
    Adjoint(M);
    Scale(M, 1 / Det);
  end;
end;

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

function VectorTransform(const M: TFloatMatrix; const V: TVector3f): TVector3f;
begin
  Result[0] := M[0,0] * V[0] + M[1,0] * V[1] + M[2,0] * V[2];
  Result[1] := M[0,1] * V[0] + M[1,1] * V[1] + M[2,1] * V[2];
  Result[2] := M[0,2] * V[0] + M[1,2] * V[1] + M[2,2] * V[2];
end;

{ Transformation functions }

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

procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation);
var
  Rasterizer: TRasterizer;
begin
  Rasterizer := DefaultRasterizerClass.Create;
  try
    Transform(Dst, Src, Transformation, Rasterizer);
  finally
    Rasterizer.Free;
  end;
end;

procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation; const DstClip: TRect);
var
  Rasterizer: TRasterizer;
begin
  Rasterizer := DefaultRasterizerClass.Create;
  try
    Transform(Dst, Src, Transformation, Rasterizer, DstClip);
  finally
    Rasterizer.Free;
  end;
end;

procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation;
  Rasterizer: TRasterizer);
begin
  Transform(Dst, Src, Transformation, Rasterizer, Dst.BoundsRect);
end;

procedure Transform(Dst, Src: TCustomBitmap32; Transformation: TTransformation;
  Rasterizer: TRasterizer; const DstClip: TRect);
var
  DstRect: TRect;
  Transformer: TTransformer;
begin
  GR32.IntersectRect(DstRect, DstClip, Dst.ClipRect);

  if (DstRect.Right < DstRect.Left) or (DstRect.Bottom < DstRect.Top) then Exit;

  if not Dst.MeasuringMode then
  begin
    Transformer := TTransformer.Create(Src.Resampler, Transformation);
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

{ TTransformation }

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
  if not TransformValid then PrepareTransform;
  ReverseTransformFloat(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.ReverseTransform(const P: TFixedPoint): TFixedPoint;
begin
  if not TransformValid then PrepareTransform;
  ReverseTransformFixed(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.ReverseTransform(const P: TPoint): TPoint;
begin
  if not TransformValid then PrepareTransform;
  ReverseTransformInt(P.X, P.Y, Result.X, Result.Y);
end;

procedure TTransformation.ReverseTransformFixed(DstX, DstY: TFixed;
  out SrcX, SrcY: TFixed);
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
  if not TransformValid then PrepareTransform;
  TransformFloat(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.Transform(const P: TFixedPoint): TFixedPoint;
begin
  if not TransformValid then PrepareTransform;
  TransformFixed(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.Transform(const P: TPoint): TPoint;
begin
  if not TransformValid then PrepareTransform;
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


{ TNestedTransformation }

constructor TNestedTransformation.Create;
begin
  FItems := TList.Create;
end;

destructor TNestedTransformation.Destroy;
begin
  if Assigned(FItems) then Clear;
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


{ T3x3Transformation }

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


{ TAffineTransformation }

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
  if (Cx <> 0) or (Cy <> 0) then Translate(-Cx, -Cy);
  Alpha := DegToRad(Alpha);
  GR32_Math.SinCos(Alpha, S, C);
  M := IdentityMatrix;
  M[0, 0] := C;   M[1, 0] := S;
  M[0, 1] := -S;  M[1, 1] := C;
  FMatrix := Mult(M, Matrix);
  if (Cx <> 0) or (Cy <> 0) then Translate(Cx, Cy);
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


{ TProjectiveTransformation }

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

  if Z = 0 then Exit;

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

  if Z = 0 then Exit;

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

  if Z = 0 then Exit;

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


{ TTwirlTransformation }

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

{ TBloatTransformation }

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

{ TFishEyeTransformation }

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
    if Faw <> 0 then Faw := 1 / Faw;
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


{ TPolarTransformation }

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
  if Rt2 <> 0 then Rt2 := 1 / Sx else Rt2 := 0.00000001;
  Rt2 := Rt2 * 2 * Pi;

  Rr := Sy;
  if Rr <> 0 then Rr := 1 / Rr else Rr := 0.00000001;

  Rcx := Cx;
  if Rcx <> 0 then Rcx := 1 / Rcx else Rcx := 0.00000001;

  Rcy := Cy;
  if Rcy <> 0 then Rcy := 1 / Rcy else Rcy := 0.00000001;

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
  if Theta < 0 then Theta := Theta + PI2;

  SrcX := SrcRect.Left + Theta * Rt;
  SrcY := SrcRect.Bottom - GR32_Math.Hypot(Dcx, Dcy) * Sy;
end;


procedure TPolarTransformation.SetPhase(const Value: TFloat);
begin
  FPhase := Value;
  Changed;
end;


{ TPathTransformation }

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
      else if I > 1 then
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
      else if I > 1 then
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
  while (FTopHypot[I].Dist < fx) and (I < H) do Inc(I);


  with FTopHypot[I] do
    r := (Dist - fx) * RecDist;

  dx := (FTopCurve[I - 1].X - FTopCurve[I].X);
  dy := (FTopCurve[I - 1].Y - FTopCurve[I].Y);
  Tx := FTopCurve[I].X + r * dx;
  Ty := FTopCurve[I].Y + r * dy;

  fx := X * FBottomLength;
  I := 1;
  H := High(FBottomHypot);
  while (FBottomHypot[I].Dist < fx) and (I < H) do Inc(I);


  with FBottomHypot[I] do
    r := (Dist - fx) * RecDist;

  dx := (FBottomCurve[I - 1].X - FBottomCurve[I].X);
  dy := (FBottomCurve[I - 1].Y - FBottomCurve[I].Y);
  Bx := FBottomCurve[I].X + r * dx;
  By := FBottomCurve[I].Y + r * dy;

  DstX := Tx + Y * (Bx - Tx);
  DstY := Ty + Y * (By - Ty);
end;


{ TDisturbanceTransformation }

function TDisturbanceTransformation.GetTransformedBounds(
  const ASrcRect: TFloatRect): TFloatRect;
begin
  Result := ASrcRect;
  InflateRect(Result, 0.5 * FDisturbance, 0.5 * FDisturbance);
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

{ TRemapTransformation }

constructor TRemapTransformation.Create;
begin
  inherited;
  FScalingFixed := FixedPoint(1, 1);
  FScalingFloat := FloatPoint(1, 1);
  FOffset := FloatPoint(0,0);
  FVectorMap := TVectorMap.Create;
  //Ensuring initial setup to avoid exceptions
  FVectorMap.SetSize(1, 1);
end;

destructor TRemapTransformation.Destroy;
begin
  FVectorMap.Free;
  inherited;
end;

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
  if IsRectEmpty(SrcRect) then raise Exception.Create(RCStrSrcRectIsEmpty);
  if IsRectEmpty(FMappingRect) then raise Exception.Create(RCStrMappingRectIsEmpty);
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
  if GR32.IsRectEmpty(DstRect) then Exit;

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

{ Matrix conversion routines }

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

{CPU target and feature Function templates}

const
  FID_DETERMINANT32 = 0;
  FID_DETERMINANT64 = 1;

{Complete collection of unit templates}

var
  Registry: TFunctionRegistry;

procedure RegisterBindings;
begin
  Registry := NewRegistry('GR32_Transforms bindings');
  Registry.RegisterBinding(FID_DETERMINANT32, @@DET32);

  Registry.Add(FID_DETERMINANT32, @DET32_Pas, []);
  {$IFNDEF PUREPASCAL}
  Registry.Add(FID_DETERMINANT32, @DET32_ASM, []);
//  Registry.Add(FID_DETERMINANT32, @DET32_SSE2, [ciSSE2]);
  {$ENDIF}

  Registry.RegisterBinding(FID_DETERMINANT64, @@DET64);

  Registry.Add(FID_DETERMINANT64, @DET64_Pas, []);
  {$IFNDEF PUREPASCAL}
  Registry.Add(FID_DETERMINANT64, @DET64_ASM, []);
//  Registry.Add(FID_DETERMINANT64, @DET64_SSE2, [ciSSE2]);
  {$ENDIF}

  Registry.RebindAll;
end;

initialization
  RegisterBindings;

end.
