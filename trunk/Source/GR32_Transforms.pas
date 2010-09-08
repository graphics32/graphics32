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
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Classes, GR32, GR32_Blend, GR32_VectorMaps, GR32_Rasterizers;

type
  ETransformError = class(Exception);
  ETransformNotImplemented = class(Exception);

type
  TFloatMatrix = array[0..2, 0..2] of TFloat;     // 3x3 TFloat precision
  TFixedMatrix = array[0..2, 0..2] of TFixed;     // 3x3 fixed precision

const
  IdentityMatrix: TFloatMatrix = (
    (1, 0, 0),
    (0, 1, 0),
    (0, 0, 1));

type
  TVector3f = array[0..2] of TFloat;
  TVector3i = array[0..2] of Integer;

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

  TAffineTransformation = class(TTransformation)
  protected
    FInverseMatrix: TFloatMatrix;
    FFixedMatrix, FInverseFixedMatrix: TFixedMatrix;
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;
    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); override;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); override;
  public
    Matrix: TFloatMatrix;
    constructor Create; virtual;
    function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; override;
    procedure Clear;
    procedure Rotate(Cx, Cy, Alpha: TFloat); // degrees
    procedure Skew(Fx, Fy: TFloat);
    procedure Scale(Sx, Sy: TFloat);
    procedure Translate(Dx, Dy: TFloat);
  end;

  TProjectiveTransformation = class(TTransformation)
  private
    Wx0, Wx1, Wx2, Wx3: TFloat;
    Wy0, Wy1, Wy2, Wy3: TFloat;
    procedure SetX0(Value: TFloat);
    procedure SetX1(Value: TFloat);
    procedure SetX2(Value: TFloat);
    procedure SetX3(Value: TFloat);
    procedure SetY0(Value: TFloat);
    procedure SetY1(Value: TFloat);
    procedure SetY2(Value: TFloat);
    procedure SetY3(Value: TFloat);
  protected
    FMatrix, FInverseMatrix: TFloatMatrix;
    FFixedMatrix, FInverseFixedMatrix: TFixedMatrix;
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;
    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); override;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); override;
  public
    function  GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; override;
  published
    property X0: TFloat read Wx0 write SetX0;
    property X1: TFloat read Wx1 write SetX1;
    property X2: TFloat read Wx2 write SetX2;
    property X3: TFloat read Wx3 write SetX3;
    property Y0: TFloat read Wy0 write SetY0;
    property Y1: TFloat read Wy1 write SetY1;
    property Y2: TFloat read Wy2 write SetY2;
    property Y3: TFloat read Wy3 write SetY3;
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
    constructor Create; virtual;
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
  public
    constructor Create; virtual;
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
    constructor Create; virtual;
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


implementation

uses
  GR32_LowLevel, GR32_System, GR32_Resamplers, Math, GR32_Math;

type
  {provides access to proctected members of TCustomBitmap32 by typecasting}
  TTransformationAccess = class(TTransformation);
  TCustomBitmap32Access = class(TCustomBitmap32);


{ A bit of linear algebra }

function _DET(a1, a2, b1, b2: TFloat): TFloat; overload;
begin
  Result := a1 * b2 - a2 * b1;
end;

function _DET(a1, a2, a3, b1, b2, b3, c1, c2, c3: TFloat): TFloat; overload;
begin
  Result :=
    a1 * (b2 * c3 - b3 * c2) -
    b1 * (a2 * c3 - a3 * c2) +
    c1 * (a2 * b3 - a3 * b2);
end;

procedure Adjoint(var M: TFloatMatrix);
var
  a1, a2, a3: TFloat;
  b1, b2, b3: TFloat;
  c1, c2, c3: TFloat;
begin
  a1 := M[0,0]; a2:= M[0,1]; a3 := M[0,2];
  b1 := M[1,0]; b2:= M[1,1]; b3 := M[1,2];
  c1 := M[2,0]; c2:= M[2,1]; c3 := M[2,2];

  M[0,0]:= _DET(b2, b3, c2, c3);
  M[0,1]:=-_DET(a2, a3, c2, c3);
  M[0,2]:= _DET(a2, a3, b2, b3);

  M[1,0]:=-_DET(b1, b3, c1, c3);
  M[1,1]:= _DET(a1, a3, c1, c3);
  M[1,2]:=-_DET(a1, a3, b1, b3);

  M[2,0]:= _DET(b1, b2, c1, c2);
  M[2,1]:=-_DET(a1, a2, c1, c2);
  M[2,2]:= _DET(a1, a2, b1, b2);
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
  IntersectRect(DstRect, DstClip, Dst.ClipRect);

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
  IntersectRect(ARect, ARect, ABitmap.BoundsRect);
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
  // ReverseTransformFloat is the top precisionlevel, all decendants must override at least this level!
  raise ETransformNotImplemented.Create(Format('Reverse transformation is not implemented in %s.', [Self.Classname]));
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
  If not TransformValid then PrepareTransform;
  TransformFloat(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.Transform(const P: TFixedPoint): TFixedPoint;
begin
  If not TransformValid then PrepareTransform;
  TransformFixed(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.Transform(const P: TPoint): TPoint;
begin
  If not TransformValid then PrepareTransform;
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
  // TransformFloat is the top precisionlevel, all decendants must override at least this level!
  raise ETransformNotImplemented.Create(Format('Forward transformation is not implemented in %s.', [Self.Classname]));
end;

procedure TTransformation.TransformInt(SrcX, SrcY: Integer; out DstX, DstY: Integer);
var
  X, Y: TFixed;
begin
  TransformFixed(SrcX shl 16, SrcY shl 16, X, Y);
  DstX := FixedRound(X);
  DstY := FixedRound(Y);
end;

{ TAffineTransformation }

procedure TAffineTransformation.Clear;
begin
  Matrix := IdentityMatrix;
  Changed;
end;

constructor TAffineTransformation.Create;
begin
  Clear;
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

procedure TAffineTransformation.PrepareTransform;
begin
  FInverseMatrix := Matrix;
  Invert(FInverseMatrix);

  // calculate a fixed point (65536) factors
  FInverseFixedMatrix := FixedMatrix(FInverseMatrix);
  FFixedMatrix := FixedMatrix(Matrix);

  TransformValid := True;
end;

procedure TAffineTransformation.Rotate(Cx, Cy, Alpha: TFloat);
var
  S, C: TFloat;
  M: TFloatMatrix;
begin
  if (Cx <> 0) or (Cy <> 0) then Translate(-Cx, -Cy);
  Alpha := DegToRad(Alpha);
  S := Sin(Alpha); C := Cos(Alpha);
  M := IdentityMatrix;
  M[0,0] := C;   M[1,0] := S;
  M[0,1] := -S;  M[1,1] := C;
  Matrix := Mult(M, Matrix);
  if (Cx <> 0) or (Cy <> 0) then Translate(Cx, Cy);
  Changed;
end;

procedure TAffineTransformation.Scale(Sx, Sy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[0,0] := Sx;
  M[1,1] := Sy;
  Matrix := Mult(M, Matrix);
  Changed;  
end;

procedure TAffineTransformation.Skew(Fx, Fy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[1, 0] := Fx;
  M[0, 1] := Fy;
  Matrix := Mult(M, Matrix);
  Changed;  
end;

procedure TAffineTransformation.ReverseTransformFloat(
  DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
begin
  SrcX := DstX * FInverseMatrix[0,0] + DstY * FInverseMatrix[1,0] + FInverseMatrix[2,0];
  SrcY := DstX * FInverseMatrix[0,1] + DstY * FInverseMatrix[1,1] + FInverseMatrix[2,1];
end;

procedure TAffineTransformation.ReverseTransformFixed(
  DstX, DstY: TFixed;
  out SrcX, SrcY: TFixed);
begin
  SrcX := FixedMul(DstX, FInverseFixedMatrix[0,0]) + FixedMul(DstY, FInverseFixedMatrix[1,0]) + FInverseFixedMatrix[2,0];
  SrcY := FixedMul(DstX, FInverseFixedMatrix[0,1]) + FixedMul(DstY, FInverseFixedMatrix[1,1]) + FInverseFixedMatrix[2,1];
end;

procedure TAffineTransformation.TransformFloat(
  SrcX, SrcY: TFloat;
  out DstX, DstY: TFloat);
begin
  DstX := SrcX * Matrix[0,0] + SrcY * Matrix[1,0] + Matrix[2,0];
  DstY := SrcX * Matrix[0,1] + SrcY * Matrix[1,1] + Matrix[2,1];
end;

procedure TAffineTransformation.TransformFixed(
  SrcX, SrcY: TFixed;
  out DstX, DstY: TFixed);
begin
  DstX := FixedMul(SrcX, FFixedMatrix[0,0]) + FixedMul(SrcY, FFixedMatrix[1,0]) + FFixedMatrix[2,0];
  DstY := FixedMul(SrcX, FFixedMatrix[0,1]) + FixedMul(SrcY, FFixedMatrix[1,1]) + FFixedMatrix[2,1];
end;

procedure TAffineTransformation.Translate(Dx, Dy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[2,0] := Dx;
  M[2,1] := Dy;
  Matrix := Mult(M, Matrix);
  Changed;  
end;


{ TProjectiveTransformation }

function TProjectiveTransformation.GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect;
begin
  Result.Left   := Min(Min(Wx0, Wx1), Min(Wx2, Wx3));
  Result.Right  := Max(Max(Wx0, Wx1), Max(Wx2, Wx3));
  Result.Top    := Min(Min(Wy0, Wy1), Min(Wy2, Wy3));
  Result.Bottom := Max(Max(Wy0, Wy1), Max(Wy2, Wy3));
end;

procedure TProjectiveTransformation.PrepareTransform;
var
  dx1, dx2, px, dy1, dy2, py: TFloat;
  g, h, k: TFloat;
  R: TFloatMatrix;
begin
  px  := Wx0 - Wx1 + Wx2 - Wx3;
  py  := Wy0 - Wy1 + Wy2 - Wy3;

  if (px = 0) and (py = 0) then
  begin
    // affine mapping
    FMatrix[0,0] := Wx1 - Wx0;
    FMatrix[1,0] := Wx2 - Wx1;
    FMatrix[2,0] := Wx0;

    FMatrix[0,1] := Wy1 - Wy0;
    FMatrix[1,1] := Wy2 - Wy1;
    FMatrix[2,1] := Wy0;

    FMatrix[0,2] := 0;
    FMatrix[1,2] := 0;
    FMatrix[2,2] := 1;
  end
  else
  begin
    // projective mapping
    dx1 := Wx1 - Wx2;
    dx2 := Wx3 - Wx2;
    dy1 := Wy1 - Wy2;
    dy2 := Wy3 - Wy2;
    k := dx1 * dy2 - dx2 * dy1;
    if k <> 0 then
    begin
      g := (px * dy2 - py * dx2) / k;
      h := (dx1 * py - dy1 * px) / k;

      FMatrix[0,0] := Wx1 - Wx0 + g * Wx1;
      FMatrix[1,0] := Wx3 - Wx0 + h * Wx3;
      FMatrix[2,0] := Wx0;

      FMatrix[0,1] := Wy1 - Wy0 + g * Wy1;
      FMatrix[1,1] := Wy3 - Wy0 + h * Wy3;
      FMatrix[2,1] := Wy0;

      FMatrix[0,2] := g;
      FMatrix[1,2] := h;
      FMatrix[2,2] := 1;
    end
    else
    begin
      FillChar(FMatrix, SizeOf(FMatrix), 0);
    end;
  end;

  // denormalize texture space (u, v)
  R := IdentityMatrix;
  R[0,0] := 1 / (SrcRect.Right - SrcRect.Left);
  R[1,1] := 1 / (SrcRect.Bottom - SrcRect.Top);
  FMatrix := Mult(FMatrix, R);

  R := IdentityMatrix;
  R[2,0] := -SrcRect.Left;
  R[2,1] := -SrcRect.Top;
  FMatrix := Mult(FMatrix, R);

  FInverseMatrix := FMatrix;
  Invert(FInverseMatrix);

  FInverseFixedMatrix := FixedMatrix(FInverseMatrix);
  FFixedMatrix := FixedMatrix(FMatrix);

  TransformValid := True;
end;

procedure TProjectiveTransformation.SetX0(Value: TFloat);
begin
  Wx0 := Value;
  Changed;
end;

procedure TProjectiveTransformation.SetX1(Value: TFloat);
begin
  Wx1 := Value;
  Changed;
end;

procedure TProjectiveTransformation.SetX2(Value: TFloat);
begin
  Wx2 := Value;
  Changed;
end;

procedure TProjectiveTransformation.SetX3(Value: TFloat);
begin
  Wx3 := Value;
  Changed;
end;

procedure TProjectiveTransformation.SetY0(Value: TFloat);
begin
  Wy0 := Value;
  Changed;
end;

procedure TProjectiveTransformation.SetY1(Value: TFloat);
begin
  Wy1 := Value;
  Changed;
end;

procedure TProjectiveTransformation.SetY2(Value: TFloat);
begin
  Wy2 := Value;
  Changed;
end;

procedure TProjectiveTransformation.SetY3(Value: TFloat);
begin
  Wy3 := Value;
  Changed;
end;

procedure TProjectiveTransformation.ReverseTransformFloat(
  DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
var
  X, Y, Z: TFloat;
begin
  EMMS;
  X := DstX; Y := DstY;
  Z := FInverseMatrix[0,2] * X + FInverseMatrix[1,2] * Y + FInverseMatrix[2,2];

  if Z = 0 then Exit
  else if Z = 1 then
  begin
    SrcX := FInverseMatrix[0,0] * X + FInverseMatrix[1,0] * Y + FInverseMatrix[2,0];
    SrcY := FInverseMatrix[0,1] * X + FInverseMatrix[1,1] * Y + FInverseMatrix[2,1];
  end
  else
  begin
    Z := 1 / Z;
    SrcX := (FInverseMatrix[0,0] * X + FInverseMatrix[1,0] * Y + FInverseMatrix[2,0]) * Z;
    SrcY := (FInverseMatrix[0,1] * X + FInverseMatrix[1,1] * Y + FInverseMatrix[2,1]) * Z;
  end;
end;

procedure TProjectiveTransformation.ReverseTransformFixed(DstX, DstY: TFixed;
  out SrcX, SrcY: TFixed);
var
  Z: TFixed;
  Zf: TFloat;
begin
  Z := FixedMul(FInverseFixedMatrix[0,2], DstX) +
       FixedMul(FInverseFixedMatrix[1,2], DstY) +
       FInverseFixedMatrix[2,2];

  if Z = 0 then Exit;

  SrcX := FixedMul(FInverseFixedMatrix[0,0], DstX) +
          FixedMul(FInverseFixedMatrix[1,0], DstY) +
          FInverseFixedMatrix[2,0];

  SrcY := FixedMul(FInverseFixedMatrix[0,1], DstX) +
          FixedMul(FInverseFixedMatrix[1,1], DstY) +
          FInverseFixedMatrix[2,1];

  if Z <> FixedOne then
  begin
    EMMS;
    Zf := FixedOne / Z;
    SrcX := Round(SrcX * Zf);
    SrcY := Round(SrcY * Zf);
  end;
end;


procedure TProjectiveTransformation.TransformFixed(SrcX, SrcY: TFixed;
  out DstX, DstY: TFixed);
var
  Z: TFixed;
  Zf: TFloat;
begin
  Z := FixedMul(FFixedMatrix[0,2], SrcX) +
       FixedMul(FFixedMatrix[1,2], SrcY) +
       FFixedMatrix[2,2];

  if Z = 0 then Exit;

  DstX := FixedMul(FFixedMatrix[0,0], SrcX) +
          FixedMul(FFixedMatrix[1,0], SrcY) +
          FFixedMatrix[2,0];

  DstY := FixedMul(FFixedMatrix[0,1], SrcX) +
          FixedMul(FFixedMatrix[1,1], SrcY) +
          FFixedMatrix[2,1];

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
  X, Y, Z: TFloat;
begin
  EMMS;
  X := SrcX; Y := SrcY;
  Z := FMatrix[0,2] * X + FMatrix[1,2] * Y + FMatrix[2,2];

  if Z = 0 then Exit
  else if Z = 1 then
  begin
    DstX := FMatrix[0,0] * X + FMatrix[1,0] * Y + FMatrix[2,0];
    DstY := FMatrix[0,1] * X + FMatrix[1,1] * Y + FMatrix[2,1];
  end
  else
  begin
    Z := 1 / Z;
    DstX := (FMatrix[0,0] * X + FMatrix[1,0] * Y + FMatrix[2,0]) * Z;
    DstY := (FMatrix[0,1] * X + FMatrix[1,1] * Y + FMatrix[2,1]) * Z;
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
begin
  Cx := (ASrcRect.Left + ASrcRect.Right) / 2;
  Cy := (ASrcRect.Top + ASrcRect.Bottom) / 2;
  R := Max(Cx - ASrcRect.Left, Cy - ASrcRect.Top);
  Result.Left := Cx - R * Pi/2;
  Result.Right := Cx + R * Pi/2;
  Result.Top := Cy - R * Pi/2;
  Result.Bottom := Cy + R * Pi/2;
end;

procedure TTwirlTransformation.PrepareTransform;
begin
  with FSrcRect do
  begin
    Frx := (Right - Left) / 2;
    Fry := (Bottom - Top) / 2;
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
    Frx := (Right - Left) / 2;
    Fry := (Bottom - Top) / 2;
    if Frx <= Fry then
    begin
      FMinR := Frx;
      Sx := 1;
      Sy:= 1 / (Fry / Frx);
    end
    else
    begin
      FMinR := Fry;
      Sx:= 1 / (Frx / Fry);
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
  Cx := (DstRect.Left + DstRect.Right) / 2;
  Cy := (DstRect.Top + DstRect.Bottom) / 2;
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
    raise ETransformError.Create('Top or bottom curve is nil');

  SetLength(FTopHypot, Length(FTopCurve));
  SetLength(FBottomHypot, Length(FBottomCurve));

  L := 0;
  for I := 0 to High(FTopCurve) - 1 do
  begin
    FTopHypot[I].Dist := L;
    with FTopCurve[I + 1] do
      L := L + Hypot(FTopCurve[I].X - X, FTopCurve[I].Y - Y);
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
      L := L + Hypot(FBottomCurve[I].X - X, FBottomCurve[I].Y - Y);
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
  if IsRectEmpty(SrcRect) then raise Exception.Create('SrcRect is empty!');
  if IsRectEmpty(FMappingRect) then raise Exception.Create('MappingRect is empty!');
  with SrcRect do
  begin
    FSrcTranslationFloat.X := Left;
    FSrcTranslationFloat.Y := Top;
    FSrcScaleFloat.X := 1 / ((FVectorMap.Width - 1) / (Right - Left));
    FSrcScaleFloat.Y := 1 / ((FVectorMap.Height - 1) / (Bottom - Top));
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
  IntersectRect(DstRect, VectorMap.BoundsRect, DstRect);
  if IsRectEmpty(DstRect) then Exit;

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
            P := FixedPoint(J - Left, I - Top);
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
            P := FixedPoint(J - Left, I - Top);
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
        P := FixedPoint(J - Left, I - Top);
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

end.
