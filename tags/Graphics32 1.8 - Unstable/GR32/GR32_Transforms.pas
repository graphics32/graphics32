unit GR32_Transforms;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
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
  {$IFDEF CLX}
  Qt, Types, {$IFDEF LINUX}Libc, {$ENDIF}
  {$ELSE}
  Windows,
  {$ENDIF}
  SysUtils, Classes, GR32, GR32_Blend, GR32_Rasterizers;
    
type
  ETransformError = class(Exception);

type
  TFloatMatrix = array[0..2, 0..2] of Single;     // 3x3 single precision
  TIntegerMatrix = array[0..2, 0..2] of Integer;  // 3x3 whatever

const
  IdentityMatrix: TFloatMatrix = (
    (1, 0, 0),
    (0, 1, 0),
    (0, 0, 1));

type
  TVector3f = array[0..2] of Single;
  TVector3i = array[0..2] of Integer;

procedure Adjoint(var M: TFloatMatrix);
function Determinant(const M: TFloatMatrix): Single;
procedure Scale(var M: TFloatMatrix; Factor: Single);
procedure Invert(var M: TFloatMatrix);
function Mult(const M1, M2: TFloatMatrix): TFloatMatrix;
function VectorTransform(const M: TFloatMatrix; const V: TVector3f): TVector3f;

type
  TTransformation = class
  private
    FSrcRect: TFloatRect;
    procedure SetSrcRect(const Value: TFloatRect);
  protected
    TransformValid: Boolean;
    procedure PrepareTransform; virtual; abstract;

    procedure ReverseTransform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); virtual; abstract; // only used in transform (draw) of bitmaps
    procedure ReverseTransformInt(DstX, DstY: Integer; out SrcX, SrcY: Integer); virtual; abstract;
    procedure ReverseTransformFloat(DstX, DstY: Single; out SrcX, SrcY: Single); virtual; abstract;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); virtual; abstract;

    procedure TransformInt(SrcX, SrcY: Integer; out DstX, DstY: Integer); virtual; abstract;
    procedure TransformFloat(SrcX, SrcY: Single; out DstX, DstY: Single); virtual; abstract;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); virtual; abstract;
  public
    function  GetTransformedBounds: TRect; virtual; abstract;

    function  ReverseTransform(const P: TPoint): TPoint; overload; virtual;
    function  ReverseTransform(const P: TFixedPoint): TFixedPoint; overload; virtual;
    function  ReverseTransform(const P: TFloatPoint): TFloatPoint; overload; virtual;

    function  Transform(const P: TPoint): TPoint; overload; virtual;
    function  Transform(const P: TFixedPoint): TFixedPoint; overload; virtual;
    function  Transform(const P: TFloatPoint): TFloatPoint; overload; virtual;

    property SrcRect: TFloatRect read FSrcRect write SetSrcRect;
  end;

  TAffineTransformation = class(TTransformation)
  protected
    FInverseMatrix: TFloatMatrix;
    FIntMatrix, FInverseIntMatrix: TIntegerMatrix;
    FFixedMatrix, FInverseFixedMatrix: TIntegerMatrix;

    procedure PrepareTransform; override;

    procedure ReverseTransform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); override;

    procedure ReverseTransformInt(DstX, DstY: Integer; out SrcX, SrcY: Integer); override;
    procedure ReverseTransformFloat(DstX, DstY: Single; out SrcX, SrcY: Single); override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;

    procedure TransformInt(SrcX, SrcY: Integer; out DstX, DstY: Integer); override;
    procedure TransformFloat(SrcX, SrcY: Single; out DstX, DstY: Single); override;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); override;
  public
    Matrix: TFloatMatrix;
    constructor Create; virtual;
    function  GetTransformedBounds: TRect; override;
    procedure Clear;
    procedure Rotate(Cx, Cy, Alpha: Single); // degrees
    procedure Skew(Fx, Fy: Single);
    procedure Scale(Sx, Sy: Single);
    procedure Translate(Dx, Dy: Single);
  end;

  TProjectiveTransformation = class(TTransformation)
  private
    Wx0, Wx1, Wx2, Wx3: Single;
    Wy0, Wy1, Wy2, Wy3: Single;
    procedure SetX0(Value: Single);
    procedure SetX1(Value: Single);
    procedure SetX2(Value: Single);
    procedure SetX3(Value: Single);
    procedure SetY0(Value: Single);
    procedure SetY1(Value: Single);
    procedure SetY2(Value: Single);
    procedure SetY3(Value: Single);
  protected
    FMatrix, FInverseMatrix: TFloatMatrix;
    FIntMatrix, FInverseIntMatrix: TIntegerMatrix;
    FFixedMatrix, FInverseFixedMatrix: TIntegerMatrix;

    procedure PrepareTransform; override;

    procedure ReverseTransform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); override;
    procedure ReverseTransformInt(DstX, DstY: Integer; out SrcX, SrcY: Integer); override;
    procedure ReverseTransformFloat(DstX, DstY: Single; out SrcX, SrcY: Single); override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;

    procedure TransformInt(SrcX, SrcY: Integer; out DstX, DstY: Integer); override;
    procedure TransformFloat(SrcX, SrcY: Single; out DstX, DstY: Single); override;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); override;
  public
    function  GetTransformedBounds: TRect; override;
    property X0: Single read Wx0 write SetX0;
    property X1: Single read Wx1 write SetX1;
    property X2: Single read Wx2 write SetX2;
    property X3: Single read Wx3 write SetX3;
    property Y0: Single read Wy0 write SetY0;
    property Y1: Single read Wy1 write SetY1;
    property Y2: Single read Wy2 write SetY2;
    property Y3: Single read Wy3 write SetY3;
  end;

  TConformalTransformation = class(TTransformation)
  protected
    procedure ReverseTransform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); override;
    procedure ReverseTransformInt(DstX, DstY: Integer; out SrcX, SrcY: Integer); override;
  public
    function  GetTransformedBounds: TRect; override;
  end;

  TTwirlTransformation = class(TConformalTransformation)
  private
    Frx, Fry: Single;
    FTwirl: Single;
    procedure SetTwirl(const Value: Single);
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: Single; out SrcX, SrcY: Single); override;
  public
    function  GetTransformedBounds: TRect; override;
    property Twirl: Single read FTwirl write SetTwirl;
  end;

  TBloatTransformation = class(TConformalTransformation)
  private
    FBloatPower: Single;
    FBP: Single;
    FPiW, FPiH: Single;
    procedure SetBloatPower(const Value: Single);
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: Single; out SrcX, SrcY: Single); override;
  public
    property BloatPower: Single read FBloatPower write SetBloatPower;
  end;

  TDisturbanceTransformation = class(TConformalTransformation)
  private
    Frx, Fry: Single;
    FDisturbance: Single;
    procedure SetDisturbance(const Value: Single);
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: Single; out SrcX, SrcY: Single); override;
  public
    property Disturbance: Single read FDisturbance write SetDisturbance;
  end;

  TFishEyeTransformation = class(TConformalTransformation)
  private
    Frx, Fry: Single;
    Faw, Fsr: Single;
    FMinR: Single;
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: Single; out SrcX, SrcY: Single); override;
  public
  end;

function TransformPoints(Points: TArrayOfArrayOfFixedPoint; Transformation: TTransformation): TArrayOfArrayOfFixedPoint;

procedure Transform(Dst, Src: TBitmap32; Transformation: TTransformation; Rasterizer: TRasterizer);
procedure SetBorderTransparent(ABitmap: TBitmap32; ARect: TRect);

{ FullEdge controls how the bitmap is resampled }
var
  FullEdge: Boolean = True;

implementation

{$R-}{$Q-}  // switch off overflow and range checking

uses GR32_LowLevel, GR32_System, GR32_Resamplers, Math;

type
  {provides access to proctected members of TBitmap32 by typecasting}
  TTransformationAccess = class(TTransformation);
  TBitmap32Access = class(TBitmap32);


{ A bit of linear algebra }

function _DET(a1, a2, b1, b2: Single): Single; overload;
begin
  Result := a1 * b2 - a2 * b1;
end;

function _DET(a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single; overload;
begin
  Result :=
    a1 * (b2 * c3 - b3 * c2) -
    b1 * (a2 * c3 - a3 * c2) +
    c1 * (a2 * b3 - a3 * b2);
end;

procedure Adjoint(var M: TFloatMatrix);
var
  a1, a2, a3: Single;
  b1, b2, b3: Single;
  c1, c2, c3: Single;
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

function Determinant(const M: TFloatMatrix): Single;
begin
  Result := _DET(M[0,0], M[1,0], M[2,0],
                 M[0,1], M[1,1], M[2,1],
                 M[0,2], M[1,2], M[2,2]);
end;

procedure Scale(var M: TFloatMatrix; Factor: Single);
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      M[i,j] := M[i,j] * Factor;
end;

procedure Invert(var M: TFloatMatrix);
var
  Det: Single;
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

(*
procedure Transform(Dst, Src: TBitmap32; Transformation: TTransformation);
var
  R, SrcRect, DstRect: TRect;
  CombineOp: TDrawMode;
begin
  if not TTransformationAccess(Transformation).TransformValid then
    TTransformationAccess(Transformation).PrepareTransform;

  // clip SrcRect
  // workaround C++ Builder throwing exceptions:
  R := MakeRect(Round(Transformation.SrcRect.Left), Round(Transformation.SrcRect.Top),
                Round(Transformation.SrcRect.Right), Round(Transformation.SrcRect.Bottom));
  IntersectRect(SrcRect, R, MakeRect(0, 0, Src.Width - 1, Src.Height - 1));

  // clip DstRect
  R := Transformation.GetTransformedBounds;
  IntersectRect(DstRect, R, MakeRect(Dst.ClipRect.Left, Dst.ClipRect.Top,
    Dst.ClipRect.Right - 1, Dst.ClipRect.Bottom - 1));

  if (DstRect.Right < DstRect.Left) or (DstRect.Bottom < DstRect.Top) then Exit;

  try
    CombineOp := Src.DrawMode;
    if (CombineOp = dmCustom) and not Assigned(Src.OnPixelCombine) then
      CombineOp := dmOpaque;

    Src.Resampler.Transform(Dst, DstRect, Src, SrcRect, Transformation,
      CombineOp, Src.OnPixelCombine);
  finally
    EMMS;
  end;
  Dst.Changed;
end; *)

procedure Transform(Dst, Src: TBitmap32; Transformation: TTransformation; Rasterizer: TRasterizer);
var
  R, { SrcRect,  } DstRect: TRect;
  CombineOp: TDrawMode;
  TransformationSampler: TTransformationSampler;
begin
  if not TTransformationAccess(Transformation).TransformValid then
    TTransformationAccess(Transformation).PrepareTransform;

  // clip SrcRect
  // workaround C++ Builder throwing exceptions:
  //R := MakeRect(Round(Transformation.SrcRect.Left), Round(Transformation.SrcRect.Top),
  //              Round(Transformation.SrcRect.Right), Round(Transformation.SrcRect.Bottom));
  //IntersectRect(SrcRect, R, MakeRect(0, 0, Src.Width - 1, Src.Height - 1));

  // clip DstRect
  R := Transformation.GetTransformedBounds;
  IntersectRect(DstRect, R, MakeRect(Dst.ClipRect.Left, Dst.ClipRect.Top,
    Dst.ClipRect.Right - 1, Dst.ClipRect.Bottom - 1));

  if (DstRect.Right < DstRect.Left) or (DstRect.Bottom < DstRect.Top) then Exit;

  TransformationSampler := TTransformationSampler.Create(Src, Transformation);
  try
    CombineOp := Src.DrawMode;
    if (CombineOp = dmCustom) and not Assigned(Src.OnPixelCombine) then
      CombineOp := dmOpaque;

    Rasterizer.Sampler := TransformationSampler;
    Rasterizer.Rasterize(Dst, DstRect, Src.MasterAlpha, CombineOp, Src.CombineMode, Src.OnPixelCombine);
    //Src.Resampler.Transform(Dst, DstRect, Src, SrcRect, Transformation,
    //  CombineOp, Src.OnPixelCombine);
  finally
    EMMS;
    TransformationSampler.Free;
  end;
  Dst.Changed;
end;

procedure SetBorderTransparent(ABitmap: TBitmap32; ARect: TRect);
var
  I: Integer;
begin
  if TestClip(ARect.Left, ARect.Right, ABitmap.Width) and
    TestClip(ARect.Top, ARect.Bottom, ABitmap.Height) then
  begin
    for I := ARect.Left to ARect.Right do
      ABitmap[I, ARect.Top] := ABitmap[I, ARect.Top] and $00FFFFFF;

    for I := ARect.Left to ARect.Right do
      ABitmap[I, ARect.Bottom] := ABitmap[I, ARect.Bottom] and $00FFFFFF;

    if ARect.Bottom > ARect.Top + 1 then
      for I := ARect.Top + 1 to ARect.Bottom - 1 do
      begin
        ABitmap[ARect.Left, I] := ABitmap[ARect.Left, I] and $00FFFFFF;
        ABitmap[ARect.Right, I] := ABitmap[ARect.Right, I] and $00FFFFFF;
      end;

    ABitmap.Changed;
  end;
end;


{ TTransformation }

function TTransformation.ReverseTransform(const P: TFloatPoint): TFloatPoint;
begin
  If not TransformValid then PrepareTransform;
  ReverseTransformFloat(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.ReverseTransform(const P: TFixedPoint): TFixedPoint;
begin
  If not TransformValid then PrepareTransform;
  ReverseTransformFixed(P.X, P.Y, Result.X, Result.Y);
end;

function TTransformation.ReverseTransform(const P: TPoint): TPoint;
begin
  If not TransformValid then PrepareTransform;
  ReverseTransformInt(P.X, P.Y, Result.X, Result.Y);
end;

procedure TTransformation.SetSrcRect(const Value: TFloatRect);
begin
  FSrcRect := Value;
  TransformValid := False;
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

{ TAffineTransformation }

procedure TAffineTransformation.Clear;
begin
  Matrix := IdentityMatrix;
  TransformValid := False;
end;

constructor TAffineTransformation.Create;
begin
  Clear;
end;

function TAffineTransformation.GetTransformedBounds: TRect;
var
  V1, V2, V3, V4: TVector3f;
begin
  V1[0] := FSrcRect.Left;  V1[1] := FSrcRect.Top;    V1[2] := 1;
  V2[0] := FSrcRect.Right; V2[1] := V1[1];           V2[2] := 1;
  V3[0] := V1[0];          V3[1] := FSrcRect.Bottom; V3[2] := 1;
  V4[0] := V2[0];          V4[1] := V3[1];           V4[2] := 1;
  V1 := VectorTransform(Matrix, V1);
  V2 := VectorTransform(Matrix, V2);
  V3 := VectorTransform(Matrix, V3);
  V4 := VectorTransform(Matrix, V4);
  Result.Left   := Round(Min(Min(V1[0], V2[0]), Min(V3[0], V4[0])) - 0.5);
  Result.Right  := Round(Max(Max(V1[0], V2[0]), Max(V3[0], V4[0])) + 0.5);
  Result.Top    := Round(Min(Min(V1[1], V2[1]), Min(V3[1], V4[1])) - 0.5);
  Result.Bottom := Round(Max(Max(V1[1], V2[1]), Max(V3[1], V4[1])) + 0.5);
end;

procedure TAffineTransformation.PrepareTransform;
begin
  FInverseMatrix := Matrix;
  Invert(FInverseMatrix);

  // calculate a fixed point (4096) factors
  FInverseIntMatrix[0,0] := Round(FInverseMatrix[0,0] * 4096);
  FInverseIntMatrix[1,0] := Round(FInverseMatrix[1,0] * 4096);
  FInverseIntMatrix[2,0] := Round(FInverseMatrix[2,0] * 4096);
  FInverseIntMatrix[0,1] := Round(FInverseMatrix[0,1] * 4096);
  FInverseIntMatrix[1,1] := Round(FInverseMatrix[1,1] * 4096);
  FInverseIntMatrix[2,1] := Round(FInverseMatrix[2,1] * 4096);

  FIntMatrix[0,0] := Round(Matrix[0,0] * 4096);
  FIntMatrix[1,0] := Round(Matrix[1,0] * 4096);
  FIntMatrix[2,0] := Round(Matrix[2,0] * 4096);
  FIntMatrix[0,1] := Round(Matrix[0,1] * 4096);
  FIntMatrix[1,1] := Round(Matrix[1,1] * 4096);
  FIntMatrix[2,1] := Round(Matrix[2,1] * 4096);

  // calculate a fixed point (65536) factors
  FInverseFixedMatrix[0,0] := Round(FInverseMatrix[0,0] * 65536);
  FInverseFixedMatrix[1,0] := Round(FInverseMatrix[1,0] * 65536);
  FInverseFixedMatrix[2,0] := Round(FInverseMatrix[2,0] * 65536);
  FInverseFixedMatrix[0,1] := Round(FInverseMatrix[0,1] * 65536);
  FInverseFixedMatrix[1,1] := Round(FInverseMatrix[1,1] * 65536);
  FInverseFixedMatrix[2,1] := Round(FInverseMatrix[2,1] * 65536);

  FFixedMatrix[0,0] := Round(Matrix[0,0] * 65536);
  FFixedMatrix[1,0] := Round(Matrix[1,0] * 65536);
  FFixedMatrix[2,0] := Round(Matrix[2,0] * 65536);
  FFixedMatrix[0,1] := Round(Matrix[0,1] * 65536);
  FFixedMatrix[1,1] := Round(Matrix[1,1] * 65536);
  FFixedMatrix[2,1] := Round(Matrix[2,1] * 65536);

  TransformValid := True;
end;

procedure TAffineTransformation.Rotate(Cx, Cy, Alpha: Single);
var
  S, C: Single;
  M: TFloatMatrix;
begin
  TransformValid := False;
  if (Cx <> 0) or (Cy <> 0) then Translate(-Cx, -Cy);
  Alpha := DegToRad(Alpha);
  S := Sin(Alpha); C := Cos(Alpha);
  M := IdentityMatrix;
  M[0,0] := C;   M[1,0] := S;
  M[0,1] := -S;  M[1,1] := C;
  Matrix := Mult(M, Matrix);
  if (Cx <> 0) or (Cy <> 0) then Translate(Cx, Cy);
end;

procedure TAffineTransformation.Scale(Sx, Sy: Single);
var
  M: TFloatMatrix;
begin
  TransformValid := False;
  M := IdentityMatrix;
  M[0,0] := Sx;
  M[1,1] := Sy;
  Matrix := Mult(M, Matrix);
end;

procedure TAffineTransformation.Skew(Fx, Fy: Single);
var
  M: TFloatMatrix;
begin
  TransformValid := False;
  M := IdentityMatrix;
  M[1, 0] := Fx;
  M[0, 1] := Fy;
  Matrix := Mult(M, Matrix);
end;

procedure TAffineTransformation.ReverseTransformInt(
  DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
begin
  SrcX := SAR_12(DstX * FInverseIntMatrix[0,0] + DstY * FInverseIntMatrix[1,0] + FInverseIntMatrix[2,0] + 2047);
  SrcY := SAR_12(DstX * FInverseIntMatrix[0,1] + DstY * FInverseIntMatrix[1,1] + FInverseIntMatrix[2,1] + 2047);
end;

procedure TAffineTransformation.ReverseTransformFloat(
  DstX, DstY: Single;
  out SrcX, SrcY: Single);
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

procedure TAffineTransformation.ReverseTransform256(
  DstX, DstY: Integer;
  out SrcX256, SrcY256: Integer);
begin
  SrcX256 := SAR_4(DstX * FInverseIntMatrix[0,0] + DstY * FInverseIntMatrix[1,0] + FInverseIntMatrix[2,0] + 7);
  SrcY256 := SAR_4(DstX * FInverseIntMatrix[0,1] + DstY * FInverseIntMatrix[1,1] + FInverseIntMatrix[2,1] + 7);
end;

procedure TAffineTransformation.TransformInt(
  SrcX, SrcY: Integer;
  out DstX, DstY: Integer);
begin
  DstX := SAR_12(SrcX * FIntMatrix[0,0] + SrcY * FIntMatrix[1,0] + FIntMatrix[2,0] + 2047);
  DstY := SAR_12(SrcX * FIntMatrix[0,1] + SrcY * FIntMatrix[1,1] + FIntMatrix[2,1] + 2047);
end;

procedure TAffineTransformation.TransformFloat(
  SrcX, SrcY: Single;
  out DstX, DstY: Single);
begin
  DstX := SrcX * Matrix[0,0] + SrcY * Matrix[1,0] + Matrix[2,0];
  DstY := SrcY * Matrix[0,1] + SrcY * Matrix[1,1] + Matrix[2,1];
end;

procedure TAffineTransformation.TransformFixed(
  SrcX, SrcY: TFixed;
  out DstX, DstY: TFixed);
begin
  DstX := FixedMul(SrcX, FFixedMatrix[0,0]) + FixedMul(SrcY, FFixedMatrix[1,0]) + FFixedMatrix[2,0];
  DstY := FixedMul(SrcX, FFixedMatrix[0,1]) + FixedMul(SrcY, FFixedMatrix[1,1]) + FFixedMatrix[2,1];
end;

procedure TAffineTransformation.Translate(Dx, Dy: Single);
var
  M: TFloatMatrix;
begin
  TransformValid := False;
  M := IdentityMatrix;
  M[2,0] := Dx;
  M[2,1] := Dy;
  Matrix := Mult(M, Matrix);
end;


{ TProjectiveTransformation }

function TProjectiveTransformation.GetTransformedBounds: TRect;
begin
  Result.Left   := Round(Min(Min(Wx0, Wx1), Min(Wx2, Wx3)) - 0.5);
  Result.Right  := Round(Max(Max(Wx0, Wx1), Max(Wx2, Wx3)) + 0.5);
  Result.Top    := Round(Min(Min(Wy0, Wy1), Min(Wy2, Wy3)) - 0.5);
  Result.Bottom := Round(Max(Max(Wy0, Wy1), Max(Wy2, Wy3)) + 0.5);
end;

procedure TProjectiveTransformation.PrepareTransform;
var
  dx1, dx2, px, dy1, dy2, py: Single;
  g, h, k: Single;
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

  TransformValid := True;
end;

procedure TProjectiveTransformation.SetX0(Value: Single);
begin
  Wx0 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetX1(Value: Single);
begin
  Wx1 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetX2(Value: Single);
begin
  Wx2 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetX3(Value: Single);
begin
  Wx3 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetY0(Value: Single);
begin
  Wy0 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetY1(Value: Single);
begin
  Wy1 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetY2(Value: Single);
begin
  Wy2 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetY3(Value: Single);
begin
  Wy3 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.ReverseTransformInt(
  DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
var
  X, Y, Z: Single;
begin
  EMMS;
  X := DstX; Y := DstY;
  Z := FInverseMatrix[0,2] * X + FInverseMatrix[1,2] * Y + FInverseMatrix[2,2];
  if Z = 0 then Exit
  else if Z = 1 then
  begin
    SrcX := Round(FInverseMatrix[0,0] * X + FInverseMatrix[1,0] * Y + FInverseMatrix[2,0]);
    SrcY := Round(FInverseMatrix[0,1] * X + FInverseMatrix[1,1] * Y + FInverseMatrix[2,1]);
  end
  else
  begin
    Z := 1 / Z;
    SrcX := Round((FInverseMatrix[0,0] * X + FInverseMatrix[1,0] * Y + FInverseMatrix[2,0]) * Z);
    SrcY := Round((FInverseMatrix[0,1] * X + FInverseMatrix[1,1] * Y + FInverseMatrix[2,1]) * Z);
  end;
end;

procedure TProjectiveTransformation.ReverseTransformFloat(
  DstX, DstY: Single;
  out SrcX, SrcY: Single);
var
  X, Y, Z: Single;
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

procedure TProjectiveTransformation.ReverseTransformFixed(
  DstX, DstY: TFixed;
  out SrcX, SrcY: TFixed);
var
  X, Y, Z: Single;
begin
  EMMS;
  X := DstX; Y := DstY;
  Z := FInverseMatrix[0,2] * X + FInverseMatrix[1,2] * Y + FInverseMatrix[2,2];
  if Z = 0 then Exit
  else if Z = 1 then
  begin
    SrcX := Round((FInverseMatrix[0,0] * X + FInverseMatrix[1,0] * Y + FInverseMatrix[2,0]) * 65536);
    SrcY := Round((FInverseMatrix[0,1] * X + FInverseMatrix[1,1] * Y + FInverseMatrix[2,1]) * 65536);
  end
  else
  begin
    Z := 1 / Z;
    SrcX := Round(((FInverseMatrix[0,0] * X + FInverseMatrix[1,0] * Y + FInverseMatrix[2,0]) * Z) * 65536);
    SrcY := Round(((FInverseMatrix[0,1] * X + FInverseMatrix[1,1] * Y + FInverseMatrix[2,1]) * Z) * 65536);
  end;
end;

procedure TProjectiveTransformation.ReverseTransform256(
  DstX, DstY: Integer;
  out SrcX256, SrcY256: Integer);
var
  X, Y, Z: Single;
begin
  EMMS;
  X := DstX; Y := DstY;
  Z := FInverseMatrix[0,2] * X + FInverseMatrix[1,2] * Y + FInverseMatrix[2,2];
  if Z = 0 then Exit
  else if Z = 1 then
  begin
    SrcX256 := Round(256 * (FInverseMatrix[0,0] * X + FInverseMatrix[1,0] * Y + FInverseMatrix[2,0]));
    SrcY256 := Round(256 * (FInverseMatrix[0,1] * X + FInverseMatrix[1,1] * Y + FInverseMatrix[2,1]));
  end
  else
  begin
    Z := 1 / Z;
    SrcX256 := Round(256 * (FInverseMatrix[0,0] * X + FInverseMatrix[1,0] * Y + FInverseMatrix[2,0]) * Z);
    SrcY256 := Round(256 * (FInverseMatrix[0,1] * X + FInverseMatrix[1,1] * Y + FInverseMatrix[2,1]) * Z);
  end;
end;


procedure TProjectiveTransformation.TransformFixed(
  SrcX, SrcY: TFixed;
  out DstX, DstY: TFixed);
var
  X, Y, Z: Single;
begin
  EMMS;
  X := DstX; Y := DstY;
  Z := FMatrix[0,2] * X + FMatrix[1,2] * Y + FMatrix[2,2];
  if Z = 0 then Exit
  else if Z = 1 then
  begin
    DstX := Round((FMatrix[0,0] * X + FMatrix[1,0] * Y + FMatrix[2,0]) * 65536);
    DstY := Round((FMatrix[0,1] * X + FMatrix[1,1] * Y + FMatrix[2,1]) * 65536);
  end
  else
  begin
    Z := 1 / Z;
    DstX := Round(((FMatrix[0,0] * X + FMatrix[1,0] * Y + FMatrix[2,0]) * Z) * 65536);
    DstY := Round(((FMatrix[0,1] * X + FMatrix[1,1] * Y + FMatrix[2,1]) * Z) * 65536);
  end;
end;

procedure TProjectiveTransformation.TransformFloat(
  SrcX, SrcY: Single;
  out DstX, DstY: Single);
var
  X, Y, Z: Single;
begin
  EMMS;
  X := DstX; Y := DstY;
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

procedure TProjectiveTransformation.TransformInt(
  SrcX, SrcY: Integer;
  out DstX, DstY: Integer);
var
  X, Y, Z: Single;
begin
  EMMS;
  X := DstX; Y := DstY;
  Z := FInverseMatrix[0,2] * X + FInverseMatrix[1,2] * Y + FInverseMatrix[2,2];
  if Z = 0 then Exit
  else if Z = 1 then
  begin
    DstX := Round(FInverseMatrix[0,0] * X + FInverseMatrix[1,0] * Y + FInverseMatrix[2,0]);
    DstY := Round(FInverseMatrix[0,1] * X + FInverseMatrix[1,1] * Y + FInverseMatrix[2,1]);
  end
  else
  begin
    Z := 1 / Z;
    DstX := Round((FInverseMatrix[0,0] * X + FInverseMatrix[1,0] * Y + FInverseMatrix[2,0]) * Z);
    DstY := Round((FInverseMatrix[0,1] * X + FInverseMatrix[1,1] * Y + FInverseMatrix[2,1]) * Z);
  end;
end;


{ TConformalTransformation }

function TConformalTransformation.GetTransformedBounds: TRect;
begin
  Result := MakeRect(FSrcRect);
end;

procedure TConformalTransformation.ReverseTransform256(DstX, DstY: Integer;
  out SrcX256, SrcY256: Integer);
var
  X, Y: Single;
begin
  EMMS;
  ReverseTransformFloat(DstX, DstY, X, Y);
  SrcX256 := Round(X * 256);
  SrcY256 := Round(Y * 256);
end;

procedure TConformalTransformation.ReverseTransformInt(DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
var
  X, Y: Single;
begin
  EMMS;
  ReverseTransformFloat(DstX, DstY, X, Y);
  SrcX := Round(X);
  SrcY := Round(Y);
end;

{ TTwirlTransformation }

function TTwirlTransformation.GetTransformedBounds: TRect;
var
  Cx, Cy, R: Single;
begin
  Cx := (FSrcRect.Left + FSrcRect.Right) / 2;
  Cy := (FSrcRect.Top + FSrcRect.Bottom) / 2;
  R := Max(Cx - FSrcRect.Left, Cy - FSrcRect.Top);
  Result.Left := Round(Cx - R * Pi/2);
  Result.Right := Round(Cx + R * Pi/2);
  Result.Top := Round(Cy - R * Pi/2);
  Result.Bottom := Round(Cy + R * Pi/2);
end;

procedure TTwirlTransformation.PrepareTransform;
begin
  FTwirl := 0.03;
  with FSrcRect do
  begin
    Frx := (Right - Left -1) / 2;
    Fry := (Bottom - Top -1) / 2;
  end;
end;

procedure SinCos(const Theta: Single; var Sin, Cos: Single);
asm
        FLD  Theta
        FSINCOS
        FSTP DWORD PTR [EDX]    // cosine
        FSTP DWORD PTR [EAX]    // sine
end;

procedure TTwirlTransformation.ReverseTransformFloat(DstX, DstY: Single;
  out SrcX, SrcY: Single);
var
  xf, yf, r, t: Single;
begin
  xf := DstX - Frx;
  yf := DstY - Fry;

  r := Sqrt(Sqr(xf) + Sqr(yf));
  t := ArcTan2(yf, xf) + r * FTwirl;
  SinCos(t, yf, xf);

  SrcX := Frx + r * xf;
  SrcY := Fry + r * yf;
end;

procedure TTwirlTransformation.SetTwirl(const Value: Single);
begin
  FTwirl := Value;
  TransformValid := False;  
end;

{ TBloatTransformation }

procedure TBloatTransformation.PrepareTransform;
begin
  FBloatPower := 0.3;
  FPiW := (Pi / (FSrcRect.Right - FSrcRect.Left - 1));
  FPiH := (Pi / (FSrcRect.Bottom - FSrcRect.Top - 1));
  FBP := FBloatPower * Max(FSrcRect.Right - FSrcRect.Left, FSrcRect.Bottom - FSrcRect.Top);
end;

procedure TBloatTransformation.ReverseTransformFloat(DstX, DstY: Single;
  out SrcX, SrcY: Single);
var
  SinY, CosY, SinX, CosX, t: single;
begin
  SinCos(FPiH * DstY, SinY, CosY);
  SinCos(FPiW * DstX, SinX, CosX);
  t := FBP * SinY * SinX;
  SrcX := DstX + t * CosX;
  SrcY := DstY + t * CosY;
end;

procedure TBloatTransformation.SetBloatPower(const Value: Single);
begin
  FBloatPower := Value;
  TransformValid := False;
end;


{ TFishEyeTransformation }

procedure TFishEyeTransformation.PrepareTransform;
begin
  with FSrcRect do begin
    Frx := (Right - Left - 1) / 2;
    Fry := (Bottom - Top - 1) / 2;
    FMinR := Min(Frx, Fry);
    Fsr := 1 / FMinR;
    Faw := ArcSin(EnsureRange(FMinR * Fsr, -1, 1));
    if Faw <> 0 then Faw := 1/Faw;
    Faw := Faw * FMinR
  end;
end;

procedure TFishEyeTransformation.ReverseTransformFloat(DstX, DstY: Single;
  out SrcX, SrcY: Single);
var
  d, Xrx, Yry: Single;
begin
  Yry := DstY - Fry;
  Xrx := DstX - Frx;
  d := Sqrt(Sqr(Xrx) + Sqr(Yry));
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

{ TDisturbanceTransformation }

procedure TDisturbanceTransformation.PrepareTransform;
begin
  with FSrcRect do
  begin
    Frx := Right - Left - 1;
    Fry := Bottom - Top - 1;
  end;
end;

procedure TDisturbanceTransformation.ReverseTransformFloat(DstX,
  DstY: Single; out SrcX, SrcY: Single);
begin
  SrcX := DstX + (Random - 0.5) * Frx * FDisturbance;
  SrcY := DstY + (Random - 0.5) * Fry * FDisturbance;
end;

procedure TDisturbanceTransformation.SetDisturbance(const Value: Single);
begin
  FDisturbance := Value;
end;

end.
