unit GR32_TransformationMap;

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
 * Michael Hansen <dyster_tid@hotmail.com
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
   Windows, Types, SysUtils, Classes, GR32, GR32_Transforms;

type

  { TCustomShape - Base class for  shapes }

  TCustomShape = class
  private
    FShapeValid: Boolean;
    FShapeWeight: Single;
    FixedShapeWeight: TFixed;
    procedure SetShapeWeight(const Value: Single);
  public
    constructor Create; virtual;
    function ShapeValueFixed(const Value: TFixed): TFixed; virtual;
    function ShapeValueFloat(const Value: Single): Single; virtual; abstract;
    property ShapeWeight: Single read FShapeWeight write SetShapeWeight;
    property ShapeValid: Boolean read FShapeValid;
    procedure PrepareShape; virtual;
    procedure FinalizeShape; virtual;
  end;
  TCustomShapeClass = class of TCustomShape;

  { Shape callback proc types }

  TShapeValueFixed = function (const Value: TFixed): TFixed of object;
  TShapeValueFloat = function (const Value: Single): Single of object;

var
  { ShapeList class registerlist }
  ShapeList: TList;

type
  { TConstantShape - sets result to a constant value }

  TConstantShape = class(TCustomShape)
  private
    FConstantValue: Single;
    FixedConstantValue: TFixed;
    procedure SetConstantValue(const Value: Single);
  public
    constructor Create; override;
    property ConstantValue: Single read FConstantValue write SetConstantValue;
    function ShapeValueFixed(const Value: TFixed): TFixed; override;
    function ShapeValueFloat(const Value: Single): Single; override;
  end;

  { TLinearShape }

  TLinearShape = class(TCustomShape)
  public
    function ShapeValueFixed(const Value: TFixed): TFixed; override;
    function ShapeValueFloat(const Value: Single): Single; override;
  end;

  { TGaussianShape }

  TGaussianShape = class(TCustomShape)
  public
    function ShapeValueFloat(const Value: Single): Single; override;
  end;

  TDefaultShape = class(TLinearShape);


  { TCustomCombiner - Base class for combining values }

  TCustomCombiner = class
  private
    FShape: TCustomShape;
    FCombineRect: TFloatRect;
    FCombineValid: Boolean;
    FMasterWeight: Single;
    procedure SetMasterWeight(Value: Single);
    function GetCombineValid: Boolean;
    procedure SetShape(Shape: TCustomShape);
    function GetShapeClassName: string;
    procedure SetShapeClassName(Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property MasterWeight: Single read FMasterWeight write SetMasterWeight;
    property CombineValid: Boolean read GetCombineValid;
    procedure PrepareCombine(CombineRect: TFloatRect); virtual;
    procedure FinalizeCombine; virtual;
  published
    property ShapeClassName: string read GetShapeClassName write SetShapeClassName;
    property Shape: TCustomShape read FShape write SetShape;
  end;

  { TCustomVectorCombiner - baseclass for all vectorcombiners, a simple framework
    for combining points
   'F': point to be merged/combined, weighted by masterweight ("foreground point")

   'P': progression variables from the loop, which is in the following ranges
        for the different callback versions (both P.X and P.Y):
          CombineInt, CombineFixed, P = [0..FixedOne ($10000)]
          CombineFloat, P = [0..1]
        The P variable allows different sorts of maskings.

   'B': basepoint on which F is merged ("background point")
   }
  TCustomVectorCombiner = class(TCustomCombiner)
  public
    procedure CombineInt(const F: TPoint; const P: TFixedPoint; var B: TPoint); virtual;
    procedure CombineFixed(const F, P: TFixedPoint; var B: TFixedPoint); virtual;
    procedure CombineFloat(const F, P: TFloatPoint; var B: TFloatPoint); virtual; abstract;
  end;
  TCustomVectorCombinerClass = class of TCustomVectorCombiner;

  { Vectorcombiner callback proc types }
  TVectorCombineInt = procedure(const F, P: TPoint; var B: TPoint) of object;
  TVectorCombineFixed = procedure(const F, P: TFixedPoint; var B: TFixedPoint) of object;
  TVectorCombineFloat = procedure(const F, P: TFloatPoint; var B: TFloatPoint) of object;

var
  VectorCombinerList: TList;

type

  { TAdditionVectorCombiner }
  TAdditionVectorCombiner = class(TCustomVectorCombiner)
  public
    procedure CombineFloat(const F, P: TFloatPoint; var B: TFloatPoint); override;
  end;

  { TSubtractionVectorCombiner }
  TSubtractionVectorCombiner = class(TCustomVectorCombiner)
  public
    procedure CombineFloat(const F, P: TFloatPoint; var B: TFloatPoint); override;
  end;

  { TMultiplicationVectorCombiner }
  TMultiplicationVectorCombiner = class(TCustomVectorCombiner)
  public
    procedure CombineFloat(const F, P: TFloatPoint; var B: TFloatPoint); override;
  end;

  { TDifferenceVectorCombiner }
  TDifferenceVectorCombiner = class(TCustomVectorCombiner)
  public
    procedure CombineFloat(const F, P: TFloatPoint; var B: TFloatPoint); override;
  end;

  TTransformationMap = class(TCustomMap)
  private
    FBits: TArrayOfFixedPoint;
    FVectorCombiner: TCustomVectorCombiner;
    function GetBits: PFixedPointArray;

    function GetXPoint(X,Y: Integer): TFixedPoint;
    function GetXPointS(X,Y: Integer): TFixedPoint;
    function GetXPointX(X,Y: TFixed): TFixedPoint;
    function GetXPointXS(X,Y: TFixed): TFixedPoint;

    function GetFPoint(X,Y: Integer): TFloatPoint;
    function GetFPointS(X,Y: Integer): TFloatPoint;
    function GetFPointF(X,Y: Single): TFloatPoint;
    function GetFPointFS(X,Y: Single): TFloatPoint;

    procedure SetXPoint(X,Y: Integer; const Point: TFixedPoint);
    procedure SetXPointS(X,Y: Integer; const Point: TFixedPoint);
    procedure SetXPointX(X,Y: TFixed; const Point: TFixedPoint);
    procedure SetXPointXS(X,Y: TFixed; const Point: TFixedPoint);

    procedure SetFPoint(X,Y: Integer; const Point: TFloatPoint);
    procedure SetFPointS(X,Y: Integer; const Point: TFloatPoint);
    procedure SetFPointF(X,Y: Single; const Point: TFloatPoint);
    procedure SetFPointFS(X,Y: Single; const Point: TFloatPoint);

    procedure SetVectorCombiner(VectorCombiner: TCustomVectorCombiner);
    function GetVectorCombinerClassName: string;
    procedure SetVectorCombinerClassName(Value: string);
  protected
    procedure ChangeSize(var Width, Height: Integer; NewWidth,
      NewHeight: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    procedure Merge(DstLeft, DstTop: Integer; Src: TTransformationMap;
      SrcRect: TRect; Weight: Single);

    property Bits: PFixedPointArray read GetBits;
    function BoundsRect: TRect;
    function IsEmpty: Boolean;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property FixedPointMap[X, Y: Integer]: TFixedPoint read GetXPoint
      write SetXPoint; default;
    property FixedPointMapS[X, Y: Integer]: TFixedPoint read GetXPointS
      write SetXPointS;
    property FixedPointMapX[X, Y: TFixed]: TFixedPoint read GetXPointX
      write SetXPointX;
    property FixedPointMapXS[X, Y: TFixed]: TFixedPoint read GetXPointXS
      write SetXPointXS;

    property FloatPointMap[X, Y: Integer]: TFloatPoint read GetFPoint
      write SetFPoint;
    property FloatPointMapS[X, Y: Integer]: TFloatPoint read GetFPointS
      write SetFPointS;
    property FloatPointMapF[X, Y: Single]: TFloatPoint read GetFPointF
      write SetFPointF;
    property FloatPointMapFS[X, Y: Single]: TFloatPoint read GetFPointFS
      write SetFPointFS;
  published
    property VectorCombinerClassName: string read GetVectorCombinerClassName write SetVectorCombinerClassName;
    property VectorCombiner: TCustomVectorCombiner read FVectorCombiner write SetVectorCombiner;
  end;

  TRemapTransformation = class(TTransformation)
  private
    ScalingFixed: TFixedPoint;
    ScalingFloat: TFloatPoint;

    SrcTranslationFixed: TFixedPoint;
    SrcScaleFixed: TFixedPoint;
    DstTranslationFixed: TFixedPoint;
    DstScaleFixed: TFixedPoint;

    SrcTranslationFloat: TFloatPoint;
    SrcScaleFloat: TFloatPoint;
    DstTranslationFloat: TFloatPoint;
    DstScaleFloat: TFloatPoint;

    FMappingRect: TFloatRect;
    procedure SetMappingRect(Rect: TFloatRect);
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); override;
    procedure ReverseTransformInt(DstX, DstY: Integer; out SrcX, SrcY: Integer); override;
    procedure ReverseTransformFloat(DstX, DstY: Single; out SrcX, SrcY: Single); override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;
  public
    TransformationMap : TTransformationMap;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RenderTransformation(Transformation: TTransformation; DstRect: TRect);
    function  GetTransformedBounds: TRect; override;
    procedure Scale(Sx, Sy: Single);
    property MappingRect: TFloatRect read FMappingRect write SetMappingRect;
  end;

{ General routines for registering and setting up custom class registerlists }

procedure RegisterCustomClass(CustomClass: TClass;var CustomRegister: TList);
function GetCustomClassNames(CustomRegister: TList): TStrings;
function FindCustomClass(ClassName: string; CustomRegister: TList): TClass;

implementation

uses Math, GR32_Lowlevel, GR32_Blend;

type
  TTransformationAccess = class(TTransformation);


{ General routines for registering and setting up custom class registerlists }

procedure RegisterCustomClass(CustomClass: TClass; var CustomRegister: TList);
begin
  if not Assigned(CustomRegister) then
    CustomRegister := TList.Create;
  CustomRegister.Add(CustomClass);
end;

function GetCustomClassNames(CustomRegister: TList): TStrings;
var
  I: Integer;
begin
  if not Assigned(CustomRegister) then
    Result := nil
  else
  begin
    Result := TStringList.Create;
    for I := 0 to CustomRegister.Count - 1 do
      Result.Add(TClass(CustomRegister.List[I]).ClassName);
  end;
end;

function FindCustomClass(ClassName: string; CustomRegister: TList): TClass;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(CustomRegister) then
    for I := 0 to CustomRegister.Count - 1 do
      if TClass(CustomRegister.List[I]).ClassName = ClassName then
      begin
        Result := TClass(CustomRegister.List[I]);
        Exit;
      end;
end;

{ TTransformationMap }

function CombinePointsReg(const A, B: TFixedPoint; Weight256: Integer): TFixedPoint;
begin
  Result.X := A.X +  SAR_8((B.X - A.X) * Weight256);
  Result.Y := A.Y +  SAR_8((B.Y - A.Y) * Weight256);
end;

procedure CombinePointsMem(const A: TFixedPoint;var  B: TFixedPoint; Weight256: Integer);
begin
  B.X := A.X +  SAR_8((B.X - A.X) * Weight256);
  B.Y := A.Y +  SAR_8((B.Y - A.Y) * Weight256);
end;

function TTransformationMap.BoundsRect: TRect;
begin
  Result := Rect(0,0, Width, Height);
end;

procedure TTransformationMap.ChangeSize(var Width, Height: Integer;
  NewWidth, NewHeight: Integer);
begin
  inherited;
  FBits := nil;
  Width := 0;
  Height := 0;
  if (NewWidth > 0) and (NewHeight > 0) then
  begin
    SetLength(FBits, NewWidth * NewHeight);
    if FBits = nil then raise Exception.Create('Can''t allocate TransformationMap!');
    FillLongword(FBits[0], NewWidth * NewHeight * 2, 0);
  end;
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TTransformationMap.Clear;
begin
  FillLongword(FBits[0], Width * Height * 2, 0);
end;

constructor TTransformationMap.Create;
begin
  inherited;
  FVectorCombiner := TAdditionVectorCombiner.Create;
end;

destructor TTransformationMap.Destroy;
begin
  Lock;
  try
    SetSize(0, 0);
    FVectorCombiner.Free;
  finally
    Unlock;
  end;
  inherited;
end;

function TTransformationMap.GetBits: PFixedPointArray;
begin
  Result := @FBits[0];
end;

function TTransformationMap.GetFPoint(X, Y: Integer): TFloatPoint;
begin
  Result := FloatPoint(FBits[X + Y * Width]);
end;

function TTransformationMap.GetFPointF(X, Y: Single): TFloatPoint;
begin
  Result := FloatPoint(GetXPointX(Fixed(X), Fixed(Y)));
end;

function TTransformationMap.GetFPointFS(X, Y: Single): TFloatPoint;
begin
  Result := FloatPoint(GetXPointXS(Fixed(X), Fixed(Y)));
end;

function TTransformationMap.GetFPointS(X, Y: Integer): TFloatPoint;
begin
  if X < 0 then X := 0 else
    if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0 else
    if Y >= Height then Y := Height - 1;
  Result := GetFPoint(X,Y);
end;

function TTransformationMap.GetVectorCombinerClassName: string;
begin
   Result := FVectorCombiner.ClassName;
end;

function TTransformationMap.GetXPoint(X, Y: Integer): TFixedPoint;
begin
  Result:= FBits[X + Y * Width];
end;

function TTransformationMap.GetXPointS(X, Y: Integer): TFixedPoint;
begin
  if X < 0 then X := 0 else
    if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0 else
    if Y >= Height then Y := Height - 1;
  Result := GetXPoint(X,Y);
end;

function TTransformationMap.GetXPointX(X, Y: TFixed): TFixedPoint;
const
  Next = SizeOf(TFixedPoint);
var
  WX,WY: Integer;
  P, W: Integer;
begin
  WX := SAR_16(X + $807E);
  WY := SAR_16(Y + $807E);
  W := Width;
  if (WX >= 0) and (WX < W) and (WY >= 0) and (WY < Height) then
  begin
    P :=  Integer(@FBits[WX + WY * W]);
    W := W * Next;
    WX := SAR_8(X + $7F) and $FF;
    WY := SAR_8(Y + $7F) and $FF;
    Result := CombinePointsReg(CombinePointsReg(PFixedPoint(P)^, PFixedPoint(P + Next)^, WX),
                               CombinePointsReg(PFixedPoint(P + W)^, PFixedPoint(P + W + Next)^, WX), WY);
  end else
  begin
    Result.X:= 0;
    Result.Y:= 0;
  end;
end;

function TTransformationMap.GetXPointXS(X, Y: TFixed): TFixedPoint;
var
  WX,WY: Integer;
begin
  WX := SAR_8(X) and $FF;
  WY := SAR_8(Y) and $FF;

  X := SAR_16(X);
  Y := SAR_16(Y);

  Result := CombinePointsReg(CombinePointsReg(FixedPointMapS[X,Y],
                                              FixedPointMapS[X + 1,Y], WX),
                             CombinePointsReg(FixedPointMapS[X,Y + 1],
                                              FixedPointMapS[X + 1,Y + 1], WX),
                                              WY);
end;

function TTransformationMap.IsEmpty: Boolean;
begin
  Result := false;
  if (Width = 0) or (Height = 0) or (FBits = nil)then Result := True;
end;

const
  MeshIdent = 'yfqLhseM';

type
  {TTransformationMap supports the photoshop liquify mesh fileformat .msh}
  TPSLiquifyMeshHeader = record
    Pad0  : dword;
    Ident : array [0..7] of Char;
    Pad1  : dword;
    Width : dword;
    Height: dword;
  end;

procedure TTransformationMap.LoadFromFile(const FileName: string);

  procedure ConvertVertices;
  var
    i: Integer;
  begin
    for i:= 0 to Length( FBits ) - 1 do
      FBits[i]:= FixedPoint( TFloatPoint( FBits[i] ) ); //Not a mistake!
  end;

var
  Header: TPSLiquifyMeshHeader;
  MeshFile: File;
begin
  If FileExists( Filename ) then try
    AssignFile(MeshFile, FileName);
    Reset(MeshFile, 1);
    BlockRead(MeshFile, Header, SizeOf(TPSLiquifyMeshHeader));
    if Lowercase(String(Header.Ident)) <> Lowercase(MeshIdent) then
      Exception.Create('Bad format - Photoshop .msh expected!');
    with Header do begin
      SetSize( Width, Height );
      BlockRead(MeshFile, FBits[0], Width * Height * SizeOf(TFixedPoint));
      ConvertVertices;
    end;
  finally
    CloseFile(MeshFile);
  end else Exception.Create('File not found!');
end;

procedure TTransformationMap.Merge(DstLeft, DstTop: Integer;
  Src: TTransformationMap; SrcRect: TRect; Weight: Single);
var
  I,J,P,Q: Integer;
  DstRect: TRect;
  SrcP, DstP, Progression: TFixedPoint;
  ProgressionX, ProgressionY: TFixed;
  Combiner: TVectorCombineFixed;
begin
  if Src.IsEmpty then Exception.Create('Src is empty!');
  if IsEmpty then Exception.Create('Base is empty!');
  if Weight <= 0 then Exit;
  EnsureRange(Weight, 0, 1);
  IntersectRect( SrcRect, Src.BoundsRect, SrcRect);

  DstRect.Left := DstLeft;
  DstRect.Top := DstTop;
  DstRect.Right := SrcRect.Right - SrcRect.Left;
  DstRect.Bottom := SrcRect.Bottom - SrcRect.Top;

  IntersectRect(DstRect, BoundsRect, DstRect);
  if IsRectEmpty(DstRect) then Exit;

  P := SrcRect.Left;
  Q := SrcRect.Top;

  ProgressionX := Fixed(1 / (DstRect.Right - DstRect.Left - 1));
  ProgressionY := Fixed(1 / (DstRect.Bottom - DstRect.Top - 1));

  with FVectorCombiner do
  begin
    Combiner := CombineFixed;
    if not CombineValid then PrepareCombine(FloatRect(DstRect));
  end;

  Progression.Y := 0;
  for I := DstRect.Top to DstRect.Bottom - 1 do
  begin
    Progression.X := 0;
    for J := DstRect.Top to DstRect.Bottom - 1 do
    begin
      SrcP := Src.FixedPointMap[P, Q];
      DstP := FixedPointMap[I, J];
      Combiner(SrcP, Progression, DstP);
      DstP.X := Round(DstP.X + (SrcP.X - DstP.X) * Weight);
      DstP.Y := Round(DstP.Y + (SrcP.Y - DstP.Y) * Weight);
      FixedPointMap[I, J] := DstP;
      Inc(P);
      Inc(Progression.X, ProgressionX);
    end;
    Inc(Q);
    Inc(Progression.Y, ProgressionY);
  end;
  VectorCombiner.FinalizeCombine;
end;

procedure TTransformationMap.SaveToFile(const FileName: string);

  procedure ConvertVerticesX;
  var i: Integer;
  begin
    for i := 0 to Length(FBits) - 1 do
      FBits[i] := FixedPoint(TFloatPoint(FBits[i])); //Not a mistake!
  end;

  procedure ConvertVerticesF;
  var i: Integer;
  begin
    for i := 0 to Length(FBits) - 1 do
      TFloatPoint(FBits[i]) := FloatPoint(FBits[i]); //Not a mistake!
  end;

var
  Header: TPSLiquifyMeshHeader;
  MeshFile: File;
  Pad: Cardinal;
begin
  try
    AssignFile(MeshFile, FileName);
    Rewrite(MeshFile, 1);
    with Header do begin
      Pad0 := $02000000;
      Ident := MeshIdent;
      Pad1 := $00000002;
      Width := Self.Width;
      Height := Self.Height;
    end;
    BlockWrite(MeshFile, Header, SizeOf(TPSLiquifyMeshHeader));
    with Header do begin
      ConvertVerticesF;
      BlockWrite(MeshFile, FBits[0], Length(FBits) * SizeOf(TFixedPoint));
      ConvertVerticesX;
    end;
    if Odd(Length(FBits) * SizeOf(TFixedPoint)-1) then begin
      Pad := $00000000;
      BlockWrite(MeshFile, Pad, 4);
      BlockWrite(MeshFile, Pad, 4);
    end;
  finally
    CloseFile(MeshFile);
  end;
end;

procedure TTransformationMap.SetFPoint(X, Y: Integer; const Point: TFloatPoint);
begin
  FBits[X + Y * Width] := FixedPoint(Point);
end;

procedure TTransformationMap.SetFPointF(X, Y: Single; const Point: TFloatPoint);
begin
  SetXPointX(Fixed(X), Fixed(Y), FixedPoint(Point));
end;

procedure TTransformationMap.SetFPointFS(X, Y: Single; const Point: TFloatPoint);
begin
  SetXPointXS(Fixed(X), Fixed(Y), FixedPoint(Point));
end;

procedure TTransformationMap.SetFPointS(X, Y: Integer; const Point: TFloatPoint);
begin
  if X < 0 then X := 0 else
    if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0 else
    if Y >= Height then Y := Height - 1;
  SetFPoint(X, Y, Point);
end;

procedure TTransformationMap.SetVectorCombiner(
  VectorCombiner: TCustomVectorCombiner);
begin
  if Assigned(VectorCombiner) then
  begin
    if Assigned(FVectorCombiner) then FVectorCombiner.Free;
    FVectorCombiner := VectorCombiner;
    Changed;
  end;
end;

procedure TTransformationMap.SetVectorCombinerClassName(Value: string);
var
  VectorCombinerClass: TCustomVectorCombinerClass;
begin
  if (Value <> '') and (FVectorCombiner.ClassName <> Value) then
  begin
    VectorCombinerClass := TCustomVectorCombinerClass(FindCustomClass(Value, VectorCombinerList));
    if Assigned(VectorCombinerClass) then
    begin
      FVectorCombiner.Free;
      FVectorCombiner := VectorCombinerClass.Create;
      Changed;
    end;
  end;
end;

procedure TTransformationMap.SetXPoint(X, Y: Integer; const Point: TFixedPoint);
begin
  FBits[X + Y * Width] := Point;
end;

procedure TTransformationMap.SetXPointS(X, Y: Integer; const Point: TFixedPoint);
begin
  if X < 0 then X := 0 else
    if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0 else
    if Y >= Height then Y := Height - 1;
  SetXPoint(X, Y, Point);
end;

procedure TTransformationMap.SetXPointX(X, Y: TFixed; const Point: TFixedPoint);
var
  flrx, flry, celx, cely: Integer;
  P: PFixedPoint;
begin
  flrx := X and $FFFF;
  flry := Y and $FFFF;

  asm
    SAR X, 16
    SAR Y, 16
  end;

  celx := flrx xor $FFFF;
  cely := flry xor $FFFF;

  P := @FBits[X + Y * Width];

  CombinePointsMem(Point, P^, FixedMul(celx, cely) ); Inc(P);
  CombinePointsMem(Point, P^, FixedMul(flrx, cely) ); Inc(P, Width);
  CombinePointsMem(Point, P^, FixedMul(flrx, flry) ); Dec(P);
  CombinePointsMem(Point, P^, FixedMul(celx, flry) );
end;

procedure TTransformationMap.SetXPointXS(X, Y: TFixed; const Point: TFixedPoint);
var
  flrx, flry, celx, cely: Integer;
  P: PFixedPoint;
begin
  if (X < -$10000) or (Y < -$10000) then Exit;

  flrx := X and $FFFF;
  flry := Y and $FFFF;

  asm
    SAR X, 16
    SAR Y, 16
  end;

  if (X >= Width) or (Y >= Height) then Exit;

  celx := flrx xor $FFFF;
  cely := flry xor $FFFF;
  P := @FBits[X + Y * Width];

  if (X >= 0) and (Y >= 0)then
  begin
    CombinePointsMem(Point, P^, FixedMul(celx, cely) ); Inc(P);
    CombinePointsMem(Point, P^, FixedMul(flrx, cely) ); Inc(P, Width);
    CombinePointsMem(Point, P^, FixedMul(flrx, flry) ); Dec(P);
    CombinePointsMem(Point, P^, FixedMul(celx, flry) );
  end
  else
  begin
    if (X >= 0) and (Y >= 0) then CombinePointsMem(Point, P^, FixedMul(celx, cely)); Inc(P);
    if (X < Width - 1) and (Y >= 0) then CombinePointsMem(Point, P^, FixedMul(flrx, cely)); Inc(P, Width);
    if (X < Width - 1) and (Y < Height - 1) then CombinePointsMem(Point, P^, FixedMul(flrx, flry)); Dec(P);
    if (X >= 0) and (Y < Height - 1) then CombinePointsMem(Point, P^, FixedMul(celx, flry));
  end;
end;

{ TRemapTransformation }

constructor TRemapTransformation.Create;
begin
  inherited;
  ScalingFixed := FixedPoint(1, 1);
  ScalingFloat := FloatPoint(1, 1);
  TransformationMap := TTransformationMap.Create;
  //Ensuring initial setup to avoid exceptions
  TransformationMap.SetSize(1, 1);
end;

destructor TRemapTransformation.Destroy;
begin
  TransformationMap.Free;
  inherited;
end;

function TRemapTransformation.GetTransformedBounds: TRect;
begin
  Result := TransformationMap.BoundsRect;
//  Dec(Result.Right);
//  Dec(Result.Bottom);
end;

procedure TRemapTransformation.PrepareTransform;
begin
  if IsRectEmptyF(SrcRect) then raise Exception.Create('SrcRect is empty!');
  if IsRectEmptyF(FMappingRect) then raise Exception.Create('MappingRect is empty!');
  with SrcRect do
  begin
    SrcTranslationFloat.X := Left;
    SrcTranslationFloat.Y := Top;
    SrcScaleFloat.X := 1 / ((TransformationMap.Width - 1) / (Right - Left));
    SrcScaleFloat.Y := 1 / ((TransformationMap.Height - 1) / (Bottom - Top));
    SrcTranslationFixed := FixedPoint(SrcTranslationFloat);
    SrcScaleFixed := FixedPoint(SrcScaleFloat);
  end;

  with FMappingRect do
  begin
    DstTranslationFloat.X := Left;
    DstTranslationFloat.Y := Top;
    DstScaleFloat.X := (TransformationMap.Width - 1) / (Right - Left);
    DstScaleFloat.Y := (TransformationMap.Height - 1) / (Bottom - Top);
    DstTranslationFixed := FixedPoint(DstTranslationFloat);
    DstScaleFixed := FixedPoint(DstScaleFloat);
  end;
end;

procedure TRemapTransformation.RenderTransformation(
  Transformation: TTransformation; DstRect: TRect);
var
  I, J: Integer;
  P, Q, Progression: TFixedPoint;
  ProgressionX, ProgressionY: TFixed;
  Combiner: TVectorCombineFixed;
  MapPtr: PFixedPointArray;
begin
  IntersectRect(DstRect, TransformationMap.BoundsRect, DstRect);
  if IsRectEmpty(DstRect) then Exit;

  if not TTransformationAccess(Transformation).TransformValid then
    TTransformationAccess(Transformation).PrepareTransform;

  ProgressionX := Fixed(2 / (DstRect.Right - DstRect.Left - 1));
  ProgressionY := Fixed(2 / (DstRect.Bottom - DstRect.Top - 1));

  with TransformationMap.VectorCombiner do
  begin
    Combiner := CombineFixed;
    if not CombineValid then PrepareCombine(FloatRect(DstRect));
  end;

  Progression.Y := - FixedOne;
  with DstRect do for I := Top to Bottom - 1 do
  begin
    Progression.X := - FixedOne;
    MapPtr := @TransformationMap.GetBits[I * TransformationMap.Width];
    for J := Left to Right - 1 do
    begin
      //Subtract loop vars to ensure correct transformation output
      P := FixedPoint(J - Left, I - Top);
      Q := Transformation.ReverseTransform(P);
      //Make the transformed vector relative
      Q.X := Q.X - P.X;
      Q.Y := Q.Y - P.Y;
      Combiner(Q, Progression, MapPtr[J]);
      Inc(Progression.X, ProgressionX);
    end;
   Inc(Progression.Y, ProgressionY);
  end;
  TransformationMap.VectorCombiner.FinalizeCombine;
  TransformValid := False;
end;

procedure TRemapTransformation.ReverseTransform256(DstX, DstY: Integer;
  out SrcX256, SrcY256: Integer);
begin
  with TransformationMap.FixedPointMap[DstX, DstY] do
  begin
    DstX:= DstX * FixedOne - DstTranslationFixed.X;
    DstY:= DstY * FixedOne - DstTranslationFixed.Y;
    DstX := FixedMul(DstX , DstScaleFixed.X);
    DstY := FixedMul(DstY , DstScaleFixed.Y);

    DstX:= DstX + FixedMul(X, ScalingFixed.X);
    DstY:= DstY + FixedMul(Y, ScalingFixed.Y);

    DstX := FixedMul(DstX, SrcScaleFixed.X);
    DstY := FixedMul(DstY, SrcScaleFixed.Y);
    DstX:= DstX + SrcTranslationFixed.X;
    DstY:= DstY + SrcTranslationFixed.Y;

    SrcX256 := SAR_8(DstX + $7F);
    SrcY256 := SAR_8(DstY + $7F);
  end;
end;

procedure TRemapTransformation.ReverseTransformFixed(DstX, DstY: TFixed;
  out SrcX, SrcY: TFixed);
begin
  with TransformationMap.FixedPointMapX[DstX, DstY] do
  begin
    DstX := DstX - DstTranslationFixed.X;
    DstY := DstY - DstTranslationFixed.Y;
    DstX := FixedMul(DstX , DstScaleFixed.X);
    DstY := FixedMul(DstY , DstScaleFixed.Y);

    DstX:= DstX + FixedMul(X, ScalingFixed.X);
    DstY:= DstY + FixedMul(Y, ScalingFixed.Y);

    DstX := FixedMul(DstX, SrcScaleFixed.X);
    DstY := FixedMul(DstY, SrcScaleFixed.Y);
    SrcX := DstX + SrcTranslationFixed.X;
    SrcY := DstY + SrcTranslationFixed.Y;
  end;
end;

procedure TRemapTransformation.ReverseTransformFloat(DstX, DstY: Single;
  out SrcX, SrcY: Single);
begin
  with TransformationMap.FloatPointMapF[DstX, DstY] do
  begin
    DstX := DstX - DstTranslationFloat.X;
    DstY := DstY - DstTranslationFloat.Y;
    DstX := DstX * DstScaleFloat.X;
    DstY := DstY * DstScaleFloat.Y;

    DstX:= DstX + X * ScalingFloat.X;
    DstY:= DstY + Y * ScalingFloat.Y;

    DstX := DstX * SrcScaleFloat.X;
    DstY := DstY * SrcScaleFloat.Y;
    SrcX := DstX + SrcTranslationFloat.X;
    SrcY := DstY + SrcTranslationFloat.Y;
  end;
end;

procedure TRemapTransformation.ReverseTransformInt(DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
begin
  with TransformationMap.FixedPointMap[DstX, DstY] do
  begin
    DstX := DstX * FixedOne - DstTranslationFixed.X;
    DstY := DstY * FixedOne - DstTranslationFixed.Y;
    DstX := FixedMul(DstX , DstScaleFixed.X);
    DstY := FixedMul(DstY , DstScaleFixed.Y);

    DstX := DstX + FixedMul(X, ScalingFixed.X);
    DstY := DstY + FixedMul(Y, ScalingFixed.Y);

    DstX := FixedMul(DstX, SrcScaleFixed.X);
    DstY := FixedMul(DstY, SrcScaleFixed.Y);
    SrcX := FixedRound(DstX + SrcTranslationFixed.X);
    SrcY := FixedRound(DstY + SrcTranslationFixed.Y);
  end;
end;

procedure TRemapTransformation.Scale(Sx, Sy: Single);
begin
  ScalingFixed.X := Fixed(Sx);
  ScalingFixed.Y := Fixed(Sy);
  ScalingFloat.X := Sx;
  ScalingFloat.Y := Sy;
end;

procedure TRemapTransformation.SetMappingRect(Rect: TFloatRect);
begin
  FMappingRect := Rect;
  TransformValid := False;
end;

{ TCustomCombine }

constructor TCustomCombiner.Create;
begin
  FMasterWeight := 1.0;
  FCombineValid := False;
  Shape := TDefaultShape.Create;
end;

destructor TCustomCombiner.Destroy;
begin
  Shape.Free;
  inherited;
end;

procedure TCustomCombiner.FinalizeCombine;
begin
  Shape.FinalizeShape;
  FCombineValid := False;
end;

function TCustomCombiner.GetCombineValid: Boolean;
begin
  Result := FCombineValid and Shape.ShapeValid;
end;

function TCustomCombiner.GetShapeClassName: string;
begin
   Result := FShape.ClassName;
end;

procedure TCustomCombiner.SetShape(
  Shape: TCustomShape);
begin
  if Assigned(Shape) then
  begin
    if Assigned(FShape) then FShape.Free;
    FShape := Shape;
    FCombineValid := False;
  end;
end;

procedure TCustomCombiner.SetShapeClassName(Value: string);
var
  ShapeClass: TCustomShapeClass;
begin
  if (Value <> '') and (FShape.ClassName <> Value) then
  begin
    ShapeClass := TCustomShapeClass(FindCustomClass(Value, ShapeList));
    if Assigned(ShapeClass) then
    begin
      FShape.Free;
      FShape := ShapeClass.Create;
      FCombineValid := False;
    end;
  end;
end;

procedure TCustomCombiner.PrepareCombine(CombineRect: TFloatRect);
begin
  if IsRectEmptyF(CombineRect) then raise Exception.Create('CombineRect is empty!');
  if not Shape.ShapeValid then Shape.PrepareShape;
  FCombineRect := CombineRect;
  FCombineValid := True;
end;

procedure TCustomCombiner.SetMasterWeight(Value: Single);
begin
  FMasterWeight := Value;
  FCombineValid := False;
end;

{ TCustomVectorCombine }

procedure TCustomVectorCombiner.CombineFixed(const F, P: TFixedPoint;
  var B: TFixedPoint);
var
  _B: TFloatPoint;
begin
  EMMS;
  _B := FloatPoint(B);
  CombineFloat(FloatPoint(F), FloatPoint(P), _B);
  B := FixedPoint(_B);
end;

procedure TCustomVectorCombiner.CombineInt(const F: TPoint; const P: TFixedPoint;
  var B: TPoint);
var
  _B, _P: TFloatPoint;
begin
  EMMS;
  _B := FloatPoint(B);
  _P.X := P.X * FixedToFloat;
  _P.Y := P.Y * FixedToFloat;
  CombineFloat(FloatPoint(F), _P, _B);
  B := Point(_B);
end;

{ TAdditionVectorCombiner }

procedure TAdditionVectorCombiner.CombineFloat(const F, P: TFloatPoint;
  var B: TFloatPoint);
var
  O: TFloatPoint;
  W: Single;
begin
  O.X := B.X + F.X;
  O.Y := B.Y + F.Y;

  with Shape do
    W := ShapeValueFloat(1 - Abs(P.X)) *
         ShapeValueFloat(1 - Abs(P.Y)) *
         FMasterWeight;

  B.X := B.X + (O.X - B.X) * W;
  B.Y := B.Y + (O.Y - B.Y) * W;
end;

{ TSubtractionVectorCombiner }

procedure TSubtractionVectorCombiner.CombineFloat(const F, P: TFloatPoint;
  var B: TFloatPoint);
var
  O: TFloatPoint;
  W: Single;
begin
  O.X := B.X - F.X;
  O.Y := B.Y - F.Y;

  with Shape do
    W := ShapeValueFloat(1 - Abs(P.X)) *
         ShapeValueFloat(1 - Abs(P.Y)) *
         FMasterWeight;

  B.X := B.X + (O.X - B.X) * W;
  B.Y := B.Y + (O.Y - B.Y) * W;
end;

{ TMultiplicationVectorCombiner }

procedure TMultiplicationVectorCombiner.CombineFloat(const F,
  P: TFloatPoint; var B: TFloatPoint);
var
  O: TFloatPoint;
  W: Single;
begin
  O.X := B.X * F.X;
  O.Y := B.Y * F.Y;

  with Shape do
    W := ShapeValueFloat(1 - Abs(P.X)) *
         ShapeValueFloat(1 - Abs(P.Y)) *
         FMasterWeight;

  B.X := B.X + (O.X - B.X) * W;
  B.Y := B.Y + (O.Y - B.Y) * W;
end;

{ TDifferenceVectorCombiner }

procedure TDifferenceVectorCombiner.CombineFloat(const F, P: TFloatPoint;
  var B: TFloatPoint);
var
  O: TFloatPoint;
  W: Single;
begin
  O.X := Abs(B.X - F.X);
  O.Y := Abs(B.Y - F.Y);

  with Shape do
    W := ShapeValueFloat(1 - Abs(P.X)) *
         ShapeValueFloat(1 - Abs(P.Y)) *
         FMasterWeight;

  B.X := B.X + (O.X - B.X) * W;
  B.Y := B.Y + (O.Y - B.Y) * W;
end;

{ TCustomShape }

constructor TCustomShape.Create;
begin
  ShapeWeight := 1.0;
  FShapeValid := False;
end;

procedure TCustomShape.FinalizeShape;
begin
  FShapeValid := False;
end;

procedure TCustomShape.PrepareShape;
begin
  FShapeValid := True;
end;

procedure TCustomShape.SetShapeWeight(const Value: Single);
begin
  FShapeWeight := Value;
  FixedShapeWeight := Fixed(Value);
  FShapeValid := False;
end;

function TCustomShape.ShapeValueFixed(const Value: TFixed): TFixed;
var
  S: Single;
begin
  EMMS;
  S := Value * FixedToFloat;
  Result := Fixed(ShapeValueFloat(S));
end;

{ TConstantShape }

constructor TConstantShape.Create;
begin
  inherited;
  ConstantValue := 1.0;
end;

procedure TConstantShape.SetConstantValue(const Value: Single);
begin
  FConstantValue := Value;
  FixedConstantValue := Fixed(Value);
  FShapeValid := False;
end;

function TConstantShape.ShapeValueFixed(const Value: TFixed): TFixed;
begin
  Result := FixedMul(FixedConstantValue, FixedShapeWeight);
end;

function TConstantShape.ShapeValueFloat(const Value: Single): Single;
begin
  Result := FConstantValue * FShapeWeight;
end;

{ TLinearShape }

function TLinearShape.ShapeValueFixed(const Value: TFixed): TFixed;
begin
  Result := FixedMul(Value, FixedShapeWeight);
end;

function TLinearShape.ShapeValueFloat(const Value: Single): Single;
begin
  Result := Value * FShapeWeight;
end;

{ TGaussianShape }

function TGaussianShape.ShapeValueFloat(const Value: Single): Single;
begin
  Result := Sqr(Sin(0.5 * Value * PI));
end;

initialization
  { Register VectorCombiners }
  RegisterCustomClass(TAdditionVectorCombiner, VectorCombinerList);
  RegisterCustomClass(TSubtractionVectorCombiner, VectorCombinerList);
  RegisterCustomClass(TMultiplicationVectorCombiner, VectorCombinerList);
  RegisterCustomClass(TDifferenceVectorCombiner, VectorCombinerList);

  { Register Shapes }
  RegisterCustomClass(TConstantShape, ShapeList);
  RegisterCustomClass(TLinearShape, ShapeList);
  RegisterCustomClass(TGaussianShape, ShapeList);

finalization
  VectorCombinerList.Free;
  ShapeList.Free;
end.
