unit GR32_VectorMaps;

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
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Mattias Andersson <mattias@centaurix.com>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
   Windows, Types, SysUtils, Classes, GR32, GR32_Transforms, GR32_Containers;

type
  TFixedVector = TFixedPoint;
  TFloatVector = TFloatPoint;
  TArrayOfFixedVector = array of TFixedVector;


  { TCustomContour - Base class for  Contours }

  TCustomContour = class
  private
    FContourValid: Boolean;
    FContourWeight: Single;
    FixedContourWeight: TFixed;
    procedure SetContourWeight(const Value: Single);
  public
    constructor Create; virtual;
    function ContourValueFixed(const Value: TFixed): TFixed; virtual;
    function ContourValueFloat(const Value: Single): Single; virtual; abstract;
    property ContourValid: Boolean read FContourValid;
    procedure PrepareContour; virtual;
    procedure FinalizeContour; virtual;
  published
    property ContourWeight: Single read FContourWeight write SetContourWeight;
  end;
  TCustomContourClass = class of TCustomContour;

  { Contour callback proc types }

  TContourValueFixed = function (const Value: TFixed): TFixed of object;
  TContourValueFloat = function (const Value: Single): Single of object;

var
  { ContourList class registerlist }
  ContourList: TClassList;

type
  { TConstantContour - sets result to a constant value }

  TConstantContour = class(TCustomContour)
  private
    FConstantValue: Single;
    FixedConstantValue: TFixed;
    procedure SetConstantValue(const Value: Single);
  public
    constructor Create; override;
    function ContourValueFixed(const Value: TFixed): TFixed; override;
    function ContourValueFloat(const Value: Single): Single; override;
  published
    property ConstantValue: Single read FConstantValue write SetConstantValue;
  end;

  { TLinearContour }

  TLinearContour = class(TCustomContour)
  public
    function ContourValueFixed(const Value: TFixed): TFixed; override;
    function ContourValueFloat(const Value: Single): Single; override;
  end;

  { TGaussianContour }

  TGaussianContour = class(TCustomContour)
  public
    function ContourValueFloat(const Value: Single): Single; override;
  end;

  { TLinearSineContour }

  TLinearSineContour = class(TCustomContour)
  private
    FWavesCount: Integer;
    procedure SetWavesCount(const Value: Integer);
  public
    constructor Create; override;
    function ContourValueFloat(const Value: Single): Single; override;
  published
    property WavesCount: Integer read FWavesCount write SetWavesCount;
  end;

  { TStairwayContour }

  TStairwayContour = class(TCustomContour)
  private
    Scaler : Single;
    FStepCount: Integer;
    procedure SetStepCount(const Value: Integer);
 public
    constructor Create; override;
    function ContourValueFloat(const Value: Single): Single; override;
  published
    property StepCount: Integer read FStepCount write SetStepCount;
  end;

  TDefaultContour = class(TConstantContour);


  { TCustomCombiner - Base class for combining values }

  TCustomCombiner = class
  private
    FContour: TCustomContour;
    FCombineRect: TFloatRect;
    FCombineValid: Boolean;
    FMasterWeight: Single;
    procedure SetMasterWeight(Value: Single);
    function GetCombineValid: Boolean;
    procedure SetContour(Contour: TCustomContour);
    function GetContourClassName: string;
    procedure SetContourClassName(Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property CombineValid: Boolean read GetCombineValid;
    procedure PrepareCombine(CombineRect: TFloatRect); virtual;
    procedure FinalizeCombine; virtual;
  published
    property MasterWeight: Single read FMasterWeight write SetMasterWeight;
    property ContourClassName: string read GetContourClassName write SetContourClassName;
    property Contour: TCustomContour read FContour write SetContour;
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
    procedure CombineInt(const F: TPoint; const P: TFixedVector; var B: TPoint); virtual;
    procedure CombineFixed(const F, P: TFixedVector; var B: TFixedVector); virtual;
    procedure CombineFloat(const F, P: TFloatVector; var B: TFloatVector); virtual; abstract;
  end;
  TCustomVectorCombinerClass = class of TCustomVectorCombiner;

  { Vectorcombiner callback proc types }
  TVectorCombineInt = procedure(const F, P: TPoint; var B: TPoint) of object;
  TVectorCombineFixed = procedure(const F, P: TFixedVector; var B: TFixedVector) of object;
  TVectorCombineFloat = procedure(const F, P: TFloatVector; var B: TFloatVector) of object;

var
  VectorCombinerList: TClassList;

type

  { TReplacementVectorCombiner }
  TReplacementVectorCombiner = class(TCustomVectorCombiner)
  public
    procedure CombineFloat(const F, P: TFloatVector; var B: TFloatVector); override;
  end;

  { TAdditionVectorCombiner }
  TAdditionVectorCombiner = class(TCustomVectorCombiner)
  public
    procedure CombineFloat(const F, P: TFloatVector; var B: TFloatVector); override;
  end;

  { TSubtractionVectorCombiner }
  TSubtractionVectorCombiner = class(TCustomVectorCombiner)
  public
    procedure CombineFloat(const F, P: TFloatVector; var B: TFloatVector); override;
  end;

  { TMultiplicationVectorCombiner }
  TMultiplicationVectorCombiner = class(TCustomVectorCombiner)
  public
    procedure CombineFloat(const F, P: TFloatVector; var B: TFloatVector); override;
  end;

  { TDifferenceVectorCombiner }
  TDifferenceVectorCombiner = class(TCustomVectorCombiner)
  public
    procedure CombineFloat(const F, P: TFloatVector; var B: TFloatVector); override;
  end;

  TVectorMap = class(TCustomMap)
  private
    FVectors: TArrayOfFixedVector;
    FVectorCombiner: TCustomVectorCombiner;
    function GetBits: PFixedPointArray;

    function GetFixedVector(X,Y: Integer): TFixedVector;
    function GetFixedVectorS(X,Y: Integer): TFixedVector;
    function GetFixedVectorX(X,Y: TFixed): TFixedVector;
    function GetFixedVectorXS(X,Y: TFixed): TFixedVector;

    function GetFloatVector(X,Y: Integer): TFloatVector;
    function GetFloatVectorS(X,Y: Integer): TFloatVector;
    function GetFloatVectorF(X,Y: Single): TFloatVector;
    function GetFloatVectorFS(X,Y: Single): TFloatVector;

    procedure SetFixedVector(X,Y: Integer; const Point: TFixedVector);
    procedure SetFixedVectorS(X,Y: Integer; const Point: TFixedVector);
    procedure SetFixedVectorX(X,Y: TFixed; const Point: TFixedVector);
    procedure SetFixedVectorXS(X,Y: TFixed; const Point: TFixedVector);

    procedure SetFloatVector(X,Y: Integer; const Point: TFloatVector);
    procedure SetFloatVectorS(X,Y: Integer; const Point: TFloatVector);
    procedure SetFloatVectorF(X,Y: Single; const Point: TFloatVector);
    procedure SetFloatVectorFS(X,Y: Single; const Point: TFloatVector);

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
    procedure Merge(DstLeft, DstTop: Integer; Src: TVectorMap;
      SrcRect: TRect);

    property Bits: PFixedPointArray read GetBits;
    function BoundsRect: TRect;
    function IsEmpty: Boolean;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property FixedVector[X, Y: Integer]: TFixedVector read GetFixedVector
      write SetFixedVector; default;
    property FixedVectorS[X, Y: Integer]: TFixedVector read GetFixedVectorS
      write SetFixedVectorS;
    property FixedVectorX[X, Y: TFixed]: TFixedVector read GetFixedVectorX
      write SetFixedVectorX;
    property FixedVectorXS[X, Y: TFixed]: TFixedVector read GetFixedVectorXS
      write SetFixedVectorXS;

    property FloatVector[X, Y: Integer]: TFloatVector read GetFloatVector
      write SetFloatVector;
    property FloatVectorS[X, Y: Integer]: TFloatVector read GetFloatVectorS
      write SetFloatVectorS;
    property FloatVectorF[X, Y: Single]: TFloatVector read GetFloatVectorF
      write SetFloatVectorF;
    property FloatVectorFS[X, Y: Single]: TFloatVector read GetFloatVectorFS
      write SetFloatVectorFS;
  published
    property VectorCombinerClassName: string read GetVectorCombinerClassName write SetVectorCombinerClassName;
    property VectorCombiner: TCustomVectorCombiner read FVectorCombiner write SetVectorCombiner;
  end;

  TRemapTransformation = class(TTransformation)
  private
    ScalingFixed: TFixedVector;
    ScalingFloat: TFloatVector;

    SrcTranslationFixed: TFixedVector;
    SrcScaleFixed: TFixedVector;
    DstTranslationFixed: TFixedVector;
    DstScaleFixed: TFixedVector;

    SrcTranslationFloat: TFloatVector;
    SrcScaleFloat: TFloatVector;
    DstTranslationFloat: TFloatVector;
    DstScaleFloat: TFloatVector;

    OffsetFixed : TFixedVector;
    OffsetInt : TPoint;

    FMappingRect: TFloatRect;
    FOffset: TFloatVector;
    procedure SetMappingRect(Rect: TFloatRect);
    procedure SetOffset(const Value: TFloatVector);
  protected
    procedure PrepareTransform; override;
//    procedure ReverseTransform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); override;
    procedure ReverseTransformInt(DstX, DstY: Integer; out SrcX, SrcY: Integer); override;
    procedure ReverseTransformFloat(DstX, DstY: Single; out SrcX, SrcY: Single); override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;
  public
    TransformationMap : TVectorMap;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RenderTransformation(Transformation: TTransformation; DstRect: TRect);
    function  GetTransformedBounds: TRect; override;
    procedure Scale(Sx, Sy: Single);
    property MappingRect: TFloatRect read FMappingRect write SetMappingRect;
    property Offset: TFloatVector read FOffset write SetOffset;
  end;

implementation

uses Math, GR32_Lowlevel, GR32_Blend;

type
  TTransformationAccess = class(TTransformation);

{ TVectorMap }

function CombinePointsReg(const A, B: TFixedVector; Weight: TFixed): TFixedVector;
begin
  Result.X := A.X + FixedMul(B.X - A.X, Weight);
  Result.Y := A.Y + FixedMul(B.Y - A.Y, Weight);
end;

procedure CombinePointsMem(const A: TFixedVector;var  B: TFixedVector; Weight: TFixed);
begin
  B.X := A.X + FixedMul(B.X - A.X, Weight);
  B.Y := A.Y + FixedMul(B.Y - A.Y, Weight);
end;

function TVectorMap.BoundsRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

procedure TVectorMap.ChangeSize(var Width, Height: Integer;
  NewWidth, NewHeight: Integer);
begin
  inherited;
  FVectors := nil;
  Width := 0;
  Height := 0;
  SetLength(FVectors, NewWidth * NewHeight);
  if (NewWidth > 0) and (NewHeight > 0) then
  begin
    if FVectors = nil then raise Exception.Create('Can''t allocate TransformationMap!');
    FillLongword(FVectors[0], NewWidth * NewHeight * 2, 0);
  end;
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TVectorMap.Clear;
begin
  FillLongword(FVectors[0], Width * Height * 2, 0);
end;

constructor TVectorMap.Create;
begin
  inherited;
  FVectorCombiner := TAdditionVectorCombiner.Create;
end;

destructor TVectorMap.Destroy;
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

function TVectorMap.GetBits: PFixedPointArray;
begin
  Result := @FVectors[0];
end;

function TVectorMap.GetFloatVector(X, Y: Integer): TFloatVector;
begin
  Result := FloatPoint(FVectors[X + Y * Width]);
end;

function TVectorMap.GetFloatVectorF(X, Y: Single): TFloatVector;
begin
  Result := FloatPoint(GetFixedVectorX(Fixed(X), Fixed(Y)));
end;

function TVectorMap.GetFloatVectorFS(X, Y: Single): TFloatVector;
begin
  Result := FloatPoint(GetFixedVectorXS(Fixed(X), Fixed(Y)));
end;

function TVectorMap.GetFloatVectorS(X, Y: Integer): TFloatVector;
begin
  if X < 0 then X := 0 else
    if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0 else
    if Y >= Height then Y := Height - 1;
  Result := GetFloatVector(X,Y);
end;

function TVectorMap.GetVectorCombinerClassName: string;
begin
   Result := FVectorCombiner.ClassName;
end;

function TVectorMap.GetFixedVector(X, Y: Integer): TFixedVector;
begin
  Result := FVectors[X + Y * Width];
end;

function TVectorMap.GetFixedVectorS(X, Y: Integer): TFixedVector;
begin
  if X < 0 then X := 0 else
    if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0 else
    if Y >= Height then Y := Height - 1;
  Result := GetFixedVector(X,Y);
end;

{ function TVectorMap.GetFixedVectorX(X, Y: TFixed): TFixedVector;
const
  Next = SizeOf(TFixedVector);
var
  WX,WY: TFixed;
  P, W: Integer;
begin
  WX := SAR_16(X + $807E);
  WY := SAR_16(Y + $807E);
  W := Width;
  if (WX >= 0) and (WX < W - 1) and (WY >= 0) and (WY < Height - 1) then
  begin
    P := Integer(@FVectors[WX + WY * W]);
    W := W * Next;
    WX := (X + $807E) and $FFFF;
    WY := (Y + $807E) and $FFFF;
    Result := CombinePointsReg(CombinePointsReg(PFixedPoint(P)^, PFixedPoint(P + Next)^, WX),
                               CombinePointsReg(PFixedPoint(P + W)^, PFixedPoint(P + W + Next)^, WX), WY);
  end else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end; }

function TVectorMap.GetFixedVectorX(X, Y: TFixed): TFixedVector;
const
  Next = SizeOf(TFixedVector);
var
  WX,WY: TFixed;
  P, W, H: Integer;
begin
  WX := SAR_16(X + $807E);
  WY := SAR_16(Y + $807E);
  W := Width;
  H := Next;
  if (WX >= 0) and (WX <= W - 1) and (WY >= 0) and (WY <= Height - 1) then
  begin
    P := Integer(@FVectors[WX + WY * W]);
    if (WY = Height - 1) then W := 0 else W := W * Next;
    if (WX = W - 1) then H := Next else H := 0;
    WX := (X + $807E) and $FFFF;
    WY := (Y + $807E) and $FFFF;
    Result := CombinePointsReg(CombinePointsReg(PFixedPoint(P)^, PFixedPoint(P + H)^, WX),
                               CombinePointsReg(PFixedPoint(P + W)^, PFixedPoint(P + W + H)^, WX), WY);
  end else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

function TVectorMap.GetFixedVectorXS(X, Y: TFixed): TFixedVector;
var
  WX,WY: TFixed;
begin
  WX := X and $FFFF;
  WY := Y and $FFFF;

  X := SAR_16(X);
  Y := SAR_16(Y);

  Result := CombinePointsReg(CombinePointsReg(FixedVectorS[X,Y],
                                              FixedVectorS[X + 1,Y], WX),
                             CombinePointsReg(FixedVectorS[X,Y + 1],
                                              FixedVectorS[X + 1,Y + 1], WX),
                                              WY);
end;

function TVectorMap.IsEmpty: Boolean;
begin
  Result := false;
  if (Width = 0) or (Height = 0) or (FVectors = nil)then Result := True;
end;

const
  MeshIdent = 'yfqLhseM';

type
  {TVectorMap supports the photoshop liquify mesh fileformat .msh}
  TPSLiquifyMeshHeader = record
    Pad0  : dword;
    Ident : array [0..7] of Char;
    Pad1  : dword;
    Width : dword;
    Height: dword;
  end;

procedure TVectorMap.LoadFromFile(const FileName: string);

  procedure ConvertVertices;
  var
    i: Integer;
  begin
    for i:= 0 to Length( FVectors ) - 1 do
      FVectors[i]:= FixedPoint( TFloatVector( FVectors[i] ) ); //Not a mistake!
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
      BlockRead(MeshFile, FVectors[0], Width * Height * SizeOf(TFixedVector));
      ConvertVertices;
    end;
  finally
    CloseFile(MeshFile);
  end else Exception.Create('File not found!');
end;

procedure TVectorMap.Merge(DstLeft, DstTop: Integer;
  Src: TVectorMap; SrcRect: TRect);
var
  I,J,P: Integer;
  DstRect: TRect;
  Progression: TFixedVector;
  ProgressionX, ProgressionY: TFixed;
  Combiner: TVectorCombineFixed;
  DstPtr : PFixedPointArray;
  SrcPtr : PFixedPoint;
begin
  if Src.IsEmpty then Exception.Create('Src is empty!');
  if IsEmpty then Exception.Create('Base is empty!');
  IntersectRect( SrcRect, Src.BoundsRect, SrcRect);

  DstRect.Left := DstLeft;
  DstRect.Top := DstTop;
  DstRect.Right := DstLeft + (SrcRect.Right - SrcRect.Left);
  DstRect.Bottom := DstTop + (SrcRect.Bottom - SrcRect.Top);

  IntersectRect(DstRect, BoundsRect, DstRect);
  if IsRectEmpty(DstRect) then Exit;

  ProgressionX := Fixed(2 / (DstRect.Right - DstRect.Left - 1));
  ProgressionY := Fixed(2 / (DstRect.Bottom - DstRect.Top - 1));

  with Src.VectorCombiner do
  begin
    Combiner := CombineFixed;
    if not CombineValid then PrepareCombine(FloatRect(DstRect));
  end;

  P := SrcRect.Top * Src.Width;
  Progression.Y := - FixedOne;
  for I := DstRect.Top to DstRect.Bottom do
  begin
    Progression.X := - FixedOne;
    DstPtr := @GetBits[I * Width];
    SrcPtr := @Src.GetBits[SrcRect.Left + P];
    for J := DstRect.Left to DstRect.Right do
    begin
      Combiner(SrcPtr^, Progression, DstPtr[J]);
      Inc(SrcPtr);
      Inc(Progression.X, ProgressionX);
    end;
    Inc(P, Src.Width);
    Inc(Progression.Y, ProgressionY);
  end;
  VectorCombiner.FinalizeCombine;
end;

procedure TVectorMap.SaveToFile(const FileName: string);

  procedure ConvertVerticesX;
  var i: Integer;
  begin
    for i := 0 to Length(FVectors) - 1 do
      FVectors[i] := FixedPoint(TFloatVector(FVectors[i])); //Not a mistake!
  end;

  procedure ConvertVerticesF;
  var i: Integer;
  begin
    for i := 0 to Length(FVectors) - 1 do
      TFloatVector(FVectors[i]) := FloatPoint(FVectors[i]); //Not a mistake!
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
      BlockWrite(MeshFile, FVectors[0], Length(FVectors) * SizeOf(TFixedVector));
      ConvertVerticesX;
    end;
    if Odd(Length(FVectors) * SizeOf(TFixedVector)-1) then begin
      Pad := $00000000;
      BlockWrite(MeshFile, Pad, 4);
      BlockWrite(MeshFile, Pad, 4);
    end;
  finally
    CloseFile(MeshFile);
  end;
end;

procedure TVectorMap.SetFloatVector(X, Y: Integer; const Point: TFloatVector);
begin
  FVectors[X + Y * Width] := FixedPoint(Point);
end;

procedure TVectorMap.SetFloatVectorF(X, Y: Single; const Point: TFloatVector);
begin
  SetFixedVectorX(Fixed(X), Fixed(Y), FixedPoint(Point));
end;

procedure TVectorMap.SetFloatVectorFS(X, Y: Single; const Point: TFloatVector);
begin
  SetFixedVectorXS(Fixed(X), Fixed(Y), FixedPoint(Point));
end;

procedure TVectorMap.SetFloatVectorS(X, Y: Integer; const Point: TFloatVector);
begin
  if X < 0 then X := 0 else
    if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0 else
    if Y >= Height then Y := Height - 1;
  SetFloatVector(X, Y, Point);
end;

procedure TVectorMap.SetVectorCombiner(
  VectorCombiner: TCustomVectorCombiner);
begin
  if Assigned(VectorCombiner) then
  begin
    if Assigned(FVectorCombiner) then FVectorCombiner.Free;
    FVectorCombiner := VectorCombiner;
    Changed;
  end;
end;

procedure TVectorMap.SetVectorCombinerClassName(Value: string);
var
  VectorCombinerClass: TCustomVectorCombinerClass;
begin
  if (Value <> '') and (FVectorCombiner.ClassName <> Value) then
  begin
    VectorCombinerClass := TCustomVectorCombinerClass(VectorCombinerList.Find(Value));
    if Assigned(VectorCombinerClass) then
    begin
      FVectorCombiner.Free;
      FVectorCombiner := VectorCombinerClass.Create;
      Changed;
    end;
  end;
end;

procedure TVectorMap.SetFixedVector(X, Y: Integer; const Point: TFixedVector);
begin
  FVectors[X + Y * Width] := Point;
end;

procedure TVectorMap.SetFixedVectorS(X, Y: Integer; const Point: TFixedVector);
begin
  if X < 0 then X := 0 else
    if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0 else
    if Y >= Height then Y := Height - 1;
  SetFixedVector(X, Y, Point);
end;

procedure TVectorMap.SetFixedVectorX(X, Y: TFixed; const Point: TFixedVector);
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

  P := @FVectors[X + Y * Width];

  CombinePointsMem(Point, P^, FixedMul(celx, cely) ); Inc(P);
  CombinePointsMem(Point, P^, FixedMul(flrx, cely) ); Inc(P, Width);
  CombinePointsMem(Point, P^, FixedMul(flrx, flry) ); Dec(P);
  CombinePointsMem(Point, P^, FixedMul(celx, flry) );
end;

procedure TVectorMap.SetFixedVectorXS(X, Y: TFixed; const Point: TFixedVector);
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
  P := @FVectors[X + Y * Width];

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
  Offset := FloatPoint(0,0);
  TransformationMap := TVectorMap.Create;
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
  OffsetRect(Result, Round(FOffset.X), Round(FOffset.Y));
{  R := MakeRect(FMappingRect);
  if R.Left < Result.Left then Result.Left := R.Left;
  if R.Top < Result.Top then Result.Top := R.Top;
  if R.Right > Result.Right then Result.Right := R.Right;
  if R.Bottom > Result.Bottom then Result.Bottom := R.Bottom;   }
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

 function FitPoint(P: TFixedVector): TFixedVector;
 begin
    Dec(P.X, OffsetFixed.X);
    P.X := P.X - DstTranslationFixed.X;
    P.X := FixedMul(P.X , DstScaleFixed.X);
    P.X := P.X + FixedMul(P.X, ScalingFixed.X);
    P.X := FixedMul(P.X, SrcScaleFixed.X);
    Result.X := P.X + SrcTranslationFixed.X;

    Dec(P.Y, OffsetFixed.Y);
    P.Y := P.Y - DstTranslationFixed.Y;
    P.Y := FixedMul(P.Y , DstScaleFixed.Y);
    P.Y := P.Y + FixedMul(P.Y, ScalingFixed.Y);
    P.Y := FixedMul(P.Y, SrcScaleFixed.Y);
    Result.Y := P.Y + SrcTranslationFixed.Y;
 end;

var
  I, J: Integer;
  P, Q, Progression: TFixedVector;
  ProgressionX, ProgressionY: TFixed;
  Combiner: TVectorCombineFixed;
  MapPtr: PFixedPointArray;
begin
  IntersectRect(DstRect, TransformationMap.BoundsRect, DstRect);
  if IsRectEmpty(DstRect) then Exit;

  if not TTransformationAccess(Transformation).TransformValid then
    TTransformationAccess(Transformation).PrepareTransform;

  ProgressionX := Fixed(1 / (DstRect.Right - DstRect.Left - 1));
  ProgressionY := Fixed(1 / (DstRect.Bottom - DstRect.Top - 1));

  with TransformationMap.VectorCombiner do
  begin
    Combiner := CombineFixed;
    if not CombineValid then PrepareCombine(FloatRect(DstRect));
  end;

  Progression.Y := 0;
  with DstRect do for I := Top to Bottom do
  begin
    Progression.X := 0;
    MapPtr := @TransformationMap.GetBits[I * TransformationMap.Width];
    for J := Left to Right do
    begin
      //Subtract loop vars to ensure correct transformation output
      P := FixedPoint(J - Left, I - Top);
//      P := FitPoint(P);

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

{procedure TRemapTransformation.ReverseTransform256(DstX, DstY: Integer;
  out SrcX256, SrcY256: Integer);
begin
  with TransformationMap.FixedVector[DstX - OffsetInt.X, DstY - OffsetInt.Y] do
  begin
    DstX := DstX * FixedOne - DstTranslationFixed.X;
    DstY := DstY * FixedOne - DstTranslationFixed.Y;
    DstX := FixedMul(DstX , DstScaleFixed.X);
    DstY := FixedMul(DstY , DstScaleFixed.Y);

    DstX := DstX + FixedMul(X, ScalingFixed.X);
    DstY := DstY + FixedMul(Y, ScalingFixed.Y);

    DstX := FixedMul(DstX, SrcScaleFixed.X);
    DstY := FixedMul(DstY, SrcScaleFixed.Y);
    DstX := DstX + SrcTranslationFixed.X;
    DstY := DstY + SrcTranslationFixed.Y;

    SrcX256 := SAR_8(DstX + $7F);
    SrcY256 := SAR_8(DstY + $7F);
  end;
end;  }

procedure TRemapTransformation.ReverseTransformFixed(DstX, DstY: TFixed;
  out SrcX, SrcY: TFixed);
begin
  with TransformationMap.FixedVectorX[DstX - OffsetFixed.X, DstY - OffsetFixed.Y] do
  begin
    DstX := DstX - DstTranslationFixed.X;
    DstX := FixedMul(DstX , DstScaleFixed.X);
    DstX := DstX + FixedMul(X, ScalingFixed.X);
    DstX := FixedMul(DstX, SrcScaleFixed.X);
    SrcX := DstX + SrcTranslationFixed.X;

    DstY := DstY - DstTranslationFixed.Y;
    DstY := FixedMul(DstY , DstScaleFixed.Y);
    DstY := DstY + FixedMul(Y, ScalingFixed.Y);
    DstY := FixedMul(DstY, SrcScaleFixed.Y);
    SrcY := DstY + SrcTranslationFixed.Y;
  end;
end;

procedure TRemapTransformation.ReverseTransformFloat(DstX, DstY: Single;
  out SrcX, SrcY: Single);
begin
  with TransformationMap.FloatVectorF[DstX - FOffset.X, DstY - FOffset.Y] do
  begin
    DstX := DstX - DstTranslationFloat.X;
    DstY := DstY - DstTranslationFloat.Y;
    DstX := DstX * DstScaleFloat.X;
    DstY := DstY * DstScaleFloat.Y;

    DstX := DstX + X * ScalingFloat.X;
    DstY := DstY + Y * ScalingFloat.Y;

    DstX := DstX * SrcScaleFloat.X;
    DstY := DstY * SrcScaleFloat.Y;
    SrcX := DstX + SrcTranslationFloat.X;
    SrcY := DstY + SrcTranslationFloat.Y;
  end;
end;

procedure TRemapTransformation.ReverseTransformInt(DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
begin
  with TransformationMap.FixedVector[DstX - OffsetInt.X, DstY - OffsetInt.Y] do
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

procedure TRemapTransformation.SetOffset(const Value: TFloatVector);
begin
  FOffset := Value;
  OffsetInt := Point(Value);
  OffsetFixed := FixedPoint(Value);
  TransformValid := False;
end;

{ TCustomCombine }

constructor TCustomCombiner.Create;
begin
  FMasterWeight := 1.0;
  FCombineValid := False;
  Contour := TDefaultContour.Create;
end;

destructor TCustomCombiner.Destroy;
begin
  Contour.Free;
  inherited;
end;

procedure TCustomCombiner.FinalizeCombine;
begin
  Contour.FinalizeContour;
  FCombineValid := False;
end;

function TCustomCombiner.GetCombineValid: Boolean;
begin
  Result := FCombineValid and Contour.ContourValid;
end;

function TCustomCombiner.GetContourClassName: string;
begin
   Result := FContour.ClassName;
end;

procedure TCustomCombiner.SetContour(
  Contour: TCustomContour);
begin
  if Assigned(Contour) then
  begin
    if Assigned(FContour) then FContour.Free;
    FContour := Contour;
    FCombineValid := False;
  end;
end;

procedure TCustomCombiner.SetContourClassName(Value: string);
var
  ContourClass: TCustomContourClass;
begin
  if (Value <> '') and (FContour.ClassName <> Value) then
  begin
    ContourClass := TCustomContourClass(ContourList.Find(Value));
    if Assigned(ContourClass) then
    begin
      FContour.Free;
      FContour := ContourClass.Create;
      FCombineValid := False;
    end;
  end;
end;

procedure TCustomCombiner.PrepareCombine(CombineRect: TFloatRect);
begin
  if IsRectEmptyF(CombineRect) then raise Exception.Create('CombineRect is empty!');
  if not Contour.ContourValid then Contour.PrepareContour;
  FCombineRect := CombineRect;
  FCombineValid := True;
end;

procedure TCustomCombiner.SetMasterWeight(Value: Single);
begin
  FMasterWeight := Value;
  FCombineValid := False;
end;

{ TCustomVectorCombine }

procedure TCustomVectorCombiner.CombineFixed(const F, P: TFixedVector;
  var B: TFixedVector);
var
  _B: TFloatVector;
begin
  EMMS;
  _B := FloatPoint(B);
  CombineFloat(FloatPoint(F), FloatPoint(P), _B);
  B := FixedPoint(_B);
end;

procedure TCustomVectorCombiner.CombineInt(const F: TPoint; const P: TFixedVector;
  var B: TPoint);
var
  _B, _P: TFloatVector;
begin
  EMMS;
  _B := FloatPoint(B);
  _P.X := P.X * FixedToFloat;
  _P.Y := P.Y * FixedToFloat;
  CombineFloat(FloatPoint(F), _P, _B);
  B := Point(_B);
end;

{ TAdditionVectorCombiner }

procedure TAdditionVectorCombiner.CombineFloat(const F, P: TFloatVector;
  var B: TFloatVector);
var
  O: TFloatVector;
  W: Single;
begin
  O.X := B.X + F.X;
  O.Y := B.Y + F.Y;

  with Contour do
    W := EnsureRange(ContourValueFloat(1 - Abs(P.X)), 0, 1) *
         EnsureRange(ContourValueFloat(1 - Abs(P.Y)), 0, 1) *
         FMasterWeight;

  B.X := B.X + (O.X - B.X) * W;
  B.Y := B.Y + (O.Y - B.Y) * W;
end;

{ TSubtractionVectorCombiner }

procedure TSubtractionVectorCombiner.CombineFloat(const F, P: TFloatVector;
  var B: TFloatVector);
var
  O: TFloatVector;
  W: Single;
begin
  O.X := B.X - F.X;
  O.Y := B.Y - F.Y;

  with Contour do
    W := ContourValueFloat(1 - Abs(P.X)) *
         ContourValueFloat(1 - Abs(P.Y)) *
         FMasterWeight;

  B.X := B.X + (O.X - B.X) * W;
  B.Y := B.Y + (O.Y - B.Y) * W;
end;

{ TMultiplicationVectorCombiner }

procedure TMultiplicationVectorCombiner.CombineFloat(const F,
  P: TFloatVector; var B: TFloatVector);
var
  O: TFloatVector;
  W: Single;
begin
  O.X := B.X * F.X;
  O.Y := B.Y * F.Y;

  with Contour do
    W := ContourValueFloat(1 - Abs(P.X)) *
         ContourValueFloat(1 - Abs(P.Y)) *
         FMasterWeight;

  B.X := B.X + (O.X - B.X) * W;
  B.Y := B.Y + (O.Y - B.Y) * W;
end;

{ TDifferenceVectorCombiner }

procedure TDifferenceVectorCombiner.CombineFloat(const F, P: TFloatVector;
  var B: TFloatVector);
var
  O: TFloatVector;
  W: Single;
begin
  O.X := Abs(B.X - F.X);
  O.Y := Abs(B.Y - F.Y);

  with Contour do
    W := ContourValueFloat(1 - Abs(P.X)) *
         ContourValueFloat(1 - Abs(P.Y)) *
         FMasterWeight;

  B.X := B.X + (O.X - B.X) * W;
  B.Y := B.Y + (O.Y - B.Y) * W;
end;

{ TCustomContour }

constructor TCustomContour.Create;
begin
  ContourWeight := 1.0;
  FContourValid := False;
end;

procedure TCustomContour.FinalizeContour;
begin
  FContourValid := False;
end;

procedure TCustomContour.PrepareContour;
begin
  FContourValid := True;
end;

procedure TCustomContour.SetContourWeight(const Value: Single);
begin
  FContourWeight := Value;
  FixedContourWeight := Fixed(Value);
  FContourValid := False;
end;

function TCustomContour.ContourValueFixed(const Value: TFixed): TFixed;
var
  S: Single;
begin
  EMMS;
  S := Value * FixedToFloat;
  Result := Fixed(ContourValueFloat(S));
end;

{ TConstantContour }

constructor TConstantContour.Create;
begin
  inherited;
  ConstantValue := 1.0;
end;

procedure TConstantContour.SetConstantValue(const Value: Single);
begin
  FConstantValue := Value;
  FixedConstantValue := Fixed(Value);
  FContourValid := False;
end;

function TConstantContour.ContourValueFixed(const Value: TFixed): TFixed;
begin
  Result := FixedMul(FixedConstantValue, FixedContourWeight);
end;

function TConstantContour.ContourValueFloat(const Value: Single): Single;
begin
  Result := FConstantValue * FContourWeight;
end;

{ TLinearContour }

function TLinearContour.ContourValueFixed(const Value: TFixed): TFixed;
begin
  Result := FixedMul(Value, FixedContourWeight);
end;

function TLinearContour.ContourValueFloat(const Value: Single): Single;
begin
  Result := Value * FContourWeight;
end;

{ TGaussianContour }

function TGaussianContour.ContourValueFloat(const Value: Single): Single;
begin
  Result := Sqr(Sin(0.5 * Value * PI));
end;

{ TLinearSineContour }

constructor TLinearSineContour.Create;
begin
  inherited;
  FWavesCount := 10;
end;

procedure TLinearSineContour.SetWavesCount(const Value: Integer);
begin
  FWavesCount := Value;
  FContourValid := False;
end;

function TLinearSineContour.ContourValueFloat(const Value: Single): Single;
begin
  Result := 0.5 + Cos(Value * PI * FWavesCount) * 0.5;
  Result := 1 - Result; 
  Result := Result * Value; // Linearity
end;

{ TStairwayContour }

constructor TStairwayContour.Create;
begin
  inherited;
  StepCount := 10;
end;

procedure TStairwayContour.SetStepCount(const Value: Integer);
begin
  FStepCount := Value;
  Scaler := 1 / Value;
  FContourValid := False;
end;

function TStairwayContour.ContourValueFloat(const Value: Single): Single;
begin
  Result := Round(FStepCount * Value) * Scaler;
end;

{ TReplacementVectorCombiner }

procedure TReplacementVectorCombiner.CombineFloat(const F, P: TFloatVector;
  var B: TFloatVector);
var
  W: Single;
begin
  with Contour do
    W := EnsureRange(ContourValueFloat(1 - Abs(P.X)), 0, 1) *
         EnsureRange(ContourValueFloat(1 - Abs(P.Y)), 0, 1) *
         FMasterWeight;

  B.X := F.X * W;
  B.Y := F.Y * W;
end;

initialization
  { Register VectorCombiners }
  VectorCombinerList := TClassList.Create;
  VectorCombinerList.Add(TReplacementVectorCombiner);
  VectorCombinerList.Add(TAdditionVectorCombiner);
  VectorCombinerList.Add(TSubtractionVectorCombiner);
  VectorCombinerList.Add(TMultiplicationVectorCombiner);
  VectorCombinerList.Add(TDifferenceVectorCombiner);

  { Register Contours }
  ContourList := TClassList.Create;
  ContourList.Add(TConstantContour);
  ContourList.Add(TLinearContour);
  ContourList.Add(TGaussianContour);
  ContourList.Add(TLinearSineContour);
  ContourList.Add(TStairwayContour);

finalization
  VectorCombinerList.Free;
  ContourList.Free;
end.
