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
   Windows, Types, SysUtils, GR32, GR32_Transforms;

type
  {TFixedPointMergeProc controls what happens when a TTransformationmaps is
   merged into another. 'F' is a fixedpoint the foreground map (the one passed
   to TTransformationmap.Merge), 'B' is a fixedpoint in the base or background
   map and is also where the result is put, which later on is scaled by the
   weight parameter in TTransformationmap.Merge}
  TFixedPointMergeProc = procedure(F: TFixedPoint; var B: TFixedPoint);

  TTransformationMap = class(TCustomMap)
  private
    FBits: TArrayOfFixedPoint;
    function GetBits: PFixedPointArray;

    function GetXPoint(X,Y: Integer): TFixedPoint;
    function GetXPointS(X,Y: Integer): TFixedPoint;
    function GetXPointX(X,Y: TFixed): TFixedPoint;
    function GetXPointXS(X,Y: TFixed): TFixedPoint;

    function GetFPoint(X,Y: Integer): TFloatPoint;
    function GetFPointS(X,Y: Integer): TFloatPoint;
    function GetFPointF(X,Y: Single): TFloatPoint;
    function GetFPointFS(X,Y: Single): TFloatPoint;

    procedure SetXPoint(X,Y: Integer; Point: TFixedPoint);
    procedure SetXPointS(X,Y: Integer; Point: TFixedPoint);
    procedure SetXPointX(X,Y: TFixed; Point: TFixedPoint);
    procedure SetXPointXS(X,Y: TFixed; Point: TFixedPoint);

    procedure SetFPoint(X,Y: Integer; Point: TFloatPoint);
    procedure SetFPointS(X,Y: Integer; Point: TFloatPoint);
    procedure SetFPointF(X,Y: Single; Point: TFloatPoint);
    procedure SetFPointFS(X,Y: Single; Point: TFloatPoint);
  protected
    procedure ChangeSize(var Width, Height: Integer; NewWidth,
      NewHeight: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    procedure Merge(DstLeft, DstTop: Integer; Src: TTransformationMap;
      SrcRect: TRect; Weight: Single; MergeProc: TFixedPointMergeProc);

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

    FDstRect: TFloatRect;
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
    function  GetTransformedBounds: TRect; override;
    procedure Scale(Sx, Sy: Single);
    property DstRect: TFloatRect read FDstRect{GetDstRect} write FDstRect{SetDstRect};
  end;

implementation

uses Math, GR32_Lowlevel;

{ TTransformationMap }

function CombinePointsReg(A, B: TFixedPoint; Weight256: Integer): TFixedPoint;
begin
  Result.X := A.X +  SAR_8((B.X - A.X) * Weight256);
  Result.Y := A.Y +  SAR_8((B.Y - A.Y) * Weight256);
end;

procedure CombinePointsMem(A: TFixedPoint;var  B: TFixedPoint; Weight256: Integer);
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
//
end;

destructor TTransformationMap.Destroy;
begin
  Lock;
  try
    SetSize(0, 0);
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
var
  WX,WY: Integer;
  P: PFixedPoint;
  P11, P12, P22: TFixedPoint;
begin
  WX := SAR_8(X) and $FF;
  WY := SAR_8(Y) and $FF;

  X := SAR_16(X);
  Y := SAR_16(Y);

  P :=  @FBits[X + Y * Width];
  P11 := P^; Inc(P);
  P12 := P^; Inc(P, Width);
  P22 := P^; Dec(P);

  Result := CombinePointsReg(CombinePointsReg(P11, P12, WX),
                             CombinePointsReg(P^, P22, WX), WY);
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
      FBits[i]:= FixedPoint( TFloatPoint( FBits[i] ) );
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
  Src: TTransformationMap; SrcRect: TRect; Weight: Single;
  MergeProc: TFixedPointMergeProc);
var
  I,J,P,Q: Integer;
  DstRect: TRect;
  SrcP,DstP: TFixedPoint;
begin
  if Src.IsEmpty then Exception.Create('Src is empty!');
  if IsEmpty then Exception.Create('Base is empty!');
  if not Assigned(MergeProc) then Exception.Create('MergeProc is nil!');
  if Weight <= 0 then Exit;
  EnsureRange(Weight, 0, 1);
  IntersectRect( SrcRect, Src.BoundsRect, SrcRect);

  DstRect := BoundsRect;
  DstRect.Left := DstLeft;
  DstRect.Top := DstTop;
  DstRect.Right := SrcRect.Right - SrcRect.Left;
  DstRect.Bottom := SrcRect.Bottom - SrcRect.Top;

  IntersectRect(DstRect, BoundsRect, DstRect);
  if IsRectEmpty(DstRect) then Exit;

  P := SrcRect.Left;
  Q := SrcRect.Top;
  for I := DstRect.Top to DstRect.Bottom - 1 do
  begin
    for J := DstRect.Top to DstRect.Bottom - 1 do
    begin
      SrcP := Src.FixedPointMap[P, Q];
      DstP := FixedPointMap[I, J];
      MergeProc(SrcP, DstP);
      DstP.X := Round(DstP.X + (SrcP.X - DstP.X) * Weight);
      DstP.Y := Round(DstP.Y + (SrcP.Y - DstP.Y) * Weight);
      FixedPointMap[I, J] := DstP;
      Inc(P);
    end;
    Inc(Q);
  end;
end;

procedure TTransformationMap.SaveToFile(const FileName: string);

  procedure ConvertVerticesX;
  var i: Integer;
  begin
    for i := 0 to Length(FBits) - 1 do
      FBits[i] := FixedPoint(TFloatPoint(FBits[i]));
  end;

  procedure ConvertVerticesF;
  var i: Integer;
  begin
    for i := 0 to Length(FBits) - 1 do
      TFloatPoint(FBits[i]) := FloatPoint(FBits[i]);
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

procedure TTransformationMap.SetFPoint(X, Y: Integer; Point: TFloatPoint);
begin
  FBits[X + Y * Width] := FixedPoint(Point);
end;

procedure TTransformationMap.SetFPointF(X, Y: Single; Point: TFloatPoint);
begin
  SetXPointX(Fixed(X), Fixed(Y), FixedPoint(Point));
end;

procedure TTransformationMap.SetFPointFS(X, Y: Single; Point: TFloatPoint);
begin
  SetXPointXS(Fixed(X), Fixed(Y), FixedPoint(Point));
end;

procedure TTransformationMap.SetFPointS(X, Y: Integer; Point: TFloatPoint);
begin
  if X < 0 then X := 0 else
    if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0 else
    if Y >= Height then Y := Height - 1;
  SetFPoint(X, Y, Point);
end;

procedure TTransformationMap.SetXPoint(X, Y: Integer; Point: TFixedPoint);
begin
  FBits[X + Y * Width] := Point;
end;

procedure TTransformationMap.SetXPointS(X, Y: Integer; Point: TFixedPoint);
begin
  if X < 0 then X := 0 else
    if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0 else
    if Y >= Height then Y := Height - 1;
  SetXPoint(X, Y, Point);
end;

procedure TTransformationMap.SetXPointX(X, Y: TFixed; Point: TFixedPoint);
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

procedure TTransformationMap.SetXPointXS(X, Y: TFixed; Point: TFixedPoint);
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
end;

procedure TRemapTransformation.PrepareTransform;
begin
//  if IsRectEmpty(SrcRect
  with SrcRect do
  begin
    SrcTranslationFloat.X := Left;
    SrcTranslationFloat.Y := Top;
//    SrcScaleFloat.X := 1 / ((TransformationMap.Width - 1) / (Right - Left) );
//    SrcScaleFloat.Y := 1 / ((TransformationMap.Height - 1) / (Bottom - Top) );
    SrcScaleFloat.X := 1 / ((TransformationMap.Width - 1) / (Right - Left) );
    SrcScaleFloat.Y := 1 / ((TransformationMap.Height - 1) / (Bottom - Top) );
    SrcTranslationFixed := FixedPoint(SrcTranslationFloat);
    SrcScaleFixed := FixedPoint(SrcScaleFloat);
  end;

  with FDstRect do
  begin
    DstTranslationFloat.X := Left;
    DstTranslationFloat.Y := Top;
//    DstScaleFloat.X := (TransformationMap.Width - 1) / (Right - Left);
//    DstScaleFloat.Y := (TransformationMap.Height - 1) / (Bottom - Top);
    DstScaleFloat.X :=  ((TransformationMap.Width - 1) / (Right - Left));
    DstScaleFloat.Y :=  ((TransformationMap.Height - 1) / (Bottom - Top));
    DstTranslationFixed := FixedPoint(DstTranslationFloat);
    DstScaleFixed := FixedPoint(DstScaleFloat);
  end;
end;

procedure TRemapTransformation.ReverseTransform256(DstX, DstY: Integer;
  out SrcX256, SrcY256: Integer);
begin
  with TransformationMap.FixedPointMapS[DstX, DstY] do
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
  end;       {
  with TransformationMap.FixedPointMapS[DstX, DstY] do
  begin
    //Scale the vectors
    X := FixedMul(X, ScalingFixed.X);
    Y := FixedMul(Y, ScalingFixed.Y);

    //Dst Translation and Scaling (controlled by DstRect)
    Inc(X, DstX * FixedOne - DstTranslationFixed.X);
    Inc(Y, DstY * FixedOne - DstTranslationFixed.Y);
    X := FixedMul(X, DstScaleFixed.X);
    Y := FixedMul(Y, DstScaleFixed.Y);

    //Src Translation, Scaling (controlled by SrcRect) and rounding
    X := DstX + FixedMul(X, SrcScaleFixed.X);
    Y := DstY + FixedMul(Y, SrcScaleFixed.Y);
    SrcX256 := SAR_8(X + $7F + SrcTranslationFixed.X);
    SrcY256 := SAR_8(Y + $7F + SrcTranslationFixed.Y);
  end; }
end;

procedure TRemapTransformation.ReverseTransformFixed(DstX, DstY: TFixed;
  out SrcX, SrcY: TFixed);
begin
  with TransformationMap.FixedPointMapXS[DstX, DstY] do
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
  with TransformationMap.FloatPointMapFS[DstX, DstY] do
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
  with TransformationMap.FixedPointMapS[DstX, DstY] do
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

end.
