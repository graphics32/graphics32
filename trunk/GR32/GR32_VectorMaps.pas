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
  protected
    FTransformationMap: TTransformationMap;
    FScaling: TFixedPoint;
    FTranslation: TFixedPoint;
    procedure ReverseTransform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); override;
    procedure ReverseTransformInt(DstX, DstY: Integer; out SrcX, SrcY: Integer); override;
    procedure ReverseTransformFloat(DstX, DstY: Single; out SrcX, SrcY: Single); override;
    procedure ReverseTransformFixed(DstX, DstY: TFixed; out SrcX, SrcY: TFixed); override;
  protected
    property Scaling: TFixedPoint read FScaling write FScaling;
    property Translation: TFixedPoint read FTranslation write FTranslation;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function  GetTransformedBounds: TRect; override;
    procedure Scale(Sx, Sy: Single);
    procedure Translate(Dx, Dy: Single);
    property TransformationMap read FTransformationMap write FTransformationMap;
  end;

function TransformPoints(Points: TArrayOfArrayOfFixedPoint; Transformation: TTransformation): TArrayOfArrayOfFixedPoint;

implementation

{ TTransformationMap }

function CombinePointsReg(A, B: TFixedPoint; Weight256: Integer): TFixedPoint;
begin
  Result.X := A.X +  SAR_8( (B.X - A.X) * Weight256 );
  Result.Y := A.Y +  SAR_8( (B.Y - A.Y) * Weight256 );
end;

procedure CombinePointsMem(A: TFixedPoint;var  B: TFixedPoint; Weight256: Integer);
begin
  B.X := A.X +  SAR_8( (B.X - A.X) * Weight256 );
  B.Y := A.Y +  SAR_8( (B.Y - A.Y) * Weight256 );
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
  Result := FloatPoint( FBits[X + Y * Width] );
end;

function TTransformationMap.GetFPointF(X, Y: Single): TFloatPoint;
begin
  Result := FloatPoint( GetXPointX(Fixed(X), Fixed(Y)) );
end;

function TTransformationMap.GetFPointFS(X, Y: Single): TFloatPoint;
begin
  Result := FloatPoint( GetXPointXS(Fixed(X), Fixed(Y)) );
end;

function TTransformationMap.GetFPointS(X, Y: Integer): TFloatPoint;
begin
  if X < 0 then X:= 0 else
   if X >= Width then X:= Width - 1;
  if Y < 0 then Y:= 0 else
   if Y >= Height then Y:= Height - 1;
  Result := GetFPoint(X,Y);
end;

function TTransformationMap.GetXPoint(X, Y: Integer): TFixedPoint;
begin
  Result:= FBits[X + Y * Width];
end;

function TTransformationMap.GetXPointS(X, Y: Integer): TFixedPoint;
begin
  if X < 0 then X:= 0 else
   if X >= Width then X:= Width - 1;
  if Y < 0 then Y:= 0 else
   if Y >= Height then Y:= Height - 1;
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

  Result:= CombinePointsReg(CombinePointsReg(P11, P12, WX),
                            CombinePointsReg(P^, P22, WX), WY);
end;

function TTransformationMap.GetXPointXS(X, Y: TFixed): TFixedPoint;
var
  WX,WY: Integer;
begin
  WX:= SAR_8(X) and $FF;
  WY:= SAR_8(Y) and $FF;

  X:= SAR_16(X);
  Y:= SAR_16(Y);

  Result := CombinePointsReg(CombinePointsReg(FixedPointMapS[X,Y],
                                              FixedPointMapS[X + 1,Y], WX),
                             CombinePointsReg(FixedPointMapS[X,Y + 1],
                                              FixedPointMapS[X + 1,Y + 1], WX),
                                              WY);
end;

function TTransformationMap.IsEmpty: Boolean;
begin
  Result:= false;
  if (Width = 0) or (Height = 0) or (FBits = nil)then Result:= True;
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

  P:= SrcRect.Left;
  Q:= SrcRect.Top;
  for I:= DstRect.Top to DstRect.Bottom - 1 do begin
    for J:= DstRect.Top to DstRect.Bottom - 1 do begin
      SrcP:= Src.FixedPointMap[P, Q];
      DstP:= FixedPointMap[I, J];
      MergeProc(SrcP, DstP);
      DstP.X:= Round(DstP.X + (SrcP.X - DstP.X) * Weight);
      DstP.Y:= Round(DstP.Y + (SrcP.Y - DstP.Y) * Weight);
      FixedPointMap[I, J]:= DstP;
      Inc(P);
    end;
    Inc(Q);
  end;
end;

procedure TTransformationMap.SaveToFile(const FileName: string);

  procedure ConvertVerticesX;
  var i: Integer;
  begin
    for i:= 0 to Length( FBits ) - 1 do
      FBits[i]:= FixedPoint( TFloatPoint( FBits[i] ) );
  end;

  procedure ConvertVerticesF;
  var i: Integer;
  begin
    for i:= 0 to Length( FBits ) - 1 do
      TFloatPoint(FBits[i]):= FloatPoint( FBits[i] );
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
      Pad0:= $02000000;
      Ident:= MeshIdent;
      Pad1:= $00000002;
      Width:= Self.Width;
      Height:= Self.Height;
    end;
    BlockWrite(MeshFile, Header, SizeOf(TPSLiquifyMeshHeader));
    with Header do begin
      ConvertVerticesF;
      BlockWrite(MeshFile, FBits[0], Length( FBits ) * SizeOf(TFixedPoint) );
      ConvertVerticesX;
    end;
    if Odd( Length( FBits ) * SizeOf(TFixedPoint)-1 ) then begin
      Pad:= $00000000;
      BlockWrite(MeshFile, Pad, 4);
      BlockWrite(MeshFile, Pad, 4);
    end;
  finally
    CloseFile(MeshFile);
  end;
end;

procedure TTransformationMap.SetFPoint(X, Y: Integer; Point: TFloatPoint);
begin
  FBits[X + Y * Width]:= FixedPoint( Point );
end;

procedure TTransformationMap.SetFPointF(X, Y: Single; Point: TFloatPoint);
begin
  SetXPointX( Fixed(X), Fixed(Y), FixedPoint(Point) );
end;

procedure TTransformationMap.SetFPointFS(X, Y: Single; Point: TFloatPoint);
begin
  SetXPointXS( Fixed(X), Fixed(Y), FixedPoint(Point) );
end;

procedure TTransformationMap.SetFPointS(X, Y: Integer; Point: TFloatPoint);
begin
  if X < 0 then X:= 0 else
   if X >= Width then X:= Width - 1;
  if Y < 0 then Y:= 0 else
   if Y >= Height then Y:= Height - 1;
  SetFPoint(X, Y, Point);
end;

procedure TTransformationMap.SetXPoint(X, Y: Integer; Point: TFixedPoint);
begin
  FBits[X + Y * Width]:= Point;
end;

procedure TTransformationMap.SetXPointS(X, Y: Integer; Point: TFixedPoint);
begin
  if X < 0 then X:= 0 else
   if X >= Width then X:= Width - 1;
  if Y < 0 then Y:= 0 else
   if Y >= Height then Y:= Height - 1;
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
    if (X >= 0) and (Y >= 0) then CombinePointsMem(Point, P^, FixedMul(celx, cely) ); Inc(P);
    if (X < Width - 1) and (Y >= 0) then CombinePointsMem(Point, P^, FixedMul(flrx, cely) ); Inc(P, Width);
    if (X < Width - 1) and (Y < Height - 1) then CombinePointsMem(Point, P^, FixedMul(flrx, flry) ); Dec(P);
    if (X >= 0) and (Y < Height - 1) then CombinePointsMem(Point, P^, FixedMul(celx, flry) );
  end;
end;

{ TRemapTransformation }

constructor TRemapTransformation.Create;
begin
  inherited;
  TransformationMap:= TTransformationMap.Create;
end;

destructor TRemapTransformation.Destroy;
begin
  TransformationMap.Free;
  inherited;
end;

function TRemapTransformation.GetTransformedBounds: TRect;
begin
  Result := MakeRect(FSrcRect);
end;

procedure TRemapTransformation.ReverseTransform256(DstX, DstY: Integer;
  out SrcX256, SrcY256: Integer);
var
  P: TFixedPoint;
begin
  inherited;
  P := TransformationMap.FixedPointMapXS[DstX * 256, DstY * 256];
  P.X := FixedMul(P.X, FScaling.X);
  P.Y := FixedMul(P.Y, FScaling.Y);
  Inc(P.X, FTranslation.X);
  Inc(P.Y, FTranslation.Y);
  SrcX256 := SAR_8(DstX + P.X);
  SrcY256 := SAR_8(DstY + P.Y);
end;

procedure TRemapTransformation.ReverseTransformFixed(DstX, DstY: TFixed;
  out SrcX, SrcY: TFixed);
var
  P: TFixedPoint;
begin
  inherited;
  P := TransformationMap.FixedPointMapXS[DstX, DstY];
  P.X := FixedMul(P.X, FScaling.X);
  P.Y := FixedMul(P.Y, FScaling.Y);
  Inc(P.X, FTranslation.X);
  Inc(P.Y, FTranslation.Y);
  SrcX := DstX + P.X;
  SrcY := DstY + P.Y;
end;

procedure TRemapTransformation.ReverseTransformFloat(DstX, DstY: Single;
  out SrcX, SrcY: Single);
var
  P: TFixedPoint;
  X,Y: TFixed;
begin
  inherited;
  X := Fixed(DstX);
  Y := Fixed(DstY);
  P := TransformationMap.FixedPointMapXS[X, Y];
  P.X := FixedMul(P.X, FScaling.X);
  P.Y := FixedMul(P.Y, FScaling.Y);
  Inc(P.X, FTranslation.X);
  Inc(P.Y, FTranslation.Y);
  SrcX:= (X + P.X) * FixedToFloat;
  SrcY:= (Y + P.Y) * FixedToFloat;
end;

procedure TRemapTransformation.ReverseTransformInt(DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
var
  P: TFixedPoint;
begin
  inherited;
  P := TransformationMap.FixedPointMapS[DstX, DstY];
  P.X := FixedMul(P.X, FScaling.X);
  P.Y := FixedMul(P.Y, FScaling.Y);
  Inc(P.X, FTranslation.X);
  Inc(P.Y, FTranslation.Y);
  SrcX := DstX + FixedRound(P.X);
  SrcY := DstY + FixedRound(P.Y);
end;

procedure TRemapTransformation.Scale(Sx, Sy: Single);
begin
  FScaling.X := Fixed(Sx);
  FScaling.Y := Fixed(Sy);
end;

procedure TRemapTransformation.Translate(Dx, Dy: Single);
begin
  FTranslation.X := Fixed(Dx);
  FTranslation.Y := Fixed(Dy);
end;

end.
