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
  Windows, Types, Classes, GR32, GR32_Containers;

type
  TFixedVector = TFixedPoint;
  PFixedVector = ^TFixedVector;
  TFloatVector = TFloatPoint;
  PFloatVector = ^TFloatVector;
  TArrayOfFixedVector = array of TFixedVector;
  PArrayOfFixedVector = ^TArrayOfFixedVector;
  TArrayOfFloatVector = array of TFloatVector;
  PArrayOfFloatVector = ^TArrayOfFixedVector;

type
  TVectorCombineMode = (vcmAdd, vcmReplace, vcmCustom);
  TVectorCombineEvent= procedure(F, P: TFixedVector; var B: TFixedVector) of object;

  TVectorMap = class(TCustomMap)
  private
    FVectors: TArrayOfFixedVector;
    FOnVectorCombine: TVectorCombineEvent;
    FVectorCombineMode: TVectorCombineMode;
    function GetVectors: PFixedPointArray;
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
    procedure SetVectorCombineMode(const Value: TVectorCombineMode);
  protected
    procedure ChangeSize(var Width, Height: Integer; NewWidth,
      NewHeight: Integer); override;
  public
    destructor Destroy; override;

    procedure Clear;
    procedure Merge(DstLeft, DstTop: Integer; Src: TVectorMap; SrcRect: TRect);

    property Vectors: PFixedPointArray read GetVectors;
    function BoundsRect: TRect;
    function IsEmpty: Boolean;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property FixedVector[X, Y: Integer]: TFixedVector read GetFixedVector write SetFixedVector; default;
    property FixedVectorS[X, Y: Integer]: TFixedVector read GetFixedVectorS write SetFixedVectorS;
    property FixedVectorX[X, Y: TFixed]: TFixedVector read GetFixedVectorX write SetFixedVectorX;
    property FixedVectorXS[X, Y: TFixed]: TFixedVector read GetFixedVectorXS write SetFixedVectorXS;

    property FloatVector[X, Y: Integer]: TFloatVector read GetFloatVector write SetFloatVector;
    property FloatVectorS[X, Y: Integer]: TFloatVector read GetFloatVectorS write SetFloatVectorS;
    property FloatVectorF[X, Y: Single]: TFloatVector read GetFloatVectorF write SetFloatVectorF;
    property FloatVectorFS[X, Y: Single]: TFloatVector read GetFloatVectorFS write SetFloatVectorFS;
  published
    property VectorCombineMode: TVectorCombineMode read FVectorCombineMode write SetVectorCombineMode;
    property OnVectorCombine: TVectorCombineEvent read FOnVectorCombine write FOnVectorCombine;
  end;

implementation

uses
  Math, SysUtils, GR32_Lowlevel, GR32_Blend, GR32_Transforms;

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

destructor TVectorMap.Destroy;
begin
  Lock;
  try
    SetSize(0, 0);
  finally
    Unlock;
  end;
  inherited;
end;

function TVectorMap.GetVectors: PFixedPointArray;
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

procedure TVectorMap.Merge(DstLeft, DstTop: Integer; Src: TVectorMap; SrcRect: TRect);
var
  I,J,P: Integer;
  DstRect: TRect;
  Progression: TFixedVector;
  ProgressionX, ProgressionY: TFixed;
  CombineCallback: TVectorCombineEvent;
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

  P := SrcRect.Top * Src.Width;
  Progression.Y := - FixedOne;
  case Src.FVectorCombineMode of
    vcmAdd:
      begin
        for I := DstRect.Top to DstRect.Bottom do
        begin
          DstPtr := @GetVectors[I * Width];
          SrcPtr := @Src.GetVectors[SrcRect.Left + P];
          for J := DstRect.Left to DstRect.Right do
          begin
            Inc(SrcPtr^.X, DstPtr[J].X);
            Inc(SrcPtr^.Y, DstPtr[J].Y);
            Inc(SrcPtr);
          end;
          Inc(P, Src.Width);
        end;
      end;
    vcmReplace:
      begin
        for I := DstRect.Top to DstRect.Bottom do
        begin
          DstPtr := @GetVectors[I * Width];
          SrcPtr := @Src.GetVectors[SrcRect.Left + P];
          for J := DstRect.Left to DstRect.Right do
          begin
            SrcPtr^.X := DstPtr[J].X;
            SrcPtr^.Y := DstPtr[J].Y;
            Inc(SrcPtr);
          end;
          Inc(P, Src.Width);
        end;
      end;
  else
    CombineCallback := Src.FOnVectorCombine;
    ProgressionX := Fixed(2 / (DstRect.Right - DstRect.Left - 1));
    ProgressionY := Fixed(2 / (DstRect.Bottom - DstRect.Top - 1));
    for I := DstRect.Top to DstRect.Bottom do
    begin
      Progression.X := - FixedOne;
      DstPtr := @GetVectors[I * Width];
      SrcPtr := @Src.GetVectors[SrcRect.Left + P];
      for J := DstRect.Left to DstRect.Right do
      begin
        CombineCallback(SrcPtr^, Progression, DstPtr[J]);
        Inc(SrcPtr);
        Inc(Progression.X, ProgressionX);
      end;
      Inc(P, Src.Width);
      Inc(Progression.Y, ProgressionY);
    end;
  end;
  //VectorCombiner.FinalizeCombine;
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

procedure TVectorMap.SetVectorCombineMode(const Value: TVectorCombineMode);
begin
  if FVectorCombineMode <> Value then
  begin
    FVectorCombineMode := Value;
    Changed;
  end;
end;

end.
