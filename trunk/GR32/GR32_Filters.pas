unit GR32_Filters;

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
 * Portions created by the Initial Developer are Copyright (C) 2000-2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *  Michael Hansen <dyster_tid@hotmail.com>
 *      - 2007/02/25 - Logical Mask Operations
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
  Classes, SysUtils, GR32, GR32_Blend, GR32_System;

{ Basic processing }
type
  TLUT8 = array [Byte] of Byte;
  TLogicalOperator = (loXOR, loAND, loOR);

{ Function Prototypes }
  TLogicalMaskInplace  = procedure(RowPtr: PColor32; Mask: TColor32; Count: Integer);
  TLogicalMaskSrcToDst  = procedure(SrcRow, DstRow: PColor32; Count: Integer; Mask: TColor32);

var
{ Function Variables }
  MaskInplace_XOR: TLogicalMaskInplace;
  MaskSrcToDst_XOR: TLogicalMaskSrcToDst;

  MaskInplace_AND: TLogicalMaskInplace;
  MaskSrcToDst_AND: TLogicalMaskSrcToDst;

  MaskInplace_OR: TLogicalMaskInplace;
  MaskSrcToDst_OR: TLogicalMaskSrcToDst;

{ Access to masked logical operation functions corresponding to a logical operation mode }
  LOGICALMASKPROC_INPLACE: array[TLogicalOperator] of  TLogicalMaskInplace;
  LOGICALMASKPROC_SRCTODST: array[TLogicalOperator] of TLogicalMaskSrcToDst;

procedure CopyAlpha(Dst, Src: TBitmap32);
procedure AlphaToGrayscale(Dst, Src: TBitmap32);
procedure IntensityToAlpha(Dst, Src: TBitmap32);
procedure Invert(Dst, Src: TBitmap32; A: Boolean = True; R: Boolean = True;
  G: Boolean = True; B : Boolean = True);
procedure InvertRGB(Dst, Src: TBitmap32);
procedure ColorToGrayscale(Dst, Src: TBitmap32; PreserveAlpha: Boolean = False);
procedure ApplyLUT(Dst, Src: TBitmap32; const LUT: TLUT8; PreserveAlpha: Boolean = False);
procedure ChromaKey(ABitmap: TBitmap32; TrColor: TColor32);

function CreateBooleanMask(A,R,G,B: Boolean): TColor32;
function CreateByteMask(A,R,G,B: Byte): TColor32;

procedure ApplyLogicalMask(Src: TBitmap32; SrcRect: TRect; Dst: TBitmap32;
  DstX, DstY: Integer; Mask: TColor32; LogicalOperator: TLogicalOperator);  overload;
procedure ApplyLogicalMask(ABitmap: TBitmap32; ARect: TRect; Mask: TColor32;
  LogicalOperator: TLogicalOperator); overload;

procedure CheckParams(Dst, Src: TBitmap32; ResizeDst: Boolean = True);

implementation

uses
  GR32_Lowlevel;

const
  SEmptyBitmap = 'The bitmap is nil';
  SEmptySource = 'The source is nil';
  SEmptyDestination = 'Destination is nil';
  SNoInPlace = 'In-place operation is not supported here';

procedure CheckParams(Dst, Src: TBitmap32; ResizeDst: Boolean = True);
begin
  if not Assigned(Src) then
    raise Exception.Create(SEmptySource);

  if not Assigned(Dst) then
    raise Exception.Create(SEmptyDestination);

  if ResizeDst then Dst.SetSize(Src.Width, Src.Height);
end;

procedure CopyAlpha(Dst, Src: TBitmap32);
var
  I: Integer;
  D, S: PColor32;
begin
  CheckParams(Dst, Src);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];
  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    D^ := (D^ and $00FFFFFF) or (S^ and $FF000000);
    Inc(S); Inc(D);
  end;
  Dst.Changed;
end;

procedure AlphaToGrayscale(Dst, Src: TBitmap32);
var
  I: Integer;
  D, S: PColor32;
begin
  CheckParams(Dst, Src);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];
  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    D^ := Gray32(AlphaComponent(S^));
    Inc(S); Inc(D);
  end;
  Dst.Changed;
end;

procedure IntensityToAlpha(Dst, Src: TBitmap32);
var
  I: Integer;
  D, S: PColor32;
begin
  CheckParams(Dst, Src);
  Dst.SetSize(Src.Width, Src.Height);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];
  for I := 0 to Src.Width * Src.Height - 1 do
  begin
    D^ := SetAlpha(D^, Intensity(S^));
    Inc(S); Inc(D);
  end;
  Dst.Changed;
end;

procedure Invert(Dst, Src: TBitmap32; A,R,G,B: Boolean);
var
  Mask: TColor32;
begin
  Mask := CreateBooleanMask(A, R, G, B);
  if Src = Dst then
  begin
    //Inplace
    CheckParams(Dst, Src, False);
    ApplyLogicalMask(Src, Src.BoundsRect, Mask, loXOR);
  end
  else
  begin
    //Src -> Dst
    CheckParams(Dst, Src);
    ApplyLogicalMask(Src, Src.BoundsRect, Dst, 0, 0, Mask, loXOR);
  end;
end;

procedure InvertRGB(Dst, Src: TBitmap32);
begin
  Invert(Src, Dst, False, True, True, True);
end;

procedure ColorToGrayscale(Dst, Src: TBitmap32; PreserveAlpha: Boolean = False);
var
  I: Integer;
  D, S: PColor32;
begin
  CheckParams(Dst, Src);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];
  
  if PreserveAlpha then
    for I := 0 to Src.Width * Src.Height - 1 do
    begin
      D^ := Gray32(Intensity(S^), AlphaComponent(S^));
      Inc(S); Inc(D);
    end
  else
    for I := 0 to Src.Width * Src.Height - 1 do
    begin
      D^ := Gray32(Intensity(S^));
      Inc(S); Inc(D);
    end;
    
  Dst.Changed;
end;

procedure ApplyLUT(Dst, Src: TBitmap32; const LUT: TLUT8; PreserveAlpha: Boolean = False);
var
  I: Integer;
  D, S: PColor32Entry;
begin
  CheckParams(Dst, Src);
  D := @Dst.Bits[0];
  S := @Src.Bits[0];

  if PreserveAlpha then
    for I := 0 to Src.Width * Src.Height - 1 do
    begin
      D.ARGB := D.ARGB and $FF000000 + LUT[S.B] + LUT[S.G] shl 8 + LUT[S.R] shl 16;
      Inc(S);
      Inc(D);
    end
  else
    for I := 0 to Src.Width * Src.Height - 1 do
    begin
      D.ARGB := $FF000000 + LUT[S.B] + LUT[S.G] shl 8 + LUT[S.R] shl 16;
      Inc(S);
      Inc(D);
    end;
    
  Dst.Changed;
end;

procedure ChromaKey(ABitmap: TBitmap32; TrColor: TColor32);
var
  P: PColor32;
  C: TColor32;
  I: Integer;
begin
  TrColor := TrColor and $00FFFFFF;
  with ABitmap do
  begin
    P := PixelPtr[0, 0];
    for I := 0 to Width * Height - 1 do
    begin
      C := P^ and $00FFFFFF;
      if C = TrColor then P^ := C;
      Inc(P)
    end;
  end;

  ABitmap.Changed;
end;

function CreateBooleanMask(A,R,G,B: Boolean): TColor32;
begin
  Result := 0;
  if A then Inc(Result, $FF000000);
  if R then Inc(Result, $00FF0000);
  if G then Inc(Result, $0000FF00);
  if B then Inc(Result, $000000FF);
end;

function CreateByteMask(A,R,G,B: Byte): TColor32;
begin
  Result := Color32(R, G, B, A);
end;

procedure ApplyLogicalMask(Src: TBitmap32; SrcRect: TRect; Dst: TBitmap32;
  DstX, DstY: Integer; Mask: TColor32; LogicalOperator: TLogicalOperator);
var
  I, Count: Integer;
  DstRect: TRect;
  SrcDstProc : TLogicalMaskSrcToDst;
begin
  CheckParams(Dst, Src, False);

  SrcDstProc := LOGICALMASKPROC_SRCTODST[LogicalOperator];

  if Assigned(SrcDstProc) then
  with Dst do
  begin
    IntersectRect(SrcRect, SrcRect, Src.BoundsRect);
    if (SrcRect.Right < SrcRect.Left) or (SrcRect.Bottom < SrcRect.Top) then Exit;

    DstX := Clamp(DstX, 0, Width - 1);
    DstY := Clamp(DstY, 0, Height - 1);

    DstRect.TopLeft := Point(DstX, DstY);
    DstRect.Right := DstX + SrcRect.Right - SrcRect.Left;
    DstRect.Bottom := DstY + SrcRect.Bottom - SrcRect.Top;

    IntersectRect(DstRect, DstRect, Dst.BoundsRect);
    IntersectRect(DstRect, DstRect, Dst.ClipRect);


    if not MeasuringMode then
    begin
      BeginUpdate;
      try
        with DstRect do
        if (Bottom - Top) > 0 then
        begin
          Count := Right - Left;
          if Count > 0 then
              for I := 0 to Bottom - Top - 1 do
                SrcDstProc(Src.PixelPtr[SrcRect.Left, SrcRect.Top + I], PixelPtr[Left, Top + I], Count, Mask)
        end;
      finally
        EndUpdate;
      end;
    end;

    Changed(DstRect);
  end;
end;

procedure ApplyLogicalMask(ABitmap: TBitmap32; ARect: TRect; Mask: TColor32;
  LogicalOperator: TLogicalOperator);
var
  I, Count: Integer;
  InplaceProc : TLogicalMaskInplace;
begin
  if not Assigned(ABitmap) then
    raise Exception.Create(SEmptyBitmap);

  InplaceProc := LOGICALMASKPROC_INPLACE[LogicalOperator];

  if Assigned(InplaceProc) then
  with ABitmap do
  begin
    IntersectRect(ARect, ARect, BoundsRect);
    IntersectRect(ARect, ARect, ClipRect);
    if (ARect.Right < ARect.Left) or (ARect.Bottom < ARect.Top) then Exit;

    if not MeasuringMode then
    begin
      BeginUpdate;
      try
        with ARect do
        if (Bottom - Top) > 0 then
        begin
          Count := Right - Left;
          if Count > 0 then
          begin
            if Count = Width then
              InplaceProc(PixelPtr[Left, Top], Mask, Count * (Bottom - Top))
            else
              for I := Top to Bottom - 1 do
                InplaceProc(PixelPtr[Left, I], Mask, Count);
          end;
        end;
      finally
        EndUpdate;
      end;
    end;

    Changed(ARect);
  end;
end;

{ In-place logical mask functions }
{ Non - MMX versions}

procedure _XOR_Inplace(Row: PColor32; Mask: TColor32; Count: Integer);
// No speedup achieveable using MMX
asm
   TEST  ECX, ECX
   JZ    @Exit

   PUSH  EBX
   MOV   EBX, ECX
   SHR   ECX, 4
   SHL   ECX, 4
   JZ    @PrepSingleLoop
   LEA   EAX, [EAX + ECX * 4]
   SHL   ECX, 2
   NEG   ECX

  @ChunkLoop:
   //16x unrolled loop
   XOR   [EAX + ECX], EDX
   XOR   [EAX + ECX + 4], EDX
   XOR   [EAX + ECX + 8], EDX
   XOR   [EAX + ECX + 12], EDX

   XOR   [EAX + ECX + 16], EDX
   XOR   [EAX + ECX + 20], EDX
   XOR   [EAX + ECX + 24], EDX
   XOR   [EAX + ECX + 28], EDX

   XOR   [EAX + ECX + 32], EDX
   XOR   [EAX + ECX + 36], EDX
   XOR   [EAX + ECX + 40], EDX
   XOR   [EAX + ECX + 44], EDX

   XOR   [EAX + ECX + 48], EDX
   XOR   [EAX + ECX + 52], EDX
   XOR   [EAX + ECX + 56], EDX
   XOR   [EAX + ECX + 60], EDX

   ADD   ECX, 16 * 4
   JNZ   @ChunkLoop

  @PrepSingleLoop:
   MOV   ECX, EBX
   SHR   EBX, 4
   SHL   EBX, 4
   SUB   ECX, EBX
   JZ    @PopExit

   LEA   EAX, [EAX + ECX * 4]
   NEG   ECX

  @SingleLoop:
   XOR   [EAX + ECX * 4], EDX
   INC   ECX
   JNZ   @SingleLoop

  @PopExit:
   POP   EBX

  @Exit:
end;

procedure _OR_Inplace(Row: PColor32; Mask: TColor32; Count: Integer);
// No speedup achieveable using MMX
asm
   TEST  ECX, ECX
   JZ    @Exit

   PUSH  EBX
   MOV   EBX, ECX
   SHR   ECX, 4
   SHL   ECX, 4
   JZ    @PrepSingleLoop
   LEA   EAX, [EAX + ECX * 4]
   SHL   ECX, 2
   NEG   ECX

  @ChunkLoop:
   //16x unrolled loop
   OR   [EAX + ECX], EDX
   OR   [EAX + ECX + 4], EDX
   OR   [EAX + ECX + 8], EDX
   OR   [EAX + ECX + 12], EDX

   OR   [EAX + ECX + 16], EDX
   OR   [EAX + ECX + 20], EDX
   OR   [EAX + ECX + 24], EDX
   OR   [EAX + ECX + 28], EDX

   OR   [EAX + ECX + 32], EDX
   OR   [EAX + ECX + 36], EDX
   OR   [EAX + ECX + 40], EDX
   OR   [EAX + ECX + 44], EDX

   OR   [EAX + ECX + 48], EDX
   OR   [EAX + ECX + 52], EDX
   OR   [EAX + ECX + 56], EDX
   OR   [EAX + ECX + 60], EDX

   ADD   ECX, 16 * 4
   JNZ   @ChunkLoop

  @PrepSingleLoop:
   MOV   ECX, EBX
   SHR   EBX, 4
   SHL   EBX, 4
   SUB   ECX, EBX
   JZ    @PopExit

   LEA   EAX, [EAX + ECX * 4]
   NEG   ECX

  @SingleLoop:
   OR   [EAX + ECX * 4], EDX
   INC   ECX
   JNZ   @SingleLoop

  @PopExit:
   POP   EBX

  @Exit:
end;

procedure _AND_Inplace(Row: PColor32; Mask: TColor32; Count: Integer);
// No speedup achieveable using MMX
asm
   TEST  ECX, ECX
   JZ    @Exit

   PUSH  EBX
   MOV   EBX, ECX
   SHR   ECX, 4
   SHL   ECX, 4
   JZ    @PrepSingleLoop
   LEA   EAX, [EAX + ECX * 4]
   SHL   ECX, 2
   NEG   ECX

  @ChunkLoop:
   //16x unrolled loop
   AND   [EAX + ECX], EDX
   AND   [EAX + ECX + 4], EDX
   AND   [EAX + ECX + 8], EDX
   AND   [EAX + ECX + 12], EDX

   AND   [EAX + ECX + 16], EDX
   AND   [EAX + ECX + 20], EDX
   AND   [EAX + ECX + 24], EDX
   AND   [EAX + ECX + 28], EDX

   AND   [EAX + ECX + 32], EDX
   AND   [EAX + ECX + 36], EDX
   AND   [EAX + ECX + 40], EDX
   AND   [EAX + ECX + 44], EDX

   AND   [EAX + ECX + 48], EDX
   AND   [EAX + ECX + 52], EDX
   AND   [EAX + ECX + 56], EDX
   AND   [EAX + ECX + 60], EDX

   ADD   ECX, 16 * 4
   JNZ   @ChunkLoop

  @PrepSingleLoop:
   MOV   ECX, EBX
   SHR   EBX, 4
   SHL   EBX, 4
   SUB   ECX, EBX
   JZ    @PopExit

   LEA   EAX, [EAX + ECX * 4]
   NEG   ECX

  @SingleLoop:
   AND   [EAX + ECX * 4], EDX
   INC   ECX
   JNZ   @SingleLoop

  @PopExit:
   POP   EBX

  @Exit:
end;

{ Src -> Dst logical mask functions  }
{ Non - MMX versions}

procedure _XOR_SrcToDst(SrcRow, DstRow: PColor32; Count: Integer; Mask: TColor32);
asm
   PUSH  EBX
   PUSH  EDI

   LEA   EAX, [EAX + ECX * 4]
   LEA   EDX, [EDX + ECX * 4]
   NEG   ECX

   MOV   EDI, Mask

   @Loop:
   MOV   EBX, [EAX + ECX * 4]
   XOR   EBX, EDI
   MOV   [EDX + ECX * 4], EBX
   INC   ECX
   JNZ   @Loop

   @Exit:

   POP   EDI
   POP   EBX
end;

procedure _OR_SrcToDst(SrcRow, DstRow: PColor32; Count: Integer; Mask: TColor32);
asm
   PUSH  EBX
   PUSH  EDI

   LEA   EAX, [EAX + ECX * 4]
   LEA   EDX, [EDX + ECX * 4]
   NEG   ECX

   MOV   EDI, Mask

   @Loop:
   MOV   EBX, [EAX + ECX * 4]
   OR    EBX, EDI
   MOV   [EDX + ECX * 4], EBX
   INC   ECX
   JNZ   @Loop

   @Exit:

   POP   EDI
   POP   EBX
end;

procedure _AND_SrcToDst(SrcRow, DstRow: PColor32; Count: Integer; Mask: TColor32);
asm
   PUSH  EBX
   PUSH  EDI

   LEA   EAX, [EAX + ECX * 4]
   LEA   EDX, [EDX + ECX * 4]
   NEG   ECX

   MOV   EDI, Mask

   @Loop:
   MOV   EBX, [EAX + ECX * 4]
   AND   EBX, EDI
   MOV   [EDX + ECX * 4], EBX
   INC   ECX
   JNZ   @Loop

   @Exit:

   POP   EDI
   POP   EBX
end;

{ MMX versions}

procedure M_XOR_SrcToDst(SrcRow, DstRow: PColor32; Count: Integer; Mask: TColor32);
//MMX version
var
  QMask: Int64;

asm
   PUSH  EBX
   PUSH  EDI

   TEST  ECX, ECX
   JZ    @Exit

   MOV   EBX, ECX
   SHR   ECX, 4
   SHL   ECX, 4
   JZ    @PrepSingleLoop

   SAR    ECX, 1
   LEA    EAX, [EAX + ECX * 8]
   LEA    EDX, [EDX + ECX * 8]
   NEG    ECX

   MOVD       MM7, MASK
   PUNPCKLDQ  MM7, MM7
   MOVQ       QMask, MM7
   EMMS

  @Loop:

   MOVQ   MM0, [EAX + ECX * 8]
   MOVQ   MM1, [EAX + ECX * 8 + 8]
   MOVQ   MM2, [EAX + ECX * 8 + 16]
   MOVQ   MM3, [EAX + ECX * 8 + 24]
   MOVQ   MM4, [EAX + ECX * 8 + 32]
   MOVQ   MM5, [EAX + ECX * 8 + 40]
   MOVQ   MM6, [EAX + ECX * 8 + 48]
   MOVQ   MM7, [EAX + ECX * 8 + 56]

   PXOR   MM0, QMask
   PXOR   MM1, QMask
   PXOR   MM2, QMask
   PXOR   MM3, QMask
   PXOR   MM4, QMask
   PXOR   MM5, QMask
   PXOR   MM6, QMask
   PXOR   MM7, QMask

   MOVQ   [EDX + ECX * 8], MM0
   MOVQ   [EDX + ECX * 8 + 8], MM1
   MOVQ   [EDX + ECX * 8 + 16], MM2
   MOVQ   [EDX + ECX * 8 + 24], MM3
   MOVQ   [EDX + ECX * 8 + 32], MM4
   MOVQ   [EDX + ECX * 8 + 40], MM5
   MOVQ   [EDX + ECX * 8 + 48], MM6
   MOVQ   [EDX + ECX * 8 + 56], MM7

   ADD    ECX, 8
   JS    @Loop

   EMMS

  @PrepSingleLoop:
   MOV   ECX, EBX
   SHR   EBX, 4
   SHL   EBX, 4
   SUB   ECX, EBX
   JZ    @Exit

   LEA   EAX, [EAX + ECX * 4]
   LEA   EDX, [EDX + ECX * 4]
   NEG   ECX

   MOV   EDI, Mask

  @SingleLoop:
   MOV   EBX, [EAX + ECX * 4]
   XOR   EBX, EDI
   MOV   [EDX + ECX * 4], EBX
   INC   ECX
   JNZ   @SingleLoop

  @Exit:
   POP   EDI
   POP   EBX
end;

procedure M_OR_SrcToDst(SrcRow, DstRow: PColor32; Count: Integer; Mask: TColor32);
//MMX version
var
  QMask: Int64;

asm
   PUSH  EBX
   PUSH  EDI

   TEST  ECX, ECX
   JZ    @Exit

   MOV   EBX, ECX
   SHR   ECX, 4
   SHL   ECX, 4
   JZ    @PrepSingleLoop

   SAR    ECX, 1
   LEA    EAX, [EAX + ECX * 8]
   LEA    EDX, [EDX + ECX * 8]
   NEG    ECX

   MOVD       MM7, MASK
   PUNPCKLDQ  MM7, MM7
   MOVQ       QMask, MM7
   EMMS

  @Loop:

   MOVQ   MM0, [EAX + ECX * 8]
   MOVQ   MM1, [EAX + ECX * 8 + 8]
   MOVQ   MM2, [EAX + ECX * 8 + 16]
   MOVQ   MM3, [EAX + ECX * 8 + 24]
   MOVQ   MM4, [EAX + ECX * 8 + 32]
   MOVQ   MM5, [EAX + ECX * 8 + 40]
   MOVQ   MM6, [EAX + ECX * 8 + 48]
   MOVQ   MM7, [EAX + ECX * 8 + 56]

   POR   MM0, QMask
   POR   MM1, QMask
   POR   MM2, QMask
   POR   MM3, QMask
   POR   MM4, QMask
   POR   MM5, QMask
   POR   MM6, QMask
   POR   MM7, QMask

   MOVQ   [EDX + ECX * 8], MM0
   MOVQ   [EDX + ECX * 8 + 8], MM1
   MOVQ   [EDX + ECX * 8 + 16], MM2
   MOVQ   [EDX + ECX * 8 + 24], MM3
   MOVQ   [EDX + ECX * 8 + 32], MM4
   MOVQ   [EDX + ECX * 8 + 40], MM5
   MOVQ   [EDX + ECX * 8 + 48], MM6
   MOVQ   [EDX + ECX * 8 + 56], MM7

   ADD    ECX, 8
   JS    @Loop

   EMMS

  @PrepSingleLoop:
   MOV   ECX, EBX
   SHR   EBX, 4
   SHL   EBX, 4
   SUB   ECX, EBX
   JZ    @Exit

   LEA   EAX, [EAX + ECX * 4]
   LEA   EDX, [EDX + ECX * 4]
   NEG   ECX

   MOV   EDI, Mask

  @SingleLoop:
   MOV   EBX, [EAX + ECX * 4]
   OR   EBX, EDI
   MOV   [EDX + ECX * 4], EBX
   INC   ECX
   JNZ   @SingleLoop

  @Exit:
   POP   EDI
   POP   EBX
end;

procedure M_AND_SrcToDst(SrcRow, DstRow: PColor32; Count: Integer; Mask: TColor32);
//MMX version
var
  QMask: Int64;

asm
   PUSH  EBX
   PUSH  EDI

   TEST  ECX, ECX
   JZ    @Exit

   MOV   EBX, ECX
   SHR   ECX, 4
   SHL   ECX, 4
   JZ    @PrepSingleLoop

   SAR    ECX, 1
   LEA    EAX, [EAX + ECX * 8]
   LEA    EDX, [EDX + ECX * 8]
   NEG    ECX

   MOVD       MM7, MASK
   PUNPCKLDQ  MM7, MM7
   MOVQ       QMask, MM7
   EMMS

  @Loop:

   MOVQ   MM0, [EAX + ECX * 8]
   MOVQ   MM1, [EAX + ECX * 8 + 8]
   MOVQ   MM2, [EAX + ECX * 8 + 16]
   MOVQ   MM3, [EAX + ECX * 8 + 24]
   MOVQ   MM4, [EAX + ECX * 8 + 32]
   MOVQ   MM5, [EAX + ECX * 8 + 40]
   MOVQ   MM6, [EAX + ECX * 8 + 48]
   MOVQ   MM7, [EAX + ECX * 8 + 56]

   PAND   MM0, QMask
   PAND   MM1, QMask
   PAND   MM2, QMask
   PAND   MM3, QMask
   PAND   MM4, QMask
   PAND   MM5, QMask
   PAND   MM6, QMask
   PAND   MM7, QMask

   MOVQ   [EDX + ECX * 8], MM0
   MOVQ   [EDX + ECX * 8 + 8], MM1
   MOVQ   [EDX + ECX * 8 + 16], MM2
   MOVQ   [EDX + ECX * 8 + 24], MM3
   MOVQ   [EDX + ECX * 8 + 32], MM4
   MOVQ   [EDX + ECX * 8 + 40], MM5
   MOVQ   [EDX + ECX * 8 + 48], MM6
   MOVQ   [EDX + ECX * 8 + 56], MM7

   ADD    ECX, 8
   JS    @Loop

   EMMS

  @PrepSingleLoop:
   MOV   ECX, EBX
   SHR   EBX, 4
   SHL   EBX, 4
   SUB   ECX, EBX
   JZ    @Exit

   LEA   EAX, [EAX + ECX * 4]
   LEA   EDX, [EDX + ECX * 4]
   NEG   ECX

   MOV   EDI, Mask

  @SingleLoop:
   MOV   EBX, [EAX + ECX * 4]
   AND   EBX, EDI
   MOV   [EDX + ECX * 4], EBX
   INC   ECX
   JNZ   @SingleLoop

  @Exit:
   POP   EDI
   POP   EBX
end;

{ Extended MMX versions}

procedure EM_XOR_SrcToDst(SrcRow, DstRow: PColor32; Count: Integer; Mask: TColor32);
//EMMX version
var
  QMask: Int64;

asm
   PUSH  EBX
   PUSH  EDI

   TEST  ECX, ECX
   JZ    @Exit

   MOV   EBX, ECX
   SHR   ECX, 4
   SHL   ECX, 4
   JZ    @PrepSingleLoop

   SAR    ECX, 1
   LEA    EAX, [EAX + ECX * 8]
   LEA    EDX, [EDX + ECX * 8]
   NEG    ECX

   MOVD       MM7, MASK
   PUNPCKLDQ  MM7, MM7
   MOVQ       QMask, MM7
   EMMS

  @Loop:

   MOVQ   MM0, [EAX + ECX * 8]
   MOVQ   MM1, [EAX + ECX * 8 + 8]
   MOVQ   MM2, [EAX + ECX * 8 + 16]
   MOVQ   MM3, [EAX + ECX * 8 + 24]
   MOVQ   MM4, [EAX + ECX * 8 + 32]
   MOVQ   MM5, [EAX + ECX * 8 + 40]
   MOVQ   MM6, [EAX + ECX * 8 + 48]
   MOVQ   MM7, [EAX + ECX * 8 + 56]

   PXOR   MM0, QMask
   PXOR   MM1, QMask
   PXOR   MM2, QMask
   PXOR   MM3, QMask
   PXOR   MM4, QMask
   PXOR   MM5, QMask
   PXOR   MM6, QMask
   PXOR   MM7, QMask

   MOVNTQ   [EDX + ECX * 8], MM0
   MOVNTQ   [EDX + ECX * 8 + 8], MM1
   MOVNTQ   [EDX + ECX * 8 + 16], MM2
   MOVNTQ   [EDX + ECX * 8 + 24], MM3
   MOVNTQ   [EDX + ECX * 8 + 32], MM4
   MOVNTQ   [EDX + ECX * 8 + 40], MM5
   MOVNTQ   [EDX + ECX * 8 + 48], MM6
   MOVNTQ   [EDX + ECX * 8 + 56], MM7

   ADD    ECX, 8
   JS    @Loop

   EMMS

  @PrepSingleLoop:
   MOV   ECX, EBX
   SHR   EBX, 4
   SHL   EBX, 4
   SUB   ECX, EBX
   JZ    @Exit

   LEA   EAX, [EAX + ECX * 4]
   LEA   EDX, [EDX + ECX * 4]
   NEG   ECX

   MOV   EDI, Mask

  @SingleLoop:
   MOV   EBX, [EAX + ECX * 4]
   XOR   EBX, EDI
   MOV   [EDX + ECX * 4], EBX
   INC   ECX
   JNZ   @SingleLoop

  @Exit:
   POP   EDI
   POP   EBX
end;

procedure EM_OR_SrcToDst(SrcRow, DstRow: PColor32; Count: Integer; Mask: TColor32);
//EMMX version
var
  QMask: Int64;

asm
   PUSH  EBX
   PUSH  EDI

   TEST  ECX, ECX
   JZ    @Exit

   MOV   EBX, ECX
   SHR   ECX, 4
   SHL   ECX, 4
   JZ    @PrepSingleLoop

   SAR    ECX, 1
   LEA    EAX, [EAX + ECX * 8]
   LEA    EDX, [EDX + ECX * 8]
   NEG    ECX

   MOVD       MM7, MASK
   PUNPCKLDQ  MM7, MM7
   MOVQ       QMask, MM7
   EMMS

  @Loop:

   MOVQ   MM0, [EAX + ECX * 8]
   MOVQ   MM1, [EAX + ECX * 8 + 8]
   MOVQ   MM2, [EAX + ECX * 8 + 16]
   MOVQ   MM3, [EAX + ECX * 8 + 24]
   MOVQ   MM4, [EAX + ECX * 8 + 32]
   MOVQ   MM5, [EAX + ECX * 8 + 40]
   MOVQ   MM6, [EAX + ECX * 8 + 48]
   MOVQ   MM7, [EAX + ECX * 8 + 56]

   POR   MM0, QMask
   POR   MM1, QMask
   POR   MM2, QMask
   POR   MM3, QMask
   POR   MM4, QMask
   POR   MM5, QMask
   POR   MM6, QMask
   POR   MM7, QMask

   MOVNTQ   [EDX + ECX * 8], MM0
   MOVNTQ   [EDX + ECX * 8 + 8], MM1
   MOVNTQ   [EDX + ECX * 8 + 16], MM2
   MOVNTQ   [EDX + ECX * 8 + 24], MM3
   MOVNTQ   [EDX + ECX * 8 + 32], MM4
   MOVNTQ   [EDX + ECX * 8 + 40], MM5
   MOVNTQ   [EDX + ECX * 8 + 48], MM6
   MOVNTQ   [EDX + ECX * 8 + 56], MM7

   ADD    ECX, 8
   JS    @Loop

   EMMS

  @PrepSingleLoop:
   MOV   ECX, EBX
   SHR   EBX, 4
   SHL   EBX, 4
   SUB   ECX, EBX
   JZ    @Exit

   LEA   EAX, [EAX + ECX * 4]
   LEA   EDX, [EDX + ECX * 4]
   NEG   ECX

   MOV   EDI, Mask

  @SingleLoop:
   MOV   EBX, [EAX + ECX * 4]
   OR   EBX, EDI
   MOV   [EDX + ECX * 4], EBX
   INC   ECX
   JNZ   @SingleLoop

  @Exit:
   POP   EDI
   POP   EBX
end;

procedure EM_AND_SrcToDst(SrcRow, DstRow: PColor32; Count: Integer; Mask: TColor32);
//EMMX version
var
  QMask: Int64;

asm
   PUSH  EBX
   PUSH  EDI

   TEST  ECX, ECX
   JZ    @Exit

   MOV   EBX, ECX
   SHR   ECX, 4
   SHL   ECX, 4
   JZ    @PrepSingleLoop

   SAR    ECX, 1
   LEA    EAX, [EAX + ECX * 8]
   LEA    EDX, [EDX + ECX * 8]
   NEG    ECX

   MOVD       MM7, MASK
   PUNPCKLDQ  MM7, MM7
   MOVQ       QMask, MM7
   EMMS

  @Loop:

   MOVQ   MM0, [EAX + ECX * 8]
   MOVQ   MM1, [EAX + ECX * 8 + 8]
   MOVQ   MM2, [EAX + ECX * 8 + 16]
   MOVQ   MM3, [EAX + ECX * 8 + 24]
   MOVQ   MM4, [EAX + ECX * 8 + 32]
   MOVQ   MM5, [EAX + ECX * 8 + 40]
   MOVQ   MM6, [EAX + ECX * 8 + 48]
   MOVQ   MM7, [EAX + ECX * 8 + 56]

   PAND   MM0, QMask
   PAND   MM1, QMask
   PAND   MM2, QMask
   PAND   MM3, QMask
   PAND   MM4, QMask
   PAND   MM5, QMask
   PAND   MM6, QMask
   PAND   MM7, QMask

   MOVNTQ   [EDX + ECX * 8], MM0
   MOVNTQ   [EDX + ECX * 8 + 8], MM1
   MOVNTQ   [EDX + ECX * 8 + 16], MM2
   MOVNTQ   [EDX + ECX * 8 + 24], MM3
   MOVNTQ   [EDX + ECX * 8 + 32], MM4
   MOVNTQ   [EDX + ECX * 8 + 40], MM5
   MOVNTQ   [EDX + ECX * 8 + 48], MM6
   MOVNTQ   [EDX + ECX * 8 + 56], MM7

   ADD    ECX, 8
   JS    @Loop

   EMMS

  @PrepSingleLoop:
   MOV   ECX, EBX
   SHR   EBX, 4
   SHL   EBX, 4
   SUB   ECX, EBX
   JZ    @Exit

   LEA   EAX, [EAX + ECX * 4]
   LEA   EDX, [EDX + ECX * 4]
   NEG   ECX

   MOV   EDI, Mask

  @SingleLoop:
   MOV   EBX, [EAX + ECX * 4]
   AND   EBX, EDI
   MOV   [EDX + ECX * 4], EBX
   INC   ECX
   JNZ   @SingleLoop

  @Exit:
   POP   EDI
   POP   EBX
end;


procedure SetupFunctions;
begin
  MaskInplace_XOR := _XOR_Inplace;
  MaskInplace_OR := _OR_Inplace;
  MaskInplace_AND := _AND_Inplace;

  if HasEMMX then
  begin
    //Link Extended MMX functions
    MaskSrcToDst_XOR := EM_XOR_SrcToDst;
    MaskSrcToDst_OR := EM_OR_SrcToDst;
    MaskSrcToDst_AND := EM_AND_SrcToDst;
  end
  else
  if HasMMX then
  begin
    //Link MMX functions
    MaskSrcToDst_XOR := M_XOR_SrcToDst;
    MaskSrcToDst_OR := M_OR_SrcToDst;
    MaskSrcToDst_AND := M_AND_SrcToDst;
  end
  else
  begin
    //Link non-MMX functions
    MaskSrcToDst_XOR := _XOR_SrcToDst;
    MaskSrcToDst_OR := _OR_SrcToDst;
    MaskSrcToDst_AND := _AND_SrcToDst;
  end;

  LOGICALMASKPROC_INPLACE[loXOR] := MaskInplace_XOR;
  LOGICALMASKPROC_INPLACE[loOR] := MaskInplace_OR;
  LOGICALMASKPROC_INPLACE[loAND] := MaskInplace_AND;

  LOGICALMASKPROC_SRCTODST[loXOR] := MaskSrcToDst_XOR;
  LOGICALMASKPROC_SRCTODST[loOR] := MaskSrcToDst_OR;
  LOGICALMASKPROC_SRCTODST[loAND] := MaskSrcToDst_AND;
end;

initialization
  SetupFunctions;

end.
