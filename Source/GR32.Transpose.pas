unit GR32.Transpose;

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
 * The Original Code is Transpose for Graphics32
 *
 * The Initial Developers of the Original Code are
 * Anders Melander <anders@melander.dk>
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

// Define USE_GLOBALBUFFER to use a shared, global block buffer in CacheObliviousTransposeEx32
{$define USE_GLOBALBUFFER}
// Define USE_MOVE to use Move() instead of MoveLongword()
{$define USE_MOVE}

uses
  Classes,
  SyncObjs,
  GR32,
  GR32_LowLevel,
  GR32_System,
  GR32_Bindings;

//------------------------------------------------------------------------------
//
//      Low level transpose API
//
//------------------------------------------------------------------------------
type
  TTranspose32 = procedure(Src, Dst: Pointer; SrcWidth, SrcHeight: integer);


//------------------------------------------------------------------------------
//
//      TBitmap32 transpose routines
//
//------------------------------------------------------------------------------
procedure Transpose32(Src, Dst: TBitmap32); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure Transpose32(Src, Dst: Pointer; SrcWidth, SrcHeight: integer); overload; {$IFDEF USEINLINING} inline; {$ENDIF}


//------------------------------------------------------------------------------
//
//      Reference functions
//
//------------------------------------------------------------------------------
procedure ReferenceTranspose32(Src, Dst: Pointer; Width, Height: integer);


//------------------------------------------------------------------------------
//
//      Transpose implementations
//
//------------------------------------------------------------------------------
// Generally you will not use these directly. Instead use the Transpose32
// functions.
//------------------------------------------------------------------------------
procedure CacheObliviousTranspose32(Src, Dst: pointer; Width, Height: integer);
procedure CacheObliviousTransposeEx32(Src, Dst: pointer; Width, Height: integer);
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
procedure SuperDuperTranspose32(Src, Dst: Pointer; W, Height: integer);
{$ifend}


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
var
  _Transpose32: TTranspose32;

var
  TransposeRegistry: TFunctionRegistry;

const
  TransposeBindingFlagPascal = $0001;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation


//------------------------------------------------------------------------------
//
//      TBitmap32 transpose routine
//
//------------------------------------------------------------------------------
procedure Transpose32(Src, Dst: TBitmap32);
begin
  Dst.SetSize(Src.Height, Src.Width);
  _Transpose32(Src.Bits, Dst.Bits, Src.Width, Src.Height);
end;

procedure Transpose32(Src, Dst: Pointer; SrcWidth, SrcHeight: integer);
begin
  _Transpose32(Src, Dst, SrcWidth, SrcHeight);
end;


//------------------------------------------------------------------------------
//
//      SuperDuperTranspose32
//
//------------------------------------------------------------------------------
// Based on:
//
// - MatrixTranspose by AW
//   http://masm32.com/board/index.php?topic=6140.msg65145#msg65145
//
// - 4x4 matrix transpose by Siekmanski
//   http://masm32.com/board/index.php?topic=6127.msg65026#msg65026
//
// Ported to Delphi by Anders Melander
//------------------------------------------------------------------------------
// TODO : x64 implementation
{$ifdef TARGET_x86}
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
procedure SuperDuperTranspose32(Src, Dst: Pointer; W, Height: integer);
type
  dword = integer;
  // Parameters:
  // EAX <- Source
  // EDX <- Destination
  // ECX <- Width
  // Stack[0] <- Height
  // Preserves: EDI, ESI, EBX
var
  Source, Destination: Pointer;
  Width: dword;
  X4x4Required: dword;
  Y4x4Required: dword;
  remainderX: dword;
  remainderY: dword;
  destRowSize: dword;
  sourceRowSize: dword;
  savedDest: dword;
asm
  push edi
  push esi
  push ebx

  mov Destination, Dst
  mov Source, Src
  mov Width, W

  // How many cols % 4?
  mov eax, Width
  mov ebx, 4
  mov edx, 0
  div ebx
  mov X4x4Required, eax
  mov remainderX, edx

  // How many rows %4?
  mov eax, Height
  mov ebx, 4
  mov edx, 0
  div ebx
  mov Y4x4Required, eax
  mov remainderY, edx

  mov eax, Height
  shl eax, 2
  mov destRowSize, eax

  mov eax, Width
  shl eax, 2
  mov sourceRowSize, eax

  mov ebx, 0

  @@loop1outer:
  cmp ebx, Y4x4Required // while ebx<Y4x4Required // Height % 4
  jae @@loop1outer_exit

    // find starting point for source
    mov eax, ebx
    mul sourceRowSize
    shl eax, 2

    mov esi, Source
    add esi, eax
    mov ecx, esi // save
    // find starting point for destination
    mov eax, ebx
    shl eax, 4
    mov edi, Destination
    add edi, eax
    mov savedDest, edi // save
    push ebx

    mov ebx,0

    @@loop1inner:
    cmp ebx, X4x4Required// while ebx<X4x4Required
    jae @@loop1inner_exit

      mov eax, ebx
      shl eax, 4
      mov esi, ecx
      add esi, eax
      movups xmm0, [esi]
      add esi, sourceRowSize
      movups xmm1, [esi]
      add esi, sourceRowSize
      movups xmm2, [esi]
      add esi, sourceRowSize
      movups xmm3, [esi]

      movaps      xmm4,xmm0
      movaps      xmm5,xmm2
      unpcklps    xmm4,xmm1
      unpcklps    xmm5,xmm3
      unpckhps    xmm0,xmm1
      unpckhps    xmm2,xmm3
      movaps      xmm1,xmm4
      movaps      xmm6,xmm0
      movlhps     xmm4,xmm5
      movlhps     xmm6,xmm2
      movhlps     xmm5,xmm1
      movhlps     xmm2,xmm0

      mov eax, destRowSize
      shl eax, 2
      mul ebx
      mov edi, savedDest
      add edi, eax

      movups [edi], xmm4
      add edi, destRowSize
      movups [edi], xmm5
      add edi, destRowSize
      movups [edi], xmm6
      add edi, destRowSize
      movups [edi], xmm2

      inc ebx

      jmp @@loop1inner
    @@loop1inner_exit:
    pop ebx

    inc ebx

    jmp @@loop1outer
  @@loop1outer_exit:

  // deal with Height not multiple of 4
  cmp remainderX, 1 // .if remainderX >=1
  jb @@no_extra_x
    mov eax, X4x4Required
    shl eax, 4
    mov esi, Source
    add esi, eax

    mov eax, X4x4Required
    shl eax, 2
    mul destRowSize
    mov edi, Destination
    add edi, eax

    mov edx, 0

    @@extra_x:
    cmp edx, remainderX // while edx < remainderX
    jae @@extra_x_exit

      mov ecx, 0
      mov eax, 0

      @@extra_x_y:
      cmp ecx, Height // while ecx < Height
      jae @@extra_x_y_exit

        mov ebx, dword ptr [esi+eax]
        mov dword ptr [edi+4*ecx], ebx
        add eax, sourceRowSize
        inc ecx

        jmp @@extra_x_y
      @@extra_x_y_exit:

      add esi, 4
      add edi, destRowSize
      inc edx

      jmp @@extra_x
    @@extra_x_exit:

  @@no_extra_x:

  // deal with columns not multiple of 4
  cmp remainderY, 1 // if remainderY >=1
  jb @@no_extra_y

    mov eax, Y4x4Required
    shl eax, 2
    mul sourceRowSize
    mov esi, Source
    add esi, eax

    mov eax, Y4x4Required
    shl eax, 4
    mov edi, Destination
    add edi, eax

    mov edx,0

    @@extra_y:
    cmp edx, remainderY // while edx < remainderY
    jae @@extra_y_exit

      mov ecx, 0
      mov eax, 0

      @@extra_y_x:
      cmp ecx, Width // while ecx < Width
      jae @@extra_y_x_exit

        mov ebx, dword ptr [esi+4*ecx]
        mov dword ptr [edi+eax], ebx
        add eax, destRowSize
        inc ecx

        jmp @@extra_y_x
      @@extra_y_x_exit:

      add esi, sourceRowSize
      add edi, 4
      inc edx

      jmp @@extra_y
    @@extra_y_exit:

  @@no_extra_y:

  pop ebx
  pop esi
  pop edi
end;
{$ifend}
{$endif TARGET_x86}


//------------------------------------------------------------------------------
//
//      ReferenceTranspose32
//
//------------------------------------------------------------------------------
// Simple, no-nonsense transpose
//------------------------------------------------------------------------------
procedure ReferenceTranspose32(Src, Dst: pointer; Width, Height: integer);

  procedure CopyRow(Src, Dst: PColor32);
  var
    x: Integer;
  begin
    for x := 0 to Width-1 do
    begin
      Dst^ := Src^;
      Inc(Src);
      Inc(Dst, Height);
    end;
  end;

var
  y: integer;
begin
  for y := 0 to Height-1 do
  begin
    CopyRow(Src, Dst);
    Inc(PColor32(Src), Width);
    Inc(PColor32(Dst));
  end;
end;


//------------------------------------------------------------------------------
//
//      CacheObliviousTranspose32
//
//------------------------------------------------------------------------------
// Recursive implementation of the cache oblivious transpose algorithm.
//------------------------------------------------------------------------------
// References:
//
// - Harald Prokop
//   Master Thesis, MIT, June 1999
//   "Cache-Oblivious Algorithms"
//
//------------------------------------------------------------------------------
const
  CacheObliviousBlockSize = 32;

procedure CacheObliviousTranspose32(Src, Dst: pointer; Width, Height: integer);

  procedure Recurse(Col, Row, ColCount, RowCount: integer);
  var
    y, x: integer;
    Split: integer;
  begin
    if (RowCount <= CacheObliviousBlockSize) and (ColCount <= CacheObliviousBlockSize) then
    begin
      // Transpose block
      for y := Row to Row+RowCount-1 do
        for x := Col to Col+ColCount-1 do
          // Dst[y, x] := Src[x, y]
          PColor32Array(Dst)[y + x * Height] := PColor32Array(Src)[x + y * Width];
    end else
    // Subdivide the longer side
    if (RowCount >= ColCount) then
    begin // Split vertically
      Split := RowCount div 2;
      Recurse(Col, Row, ColCount, Split);

      Inc(Row, Split);
      Dec(RowCount, Split);
      Recurse(Col, Row, ColCount, RowCount);
    end else
    begin // Split horizontally
      Split := ColCount div 2;
      Recurse(Col, Row, Split, RowCount);

      Inc(Col, Split);
      Dec(ColCount, Split);
      Recurse(Col, Row, ColCount, RowCount);
    end;
  end;

begin
  Recurse(0, 0, Width, Height);
end;

//------------------------------------------------------------------------------
// CacheObliviousTransposeEx32 internally transposes to a temporary block buffer
// which is small enough to be cached by the CPU, and then copies from that
// buffer to the destination.
//------------------------------------------------------------------------------
{$ifdef USE_GLOBALBUFFER}
var
  CacheObliviousTransposeBuffer: pointer;
{$endif USE_GLOBALBUFFER}

procedure CacheObliviousTransposeEx32(Src, Dst: pointer; Width, Height: integer);
var
  BlockBuffer: pointer;

  procedure Recurse(Src, Dst: PColor32; X, Y: integer; ColCount, RowCount: Integer);
  var
    Split: Integer;
    BlockX, BlockY: integer;
    p: PColor32;
  begin
    if (ColCount <= CacheObliviousBlockSize) and (RowCount <= CacheObliviousBlockSize) then
    begin
      // Transpose to block buffer
      for BlockY := 0 to RowCount-1 do
        for BlockX := 0 to ColCount-1 do
          // Dst[y, x] := Src[x, y]
          PColor32Array(BlockBuffer)[BlockY + BlockX * CacheObliviousBlockSize] := PColor32Array(Src)[BlockX + BlockY * Width];

      // Copy from block buffer
      p := BlockBuffer;
{$ifdef USE_MOVE}
      RowCount := RowCount * SizeOf(TColor32); // Count is now in bytes
{$endif USE_MOVE}
      for BlockY := 0 to ColCount-1 do
      begin
{$ifdef USE_MOVE}
        Move(p^, Dst^, RowCount);
{$else  USE_MOVE}
        MoveLongword(p^, Dst^, RowCount);
{$endif USE_MOVE}
        Inc(p, CacheObliviousBlockSize);
        Inc(Dst, Height);
      end;
    end else
    // Subdivide the longer side
    if (RowCount >= ColCount) then
    begin // Split vertically
      Split := RowCount div 2;
      Recurse(Src, Dst, X, Y, ColCount, Split);

      Inc(Src, Split*Width);
      Inc(Dst, Split);
      Inc(Y, Split);
      Dec(RowCount, Split);

      Recurse(Src, Dst, X, Y, ColCount, RowCount);
    end else
    begin // Split horizontally
      Split := ColCount div 2;
      Recurse(Src, Dst, X, Y, Split, RowCount);

      Inc(Src, Split);
      Inc(Dst, Split*Height);
      Inc(X, Split);
      Dec(ColCount, Split);

      Recurse(Src, Dst, X, Y, ColCount, RowCount);
    end;
  end;

{$ifdef USE_GLOBALBUFFER}
var
  LocalBuffer: pointer;
{$endif USE_GLOBALBUFFER}
begin
{$ifdef USE_GLOBALBUFFER}
  BlockBuffer := TInterlocked.Exchange(CacheObliviousTransposeBuffer, nil);
  if (BlockBuffer = nil) then
  begin
    GetMem(LocalBuffer, CacheObliviousBlockSize*CacheObliviousBlockSize*SizeOf(TColor32));
    BlockBuffer := LocalBuffer;
  end else
    LocalBuffer := nil;
{$else USE_GLOBALBUFFER}
  GetMem(BlockBuffer, CacheObliviousBlockSize*CacheObliviousBlockSize*SizeOf(TColor32));
{$endif USE_GLOBALBUFFER}

  Recurse(Src, Dst, 0, 0, Width, Height);

{$ifdef USE_GLOBALBUFFER}
  if (LocalBuffer <> nil) then
    FreeMem(LocalBuffer)
  else
    CacheObliviousTransposeBuffer := BlockBuffer;
{$else USE_GLOBALBUFFER}
  FreeMem(BlockBuffer)
{$endif USE_GLOBALBUFFER}
end;


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
procedure RegisterBindings;
begin
  TransposeRegistry := NewRegistry('GR32.Transpose bindings');

  TransposeRegistry.RegisterBinding(@@_Transpose32);

  TransposeRegistry.Add(@@_Transpose32, @ReferenceTranspose32, TransposeBindingFlagPascal);
  TransposeRegistry.Add(@@_Transpose32, @CacheObliviousTranspose32, TransposeBindingFlagPascal, -16);
  TransposeRegistry.Add(@@_Transpose32, @CacheObliviousTransposeEx32, TransposeBindingFlagPascal, -24);

{$ifdef TARGET_x86}
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
  TransposeRegistry.Add(@@_Transpose32, @SuperDuperTranspose32, [isSSE2], -32);
{$ifend}
{$endif TARGET_x86}

  TransposeRegistry.RebindAll;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  RegisterBindings;
{$ifdef USE_GLOBALBUFFER}
  GetMem(CacheObliviousTransposeBuffer, CacheObliviousBlockSize*CacheObliviousBlockSize*SizeOf(TColor32));
{$endif USE_GLOBALBUFFER}
finalization
{$ifdef USE_GLOBALBUFFER}
  FreeMem(CacheObliviousTransposeBuffer);
{$endif USE_GLOBALBUFFER}
end.
