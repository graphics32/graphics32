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
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
procedure SuperDuperTranspose32(Src, Dst: Pointer; W, Height: integer); //{$IFDEF FPC}assembler;{$ENDIF}
// TODO : This has become a mess. Split into separate x86 and x64 implementations.
type
  dword = Cardinal;
  // Parameters (x86):
  //   EAX <- Source
  //   EDX <- Destination
  //   ECX <- Width
  //   Stack[0] <- Height
  //   Preserves: EDI, ESI, EBX
  //
  // Parameters (x64):
  //   RCX <- Source
  //   RDX <- Destination
  //   R8 <- Width
  //   R9 <- Height
  //   Preserves: RDI, RSI, RBX, XMM4, XMM5, XMM6
var
  Source, Destination: Pointer;
{$if defined(TARGET_x86)}
  Width: dword;
{$ifend}
  X4x4Required: dword;
  Y4x4Required: dword;
  remainderX: dword;
  remainderY: dword;
{$if defined(TARGET_x86)}
  destRowSize: dword; // R10
  sourceRowSize: dword; // R11
{$ifend}
  savedDest: Pointer;
{$if defined(TARGET_x64) and defined(FPC)}begin{$ifend}
asm
{$if defined(TARGET_x64)}
{$IFNDEF FPC}
  .PUSHNV RDI
  .PUSHNV RSI
  .PUSHNV RBX
  .SAVENV XMM4
  .SAVENV XMM5
  .SAVENV XMM6
{$ELSE}
  push RDI
  push RSI
  push RBX
{$ENDIF}
{$elseif defined(TARGET_x86)}
  push edi
  push esi
  push ebx
{$else}
{$message fatal 'Unsupported target'}
{$ifend}

{$if defined(TARGET_x64)}
{$elseif defined(TARGET_x86)}
{$ifend}

{$if defined(TARGET_x64)}
  mov Destination, RDX
  mov Source, RCX
{$elseif defined(TARGET_x86)}
  mov Destination, Dst
  mov Source, Src
{$ifend}
{$if defined(TARGET_x86)}
  mov Width, W
{$ifend}

  // How many cols % 4?
  mov eax, W
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
{$if defined(TARGET_x86)}
  mov destRowSize, eax
{$elseif defined(TARGET_x64)}
  mov r10, rax
{$ifend}

{$if defined(TARGET_x86)}
  mov eax, Width
{$elseif defined(TARGET_x64)}
  mov eax, W
{$ifend}
  shl eax, 2
{$if defined(TARGET_x86)}
  mov sourceRowSize, eax
{$elseif defined(TARGET_x64)}
  mov r11, rax
{$ifend}

  mov ebx, 0

  @@loop1outer:
  cmp ebx, Y4x4Required // while ebx<Y4x4Required // Height % 4
  jae @@loop1outer_exit

    // find starting point for source
    mov eax, ebx
{$if defined(TARGET_x86)}
    mul sourceRowSize
{$elseif defined(TARGET_x64)}
    mul r11
{$ifend}
    shl eax, 2

{$if defined(TARGET_x86)}
    mov esi, Source
    add esi, eax
    mov ecx, esi // save
{$elseif defined(TARGET_x64)}
    mov rsi, Source
    add rsi, rax
    mov rcx, rsi // save
{$ifend}
    // find starting point for destination
    mov eax, ebx
    shl eax, 4
{$if defined(TARGET_x86)}
    mov edi, Destination
    add edi, eax
    mov savedDest, edi // save
    push ebx
{$elseif defined(TARGET_x64)}
    mov rdi, Destination
    add rdi, rax
    mov savedDest, rdi // save
    push rbx
{$ifend}

    mov ebx, 0

    @@loop1inner:
    cmp ebx, X4x4Required// while ebx<X4x4Required
    jae @@loop1inner_exit

      mov eax, ebx
      shl eax, 4
{$if defined(TARGET_x86)}
      mov esi, ecx
      add esi, eax
      movups xmm0, [esi]
      add esi, sourceRowSize
      movups xmm1, [esi]
      add esi, sourceRowSize
      movups xmm2, [esi]
      add esi, sourceRowSize
      movups xmm3, [esi]
{$elseif defined(TARGET_x64)}
      mov rsi, rcx
      add rsi, rax
      movups xmm0, [rsi]
      add rsi, r11
      movups xmm1, [rsi]
      add rsi, r11
      movups xmm2, [rsi]
      add rsi, r11
      movups xmm3, [rsi]
{$ifend}

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

{$if defined(TARGET_x86)}
      mov eax, destRowSize
{$elseif defined(TARGET_x64)}
      mov rax, r10
{$ifend}
      shl eax, 2
      mul ebx
{$if defined(TARGET_x86)}
      mov edi, savedDest
      add edi, eax
{$elseif defined(TARGET_x64)}
      mov rdi, savedDest
      add rdi, rax
{$ifend}

{$if defined(TARGET_x86)}
      movups [edi], xmm4
      add edi, destRowSize
      movups [edi], xmm5
      add edi, destRowSize
      movups [edi], xmm6
      add edi, destRowSize
      movups [edi], xmm2
{$elseif defined(TARGET_x64)}
      movups [rdi], xmm4
      add rdi, r10
      movups [rdi], xmm5
      add rdi, r10
      movups [rdi], xmm6
      add rdi, r10
      movups [rdi], xmm2
{$ifend}

      inc ebx

      jmp @@loop1inner
    @@loop1inner_exit:
{$if defined(TARGET_x86)}
    pop ebx
{$elseif defined(TARGET_x64)}
    pop rbx
{$ifend}

    inc ebx

    jmp @@loop1outer
  @@loop1outer_exit:

  // deal with Height not multiple of 4
  cmp remainderX, 1 // .if remainderX >=1
  jb @@no_extra_x
    mov eax, X4x4Required
    shl eax, 4
{$if defined(TARGET_x86)}
    mov esi, Source
    add esi, eax
{$elseif defined(TARGET_x64)}
    mov rsi, Source
    add rsi, rax
{$ifend}

    mov eax, X4x4Required
    shl eax, 2
{$if defined(TARGET_x86)}
    mul destRowSize
    mov edi, Destination
    add edi, eax
{$elseif defined(TARGET_x64)}
    mul r10
    mov rdi, Destination
    add rdi, rax
{$ifend}

    mov edx, 0

    @@extra_x:
    cmp edx, remainderX // while edx < remainderX
    jae @@extra_x_exit

      mov ecx, 0
      mov eax, 0

      @@extra_x_y:
      cmp ecx, Height // while ecx < Height
      jae @@extra_x_y_exit

{$if defined(TARGET_x86)}
        mov ebx, dword ptr [esi+eax]
        mov dword ptr [edi+4*ecx], ebx
{$elseif defined(TARGET_x64)}
        mov ebx, dword ptr [rsi+rax]
        mov dword ptr [rdi+4*rcx], ebx
{$ifend}
{$if defined(TARGET_x86)}
        add eax, sourceRowSize
{$elseif defined(TARGET_x64)}
        add rax, r11
{$ifend}
        inc ecx

        jmp @@extra_x_y
      @@extra_x_y_exit:

{$if defined(TARGET_x86)}
      add esi, 4
      add edi, destRowSize
{$elseif defined(TARGET_x64)}
      add rsi, 4
      add rdi, r10
{$ifend}
      inc edx

      jmp @@extra_x
    @@extra_x_exit:

  @@no_extra_x:

  // deal with columns not multiple of 4
  cmp remainderY, 1 // if remainderY >=1
  jb @@no_extra_y

    mov eax, Y4x4Required
    shl eax, 2
{$if defined(TARGET_x86)}
    mul sourceRowSize
{$elseif defined(TARGET_x64)}
    mul r11
{$ifend}
{$if defined(TARGET_x86)}
    mov esi, Source
    add esi, eax
{$elseif defined(TARGET_x64)}
    mov rsi, Source
    add rsi, rax
{$ifend}

    mov eax, Y4x4Required
    shl eax, 4
{$if defined(TARGET_x86)}
    mov edi, Destination
    add edi, eax
{$elseif defined(TARGET_x64)}
    mov rdi, Destination
    add rdi, rax
{$ifend}

    mov edx,0

    @@extra_y:
    cmp edx, remainderY // while edx < remainderY
    jae @@extra_y_exit

      mov ecx, 0
      mov eax, 0

      @@extra_y_x:
{$if defined(TARGET_x86)}
      cmp ecx, Width // while ecx < Width
{$elseif defined(TARGET_x64)}
      cmp ecx, W // while ecx < Width
{$ifend}
      jae @@extra_y_x_exit

{$if defined(TARGET_x86)}
        mov ebx, dword ptr [esi+4*ecx]
        mov dword ptr [edi+eax], ebx
{$elseif defined(TARGET_x64)}
        mov ebx, dword ptr [rsi+4*rcx]
        mov dword ptr [rdi+rax], ebx
{$ifend}
{$if defined(TARGET_x86)}
        add eax, destRowSize
{$elseif defined(TARGET_x64)}
        add rax, r10
{$ifend}
        inc ecx

        jmp @@extra_y_x
      @@extra_y_x_exit:

{$if defined(TARGET_x86)}
      add esi, sourceRowSize
      add edi, 4
{$elseif defined(TARGET_x64)}
      add rsi, r11
      add rdi, 4
{$ifend}
      inc edx

      jmp @@extra_y
    @@extra_y_exit:

  @@no_extra_y:

{$if defined(TARGET_x64)}
{$IFDEF FPC}
  pop RDI
  pop RSI
  pop RBX
{$ENDIF}
{$elseif defined(TARGET_x86)}
  pop ebx
  pop esi
  pop edi
{$ifend}

{$if defined(TARGET_x64) and defined(FPC)}end['XMM4', 'XMM5', 'XMM6'];{$ifend}
end;
{$ifend}


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
{$ifndef FPC}
  BlockBuffer := TInterlocked.Exchange(CacheObliviousTransposeBuffer, nil);
{$else}
  BlockBuffer := InterlockedExchange(CacheObliviousTransposeBuffer, nil);
{$endif}
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

  TransposeRegistry.RegisterBinding(@@_Transpose32, '_Transpose32');

  TransposeRegistry[@@_Transpose32].Add(@ReferenceTranspose32,          [isReference]).Name := 'ReferenceTranspose32';
  TransposeRegistry[@@_Transpose32].Add(@CacheObliviousTranspose32,     [isPascal],     -16).Name := 'CacheObliviousTranspose32';
  TransposeRegistry[@@_Transpose32].Add(@CacheObliviousTransposeEx32,   [isPascal],     -32).Name := 'CacheObliviousTransposeEx32';

{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
  // TODO : SuperDuperTranspose32 has been profiled to be on average 3 times slower
  // than CacheObliviousTransposeEx32 in the Gaussian blur benchmark.
  // It's still vastly faster in most real-world situations so we give it priority.
  TransposeRegistry[@@_Transpose32].Add(@SuperDuperTranspose32,         [isSSE2],       -48).Name := 'SuperDuperTranspose32';
{$ifend}

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
