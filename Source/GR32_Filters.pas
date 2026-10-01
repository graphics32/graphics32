unit GR32_Filters;

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
 * The Initial Developer of the Original Code is Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  Classes, SysUtils, GR32;

{ Basic processing }
type
  TLUT8 = array [Byte] of Byte;
  TLogicalOperator = (loXOR, loAND, loOR);


//------------------------------------------------------------------------------
//
//      CopyComponents
//
//------------------------------------------------------------------------------
(*
** CopyComponents copies specified color components from a source bitmap to a
** destination bitmap.
**
** - If the source and destination bitmaps are the same, or if no components
**   are specified, the function exits without performing any operation.
**
** - In the first overload, the destination bitmap is automatically resized to
**   match the dimensions of the source bitmap, if their size differ.
**
** - In the second overload, the specified Rect of the source bitmap is copied
**   to the destination at the given coordinates (DstX, DstY). The destination
**   is not resized.
**
** - The Components parameter determines which of the ARGB channels are
**   transferred from source to destination.
*)
procedure CopyComponents(Dst, Src: TCustomBitmap32; Components: TColor32Components);overload;
procedure CopyComponents(Dst: TCustomBitmap32; DstX, DstY: Integer; Src: TCustomBitmap32; SrcRect: TRect; Components: TColor32Components); overload;


//------------------------------------------------------------------------------
//
//      AlphaToGrayscale
//
//------------------------------------------------------------------------------
(*
** AlphaToGrayscale converts a bitmap to grayscale by copying the alpha
** component of each pixel to its red, green, and blue components.
**
** - In the first overload, the operation is performed in-place.
**
** - In the second overload, the alpha channel of the source bitmap is
**   copied to the RGB channels of the destination bitmap. The destination
**   is resized if necessary.
**
** - The alpha channel of the destination bitmap remains intact provided that
**   the bitmap isn't resized.
**
** TODO: AlphaToGrayscale should sey the Destination alpha to 255 if the bitmap is resized.
*)
procedure AlphaToGrayscale(ABitmap: TCustomBitmap32); overload;
procedure AlphaToGrayscale(Dst, Src: TCustomBitmap32); overload;


//------------------------------------------------------------------------------
//
//      ColorToGrayscale
//
//------------------------------------------------------------------------------
(*
** ColorToGrayscale converts a color bitmap to grayscale based on the luminance
** (intensity) of each pixel.
**
** - In the first overload, the operation is performed in-place.
**
** - In the second overload, the source bitmap is converted and stored in the
**   destination bitmap. The destination is resized if necessary.
**
** - If PreserveAlpha is True, the source alpha values are copied to the
**   destination. If False (the default), the alpha channel is set to opaque ($FF).
*)
procedure ColorToGrayscale(ABitmap: TCustomBitmap32; PreserveAlpha: Boolean = False); overload;
procedure ColorToGrayscale(Dst, Src: TCustomBitmap32; PreserveAlpha: Boolean = False); overload;


//------------------------------------------------------------------------------
//
//      IntensityToAlpha
//
//------------------------------------------------------------------------------
(*
** IntensityToAlpha maps the weighted intensity (luminance) of each source
** pixel to the alpha channel of the corresponding destination pixel.
**
** - The destination bitmap is resized to match the source dimensions if
**   necessary.

** - The RGB channels of the destination bitmap remains intact provided that
**   the bitmap isn't resized.
*)
procedure IntensityToAlpha(Dst, Src: TCustomBitmap32);


//------------------------------------------------------------------------------
//
//      Invert
//
//------------------------------------------------------------------------------
(*
** Invert inverts (negates) the specified color components of a bitmap.
**
** - In the first overload, the operation is performed in-place.
**
** - In the second overload, the inverted result of the source bitmap is
**   stored in the destination bitmap. The destination is resized if necessary.
**
** - The Components parameter (defaulting to all components, including the alpha)
**   determines which channels are inverted.
*)
procedure Invert(ABitmap: TCustomBitmap32; Components: TColor32Components = [ccAlpha, ccRed, ccGreen, ccBlue]); overload;
procedure Invert(Dst, Src: TCustomBitmap32; Components: TColor32Components = [ccAlpha, ccRed, ccGreen, ccBlue]); overload;


//------------------------------------------------------------------------------
//
//      InvertRGB
//
//------------------------------------------------------------------------------
(*
** InvertRGB is a convenience function that inverts only the red, green, and
** blue color channels, leaving the alpha channel untouched.
*)
procedure InvertRGB(ABitmap: TCustomBitmap32); overload;
procedure InvertRGB(Dst, Src: TCustomBitmap32); overload;


//------------------------------------------------------------------------------
//
//      ApplyLUT
//
//------------------------------------------------------------------------------
(*
** ApplyLUT transforms the color channels of a bitmap using a Look-Up Table (LUT).
**
** - In the first overload, the operation is performed in-place.
**
** - In the second overload, the source bitmap is transformed and stored in
**   the destination bitmap. The destination is resized if necessary.
**
** - If PreserveAlpha is True, the alpha component of each pixel is copied
**   unchanged from source to destination. If False (the default), the alpha
**   channel is set to opaque ($FF).
*)
procedure ApplyLUT(ABitmap: TCustomBitmap32; const LUT: TLUT8; PreserveAlpha: Boolean = False); overload;
procedure ApplyLUT(Dst, Src: TCustomBitmap32; const LUT: TLUT8; PreserveAlpha: Boolean = False); overload;


//------------------------------------------------------------------------------
//
//      ChromaKey
//
//------------------------------------------------------------------------------
(*
** ChromaKey makes pixels that match a specific color transparent.
**
** - The comparison between pixel colors and KeyColor ignores the alpha channel,
**   matching only the RGB components.
**
** - Matching pixels have their alpha component set to 0 (transparent).
*)
procedure ChromaKey(ABitmap: TCustomBitmap32; KeyColor: TColor32);


//------------------------------------------------------------------------------
//
//      CreateBitmask
//
//------------------------------------------------------------------------------
(*
** CreateBitmask generates a TColor32 bitmask based on the specified color
** components. For example for use with ApplyBitmask.
**
** - The resulting mask has the bits corresponding to the selected channels
**   set to 1, and all other bits set to 0.
*)
function CreateBitmask(Components: TColor32Components): TColor32;


//------------------------------------------------------------------------------
//
//      ApplyBitmask
//
//------------------------------------------------------------------------------
(*
** ApplyBitmask performs a bitwise logical operation between bitmap pixels
** and a mask.
**
** - In the first overload, the logical operation is performed between the
**   source bitmap and the destination bitmap, using the specified source
**   rectangle and destination coordinates.
**
** - In the second overload, the operation is performed in-place on the
**   specified rectangle of the bitmap.
**
** - The LogicalOperator determines whether an AND, OR, or XOR operation
**   is applied using the Bitmask.
*)
procedure ApplyBitmask(Dst: TCustomBitmap32; DstX, DstY: Integer; Src: TCustomBitmap32; SrcRect: TRect; Bitmask: TColor32; LogicalOperator: TLogicalOperator); overload;
procedure ApplyBitmask(ABitmap: TCustomBitmap32; ARect: TRect; Bitmask: TColor32; LogicalOperator: TLogicalOperator); overload;


//------------------------------------------------------------------------------
//
//      CheckParams
//
//------------------------------------------------------------------------------
(*
** CheckParams is used by the various filter functions to validate the bitmap parameters
** and, optionally, to ensure that the destination bitmap has the required dimensions.
**
** - If either Dst or Src is nil, then an exception is raised.
**
** - If ResizeDst=True (the default), and the dimensions of Src and Dst differ, then Dst
**   is resized to the size of Src.
**
** - If ClearDst=True (the default), and Dst must be resized, then Dst is cleared as part
**   of the resize operation.
**
** The function returns True if the Dst bitmap was resized, False otherwise.
*)
function CheckParams(Dst, Src: TCustomBitmap32; ResizeDst: Boolean = True; ClearDst: boolean = True): boolean;


//------------------------------------------------------------------------------
//
//      ScaleAlpha bindings
//
//------------------------------------------------------------------------------
type
  TScaleAlphaLine = procedure(Source, Dest: PColor32; Count: Integer);
  TScaleAlphaMems = procedure(Color: PColor32; Count: Integer; Scale: Single);
  TApplyAlphaMask = procedure(Source, Dest: PColor32; Count: Integer);
  TLuminance = function(Color: TColor32): Byte;

(*
** ScaleAlphaLine scales the alpha component of Dest pixels by Source.A / 255.
**
** For each pixel:
**
**   If Source.A <> 255, Dest.A is scaled by (Dest.A * Source.A + 127) div 255.
**   RGB channels of Dest are preserved without modification.
*)
var
  ScaleAlphaLine: TScaleAlphaLine;

(*
** ScaleAlphaMems scales the alpha component of an array of pixels by a Single
** float Scale factor.
**
** A 256-byte lookup table is built once before the loop to map input alpha
** values to scaled output alpha values: LUT[A] = Clamp(Round(A * Scale)).
** RGB channels of Color are preserved without modification.
*)
var
  ScaleAlphaMems: TScaleAlphaMems;

(*
** ApplyAlphaMask_Pas computes the W3C Rec. 601 luminance of Source pixels
** (with source alpha applied) and scales Dest.A by Gray / 255.
** RGB channels of Dest are preserved without modification.
*)
var
  ApplyAlphaMask: TApplyAlphaMask;


//------------------------------------------------------------------------------
//
//      Luminance bindings
//
//------------------------------------------------------------------------------
(*
** Luminance601 calculates the W3C Rec. 601 luminance for a TColor32 pixel
** with the pixel's alpha component applied.
**
** Formula:
**
**   Y_raw = 0.299 * R + 0.587 * G + 0.114 * B
**
**   Using Q16 fixed-point math:
**
**     Y_raw = (19595 * R + 38470 * G + 7471 * B + 32768) shr 16
**     Result = (Y_raw * A + 127) div 255
**
** Luminance709 calculates the W3C Rec. 709 luminance for a TColor32 pixel
** with the pixel's alpha component applied.
**
** Formula:
**   Y_raw = 0.2126 * R + 0.7152 * G + 0.0722 * B
**
**   Using Q16 fixed-point math:
**
**     Y_raw = (13933 * R + 46871 * G + 4732 * B + 32768) shr 16
**     Result = (Y_raw * A + 127) div 255
*)
var
  Luminance601: TLuminance;
  Luminance709: TLuminance;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Types,
  GR32_Bindings,
  GR32_Blend,
  GR32_Lowlevel,
  GR32.Types.SIMD;

const
  sEmptyBitmap = 'The bitmap is nil';
  sEmptySource = 'The source is nil';
  sEmptyDestination = 'Destination is nil';

const // TODO : This belongs in GR32
{$IFNDEF RGBA_FORMAT}
  ARGB_MASK_A = $FF000000;
  ARGB_MASK_R = $00FF0000;
  ARGB_MASK_G = $0000FF00;
  ARGB_MASK_B = $000000FF;
  ARGB_SHIFT_A = 24;
  ARGB_SHIFT_R = 16;
  ARGB_SHIFT_G = 8;
  ARGB_SHIFT_B = 0;
{$ELSE}
  ARGB_MASK_A = $FF000000;
  ARGB_MASK_R = $000000FF;
  ARGB_MASK_G = $0000FF00;
  ARGB_MASK_B = $00FF0000;
  ARGB_SHIFT_A = 24;
  ARGB_SHIFT_R = 0;
  ARGB_SHIFT_G = 8;
  ARGB_SHIFT_B = 16;
{$ENDIF}

{ Function Prototypes }
type
  TLogicalMaskLine  = procedure(Dst: PColor32; Mask: TColor32; Count: Integer); //Inplace
  TLogicalMaskLineEx  = procedure(Src, Dst: PColor32; Count: Integer; Mask: TColor32); //"Src To Dst"

{$HINTS OFF}
{ masked logical operation functions }
var
  LogicalMaskLineXor: TLogicalMaskLine;
  LogicalMaskLineOr: TLogicalMaskLine;
  LogicalMaskLineAnd: TLogicalMaskLine;

  LogicalMaskLineXorEx: TLogicalMaskLineEx;
  LogicalMaskLineOrEx: TLogicalMaskLineEx;
  LogicalMaskLineAndEx: TLogicalMaskLineEx;
{$HINTS ON}

{ Access to masked logical operation functions corresponding to a logical operation mode }
const
  LOGICAL_MASK_LINE: array[TLogicalOperator] of ^TLogicalMaskLine = (
    (@@LogicalMaskLineXor),
    (@@LogicalMaskLineAnd),
    (@@LogicalMaskLineOr)
  );

  LOGICAL_MASK_LINE_EX: array[TLogicalOperator] of ^TLogicalMaskLineEx = (
    (@@LogicalMaskLineXorEx),
    (@@LogicalMaskLineAndEx),
    (@@LogicalMaskLineOrEx)
  );


//------------------------------------------------------------------------------
//
//      CheckParams
//
//------------------------------------------------------------------------------
function CheckParams(Dst, Src: TCustomBitmap32; ResizeDst: Boolean; ClearDst: boolean): boolean;
begin
  if (Src = nil) then
    raise Exception.Create(sEmptySource);

  if (Dst = nil) then
    raise Exception.Create(sEmptyDestination);

  if ResizeDst and (Src <> Dst) and ((Src.Width <> Dst.Width) or (Src.Height <> Dst.Height)) then
  begin
    Dst.SetSize(Src.Width, Src.Height, ClearDst);
    Result := True;
  end else
    Result := False;
end;


//------------------------------------------------------------------------------
//
//      CopyComponents
//
//------------------------------------------------------------------------------
procedure CopyComponents(Dst, Src: TCustomBitmap32; Components: TColor32Components);
begin
  if (Components = []) or (Src = Dst) then
    Exit;
  CheckParams(Dst, Src);
  CopyComponents(Dst, 0, 0, Src, Src.BoundsRect, Components);
end;

//------------------------------------------------------------------------------

procedure CopyComponents(Dst: TCustomBitmap32; DstX, DstY: Integer; Src: TCustomBitmap32;
  SrcRect: TRect; Components: TColor32Components);
var
  I, J, Count, ComponentCount, XOffset: Integer;
  OriginalDstX, OriginalDstY: Integer;
  Mask, NotMask: TColor32;
  SrcRow, DstRow: PColor32Array;
  PBDst, PBSrc: PByteArray;
  DstRect: TRect;
begin
  if (Components = []) then
    Exit;
  CheckParams(Dst, Src, False);

  ComponentCount := 0;
  XOffset := 0;
  Mask := 0;

  if ccAlpha in Components then
  begin
    Inc(ComponentCount);
    Inc(Mask, ARGB_MASK_A);
    XOffset := Ord(ccAlpha);
  end;

  if ccRed in Components then
  begin
    Inc(ComponentCount);
    Inc(Mask, ARGB_MASK_R);
    XOffset := Ord(ccRed);
  end;

  if ccGreen in Components then
  begin
    Inc(ComponentCount);
    Inc(Mask, ARGB_MASK_G);
    XOffset := Ord(ccGreen);
  end;

  if ccBlue in Components then
  begin
    Inc(ComponentCount);
    Inc(Mask, ARGB_MASK_B);
    XOffset := Ord(ccBlue);
  end;

  GR32.IntersectRect(SrcRect, SrcRect, Src.BoundsRect);
  if (SrcRect.Right <= SrcRect.Left) or (SrcRect.Bottom <= SrcRect.Top) then
    exit;

  OriginalDstX := DstX;
  OriginalDstY := DstY;

  DstRect.Left := DstX;
  DstRect.Top := DstY;
  DstRect.Right := DstX + (SrcRect.Right - SrcRect.Left);
  DstRect.Bottom := DstY + (SrcRect.Bottom - SrcRect.Top);

  GR32.IntersectRect(DstRect, DstRect, Dst.BoundsRect);
  GR32.IntersectRect(DstRect, DstRect, Dst.ClipRect);

  if (DstRect.Right <= DstRect.Left) or (DstRect.Bottom <= DstRect.Top) then
    exit;

  Inc(SrcRect.Left, DstRect.Left - OriginalDstX);
  Inc(SrcRect.Top, DstRect.Top - OriginalDstY);

  if not Dst.MeasuringMode then
  begin
    Dst.BeginUpdate;
    try
      Count := DstRect.Right - DstRect.Left;
      SrcRow := Pointer(Src.PixelPtr[SrcRect.Left, SrcRect.Top]);
      DstRow := Pointer(Dst.PixelPtr[DstRect.Left, DstRect.Top]);

      if Count > 16 then
      begin

        case ComponentCount of
          1: // Byte ptr approach
            begin
              PBSrc := Pointer(SrcRow);
              Inc(PBSrc, XOffset); // shift the pointer to the given component of the first pixel
              PBDst := Pointer(DstRow);
              Inc(PBDst, XOffset);

              Count := Count * 4 - 64;
              Inc(PBSrc, Count);
              Inc(PBDst, Count);

              for I := 0 to DstRect.Bottom - DstRect.Top - 1 do
              begin
                //16x unrolled loop
                J := - Count;
                repeat
                  PBDst[J] := PBSrc[J];
                  PBDst[J +  4] := PBSrc[J +  4];
                  PBDst[J +  8] := PBSrc[J +  8];
                  PBDst[J + 12] := PBSrc[J + 12];
                  PBDst[J + 16] := PBSrc[J + 16];
                  PBDst[J + 20] := PBSrc[J + 20];
                  PBDst[J + 24] := PBSrc[J + 24];
                  PBDst[J + 28] := PBSrc[J + 28];
                  PBDst[J + 32] := PBSrc[J + 32];
                  PBDst[J + 36] := PBSrc[J + 36];
                  PBDst[J + 40] := PBSrc[J + 40];
                  PBDst[J + 44] := PBSrc[J + 44];
                  PBDst[J + 48] := PBSrc[J + 48];
                  PBDst[J + 52] := PBSrc[J + 52];
                  PBDst[J + 56] := PBSrc[J + 56];
                  PBDst[J + 60] := PBSrc[J + 60];
                  Inc(J, 64)
                until J > 0;

                //The rest
                Dec(J, 64);
                while J < 0 do
                begin
                  PBDst[J + 64] := PBSrc[J + 64];
                  Inc(J, 4);
                end;
                Inc(PBSrc, Src.Width * 4);
                Inc(PBDst, Dst.Width * 4);
              end;
            end;

          2, 3: // Masked approach
            begin
              NotMask := not Mask;
              Count := Count - 8;
              Inc(DstRow, Count);
              Inc(SrcRow, Count);
              for I := 0 to DstRect.Bottom - DstRect.Top - 1 do
              begin
                //8x unrolled loop
                J := - Count;
                repeat
                  DstRow[J] := (DstRow[J] and NotMask) or (SrcRow[J] and Mask);
                  DstRow[J + 1] := (DstRow[J + 1] and NotMask) or (SrcRow[J + 1] and Mask);
                  DstRow[J + 2] := (DstRow[J + 2] and NotMask) or (SrcRow[J + 2] and Mask);
                  DstRow[J + 3] := (DstRow[J + 3] and NotMask) or (SrcRow[J + 3] and Mask);
                  DstRow[J + 4] := (DstRow[J + 4] and NotMask) or (SrcRow[J + 4] and Mask);
                  DstRow[J + 5] := (DstRow[J + 5] and NotMask) or (SrcRow[J + 5] and Mask);
                  DstRow[J + 6] := (DstRow[J + 6] and NotMask) or (SrcRow[J + 6] and Mask);
                  DstRow[J + 7] := (DstRow[J + 7] and NotMask) or (SrcRow[J + 7] and Mask);

                  Inc(J, 8);
                until J > 0;

                //The rest
                Dec(J, 8);
                while J < 0 do
                begin
                  DstRow[J + 8] := (DstRow[J + 8] and NotMask) or (SrcRow[J + 8] and Mask);
                  Inc(J);
                end;
                Inc(SrcRow, Src.Width);
                Inc(DstRow, Dst.Width);
              end;
            end;

          4: // Full copy approach approach, use MoveLongword
            for I := 0 to DstRect.Bottom - DstRect.Top - 1 do
            begin
              MoveLongword(SrcRow^, DstRow^, Count);
              Inc(SrcRow, Src.Width);
              Inc(DstRow, Dst.Width);
            end;
        end;

      end else
      begin

        NotMask := not Mask;
        for I := 0 to DstRect.Bottom - DstRect.Top - 1 do
        begin
          for J := 0 to Count - 1 do
            DstRow[J] := (DstRow[J] and NotMask) or (SrcRow[J] and Mask);
          Inc(SrcRow, Src.Width);
          Inc(DstRow, Dst.Width);
        end;

      end;
    finally
      Dst.EndUpdate;
    end;
  end;
  Dst.Changed(DstRect);
end;

//------------------------------------------------------------------------------
//
//      AlphaToGrayscale
//
//------------------------------------------------------------------------------
procedure AlphaToGrayscale(ABitmap: TCustomBitmap32);
var
  I: Integer;
  S : PColor32EntryArray;
  Alpha: Byte;
begin
  S := PColor32EntryArray(ABitmap.Bits);
  for I := 0 to ABitmap.Height * ABitmap.Width -1 do
  begin
    Alpha := S[I].A;
    with S[I] do
    begin
      R := Alpha;
      G := Alpha;
      B := Alpha;
    end;
  end;
  ABitmap.Changed;
end;

//------------------------------------------------------------------------------

procedure AlphaToGrayscale(Dst, Src: TCustomBitmap32);
var
  I: Integer;
  D, S : PColor32EntryArray;
  Alpha: Byte;
begin
  CheckParams(Dst, Src);
  S := PColor32EntryArray(Src.Bits);
  D := PColor32EntryArray(Dst.Bits);
  for I := 0 to Src.Height * Src.Width -1 do
  begin
    Alpha := S[I].A;
    with D[I] do
    begin
      R := Alpha;
      G := Alpha;
      B := Alpha;
    end;
  end;
  Dst.Changed;
end;


//------------------------------------------------------------------------------
//
//      IntensityToAlpha
//
//------------------------------------------------------------------------------
procedure IntensityToAlpha(Dst, Src: TCustomBitmap32);
var
  I: Integer;
  D, S : PColor32EntryArray;
begin
  CheckParams(Dst, Src);
  S := PColor32EntryArray(Src.Bits);
  D := PColor32EntryArray(Dst.Bits);
  for I := 0 to Src.Width * Src.Height - 1 do
    D[I].A := (S[I].R * 61 + S[I].G * 174 + S[I].B * 21) shr 8;
  Dst.Changed;
end;


//------------------------------------------------------------------------------
//
//      Invert
//
//------------------------------------------------------------------------------
procedure Invert(ABitmap: TCustomBitmap32; Components: TColor32Components);
begin
  Invert(ABitmap, ABitmap, Components);
end;

//------------------------------------------------------------------------------

procedure Invert(Dst, Src: TCustomBitmap32; Components: TColor32Components);
var
  Mask: TColor32;
begin
  if (Components = []) then
    Exit;
  Mask := CreateBitmask(Components);
  if (Src = Dst) then
  begin
    //Inplace
    CheckParams(Dst, Src, False);
    ApplyBitmask(Src, Src.BoundsRect, Mask, loXOR);
  end else
  begin
    //Src -> Dst
    CheckParams(Dst, Src);
    ApplyBitmask(Dst, 0, 0, Src, Src.BoundsRect, Mask, loXOR);
  end;
end;

//------------------------------------------------------------------------------
//
//      InvertRGB
//
//------------------------------------------------------------------------------
procedure InvertRGB(ABitmap: TCustomBitmap32);
begin
  Invert(ABitmap, [ccRed, ccGreen, ccBlue]);
end;

//------------------------------------------------------------------------------

procedure InvertRGB(Dst, Src: TCustomBitmap32);
begin
  Invert(Dst, Src, [ccRed, ccGreen, ccBlue]);
end;


//------------------------------------------------------------------------------
//
//      ColorToGrayscale
//
//------------------------------------------------------------------------------
procedure ColorToGrayscale(ABitmap: TCustomBitmap32; PreserveAlpha: Boolean);
begin
  ColorToGrayscale(ABitmap, ABitmap, PreserveAlpha);
end;

//------------------------------------------------------------------------------

procedure ColorToGrayscale(Dst, Src: TCustomBitmap32; PreserveAlpha: Boolean);
var
  I: Integer;
  D, S: PColor32;
begin
  CheckParams(Dst, Src, True, False);
  D := PColor32(Dst.Bits);
  S := PColor32(Src.Bits);

  if PreserveAlpha then
  begin
    for I := 0 to Src.Width * Src.Height - 1 do
    begin
      D^ := Gray32(Intensity(S^), AlphaComponent(S^));
      Inc(S); Inc(D);
    end;
  end else
  begin
    for I := 0 to Src.Width * Src.Height - 1 do
    begin
      D^ := Gray32(Intensity(S^));
      Inc(S); Inc(D);
    end;
  end;

  Dst.Changed;
end;


//------------------------------------------------------------------------------
//
//      ApplyLUT
//
//------------------------------------------------------------------------------
procedure ApplyLUT(ABitmap: TCustomBitmap32; const LUT: TLUT8; PreserveAlpha: Boolean);
begin
  ApplyLUT(ABitmap, ABitmap, LUT, PreserveAlpha);
end;

//------------------------------------------------------------------------------

procedure ApplyLUT(Dst, Src: TCustomBitmap32; const LUT: TLUT8; PreserveAlpha: Boolean);
var
  I: Integer;
  D, S: PColor32Entry;
begin
  CheckParams(Dst, Src, True, False);
  D := PColor32Entry(Dst.Bits);
  S := PColor32Entry(Src.Bits);

  if PreserveAlpha then
  begin
    for I := 0 to Src.Width * Src.Height - 1 do
    begin
      D.ARGB := (S.ARGB and ARGB_MASK_A) or (LUT[S.B] shl ARGB_SHIFT_B) or (LUT[S.G] shl ARGB_SHIFT_G) or (LUT[S.R] shl ARGB_SHIFT_R);
      Inc(S);
      Inc(D);
    end;
  end else
  begin
    for I := 0 to Src.Width * Src.Height - 1 do
    begin
      D.ARGB := ARGB_MASK_A or (LUT[S.B] shl ARGB_SHIFT_B) or (LUT[S.G] shl ARGB_SHIFT_G) or (LUT[S.R] shl ARGB_SHIFT_R);
      Inc(S);
      Inc(D);
    end;
  end;

  Dst.Changed;
end;


//------------------------------------------------------------------------------
//
//      ChromaKey
//
//------------------------------------------------------------------------------
procedure ChromaKey(ABitmap: TCustomBitmap32; KeyColor: TColor32);
var
  P: PColor32;
  C: TColor32;
  I: Integer;
begin
  KeyColor := KeyColor and $00FFFFFF;

  P := PColor32(ABitmap.Bits);
  for I := 0 to ABitmap.Width * ABitmap.Height - 1 do
  begin
    C := P^ and $00FFFFFF;
    if (C = KeyColor) then
      P^ := C;
    Inc(P);
  end;

  ABitmap.Changed;
end;


//------------------------------------------------------------------------------
//
//      CreateBitmask
//
//------------------------------------------------------------------------------
function CreateBitmask(Components: TColor32Components): TColor32;
begin
  Result := 0;
  if (ccAlpha in Components) then
    Inc(Result, ARGB_MASK_A);
  if (ccRed in Components) then
    Inc(Result, ARGB_MASK_R);
  if (ccGreen in Components) then
    Inc(Result, ARGB_MASK_G);
  if (ccBlue in Components) then
    Inc(Result, ARGB_MASK_B);
end;


//------------------------------------------------------------------------------
//
//      ApplyBitmask
//
//------------------------------------------------------------------------------
procedure ApplyBitmask(Dst: TCustomBitmap32; DstX, DstY: Integer; Src: TCustomBitmap32;
  SrcRect: TRect; Bitmask: TColor32; LogicalOperator: TLogicalOperator);
var
  I, Count, OriginalDstX, OriginalDstY: Integer;
  DstRect: TRect;
  MaskProc : TLogicalMaskLineEx;
begin
  CheckParams(Dst, Src, False);

  MaskProc := LOGICAL_MASK_LINE_EX[LogicalOperator]^;

  if (not Assigned(MaskProc)) then
    exit;

  GR32.IntersectRect(SrcRect, SrcRect, Src.BoundsRect);
  if (SrcRect.Right <= SrcRect.Left) or (SrcRect.Bottom <= SrcRect.Top) then
    exit;

  OriginalDstX := DstX;
  OriginalDstY := DstY;

  DstRect.Left := DstX;
  DstRect.Top := DstY;
  DstRect.Right := DstX + (SrcRect.Right - SrcRect.Left);
  DstRect.Bottom := DstY + (SrcRect.Bottom - SrcRect.Top);

  GR32.IntersectRect(DstRect, DstRect, Dst.BoundsRect);
  GR32.IntersectRect(DstRect, DstRect, Dst.ClipRect);

  if (DstRect.Right <= DstRect.Left) or (DstRect.Bottom <= DstRect.Top) then
    exit;

  Inc(SrcRect.Left, DstRect.Left - OriginalDstX);
  Inc(SrcRect.Top, DstRect.Top - OriginalDstY);

  if not Dst.MeasuringMode then
  begin
    Dst.BeginUpdate;
    try
      if (DstRect.Bottom - DstRect.Top) > 0 then
      begin
        Count := DstRect.Right - DstRect.Left;

        if Count > 0 then
          for I := 0 to DstRect.Bottom - DstRect.Top - 1 do
            MaskProc(Src.PixelPtr[SrcRect.Left, SrcRect.Top + I], Dst.PixelPtr[DstRect.Left, DstRect.Top + I], Count, Bitmask);
      end;
    finally
      Dst.EndUpdate;
    end;
  end;

  Dst.Changed(DstRect);
end;

//------------------------------------------------------------------------------

procedure ApplyBitmask(ABitmap: TCustomBitmap32; ARect: TRect; Bitmask: TColor32; LogicalOperator: TLogicalOperator);
var
  I, Count: Integer;
  MaskProc : TLogicalMaskLine;
begin
  if not Assigned(ABitmap) then
    raise Exception.Create(sEmptyBitmap);

  MaskProc := LOGICAL_MASK_LINE[LogicalOperator]^;

  if (not Assigned(MaskProc)) then
    exit;

  GR32.IntersectRect(ARect, ARect, ABitmap.BoundsRect);
  GR32.IntersectRect(ARect, ARect, ABitmap.ClipRect);
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then
    exit;

  if not ABitmap.MeasuringMode then
  begin
    ABitmap.BeginUpdate;
    try
      if (ARect.Bottom - ARect.Top) > 0 then
      begin
        Count := ARect.Right - ARect.Left;

        if Count > 0 then
        begin
          if Count = ABitmap.Width then
            MaskProc(ABitmap.PixelPtr[ARect.Left, ARect.Top], Bitmask, Count * (ARect.Bottom - ARect.Top))
          else
            for I := ARect.Top to ARect.Bottom - 1 do
              MaskProc(ABitmap.PixelPtr[ARect.Left, I], Bitmask, Count);
        end;
      end;
    finally
      ABitmap.EndUpdate;
    end;
  end;

  ABitmap.Changed(ARect);
end;


//------------------------------------------------------------------------------
//
//      In-place logical mask functions
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Pascal versions
//------------------------------------------------------------------------------
procedure XorLine_Pas(Dst: PColor32; Mask: TColor32; Count: Integer);
var
  DstRow: PColor32Array absolute Dst;
begin
  Inc(Dst, Count);
  Count := - Count;
  repeat
    DstRow[Count] := DstRow[Count] xor Mask;
    Inc(Count);
  until Count = 0;
end;

//------------------------------------------------------------------------------

procedure OrLine_Pas(Dst: PColor32; Mask: TColor32; Count: Integer);
var
  DstRow: PColor32Array absolute Dst;
begin
  Inc(Dst, Count);
  Count := - Count;
  repeat
    DstRow[Count] := DstRow[Count] or Mask;
    Inc(Count);
  until Count = 0;
end;

//------------------------------------------------------------------------------

procedure AndLine_Pas(Dst: PColor32; Mask: TColor32; Count: Integer);
var
  DstRow: PColor32Array absolute Dst;
begin
  Inc(Dst, Count);
  Count := - Count;
  repeat
    DstRow[Count] := DstRow[Count] and Mask;
    Inc(Count);
  until Count = 0;
end;

{$IFNDEF PUREPASCAL}

//------------------------------------------------------------------------------
// ASM versions
//------------------------------------------------------------------------------
procedure XorLine_ASM(Dst: PColor32; Mask: TColor32; Count: Integer); {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
// No speedup achieveable using MMX
asm
{$if defined(TARGET_x86)}
        TEST    ECX, ECX
        JZ      @Exit

        PUSH    EBX
        MOV     EBX, ECX
        SHR     ECX, 4
        SHL     ECX, 4
        JZ      @PrepSingleLoop
        LEA     EAX, [EAX + ECX * 4]
        SHL     ECX, 2
        NEG     ECX

@ChunkLoop:
        //16x unrolled loop
        XOR     [EAX + ECX], EDX
        XOR     [EAX + ECX + 4], EDX
        XOR     [EAX + ECX + 8], EDX
        XOR     [EAX + ECX + 12], EDX

        XOR     [EAX + ECX + 16], EDX
        XOR     [EAX + ECX + 20], EDX
        XOR     [EAX + ECX + 24], EDX
        XOR     [EAX + ECX + 28], EDX

        XOR     [EAX + ECX + 32], EDX
        XOR     [EAX + ECX + 36], EDX
        XOR     [EAX + ECX + 40], EDX
        XOR     [EAX + ECX + 44], EDX

        XOR     [EAX + ECX + 48], EDX
        XOR     [EAX + ECX + 52], EDX
        XOR     [EAX + ECX + 56], EDX
        XOR     [EAX + ECX + 60], EDX

        ADD     ECX, 16 * 4
        JNZ     @ChunkLoop

@PrepSingleLoop:
        MOV     ECX, EBX
        SHR     EBX, 4
        SHL     EBX, 4
        SUB     ECX, EBX
        JZ      @PopExit

        LEA     EAX, [EAX + ECX * 4]
        NEG     ECX

@SingleLoop:
        XOR     [EAX + ECX * 4], EDX
        INC     ECX
        JNZ     @SingleLoop

@PopExit:
        POP     EBX

@Exit:
{$elseif defined(TARGET_x64)}
        TEST    R8D, R8D
        JZ      @Exit

        MOV     EAX, R8D
        SHR     R8D, 4
        JZ      @PrepSingleLoop
        SHL     R8D, 4

        MOV     R9, R8
        SHL     R9, 2
        ADD     RCX, R9
        NEG     R9

@ChunkLoop:
        //16x unrolled loop
        XOR     [RCX + R9], EDX
        XOR     [RCX + R9 + 4], EDX
        XOR     [RCX + R9 + 8], EDX
        XOR     [RCX + R9 + 12], EDX

        XOR     [RCX + R9 + 16], EDX
        XOR     [RCX + R9 + 20], EDX
        XOR     [RCX + R9 + 24], EDX
        XOR     [RCX + R9 + 28], EDX

        XOR     [RCX + R9 + 32], EDX
        XOR     [RCX + R9 + 36], EDX
        XOR     [RCX + R9 + 40], EDX
        XOR     [RCX + R9 + 44], EDX

        XOR     [RCX + R9 + 48], EDX
        XOR     [RCX + R9 + 52], EDX
        XOR     [RCX + R9 + 56], EDX
        XOR     [RCX + R9 + 60], EDX

        ADD     R9, 16 * 4
        JNZ     @ChunkLoop

@PrepSingleLoop:
        AND     EAX, $0F
        JZ      @Exit
        MOV     R8D, EAX

@SingleLoop:
        XOR     [RCX], EDX
        ADD     RCX, 4
        DEC     R8D
        JNZ     @SingleLoop

@Exit:
{$ifend}
end;

//------------------------------------------------------------------------------

procedure OrLine_ASM(Dst: PColor32; Mask: TColor32; Count: Integer); {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
// No speedup achieveable using MMX
asm
{$if defined(TARGET_x86)}
        TEST    ECX, ECX
        JZ      @Exit

        PUSH    EBX
        MOV     EBX, ECX
        SHR     ECX, 4
        SHL     ECX, 4
        JZ      @PrepSingleLoop
        LEA     EAX, [EAX + ECX * 4]
        SHL     ECX, 2
        NEG     ECX

@ChunkLoop:
        //16x unrolled loop
        OR      [EAX + ECX], EDX
        OR      [EAX + ECX + 4], EDX
        OR      [EAX + ECX + 8], EDX
        OR      [EAX + ECX + 12], EDX

        OR      [EAX + ECX + 16], EDX
        OR      [EAX + ECX + 20], EDX
        OR      [EAX + ECX + 24], EDX
        OR      [EAX + ECX + 28], EDX

        OR      [EAX + ECX + 32], EDX
        OR      [EAX + ECX + 36], EDX
        OR      [EAX + ECX + 40], EDX
        OR      [EAX + ECX + 44], EDX

        OR      [EAX + ECX + 48], EDX
        OR      [EAX + ECX + 52], EDX
        OR      [EAX + ECX + 56], EDX
        OR      [EAX + ECX + 60], EDX

        ADD     ECX, 16 * 4
        JNZ     @ChunkLoop

@PrepSingleLoop:
        MOV     ECX, EBX
        SHR     EBX, 4
        SHL     EBX, 4
        SUB     ECX, EBX
        JZ      @PopExit

        LEA     EAX, [EAX + ECX * 4]
        NEG     ECX

@SingleLoop:
        OR      [EAX + ECX * 4], EDX
        INC     ECX
        JNZ     @SingleLoop

@PopExit:
        POP     EBX

@Exit:
{$elseif defined(TARGET_x64)}
        TEST    R8D, R8D
        JZ      @Exit

        MOV     EAX, R8D
        SHR     R8D, 4
        JZ      @PrepSingleLoop
        SHL     R8D, 4

        MOV     R9, R8
        SHL     R9, 2
        ADD     RCX, R9
        NEG     R9

@ChunkLoop:
        //16x unrolled loop
        OR      [RCX + R9], EDX
        OR      [RCX + R9 + 4], EDX
        OR      [RCX + R9 + 8], EDX
        OR      [RCX + R9 + 12], EDX

        OR      [RCX + R9 + 16], EDX
        OR      [RCX + R9 + 20], EDX
        OR      [RCX + R9 + 24], EDX
        OR      [RCX + R9 + 28], EDX

        OR      [RCX + R9 + 32], EDX
        OR      [RCX + R9 + 36], EDX
        OR      [RCX + R9 + 40], EDX
        OR      [RCX + R9 + 44], EDX

        OR      [RCX + R9 + 48], EDX
        OR      [RCX + R9 + 52], EDX
        OR      [RCX + R9 + 56], EDX
        OR      [RCX + R9 + 60], EDX

        ADD     R9, 16 * 4
        JNZ     @ChunkLoop

@PrepSingleLoop:
        AND     EAX, $0F
        JZ      @Exit
        MOV     R8D, EAX

@SingleLoop:
        OR      [RCX], EDX
        ADD     RCX, 4
        DEC     R8D
        JNZ     @SingleLoop

@Exit:
{$ifend}
end;

//------------------------------------------------------------------------------

procedure AndLine_ASM(Dst: PColor32; Mask: TColor32; Count: Integer); {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
// No speedup achieveable using MMX
asm
{$if defined(TARGET_x86)}
        TEST    ECX, ECX
        JZ      @Exit

        PUSH    EBX
        MOV     EBX, ECX
        SHR     ECX, 4
        SHL     ECX, 4
        JZ      @PrepSingleLoop
        LEA     EAX, [EAX + ECX * 4]
        SHL     ECX, 2
        NEG     ECX

@ChunkLoop:
        //16x unrolled loop
        AND     [EAX + ECX], EDX
        AND     [EAX + ECX + 4], EDX
        AND     [EAX + ECX + 8], EDX
        AND     [EAX + ECX + 12], EDX

        AND     [EAX + ECX + 16], EDX
        AND     [EAX + ECX + 20], EDX
        AND     [EAX + ECX + 24], EDX
        AND     [EAX + ECX + 28], EDX

        AND     [EAX + ECX + 32], EDX
        AND     [EAX + ECX + 36], EDX
        AND     [EAX + ECX + 40], EDX
        AND     [EAX + ECX + 44], EDX

        AND     [EAX + ECX + 48], EDX
        AND     [EAX + ECX + 52], EDX
        AND     [EAX + ECX + 56], EDX
        AND     [EAX + ECX + 60], EDX

        ADD     ECX, 16 * 4
        JNZ     @ChunkLoop

@PrepSingleLoop:
        MOV     ECX, EBX
        SHR     EBX, 4
        SHL     EBX, 4
        SUB     ECX, EBX
        JZ      @PopExit

        LEA     EAX, [EAX + ECX * 4]
        NEG     ECX

@SingleLoop:
        AND     [EAX + ECX * 4], EDX
        INC     ECX
        JNZ     @SingleLoop

@PopExit:
        POP     EBX

@Exit:
{$elseif defined(TARGET_x64)}
        TEST    R8D, R8D
        JZ      @Exit

        MOV     EAX, R8D
        SHR     R8D, 4
        JZ      @PrepSingleLoop
        SHL     R8D, 4

        MOV     R9, R8
        SHL     R9, 2
        ADD     RCX, R9
        NEG     R9

@ChunkLoop:
        //16x unrolled loop
        AND     [RCX + R9], EDX
        AND     [RCX + R9 + 4], EDX
        AND     [RCX + R9 + 8], EDX
        AND     [RCX + R9 + 12], EDX

        AND     [RCX + R9 + 16], EDX
        AND     [RCX + R9 + 20], EDX
        AND     [RCX + R9 + 24], EDX
        AND     [RCX + R9 + 28], EDX

        AND     [RCX + R9 + 32], EDX
        AND     [RCX + R9 + 36], EDX
        AND     [RCX + R9 + 40], EDX
        AND     [RCX + R9 + 44], EDX

        AND     [RCX + R9 + 48], EDX
        AND     [RCX + R9 + 52], EDX
        AND     [RCX + R9 + 56], EDX
        AND     [RCX + R9 + 60], EDX

        ADD     R9, 16 * 4
        JNZ     @ChunkLoop

@PrepSingleLoop:
        AND     EAX, $0F
        JZ      @Exit
        MOV     R8D, EAX

@SingleLoop:
        AND     [RCX], EDX
        ADD     RCX, 4
        DEC     R8D
        JNZ     @SingleLoop

@Exit:
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
//
//      Extended logical mask functions Src -> Dst
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Pascal versions
//------------------------------------------------------------------------------
procedure XorLineEx_Pas(Src, Dst: PColor32; Count: Integer; Mask: TColor32);
var
  SrcRow: PColor32Array absolute Src;
  DstRow: PColor32Array absolute Dst;
begin
  Inc(Dst, Count);
  Inc(Src, Count);
  Count := - Count;
  repeat
    DstRow[Count] := SrcRow[Count] xor Mask;
    Inc(Count);
  until Count = 0;
end;

//------------------------------------------------------------------------------

procedure OrLineEx_Pas(Src, Dst: PColor32; Count: Integer; Mask: TColor32);
var
  SrcRow: PColor32Array absolute Src;
  DstRow: PColor32Array absolute Dst;
begin
  Inc(Dst, Count);
  Inc(Src, Count);
  Count := - Count;
  repeat
    DstRow[Count] := SrcRow[Count] or Mask;
    Inc(Count);
  until Count = 0;
end;

//------------------------------------------------------------------------------

procedure AndLineEx_Pas(Src, Dst: PColor32; Count: Integer; Mask: TColor32);
var
  SrcRow: PColor32Array absolute Src;
  DstRow: PColor32Array absolute Dst;
begin
  Inc(Dst, Count);
  Inc(Src, Count);
  Count := - Count;
  repeat
    DstRow[Count] := SrcRow[Count] and Mask;
    Inc(Count);
  until Count = 0;
end;

{$IFNDEF PUREPASCAL}

//------------------------------------------------------------------------------
// ASM versions
//------------------------------------------------------------------------------
procedure XorLineEx_ASM(Src, Dst: PColor32; Count: Integer; Mask: TColor32); {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
asm
{$if defined(TARGET_x86)}
        PUSH    EBX
        PUSH    EDI

        LEA     EAX, [EAX + ECX * 4]
        LEA     EDX, [EDX + ECX * 4]
        NEG     ECX
        JZ      @Exit

        MOV     EDI, Mask

@Loop:
        MOV     EBX, [EAX + ECX * 4]
        XOR     EBX, EDI
        MOV     [EDX + ECX * 4], EBX
        INC     ECX
        JNZ     @Loop

@Exit:
        POP     EDI
        POP     EBX
{$elseif defined(TARGET_x64)}
        TEST    R8D, R8D
        JZ      @Exit

        MOV     R10, R8
        SHL     R10, 2
        ADD     RCX, R10
        ADD     RDX, R10
        NEG     R10

@Loop:
        MOV     EAX, [RCX + R10]
        XOR     EAX, R9D
        MOV     [RDX + R10], EAX
        ADD     R10, 4
        JNZ     @Loop

@Exit:
{$ifend}
end;

//------------------------------------------------------------------------------

procedure OrLineEx_ASM(Src, Dst: PColor32; Count: Integer; Mask: TColor32); {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
asm
{$if defined(TARGET_x86)}
        PUSH    EBX
        PUSH    EDI

        LEA     EAX, [EAX + ECX * 4]
        LEA     EDX, [EDX + ECX * 4]
        NEG     ECX
        JZ      @Exit

        MOV     EDI, Mask

@Loop:
        MOV     EBX, [EAX + ECX * 4]
        OR      EBX, EDI
        MOV     [EDX + ECX * 4], EBX
        INC     ECX
        JNZ     @Loop

@Exit:

        POP     EDI
        POP     EBX
{$elseif defined(TARGET_x64)}
        TEST    R8D, R8D
        JZ      @Exit

        MOV     R10, R8
        SHL     R10, 2
        ADD     RCX, R10
        ADD     RDX, R10
        NEG     R10

@Loop:
        MOV     EAX, [RCX + R10]
        OR      EAX, R9D
        MOV     [RDX + R10], EAX
        ADD     R10, 4
        JNZ     @Loop

@Exit:
{$ifend}
end;

//------------------------------------------------------------------------------

procedure AndLineEx_ASM(Src, Dst: PColor32; Count: Integer; Mask: TColor32); {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
asm
{$if defined(TARGET_x86)}
        PUSH    EBX
        PUSH    EDI

        LEA     EAX, [EAX + ECX * 4]
        LEA     EDX, [EDX + ECX * 4]
        NEG     ECX
        JZ      @Exit

        MOV     EDI, Mask

@Loop:
        MOV     EBX, [EAX + ECX * 4]
        AND     EBX, EDI
        MOV     [EDX + ECX * 4], EBX
        INC     ECX
        JNZ     @Loop

@Exit:

        POP     EDI
        POP     EBX
{$elseif defined(TARGET_x64)}
        TEST    R8D, R8D
        JZ      @Exit

        MOV     R10, R8
        SHL     R10, 2
        ADD     RCX, R10
        ADD     RDX, R10
        NEG     R10

@Loop:
        MOV     EAX, [RCX + R10]
        AND     EAX, R9D
        MOV     [RDX + R10], EAX
        ADD     R10, 4
        JNZ     @Loop

@Exit:
{$ifend}
end;

//------------------------------------------------------------------------------
// MMX versions
//------------------------------------------------------------------------------
{$IFNDEF OMIT_MMX}
procedure XorLineEx_MMX(Src, Dst: PColor32; Count: Integer; Mask: TColor32);
//MMX version
var
  QMask: Int64;

asm
        PUSH      EBX
        PUSH      EDI

        TEST      ECX, ECX
        JZ        @Exit

        MOV       EBX, ECX
        SHR       ECX, 4
        SHL       ECX, 4
        JZ        @PrepSingleLoop

        SAR       ECX, 1
        LEA       EAX, [EAX + ECX * 8]
        LEA       EDX, [EDX + ECX * 8]
        NEG       ECX

        MOVD      MM7, MASK
        PUNPCKLDQ MM7, MM7
        MOVQ      QMask, MM7
        EMMS

@Loop:
        MOVQ      MM0, [EAX + ECX * 8]
        MOVQ      MM1, [EAX + ECX * 8 + 8]
        MOVQ      MM2, [EAX + ECX * 8 + 16]
        MOVQ      MM3, [EAX + ECX * 8 + 24]
        MOVQ      MM4, [EAX + ECX * 8 + 32]
        MOVQ      MM5, [EAX + ECX * 8 + 40]
        MOVQ      MM6, [EAX + ECX * 8 + 48]
        MOVQ      MM7, [EAX + ECX * 8 + 56]

        PXOR      MM0, QMask
        PXOR      MM1, QMask
        PXOR      MM2, QMask
        PXOR      MM3, QMask
        PXOR      MM4, QMask
        PXOR      MM5, QMask
        PXOR      MM6, QMask
        PXOR      MM7, QMask

        MOVQ      [EDX + ECX * 8], MM0
        MOVQ      [EDX + ECX * 8 + 8], MM1
        MOVQ      [EDX + ECX * 8 + 16], MM2
        MOVQ      [EDX + ECX * 8 + 24], MM3
        MOVQ      [EDX + ECX * 8 + 32], MM4
        MOVQ      [EDX + ECX * 8 + 40], MM5
        MOVQ      [EDX + ECX * 8 + 48], MM6
        MOVQ      [EDX + ECX * 8 + 56], MM7

        ADD       ECX, 8
        JS        @Loop

        EMMS

@PrepSingleLoop:
        MOV       ECX, EBX
        SHR       EBX, 4
        SHL       EBX, 4
        SUB       ECX, EBX
        JZ        @Exit

        LEA       EAX, [EAX + ECX * 4]
        LEA       EDX, [EDX + ECX * 4]
        NEG       ECX

        MOV       EDI, Mask

@SingleLoop:
        MOV       EBX, [EAX + ECX * 4]
        XOR       EBX, EDI
        MOV       [EDX + ECX * 4], EBX
        INC       ECX
        JNZ       @SingleLoop

@Exit:
        POP       EDI
        POP       EBX
end;

//------------------------------------------------------------------------------

procedure OrLineEx_MMX(Src, Dst: PColor32; Count: Integer; Mask: TColor32);
//MMX version
var
  QMask: Int64;

asm
        PUSH      EBX
        PUSH      EDI

        TEST      ECX, ECX
        JZ        @Exit

        MOV       EBX, ECX
        SHR       ECX, 4
        SHL       ECX, 4
        JZ        @PrepSingleLoop

        SAR       ECX, 1
        LEA       EAX, [EAX + ECX * 8]
        LEA       EDX, [EDX + ECX * 8]
        NEG       ECX

        MOVD      MM7, MASK
        PUNPCKLDQ MM7, MM7
        MOVQ      QMask, MM7
        EMMS

@Loop:
        MOVQ      MM0, [EAX + ECX * 8]
        MOVQ      MM1, [EAX + ECX * 8 + 8]
        MOVQ      MM2, [EAX + ECX * 8 + 16]
        MOVQ      MM3, [EAX + ECX * 8 + 24]
        MOVQ      MM4, [EAX + ECX * 8 + 32]
        MOVQ      MM5, [EAX + ECX * 8 + 40]
        MOVQ      MM6, [EAX + ECX * 8 + 48]
        MOVQ      MM7, [EAX + ECX * 8 + 56]

        POR       MM0, QMask
        POR       MM1, QMask
        POR       MM2, QMask
        POR       MM3, QMask
        POR       MM4, QMask
        POR       MM5, QMask
        POR       MM6, QMask
        POR       MM7, QMask

        MOVQ      [EDX + ECX * 8], MM0
        MOVQ      [EDX + ECX * 8 + 8], MM1
        MOVQ      [EDX + ECX * 8 + 16], MM2
        MOVQ      [EDX + ECX * 8 + 24], MM3
        MOVQ      [EDX + ECX * 8 + 32], MM4
        MOVQ      [EDX + ECX * 8 + 40], MM5
        MOVQ      [EDX + ECX * 8 + 48], MM6
        MOVQ      [EDX + ECX * 8 + 56], MM7

        ADD       ECX, 8
        JS        @Loop

        EMMS

@PrepSingleLoop:
        MOV       ECX, EBX
        SHR       EBX, 4
        SHL       EBX, 4
        SUB       ECX, EBX
        JZ        @Exit

        LEA       EAX, [EAX + ECX * 4]
        LEA       EDX, [EDX + ECX * 4]
        NEG       ECX

        MOV       EDI, Mask

@SingleLoop:
        MOV       EBX, [EAX + ECX * 4]
        OR        EBX, EDI
        MOV       [EDX + ECX * 4], EBX
        INC       ECX
        JNZ       @SingleLoop

@Exit:
        POP       EDI
        POP       EBX
end;

//------------------------------------------------------------------------------

procedure AndLineEx_MMX(Src, Dst: PColor32; Count: Integer; Mask: TColor32);
//MMX version
var
  QMask: Int64;
asm
        PUSH      EBX
        PUSH      EDI

        TEST      ECX, ECX
        JZ        @Exit

        MOV       EBX, ECX
        SHR       ECX, 4
        SHL       ECX, 4
        JZ        @PrepSingleLoop

        SAR       ECX, 1
        LEA       EAX, [EAX + ECX * 8]
        LEA       EDX, [EDX + ECX * 8]
        NEG       ECX

        MOVD      MM7, MASK
        PUNPCKLDQ MM7, MM7
        MOVQ      QMask, MM7
        EMMS

@Loop:
        MOVQ      MM0, [EAX + ECX * 8]
        MOVQ      MM1, [EAX + ECX * 8 + 8]
        MOVQ      MM2, [EAX + ECX * 8 + 16]
        MOVQ      MM3, [EAX + ECX * 8 + 24]
        MOVQ      MM4, [EAX + ECX * 8 + 32]
        MOVQ      MM5, [EAX + ECX * 8 + 40]
        MOVQ      MM6, [EAX + ECX * 8 + 48]
        MOVQ      MM7, [EAX + ECX * 8 + 56]

        PAND      MM0, QMask
        PAND      MM1, QMask
        PAND      MM2, QMask
        PAND      MM3, QMask
        PAND      MM4, QMask
        PAND      MM5, QMask
        PAND      MM6, QMask
        PAND      MM7, QMask

        MOVQ      [EDX + ECX * 8], MM0
        MOVQ      [EDX + ECX * 8 + 8], MM1
        MOVQ      [EDX + ECX * 8 + 16], MM2
        MOVQ      [EDX + ECX * 8 + 24], MM3
        MOVQ      [EDX + ECX * 8 + 32], MM4
        MOVQ      [EDX + ECX * 8 + 40], MM5
        MOVQ      [EDX + ECX * 8 + 48], MM6
        MOVQ      [EDX + ECX * 8 + 56], MM7

        ADD       ECX, 8
        JS        @Loop

        EMMS

@PrepSingleLoop:
        MOV       ECX, EBX
        SHR       EBX, 4
        SHL       EBX, 4
        SUB       ECX, EBX
        JZ        @Exit

        LEA       EAX, [EAX + ECX * 4]
        LEA       EDX, [EDX + ECX * 4]
        NEG       ECX

        MOV       EDI, Mask

@SingleLoop:
        MOV       EBX, [EAX + ECX * 4]
        AND       EBX, EDI
        MOV       [EDX + ECX * 4], EBX
        INC       ECX
        JNZ       @SingleLoop

@Exit:
        POP       EDI
        POP       EBX
end;


//------------------------------------------------------------------------------
// Extended MMX versions
//------------------------------------------------------------------------------

procedure XorLineEx_EMMX(Src, Dst: PColor32; Count: Integer; Mask: TColor32);
//EMMX version
var
  QMask: Int64;

asm
        PUSH      EBX
        PUSH      EDI

        TEST      ECX, ECX
        JZ        @Exit

        MOV       EBX, ECX
        SHR       ECX, 4
        SHL       ECX, 4
        JZ        @PrepSingleLoop

        SAR       ECX, 1
        LEA       EAX, [EAX + ECX * 8]
        LEA       EDX, [EDX + ECX * 8]
        NEG       ECX

        MOVD      MM7, MASK
        PUNPCKLDQ MM7, MM7
        MOVQ      QMask, MM7
        EMMS

@Loop:
        MOVQ      MM0, [EAX + ECX * 8]
        MOVQ      MM1, [EAX + ECX * 8 + 8]
        MOVQ      MM2, [EAX + ECX * 8 + 16]
        MOVQ      MM3, [EAX + ECX * 8 + 24]
        MOVQ      MM4, [EAX + ECX * 8 + 32]
        MOVQ      MM5, [EAX + ECX * 8 + 40]
        MOVQ      MM6, [EAX + ECX * 8 + 48]
        MOVQ      MM7, [EAX + ECX * 8 + 56]

        PXOR      MM0, QMask
        PXOR      MM1, QMask
        PXOR      MM2, QMask
        PXOR      MM3, QMask
        PXOR      MM4, QMask
        PXOR      MM5, QMask
        PXOR      MM6, QMask
        PXOR      MM7, QMask

        MOVNTQ    [EDX + ECX * 8], MM0
        MOVNTQ    [EDX + ECX * 8 + 8], MM1
        MOVNTQ    [EDX + ECX * 8 + 16], MM2
        MOVNTQ    [EDX + ECX * 8 + 24], MM3
        MOVNTQ    [EDX + ECX * 8 + 32], MM4
        MOVNTQ    [EDX + ECX * 8 + 40], MM5
        MOVNTQ    [EDX + ECX * 8 + 48], MM6
        MOVNTQ    [EDX + ECX * 8 + 56], MM7

        ADD       ECX, 8
        JS        @Loop

        EMMS

@PrepSingleLoop:
        MOV       ECX, EBX
        SHR       EBX, 4
        SHL       EBX, 4
        SUB       ECX, EBX
        JZ        @Exit

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

//------------------------------------------------------------------------------

procedure OrLineEx_EMMX(Src, Dst: PColor32; Count: Integer; Mask: TColor32);
//EMMX version
var
  QMask: Int64;

asm
        PUSH      EBX
        PUSH      EDI

        TEST      ECX, ECX
        JZ        @Exit

        MOV       EBX, ECX
        SHR       ECX, 4
        SHL       ECX, 4
        JZ        @PrepSingleLoop

        SAR       ECX, 1
        LEA       EAX, [EAX + ECX * 8]
        LEA       EDX, [EDX + ECX * 8]
        NEG       ECX

        MOVD      MM7, MASK
        PUNPCKLDQ MM7, MM7
        MOVQ      QMask, MM7
        EMMS

@Loop:
        MOVQ      MM0, [EAX + ECX * 8]
        MOVQ      MM1, [EAX + ECX * 8 + 8]
        MOVQ      MM2, [EAX + ECX * 8 + 16]
        MOVQ      MM3, [EAX + ECX * 8 + 24]
        MOVQ      MM4, [EAX + ECX * 8 + 32]
        MOVQ      MM5, [EAX + ECX * 8 + 40]
        MOVQ      MM6, [EAX + ECX * 8 + 48]
        MOVQ      MM7, [EAX + ECX * 8 + 56]

        POR       MM0, QMask
        POR       MM1, QMask
        POR       MM2, QMask
        POR       MM3, QMask
        POR       MM4, QMask
        POR       MM5, QMask
        POR       MM6, QMask
        POR       MM7, QMask

        MOVNTQ    [EDX + ECX * 8], MM0
        MOVNTQ    [EDX + ECX * 8 + 8], MM1
        MOVNTQ    [EDX + ECX * 8 + 16], MM2
        MOVNTQ    [EDX + ECX * 8 + 24], MM3
        MOVNTQ    [EDX + ECX * 8 + 32], MM4
        MOVNTQ    [EDX + ECX * 8 + 40], MM5
        MOVNTQ    [EDX + ECX * 8 + 48], MM6
        MOVNTQ    [EDX + ECX * 8 + 56], MM7

        ADD       ECX, 8
        JS        @Loop

        EMMS

@PrepSingleLoop:
        MOV       ECX, EBX
        SHR       EBX, 4
        SHL       EBX, 4
        SUB       ECX, EBX
        JZ        @Exit

        LEA       EAX, [EAX + ECX * 4]
        LEA       EDX, [EDX + ECX * 4]
        NEG       ECX

        MOV       EDI, Mask

@SingleLoop:
        MOV       EBX, [EAX + ECX * 4]
        OR        EBX, EDI
        MOV       [EDX + ECX * 4], EBX
        INC       ECX
        JNZ       @SingleLoop

@Exit:
        POP       EDI
        POP       EBX
end;

//------------------------------------------------------------------------------

procedure AndLineEx_EMMX(Src, Dst: PColor32; Count: Integer; Mask: TColor32);
//EMMX version
var
  QMask: Int64;

asm
        PUSH      EBX
        PUSH      EDI

        TEST      ECX, ECX
        JZ        @Exit

        MOV       EBX, ECX
        SHR       ECX, 4
        SHL       ECX, 4
        JZ        @PrepSingleLoop

        SAR       ECX, 1
        LEA       EAX, [EAX + ECX * 8]
        LEA       EDX, [EDX + ECX * 8]
        NEG       ECX

        MOVD      MM7, MASK
        PUNPCKLDQ MM7, MM7
        MOVQ      QMask, MM7
        EMMS

@Loop:
        MOVQ      MM0, [EAX + ECX * 8]
        MOVQ      MM1, [EAX + ECX * 8 + 8]
        MOVQ      MM2, [EAX + ECX * 8 + 16]
        MOVQ      MM3, [EAX + ECX * 8 + 24]
        MOVQ      MM4, [EAX + ECX * 8 + 32]
        MOVQ      MM5, [EAX + ECX * 8 + 40]
        MOVQ      MM6, [EAX + ECX * 8 + 48]
        MOVQ      MM7, [EAX + ECX * 8 + 56]

        PAND      MM0, QMask
        PAND      MM1, QMask
        PAND      MM2, QMask
        PAND      MM3, QMask
        PAND      MM4, QMask
        PAND      MM5, QMask
        PAND      MM6, QMask
        PAND      MM7, QMask

        MOVNTQ    [EDX + ECX * 8], MM0
        MOVNTQ    [EDX + ECX * 8 + 8], MM1
        MOVNTQ    [EDX + ECX * 8 + 16], MM2
        MOVNTQ    [EDX + ECX * 8 + 24], MM3
        MOVNTQ    [EDX + ECX * 8 + 32], MM4
        MOVNTQ    [EDX + ECX * 8 + 40], MM5
        MOVNTQ    [EDX + ECX * 8 + 48], MM6
        MOVNTQ    [EDX + ECX * 8 + 56], MM7

        ADD       ECX, 8
        JS        @Loop

        EMMS

@PrepSingleLoop:
        MOV       ECX, EBX
        SHR       EBX, 4
        SHL       EBX, 4
        SUB       ECX, EBX
        JZ        @Exit

        LEA       EAX, [EAX + ECX * 4]
        LEA       EDX, [EDX + ECX * 4]
        NEG       ECX

        MOV       EDI, Mask

@SingleLoop:
        MOV       EBX, [EAX + ECX * 4]
        AND       EBX, EDI
        MOV       [EDX + ECX * 4], EBX
        INC       ECX
        JNZ       @SingleLoop

@Exit:
        POP       EDI
        POP       EBX
end;

{$ENDIF}
{$ENDIF}


//------------------------------------------------------------------------------
//
//      Luminance
//
//------------------------------------------------------------------------------
// Pascal versions
//------------------------------------------------------------------------------
function Luminance601_Pas(Color: TColor32): Byte;
var
  Entry: TColor32Entry;
  Y: Cardinal;
begin
  Entry.ARGB := Color;
  Y := (19595 * Entry.R + 38470 * Entry.G + 7471 * Entry.B + 32768) shr 16;
  Result := MulDiv255Table[Y, Entry.A]; // (Y * Entry.A + 127) div 255;
end;

//------------------------------------------------------------------------------

function Luminance709_Pas(Color: TColor32): Byte;
var
  Entry: TColor32Entry;
  Y: Cardinal;
begin
  Entry.ARGB := Color;
  Y := (13933 * Entry.R + 46871 * Entry.G + 4732 * Entry.B + 32768) shr 16;
  Result := MulDiv255Table[Y, Entry.A]; // (Y * Entry.A + 127) div 255;
end;


//------------------------------------------------------------------------------
// SIMD versions
//------------------------------------------------------------------------------

{$if (not defined(PURE_PASCAL)) and (not defined(OMIT_SSE2))}

const
  c_div255_bias: array[0..3] of Cardinal = (127, 127, 127, 127);
  c_div255_mult: array[0..3] of Cardinal = (257, 257, 257, 257);
  c_rgb_mask32: array[0..3] of Cardinal = ($00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF);
{$IFNDEF RGBA_FORMAT}
  c_w601_bytes: array[0..15] of Byte = (29, 150, 77, 0, 29, 150, 77, 0, 29, 150, 77, 0, 29, 150, 77, 0);
  c_w709_bytes: array[0..15] of Byte = (19, 183, 54, 0, 19, 183, 54, 0, 19, 183, 54, 0, 19, 183, 54, 0);
{$ELSE}
  c_w601_bytes: array[0..15] of Byte = (77, 150, 29, 0, 77, 150, 29, 0, 77, 150, 29, 0, 77, 150, 29, 0);
  c_w709_bytes: array[0..15] of Byte = (54, 183, 19, 0, 54, 183, 19, 0, 54, 183, 19, 0, 54, 183, 19, 0);
{$ENDIF}
  c_ones16: array[0..7] of Word = (1, 1, 1, 1, 1, 1, 1, 1);
  c_256_single: Single = 256.0;
{$IFNDEF RGBA_FORMAT}
  c_w601_words: array[0..7] of Word = (29, 150, 77, 0, 29, 150, 77, 0);
  c_w709_words: array[0..7] of Word = (19, 183, 54, 0, 19, 183, 54, 0);
  c_w601_dwords: array[0..3] of Cardinal = (7471, 38470, 19595, 0);
  c_w709_dwords: array[0..3] of Cardinal = (4732, 46871, 13933, 0);
{$ELSE}
  c_w601_words: array[0..7] of Word = (77, 150, 29, 0, 77, 150, 29, 0);
  c_w709_words: array[0..7] of Word = (54, 183, 19, 0, 54, 183, 19, 0);
  c_w601_dwords: array[0..3] of Cardinal = (19595, 38470, 7471, 0);
  c_w709_dwords: array[0..3] of Cardinal = (13933, 46871, 4732, 0);
{$ENDIF}

//------------------------------------------------------------------------------

(*
** Luminance601_SSE2 calculates Rec. 601 luminance with source alpha applied
** using SSE2 vector unpacking and PMADDWD arithmetic.
*)
function Luminance601_SSE2(Color: TColor32): Byte; {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOV       EDX, EAX
        SHR       EDX, 24
        MOVD      XMM0, EAX
        PXOR      XMM1, XMM1
        PUNPCKLBW XMM0, XMM1
        MOV       EAX, Offset c_w601_words
        MOVDQU    XMM1, [EAX]
        PMADDWD   XMM0, XMM1
        PSHUFD    XMM1, XMM0, $01
        PADDD     XMM0, XMM1
        PSRLD     XMM0, 8

        MOVD      EAX, XMM0
        IMUL      EAX, EDX
        ADD       EAX, 127
        IMUL      EAX, EAX, 257
        SHR       EAX, 16
{$elseif defined(TARGET_x64)}
        PUSH      RBX
        MOVD      XMM0, ECX
        PXOR      XMM1, XMM1
        PUNPCKLBW XMM0, XMM1
        LEA       RAX, [RIP + c_w601_words]
        MOVDQU    XMM1, [RAX]
        PMADDWD   XMM0, XMM1
        PSHUFD    XMM1, XMM0, $01
        PADDD     XMM0, XMM1
        PSRLD     XMM0, 8

        MOV       EAX, ECX
        SHR       EAX, 24
        POP       RBX
        MOVD      EDX, XMM0
        IMUL      EAX, EDX
        ADD       EAX, 127
        IMUL      EAX, EAX, 257
        SHR       EAX, 16
{$ifend}
end;

//------------------------------------------------------------------------------

(*
** Luminance601_SSE41 calculates Rec. 601 luminance using SSE4.1 PMOVZXBD
** zero-extension and 32-bit PMULLD vector multiplication.
*)
function Luminance601_SSE41(Color: TColor32): Byte; {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOV       EDX, EAX
        SHR       EDX, 24
        MOVD      XMM0, EAX
        PMOVZXBD  XMM0, XMM0
        MOV       EAX, Offset c_w601_dwords
        MOVDQU    XMM1, [EAX]
        PMULLD    XMM0, XMM1
        PSHUFD    XMM1, XMM0, $01
        PSHUFD    XMM2, XMM0, $02
        PADDD     XMM0, XMM1
        PADDD     XMM0, XMM2
        MOV       EAX, 32768
        MOVD      XMM1, EAX
        PADDD     XMM0, XMM1
        PSRLD     XMM0, 16

        MOVD      EAX, XMM0
        IMUL      EAX, EDX
        ADD       EAX, 127
        IMUL      EAX, EAX, 257
        SHR       EAX, 16
{$elseif defined(TARGET_x64)}
        MOVD      XMM0, ECX
        PMOVZXBD  XMM0, XMM0
        LEA       RAX, [RIP + c_w601_dwords]
        MOVDQU    XMM1, [RAX]
        PMULLD    XMM0, XMM1
        PSHUFD    XMM1, XMM0, $01
        PSHUFD    XMM2, XMM0, $02
        PADDD     XMM0, XMM1
        PADDD     XMM0, XMM2
        MOV       EAX, 32768
        MOVD      XMM1, EAX
        PADDD     XMM0, XMM1
        PSRLD     XMM0, 16

        MOV       EAX, ECX
        SHR       EAX, 24
        MOVD      EDX, XMM0
        IMUL      EAX, EDX
        ADD       EAX, 127
        IMUL      EAX, EAX, 257
        SHR       EAX, 16
{$ifend}
end;

//------------------------------------------------------------------------------

(*
** Luminance709_SSE2 calculates Rec. 709 luminance with source alpha applied
** using SSE2 vector unpacking and PMADDWD arithmetic.
*)
function Luminance709_SSE2(Color: TColor32): Byte; {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOV       EDX, EAX
        SHR       EDX, 24
        MOVD      XMM0, EAX
        PXOR      XMM1, XMM1
        PUNPCKLBW XMM0, XMM1
        MOV       EAX, Offset c_w709_words
        MOVDQU    XMM1, [EAX]
        PMADDWD   XMM0, XMM1
        PSHUFD    XMM1, XMM0, $01
        PADDD     XMM0, XMM1
        PSRLD     XMM0, 8

        MOVD      EAX, XMM0
        IMUL      EAX, EDX
        ADD       EAX, 127
        IMUL      EAX, EAX, 257
        SHR       EAX, 16
{$elseif defined(TARGET_x64)}
        PUSH      RBX
        MOVD      XMM0, ECX
        PXOR      XMM1, XMM1
        PUNPCKLBW XMM0, XMM1
        LEA       RAX, [RIP + c_w709_words]
        MOVDQU    XMM1, [RAX]
        PMADDWD   XMM0, XMM1
        PSHUFD    XMM1, XMM0, $01
        PADDD     XMM0, XMM1
        PSRLD     XMM0, 8

        MOV       EAX, ECX
        SHR       EAX, 24
        POP       RBX
        MOVD      EDX, XMM0
        IMUL      EAX, EDX
        ADD       EAX, 127
        IMUL      EAX, EAX, 257
        SHR       EAX, 16
{$ifend}
end;

//------------------------------------------------------------------------------

(*
** Luminance709_SSE41 calculates Rec. 709 luminance using SSE4.1 PMOVZXBD zero-extension
** and 32-bit PMULLD vector multiplication.
*)
function Luminance709_SSE41(Color: TColor32): Byte; {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOV       EDX, EAX
        SHR       EDX, 24
        MOVD      XMM0, EAX
        PMOVZXBD  XMM0, XMM0
        MOV       EAX, Offset c_w709_dwords
        MOVDQU    XMM1, [EAX]
        PMULLD    XMM0, XMM1
        PSHUFD    XMM1, XMM0, $01
        PSHUFD    XMM2, XMM0, $02
        PADDD     XMM0, XMM1
        PADDD     XMM0, XMM2
        MOV       EAX, 32768
        MOVD      XMM1, EAX
        PADDD     XMM0, XMM1
        PSRLD     XMM0, 16

        MOVD      EAX, XMM0
        IMUL      EAX, EDX
        ADD       EAX, 127
        IMUL      EAX, EAX, 257
        SHR       EAX, 16
{$elseif defined(TARGET_x64)}
        MOVD      XMM0, ECX
        PMOVZXBD  XMM0, XMM0
        LEA       RAX, [RIP + c_w709_dwords]
        MOVDQU    XMM1, [RAX]
        PMULLD    XMM0, XMM1
        PSHUFD    XMM1, XMM0, $01
        PSHUFD    XMM2, XMM0, $02
        PADDD     XMM0, XMM1
        PADDD     XMM0, XMM2
        MOV       EAX, 32768
        MOVD      XMM1, EAX
        PADDD     XMM0, XMM1
        PSRLD     XMM0, 16

        MOV       EAX, ECX
        SHR       EAX, 24
        MOVD      EDX, XMM0
        IMUL      EAX, EDX
        ADD       EAX, 127
        IMUL      EAX, EAX, 257
        SHR       EAX, 16
{$ifend}
end;

{$ifend (not defined(PURE_PASCAL)) and (not defined(OMIT_SSE2))}


//------------------------------------------------------------------------------
//
//      ScaleAlphaLine
//
//------------------------------------------------------------------------------
// Pascal version
//------------------------------------------------------------------------------
procedure ScaleAlphaLine_Pas(Source, Dest: PColor32Entry; Count: Integer);
var
  Sa: Byte;
begin
  while (Count > 0) do
  begin
    Sa := Source.A;
    if (Sa <> 255) then
    begin
      if (Sa = 0) then
        Dest.A := 0
      else
        Dest.A := MulDiv255Table[Dest.A, Sa]; // = (Dest.A * Sa + 127) div 255
    end;
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  end;
end;

//------------------------------------------------------------------------------
// SIMD versions
//------------------------------------------------------------------------------
{$if (not defined(PURE_PASCAL)) and (not defined(OMIT_SSE2))}

(*
** ScaleAlphaLine_SSE2 scales destination alpha values in blocks of 4 pixels
** using SSE2 word multiplication.
*)
procedure ScaleAlphaLine_SSE2(Source, Dest: PColor32Entry; Count: Integer); {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
asm
{$if defined(TARGET_x86)}
        TEST      ECX, ECX
        JLE       @Exit
        PUSH      EBX
        PUSH      ESI

        PXOR      XMM7, XMM7
        MOV       ESI, Offset SSE_00FF00FF_ALIGNED
        MOVDQU    XMM6, [ESI]
        MOV       ESI, Offset SSE_00800080_ALIGNED
        MOVDQU    XMM4, [ESI]
        MOV       ESI, Offset SSE_01010101_ALIGNED
        MOVDQU    XMM0, [ESI]

        SUB       ECX, 4
        JL        @Tail

@Loop4:
        MOVDQA    XMM5, XMM6
        MOVZX     EBX, Byte Ptr [EAX + 3]
        PINSRW    XMM5, EBX, 3
        MOVZX     EBX, Byte Ptr [EAX + 7]
        PINSRW    XMM5, EBX, 7

        MOVDQU    XMM1, [EDX]
        MOVDQA    XMM2, XMM1
        PUNPCKLBW XMM2, XMM7
        PMULLW    XMM2, XMM5
        PADDW     XMM2, XMM4
        PMULHUW   XMM2, XMM0

        MOVDQA    XMM5, XMM6
        MOVZX     EBX, Byte Ptr [EAX + 11]
        PINSRW    XMM5, EBX, 3
        MOVZX     EBX, Byte Ptr [EAX + 15]
        PINSRW    XMM5, EBX, 7

        MOVDQA    XMM3, XMM1
        PUNPCKHBW XMM3, XMM7
        PMULLW    XMM3, XMM5
        PADDW     XMM3, XMM4
        PMULHUW   XMM3, XMM0

        PACKUSWB  XMM2, XMM3
        MOVDQU    [EDX], XMM2

        ADD       EAX, 16
        ADD       EDX, 16
        SUB       ECX, 4
        JGE       @Loop4

@Tail:
        ADD       ECX, 4
        JZ        @PopExit

@SingleLoop:
        MOVZX     EBX, Byte Ptr [EAX + 3]
        CMP       EBX, 255
        JE        @NextSingle
        TEST      EBX, EBX
        JZ        @ZeroAlpha
        MOVZX     ESI, Byte Ptr [EDX + 3]
        IMUL      ESI, EBX
        ADD       ESI, 127
        IMUL      ESI, ESI, 257
        SHR       ESI, 16
        PUSH      EAX
        MOV       EAX, ESI
        MOV       Byte Ptr [EDX + 3], AL
        POP       EAX
        JMP       @NextSingle
@ZeroAlpha:
        MOV       Byte Ptr [EDX + 3], 0
@NextSingle:
        ADD       EAX, 4
        ADD       EDX, 4
        DEC       ECX
        JNZ       @SingleLoop

@PopExit:
        POP       ESI
        POP       EBX

@Exit:
{$elseif defined(TARGET_x64)}
        TEST      R8D, R8D
        JLE       @Exit
        PUSH      RSI
        PUSH      RBX

        PXOR      XMM7, XMM7
        LEA       RSI, [RIP + SSE_00FF00FF_ALIGNED]
        MOVDQU    XMM6, [RSI]
        LEA       RSI, [RIP + SSE_00800080_ALIGNED]
        MOVDQU    XMM4, [RSI]
        LEA       RSI, [RIP + SSE_01010101_ALIGNED]
        MOVDQU    XMM0, [RSI]

        SUB       R8D, 4
        JL        @Tail

@Loop4:
        MOVDQA    XMM5, XMM6
        MOVZX     EBX, Byte Ptr [RCX + 3]
        PINSRW    XMM5, EBX, 3
        MOVZX     EBX, Byte Ptr [RCX + 7]
        PINSRW    XMM5, EBX, 7

        MOVDQU    XMM1, [RDX]
        MOVDQA    XMM2, XMM1
        PUNPCKLBW XMM2, XMM7
        PMULLW    XMM2, XMM5
        PADDW     XMM2, XMM4
        PMULHUW   XMM2, XMM0

        MOVDQA    XMM5, XMM6
        MOVZX     EBX, Byte Ptr [RCX + 11]
        PINSRW    XMM5, EBX, 3
        MOVZX     EBX, Byte Ptr [RCX + 15]
        PINSRW    XMM5, EBX, 7

        MOVDQA    XMM3, XMM1
        PUNPCKHBW XMM3, XMM7
        PMULLW    XMM3, XMM5
        PADDW     XMM3, XMM4
        PMULHUW   XMM3, XMM0

        PACKUSWB  XMM2, XMM3
        MOVDQU    [RDX], XMM2

        ADD       RCX, 16
        ADD       RDX, 16
        SUB       R8D, 4
        JGE       @Loop4

@Tail:
        ADD       R8D, 4
        JZ        @PopExit

@SingleLoop:
        MOVZX     EBX, Byte Ptr [RCX + 3]
        CMP       EBX, 255
        JE        @NextSingle
        TEST      EBX, EBX
        JZ        @ZeroAlpha
        MOVZX     ESI, Byte Ptr [RDX + 3]
        IMUL      ESI, EBX
        ADD       ESI, 127
        IMUL      ESI, ESI, 257
        SHR       ESI, 16
        MOV       EAX, ESI
        MOV       Byte Ptr [RDX + 3], AL
        JMP       @NextSingle
@ZeroAlpha:
        MOV       Byte Ptr [RDX + 3], 0
@NextSingle:
        ADD       RCX, 4
        ADD       RDX, 4
        DEC       R8D
        JNZ       @SingleLoop

@PopExit:
        POP       RBX
        POP       RSI

@Exit:
{$ifend}
end;

{$ifend (not defined(PURE_PASCAL)) and (not defined(OMIT_SSE2))}


//------------------------------------------------------------------------------
//
//      ScaleAlphaMems
//
//------------------------------------------------------------------------------
// Pascal version
//------------------------------------------------------------------------------
(*
** ScaleAlphaMems_Pas scales the alpha component of an array of pixels by a
** Single float Scale factor.
**
** A 256-byte lookup table (LUT) is constructed up front:
**   LUT[A] = Clamp(Round(A * Scale), 0, 255).
** Each pixel's alpha channel is transformed via LUT[Color.A], while RGB color channels
** remain completely untouched.
*)
procedure ScaleAlphaMems_Pas(Color: PColor32Entry; Count: Integer; Scale: Single);
var
  LUT: TLUT8;
  I: Integer;
  V: Integer;
begin
  if (Count <= 0) then
    Exit;

  for I := 0 to 255 do
  begin
    V := Round(I * Scale);
    if (V < 0) then
      V := 0
    else
    if (V > 255) then
      V := 255;
    LUT[I] := Byte(V);
  end;

  while (Count > 0) do
  begin
    Color.A := LUT[Color.A];
    Inc(Color);
    Dec(Count);
  end;
end;

//------------------------------------------------------------------------------
// SIMD versions
//------------------------------------------------------------------------------
{$if (not defined(PURE_PASCAL)) and (not defined(OMIT_SSE2))}

(*
** ScaleAlphaMems_SSE2 scales the alpha component of an array of pixels by a float Scale
** factor using SSE2 16-bit word multiplication (PMULLW, PADDW, PMULHUW, PACKUSWB)
** in 4-pixel blocks.
*)
procedure ScaleAlphaMems_SSE2(Color: PColor32Entry; Count: Integer; Scale: Single); {$IFDEF FPC}assembler;{$ENDIF}
asm
{$if defined(TARGET_x86)}
        TEST      EDX, EDX
        JLE       @Exit
        PUSH      EBX
        PUSH      ESI

        // Load 255.0 Single float from SSE_Float255 and multiply by Scale
        MOV       EBX, Offset SSE_Float255
        MOVSS     XMM0, DWORD PTR [Scale]
        MULSS     XMM0, DWORD PTR [EBX]
        CVTSS2SI  ESI, XMM0             // Convert to integer scale weight W in ESI

        // Clamp scale weight W: if Scale <= 0, zero all alphas; if Scale > 1.0 (ESI > 255), clamp W to 255
        TEST      ESI, ESI
        JLE       @ZeroAll
        CMP       ESI, 255
        JLE       @SetupW
        MOV       ESI, 255
@SetupW:

        // Build word weight vector in XMM6: [255, 255, 255, W, 255, 255, 255, W]
        MOV       EBX, Offset SSE_00FF00FF_ALIGNED
        MOVDQU    XMM6, [EBX]
        PINSRW    XMM6, ESI, 3
        PSHUFD    XMM6, XMM6, $44

        MOV       EBX, Offset SSE_00800080_ALIGNED
        MOVDQU    XMM5, [EBX]
        MOV       EBX, Offset SSE_01010101_ALIGNED
        MOVDQU    XMM4, [EBX]

        PXOR      XMM7, XMM7
        MOV       ECX, EDX

        SUB       ECX, 4
        JL        @Tail

@Loop4:
        MOVDQU    XMM0, [EAX]           // Load 4 pixels

        MOVDQA    XMM1, XMM0
        PUNPCKLBW XMM1, XMM7
        PMULLW    XMM1, XMM6
        PADDW     XMM1, XMM5
        PMULHUW   XMM1, XMM4

        MOVDQA    XMM2, XMM0
        PUNPCKHBW XMM2, XMM7
        PMULLW    XMM2, XMM6
        PADDW     XMM2, XMM5
        PMULHUW   XMM2, XMM4

        PACKUSWB  XMM1, XMM2            // Pack 16-bit words back to 8-bit bytes
        MOVDQU    [EAX], XMM1

        ADD       EAX, 16
        SUB       ECX, 4
        JGE       @Loop4

@Tail:
        ADD       ECX, 4
        JZ        @PopExit

@SingleLoop:
        MOVZX     EBX, Byte Ptr [EAX + 3]
        IMUL      EBX, ESI
        ADD       EBX, 127
        IMUL      EBX, EBX, 257
        SHR       EBX, 16
        MOV       Byte Ptr [EAX + 3], BL
        ADD       EAX, 4
        DEC       ECX
        JNZ       @SingleLoop
        JMP       @PopExit

@ZeroAll:
        MOV       Byte Ptr [EAX + 3], 0
        ADD       EAX, 4
        DEC       EDX
        JNZ       @ZeroAll

@PopExit:
        POP       ESI
        POP       EBX

@Exit:
{$elseif defined(TARGET_x64)}
        TEST      EDX, EDX
        JLE       @Exit
        PUSH      RSI
        PUSH      RBX

        // Multiply Scale parameter (passed in XMM2) by 255.0 and convert to integer weight W
        LEA       RAX, [RIP + SSE_Float255]
        MULSS     XMM2, DWORD PTR [RAX]
        CVTSS2SI  ESI, XMM2             // ESI = W

        // Clamp scale weight W: if Scale <= 0, zero all alphas; if Scale > 1.0 (ESI > 255), clamp W to 255
        TEST      ESI, ESI
        JLE       @ZeroAll
        CMP       ESI, 255
        JLE       @SetupW
        MOV       ESI, 255
@SetupW:

        // Build word weight vector in XMM3: [255, 255, 255, W, 255, 255, 255, W]
        LEA       RBX, [RIP + SSE_00FF00FF_ALIGNED]
        MOVDQU    XMM3, [RBX]
        PINSRW    XMM3, ESI, 3
        PSHUFD    XMM3, XMM3, $44

        SUB       EDX, 4
        JL        @Tail

@Loop4:
        MOVDQU    XMM0, [RCX]           // Load 4 pixels

        MOVDQA    XMM1, XMM0
        PXOR      XMM5, XMM5
        PUNPCKLBW XMM1, XMM5
        PMULLW    XMM1, XMM3
        LEA       RBX, [RIP + SSE_00800080_ALIGNED]
        PADDW     XMM1, [RBX]
        LEA       RBX, [RIP + SSE_01010101_ALIGNED]
        PMULHUW   XMM1, [RBX]

        MOVDQA    XMM2, XMM0
        PXOR      XMM5, XMM5
        PUNPCKHBW XMM2, XMM5
        PMULLW    XMM2, XMM3
        LEA       RBX, [RIP + SSE_00800080_ALIGNED]
        PADDW     XMM2, [RBX]
        LEA       RBX, [RIP + SSE_01010101_ALIGNED]
        PMULHUW   XMM2, [RBX]

        PACKUSWB  XMM1, XMM2
        MOVDQU    [RCX], XMM1

        ADD       RCX, 16
        SUB       EDX, 4
        JGE       @Loop4

@Tail:
        ADD       EDX, 4
        JZ        @PopExit

@SingleLoop:
        MOVZX     EBX, Byte Ptr [RCX + 3]
        IMUL      EBX, ESI
        ADD       EBX, 127
        IMUL      EBX, EBX, 257
        SHR       EBX, 16
        MOV       Byte Ptr [RCX + 3], BL
        ADD       RCX, 4
        DEC       EDX
        JNZ       @SingleLoop
        JMP       @PopExit

@ZeroAll:
        MOV       Byte Ptr [RCX + 3], 0
        ADD       RCX, 4
        DEC       EDX
        JNZ       @ZeroAll

@PopExit:
        POP       RBX
        POP       RSI

@Exit:
{$ifend}
end;

{$ifend (not defined(PURE_PASCAL)) and (not defined(OMIT_SSE2))}


//------------------------------------------------------------------------------
//
//      ApplyAlphaMask
//
//------------------------------------------------------------------------------
// Pascal version
//------------------------------------------------------------------------------
procedure ApplyAlphaMask_Pas(Source, Dest: PColor32Entry; Count: Integer);
var
  Gray: Byte;
  Y: Cardinal;
begin
  while (Count > 0) do
  begin
    // Rec. 601 luminance
    Y := (19595 * Source.R + 38470 * Source.G + 7471 * Source.B + 32768) shr 16;
    Gray := MulDiv255Table[Y, Source.A]; // =  (Y * Source.A + 127) div 255;

    if (Gray <> 255) then
    begin
      if (Gray = 0) then
        Dest.A := 0
      else
        Dest.A := MulDiv255Table[Dest.A, Gray]; // = (Dest.A * Gray + 127) div 255;
    end;
    Inc(Source);
    Inc(Dest);
    Dec(Count);
  end;
end;

//------------------------------------------------------------------------------
// SIMD versions
//------------------------------------------------------------------------------
{$if (not defined(PURE_PASCAL)) and (not defined(OMIT_SSE2))}

(*
** ApplyAlphaMask_SSE41 calculates Rec. 601 luminance of Source pixels (with source alpha applied)
** and scales Dest.A by Gray / 255 using SSE4.1 32-bit PMULLD vector math in 4-pixel blocks.
*)
procedure ApplyAlphaMask_SSE41(Source, Dest: PColor32Entry; Count: Integer); {$IFDEF FPC}assembler; nostackframe;{$ENDIF}
asm
{$if defined(TARGET_x86)}
        TEST      ECX, ECX
        JLE       @Exit
        PUSH      EBX
        PUSH      ESI

        SUB       ECX, 4
        JL        @Tail

        MOV       ESI, Offset c_w601_words
        MOVDQU    XMM5, [ESI]

        PXOR      XMM6, XMM6

@Loop4:
        MOVDQU    XMM0, [EAX]           // Source 4 pixels
        MOVDQU    XMM1, [EDX]           // Dest 4 pixels

        // Low 2 pixels luminance
        MOVDQA    XMM2, XMM0
        PUNPCKLBW XMM2, XMM6
        PMADDWD   XMM2, XMM5
        PSHUFD    XMM3, XMM2, $B1
        PADDD     XMM2, XMM3
        PSRLD     XMM2, 8               // Y_raw in [0..255]
        PSHUFD    XMM2, XMM2, $D8

        // High 2 pixels luminance
        MOVDQA    XMM3, XMM0
        PUNPCKHBW XMM3, XMM6
        PMADDWD   XMM3, XMM5
        PSHUFD    XMM4, XMM3, $B1
        PADDD     XMM3, XMM4
        PSRLD     XMM3, 8               // Y_raw in [0..255]
        PSHUFD    XMM3, XMM3, $D8

        MOVLHPS   XMM2, XMM3            // Combine into XMM2: [Y0, Y1, Y2, Y3]

        MOVDQU    XMM0, [EAX]
        PSRLD     XMM0, 24              // Source Alphas

                        PMULLD    XMM2, XMM0            // Y_raw * Sa
        MOV       ESI, Offset c_div255_bias
        MOVDQU    XMM4, [ESI]
        PADDD     XMM2, XMM4
        MOV       ESI, Offset c_div255_mult
        MOVDQU    XMM4, [ESI]
        PMULLD    XMM2, XMM4
        PSRLD     XMM2, 16              // Gray = (Y_raw * Sa + 127) * 257 shr 16 in [0..255]

        MOVDQA    XMM3, XMM1
        PSRLD     XMM3, 24              // Dest Alphas

        PMULLD    XMM3, XMM2            // Dest.A * Gray
        MOV       ESI, Offset c_div255_bias
        MOVDQU    XMM4, [ESI]
        PADDD     XMM3, XMM4
        MOV       ESI, Offset c_div255_mult
        MOVDQU    XMM4, [ESI]
        PMULLD    XMM3, XMM4
        PSRLD     XMM3, 16              // New Dest.A in [0..255]

        PSLLD     XMM3, 24
        MOV       ESI, Offset c_rgb_mask32
        MOVDQU    XMM4, [ESI]
        PAND      XMM1, XMM4
        POR       XMM1, XMM3
        MOVDQU    [EDX], XMM1

        ADD       EAX, 16
        ADD       EDX, 16
        SUB       ECX, 4
        JGE       @Loop4

@Tail:
        ADD       ECX, 4
        JZ        @PopExit

@SingleLoop:
{$IFNDEF RGBA_FORMAT}
        MOVZX     EBX, Byte Ptr [EAX + 2]
        IMUL      EBX, 19595
        MOVZX     ESI, Byte Ptr [EAX + 1]
        IMUL      ESI, 38470
        ADD       EBX, ESI
        MOVZX     ESI, Byte Ptr [EAX]
        IMUL      ESI, 7471
        ADD       EBX, ESI
{$ELSE}
        MOVZX     EBX, Byte Ptr [EAX]
        IMUL      EBX, 19595
        MOVZX     ESI, Byte Ptr [EAX + 1]
        IMUL      ESI, 38470
        ADD       EBX, ESI
        MOVZX     ESI, Byte Ptr [EAX + 2]
        IMUL      ESI, 7471
        ADD       EBX, ESI
{$ENDIF}
        ADD       EBX, 32768
        SHR       EBX, 16

        MOVZX     ESI, Byte Ptr [EAX + 3]
        IMUL      EBX, ESI
        ADD       EBX, 127
        IMUL      EBX, EBX, 257
        SHR       EBX, 16

        CMP       EBX, 255
        JE        @NextSingle
        TEST      EBX, EBX
        JZ        @ZeroAlpha
        MOVZX     ESI, Byte Ptr [EDX + 3]
        IMUL      ESI, EBX
        ADD       ESI, 127
        IMUL      ESI, ESI, 257
        SHR       ESI, 16
        PUSH      EAX
        MOV       EAX, ESI
        MOV       Byte Ptr [EDX + 3], AL
        POP       EAX
        JMP       @NextSingle
@ZeroAlpha:
        MOV       Byte Ptr [EDX + 3], 0
@NextSingle:
        ADD       EAX, 4
        ADD       EDX, 4
        DEC       ECX
        JNZ       @SingleLoop

@PopExit:
        POP       ESI
        POP       EBX

@Exit:
{$elseif defined(TARGET_x64)}
        TEST      R8D, R8D
        JLE       @Exit
        PUSH      RSI
        PUSH      RBX

        SUB       R8D, 4
        JL        @Tail

@Loop4:
        MOVDQU    XMM0, [RCX]           // Source 4 pixels
        MOVDQU    XMM1, [RDX]           // Dest 4 pixels

        // Low 2 pixels luminance
        MOVDQA    XMM2, XMM0
        PXOR      XMM4, XMM4
        PUNPCKLBW XMM2, XMM4
        LEA       RSI, [RIP + c_w601_words]
        PMADDWD   XMM2, [RSI]
        PSHUFD    XMM3, XMM2, $B1
        PADDD     XMM2, XMM3
        PSRLD     XMM2, 8               // Y_raw in [0..255]
        PSHUFD    XMM2, XMM2, $D8

        // High 2 pixels luminance
        MOVDQA    XMM3, XMM0
        PUNPCKHBW XMM3, XMM4
        PMADDWD   XMM3, [RSI]
        PSHUFD    XMM4, XMM3, $B1
        PADDD     XMM3, XMM4
        PSRLD     XMM3, 8               // Y_raw in [0..255]
        PSHUFD    XMM3, XMM3, $D8

        MOVLHPS   XMM2, XMM3            // Combine into XMM2: [Y0, Y1, Y2, Y3]

        MOVDQU    XMM0, [RCX]
        PSRLD     XMM0, 24              // Source Alphas

        PMULLD    XMM2, XMM0            // Y_raw * Sa
        LEA       RSI, [RIP + c_div255_bias]
        PADDD     XMM2, [RSI]
        LEA       RSI, [RIP + c_div255_mult]
        PMULLD    XMM2, [RSI]
        PSRLD     XMM2, 16              // Gray = (Y_raw * Sa + 127) * 257 shr 16 in [0..255]

        MOVDQA    XMM4, XMM1
        PSRLD     XMM4, 24              // Dest Alphas

        PMULLD    XMM4, XMM2            // Dest.A * Gray
        LEA       RSI, [RIP + c_div255_bias]
        PADDD     XMM4, [RSI]
        LEA       RSI, [RIP + c_div255_mult]
        PMULLD    XMM4, [RSI]
        PSRLD     XMM4, 16              // New Dest.A in [0..255]

        PSLLD     XMM4, 24
        LEA       RSI, [RIP + c_rgb_mask32]
        MOVDQU    XMM0, [RSI]
        PAND      XMM1, XMM0
        POR       XMM1, XMM4
        MOVDQU    [RDX], XMM1

        ADD       RCX, 16
        ADD       RDX, 16
        SUB       R8D, 4
        JGE       @Loop4

@Tail:
        ADD       R8D, 4
        JZ        @PopExit

@SingleLoop:
{$IFNDEF RGBA_FORMAT}
        MOVZX     EBX, Byte Ptr [RCX + 2]
        IMUL      EBX, 19595
        MOVZX     ESI, Byte Ptr [RCX + 1]
        IMUL      ESI, 38470
        ADD       EBX, ESI
        MOVZX     ESI, Byte Ptr [RCX]
        IMUL      ESI, 7471
        ADD       EBX, ESI
{$ELSE}
        MOVZX     EBX, Byte Ptr [RCX]
        IMUL      EBX, 19595
        MOVZX     ESI, Byte Ptr [RCX + 1]
        IMUL      ESI, 38470
        ADD       EBX, ESI
        MOVZX     ESI, Byte Ptr [RCX + 2]
        IMUL      ESI, 7471
        ADD       EBX, ESI
{$ENDIF}
        ADD       EBX, 32768
        SHR       EBX, 16

        MOVZX     ESI, Byte Ptr [RCX + 3]
        IMUL      EBX, ESI
        ADD       EBX, 127
        IMUL      EBX, EBX, 257
        SHR       EBX, 16

        CMP       EBX, 255
        JE        @NextSingle
        TEST      EBX, EBX
        JZ        @ZeroAlpha
        MOVZX     ESI, Byte Ptr [RDX + 3]
        IMUL      ESI, EBX
        ADD       ESI, 127
        IMUL      ESI, ESI, 257
        SHR       ESI, 16
        MOV       EAX, ESI
        MOV       Byte Ptr [RDX + 3], AL
        JMP       @NextSingle
@ZeroAlpha:
        MOV       Byte Ptr [RDX + 3], 0
@NextSingle:
        ADD       RCX, 4
        ADD       RDX, 4
        DEC       R8D
        JNZ       @SingleLoop

@PopExit:
        POP       RBX
        POP       RSI

@Exit:
{$ifend}
end;

{$ifend (not defined(PURE_PASCAL)) and (not defined(OMIT_SSE2))}


//------------------------------------------------------------------------------
//
//      CPU target and feature Function templates
//
//------------------------------------------------------------------------------
var
  Registry: TFunctionRegistry;

procedure RegisterBindings;
begin
  Registry := NewRegistry('GR32_Filters bindings');
  Registry.RegisterBinding(@@LogicalMaskLineAnd, 'LogicalMaskLineAnd');
  Registry.RegisterBinding(@@LogicalMaskLineOr, 'LogicalMaskLineOr');
  Registry.RegisterBinding(@@LogicalMaskLineXor, 'LogicalMaskLineXor');
  Registry.RegisterBinding(@@LogicalMaskLineAndEx, 'LogicalMaskLineAndEx');
  Registry.RegisterBinding(@@LogicalMaskLineOrEx, 'LogicalMaskLineOrEx');
  Registry.RegisterBinding(@@LogicalMaskLineXorEx, 'LogicalMaskLineXorEx');
  Registry.RegisterBinding(@@ScaleAlphaLine, 'ScaleAlphaLine');
  Registry.RegisterBinding(@@ScaleAlphaMems, 'ScaleAlphaMems');
  Registry.RegisterBinding(@@ApplyAlphaMask, 'ApplyAlphaMask');
  Registry.RegisterBinding(@@Luminance601, 'Luminance601');
  Registry.RegisterBinding(@@Luminance709, 'Luminance709');

  Registry[@@LogicalMaskLineAnd].Add(   @AndLine_Pas,   [isPascal]).Name := 'AndLine_Pas';
  Registry[@@LogicalMaskLineOr].Add(    @OrLine_Pas,    [isPascal]).Name := 'OrLine_Pas';
  Registry[@@LogicalMaskLineXor].Add(   @XorLine_Pas,   [isPascal]).Name := 'XorLine_Pas';
  Registry[@@LogicalMaskLineAndEx].Add( @AndLineEx_Pas, [isPascal]).Name := 'AndLineEx_Pas';
  Registry[@@LogicalMaskLineOrEx].Add(  @OrLineEx_Pas,  [isPascal]).Name := 'OrLineEx_Pas';
  Registry[@@LogicalMaskLineXorEx].Add( @XorLineEx_Pas, [isPascal]).Name := 'XorLineEx_Pas';
  Registry[@@ScaleAlphaLine].Add(       @ScaleAlphaLine_Pas, [isPascal]).Name := 'ScaleAlphaLine_Pas';
  Registry[@@ScaleAlphaMems].Add(       @ScaleAlphaMems_Pas, [isPascal]).Name := 'ScaleAlphaMems_Pas';
  Registry[@@ApplyAlphaMask].Add(       @ApplyAlphaMask_Pas, [isPascal]).Name := 'ApplyAlphaMask_Pas';
  Registry[@@Luminance601].Add(         @Luminance601_Pas,   [isPascal]).Name := 'Luminance601_Pas';
  Registry[@@Luminance709].Add(         @Luminance709_Pas,   [isPascal]).Name := 'Luminance709_Pas';

{$IFNDEF PUREPASCAL}
  Registry[@@LogicalMaskLineAnd].Add(   @AndLine_ASM,   [isAssembler]).Name := 'AndLine_ASM';
  Registry[@@LogicalMaskLineOr].Add(    @OrLine_ASM,    [isAssembler]).Name := 'OrLine_ASM';
  Registry[@@LogicalMaskLineXor].Add(   @XorLine_ASM,   [isAssembler]).Name := 'XorLine_ASM';
  Registry[@@LogicalMaskLineAndEx].Add( @AndLineEx_ASM, [isAssembler]).Name := 'AndLineEx_ASM';
  Registry[@@LogicalMaskLineOrEx].Add(  @OrLineEx_ASM,  [isAssembler]).Name := 'OrLineEx_ASM';
  Registry[@@LogicalMaskLineXorEx].Add( @XorLineEx_ASM, [isAssembler]).Name := 'XorLineEx_ASM';

{$IFNDEF OMIT_SSE2}
  Registry[@@ScaleAlphaLine].Add(       @ScaleAlphaLine_SSE2, [isSSE2]).Name := 'ScaleAlphaLine_SSE2';
  Registry[@@ApplyAlphaMask].Add(       @ApplyAlphaMask_SSE41, [isSSE41]).Name := 'ApplyAlphaMask_SSE41';
  Registry[@@ScaleAlphaMems].Add(       @ScaleAlphaMems_SSE2, [isSSE2]).Name := 'ScaleAlphaMems_SSE2';
  Registry[@@Luminance601].Add(         @Luminance601_SSE2,   [isSSE2]).Name := 'Luminance601_SSE2';
  Registry[@@Luminance601].Add(         @Luminance601_SSE41,  [isSSE41]).Name := 'Luminance601_SSE41';
  Registry[@@Luminance709].Add(         @Luminance709_SSE2,   [isSSE2]).Name := 'Luminance709_SSE2';
  Registry[@@Luminance709].Add(         @Luminance709_SSE41,  [isSSE41]).Name := 'Luminance709_SSE41';
{$ENDIF}

  // TODO : rewrite MMX implementations using SSE
{$IFNDEF OMIT_MMX}
  Registry[@@LogicalMaskLineAndEx].Add( @AndLineEx_MMX, [isMMX]).Name := 'AndLineEx_MMX';
  Registry[@@LogicalMaskLineOrEx].Add(  @OrLineEx_MMX,  [isMMX]).Name := 'OrLineEx_MMX';
  Registry[@@LogicalMaskLineXorEx].Add( @XorLineEx_MMX, [isMMX]).Name := 'XorLineEx_MMX';
  Registry[@@LogicalMaskLineAndEx].Add( @AndLineEx_EMMX,[isExMMX]).Name := 'AndLineEx_EMMX';
  Registry[@@LogicalMaskLineOrEx].Add(  @OrLineEx_EMMX, [isExMMX]).Name := 'OrLineEx_EMMX';
  Registry[@@LogicalMaskLineXorEx].Add( @XorLineEx_EMMX,[isExMMX]).Name := 'XorLineEx_EMMX';
{$ENDIF}

{$ENDIF}

  Registry.RebindAll;
end;

initialization
  RegisterBindings;

end.
