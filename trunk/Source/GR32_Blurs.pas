unit GR32_Blurs;

(* BEGIN LICENSE BLOCK *********************************************************
* Version: MPL 1.1                                                             *
*                                                                              *
* The contents of this file are subject to the Mozilla Public License Version  *
* 1.1 (the "License"); you may not use this file except in compliance with     *
* the License. You may obtain a copy of the License at                         *
* http://www.mozilla.org/MPL/                                                  *
*                                                                              *
* Software distributed under the License is distributed on an "AS IS" basis,   *
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License     *
* for the specific language governing rights and limitations under the         *
* License.                                                                     *
*                                                                              *
* Alternatively, the contents of this file may be used under the terms of the  *
* Free Pascal modified version of the GNU Lesser General Public License        *
* Version 2.1 (the "FPC modified LGPL License"), in which case the provisions  *
* of this license are applicable instead of those above.                       *
* Please see the file LICENSE.txt for additional information concerning this   *
* license.                                                                     *
*                                                                              *
* The Original Code is GR32_Blurs. The Gaussian blur algorithm was inspired    *
* by code published by Mario Klingemann and has been used with his permission. *
* See also http://incubator.quasimondo.com                                     *
*                                                                              *
* Copyright 2012 - Angus Johnson                                               *
*                                                                              *
* Version 5.0 (Last updated 25-Sep-2012)                                       *
*                                                                              *
* END LICENSE BLOCK ***********************************************************)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC}
    LCLIntf,
  {$ELSE}
    Windows, Types,
  {$ENDIF}
    SysUtils, classes, Math, GR32;

procedure GaussianBlur(Bmp32: TBitmap32; Radius: TFloat); overload;
procedure GaussianBlur(Bmp32: TBitmap32; Radius: TFloat; const Rec: TRect); overload;
procedure GaussianBlur(Bmp32: TBitmap32; Radius: TFloat;
  const BlurRegion: TArrayOfFloatPoint); overload;

procedure FastBlur(Bmp32: TBitmap32; Radius: TFloat); overload;
procedure FastBlur(Bmp32: TBitmap32; Radius: TFloat; const Rec: TRect); overload;
procedure FastBlur(Bmp32: TBitmap32; Radius: TFloat;
  const BlurRegion: TArrayOfFloatPoint); overload;

procedure MotionBlur(Bmp32: TBitmap32;
  Dist, AngleDeg: TFloat; Bidirectional: Boolean = true); overload;
procedure MotionBlur(Bmp32: TBitmap32; Dist, AngleDeg: TFloat;
  const Rec: TRect; Bidirectional: Boolean = true); overload;
procedure MotionBlur(Bmp32: TBitmap32; Dist, AngleDeg: TFloat;
  const BlurRegion: TArrayOfFloatPoint; Bidirectional: Boolean = true); overload;

implementation

uses
  GR32_Blend, GR32_Polygons, GR32_LowLevel, GR32_VectorUtils, GR32_Transforms;

type
   TSumRecInt64 = record
     B, G, R, A: Int64;
     Sum: Integer;
   end;

   TSumRecord = record
     B, G, R, A: Integer;
     Sum: Integer;
   end;

const
  ChannelSize = 256; // ie 1 byte for each of A,R,G & B in TColor32
  ChannelSizeMin1 = ChannelSize -1;

{ GaussianBlur }

{$R-}

procedure GaussianBlur(Bmp32: TBitmap32; Radius: TFloat);
begin
  GaussianBlur(Bmp32, Radius, Bmp32.BoundsRect);
end;

procedure GaussianBlur(Bmp32: TBitmap32; Radius: TFloat; const Rec: TRect);
var
  Q, I, J, X, Y, ImageWidth, RowOffset, RadiusI: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixels: PColor32EntryArray;
  RadiusSq, RadiusRevSq, KernelSize: Integer;
  SumRec: TSumRecInt64;
  PreMulArray: array of TColor32Entry;
  SumArray: array of TSumRecInt64;
  GaussLUT: array of array of Cardinal;
begin
  RadiusI := Round(Radius);
  if RadiusI < 1 then
    Exit
  else if RadiusI > 128 then
    RadiusI := 128; // nb: performance degrades exponentially with >> Radius

  // initialize the look-up-table ...
  KernelSize := RadiusI * 2 + 1;
  SetLength(GaussLUT, KernelSize);
  for I := 0 to KernelSize - 1 do
    SetLength(GaussLUT[I], ChannelSize);
  for I := 1 to RadiusI do
  begin
    RadiusRevSq := Round((Radius + 1 - I) * (Radius + 1 - I));
    for J := 0 to ChannelSizeMin1 do
    begin
      GaussLUT[RadiusI - I][J] := RadiusRevSq * J;
      GaussLUT[RadiusI + I][J] := GaussLUT[RadiusI - I][J];
    end;
  end;
  RadiusSq := Round((Radius + 1) * (Radius + 1));
  for J := 0 to ChannelSizeMin1 do
    GaussLUT[RadiusI][J] := RadiusSq * J;

  ImageWidth := Bmp32.Width;
  SetLength(SumArray, ImageWidth * Bmp32.Height);

  ImagePixels := PColor32EntryArray(Bmp32.Bits);
  RecLeft := Max(Rec.Left, 0);
  RecTop := Max(Rec.Top, 0);
  RecRight := Min(Rec.Right, ImageWidth -1);
  RecBottom := Min(Rec.Bottom, Bmp32.Height -1);

  RowOffset := RecTop * ImageWidth;
  SetLength(PreMulArray, Bmp32.Width);
  for Y := RecTop to RecBottom do
  begin
    // initialize PreMulArray for the row ...
    Q := (Y * ImageWidth) + RecLeft;
    for X := RecLeft to RecRight do
      with ImagePixels[Q] do
      begin
        PreMulArray[X].A := A;
        PreMulArray[X].R := DivTable[R, A];
        PreMulArray[X].G := DivTable[G, A];
        PreMulArray[X].B := DivTable[B, A];
        inc(Q);
      end;

    for X := RecLeft to RecRight do
    begin
      SumRec.A := 0;
      SumRec.R := 0;
      SumRec.G := 0;
      SumRec.B := 0;
      SumRec.Sum := 0;

      I := Max(X - RadiusI, RecLeft);
      Q := I - (X - RadiusI);
      for I := I to Min(X + RadiusI, RecRight) do
        with PreMulArray[I] do
        begin
          Inc(SumRec.A, GaussLUT[Q][A]);
          Inc(SumRec.R, GaussLUT[Q][R]);
          Inc(SumRec.G, GaussLUT[Q][G]);
          Inc(SumRec.B, GaussLUT[Q][B]);
          Inc(SumRec.Sum, GaussLUT[Q][1]);
          Inc(Q);
        end;
      Q := RowOffset + X;
      SumArray[Q].A := SumRec.A div SumRec.Sum;
      SumArray[Q].R := SumRec.R div SumRec.Sum;
      SumArray[Q].G := SumRec.G div SumRec.Sum;
      SumArray[Q].B := SumRec.B div SumRec.Sum;
    end;
    Inc(RowOffset, ImageWidth);
  end;

  RowOffset := RecTop * ImageWidth;
  for Y := RecTop to RecBottom do
  begin
    for X := RecLeft to RecRight do
    begin
      SumRec.A := 0;
      SumRec.R := 0;
      SumRec.G := 0;
      SumRec.B := 0;
      SumRec.Sum := 0;

      I := Max(Y - RadiusI, RecTop);
      Q := I - (Y - RadiusI);
      for I := I to Min(Y + RadiusI, RecBottom) do
        with SumArray[X + I * ImageWidth] do
        begin
          Inc(SumRec.A, GaussLUT[Q][A]);
          Inc(SumRec.R, GaussLUT[Q][R]);
          Inc(SumRec.G, GaussLUT[Q][G]);
          Inc(SumRec.B, GaussLUT[Q][B]);
          Inc(SumRec.Sum, GaussLUT[Q][1]);
          Inc(Q);
        end;

      with ImagePixels[RowOffset + X] do
      begin
        A := (SumRec.A div SumRec.Sum);
        R := RcTable[A, (SumRec.R div SumRec.Sum)];
        G := RcTable[A, (SumRec.G div SumRec.Sum)];
        B := RcTable[A, (SumRec.B div SumRec.Sum)];
      end;
    end;
    Inc(RowOffset, ImageWidth);
  end;
end;

procedure GaussianBlur(Bmp32: TBitmap32; Radius: TFloat;
  const BlurRegion: TArrayOfFloatPoint);
var
  Q, I, J, X, Y, ImageWidth, RowOffset, RadiusI: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixels: PColor32EntryArray;
  RadiusSq, RadiusRevSq, KernelSize: Integer;
  SumRec: TSumRecInt64;
  SumArray: array of TSumRecInt64;
  GaussLUT: array of array of Cardinal;
  PreMulArray: array of TColor32Entry;

  Alpha: Cardinal;
  Mask: TBitmap32;
  Clr, MaskClr: TColor32Entry;
  Pts: TArrayOfFloatPoint;
  Rec: TRect;
begin
  with PolygonBounds(BlurRegion) do
    Rec := Rect(Floor(Left), Floor(Top), Ceil(Right), Ceil(Bottom));
  if Rec.Left < 0 then Rec.Left := 0;
  if Rec.Top < 0 then Rec.Top := 0;
  if Rec.Right >= Bmp32.Width then Rec.Right := Bmp32.Width -1;
  if Rec.Bottom >= Bmp32.Height then Rec.Bottom := Bmp32.Height -1;

  RadiusI := round(Radius);
  if (RadiusI < 1) or (Rec.Right <= Rec.Left) or (Rec.Bottom <= Rec.Top) then
    Exit
  else if RadiusI > 128 then
    RadiusI := 128; // nb: performance degrades exponentially with >> Radius

  Mask := TBitmap32.Create;
  try
    Mask.SetSize(Rec.Right - Rec.Left +1, Rec.Bottom - Rec.Top +1);
    SetLength(Pts, Length(BlurRegion));
    for I := 0 to High(BlurRegion) do
    begin
      Pts[I].X := BlurRegion[I].X - Rec.Left;
      Pts[I].Y := BlurRegion[I].Y - Rec.Top;
    end;
    PolygonFS(Mask, Pts, clWhite32);

    // initialize the look-up-table ...
    KernelSize := RadiusI * 2 + 1;
    SetLength(GaussLUT, KernelSize);
    for I := 0 to KernelSize - 1 do
      SetLength(GaussLUT[I], ChannelSize);
    for I := 1 to RadiusI do
    begin
      RadiusRevSq := Round((Radius + 1 - I) * (Radius + 1 - I));
      for J := 0 to ChannelSizeMin1 do
      begin
        GaussLUT[RadiusI - I][J] := RadiusRevSq * J;
        GaussLUT[RadiusI + I][J] := GaussLUT[RadiusI - I][J];
      end;
    end;
    RadiusSq := Round((Radius + 1) * (Radius + 1));
    for J := 0 to ChannelSizeMin1 do
      GaussLUT[RadiusI][J] := RadiusSq * J;

    ImageWidth := Bmp32.Width;
    SetLength(SumArray, ImageWidth * Bmp32.Height);

    ImagePixels := PColor32EntryArray(Bmp32.Bits);
    RecLeft := Max(Rec.Left, 0);
    RecTop := Max(Rec.Top, 0);
    RecRight := Min(Rec.Right, ImageWidth -1);
    RecBottom := Min(Rec.Bottom, Bmp32.Height -1);

    RowOffset := RecTop * ImageWidth;
    SetLength(PreMulArray, Bmp32.Width);
    for Y := RecTop to RecBottom do
    begin
      // initialize PreMulArray for the current row ...
      Q := (Y * ImageWidth) + RecLeft;
      for X := RecLeft to RecRight do
        with ImagePixels[Q] do
        begin
          PreMulArray[X].A := A;
          PreMulArray[X].R := DivTable[R, A];
          PreMulArray[X].G := DivTable[G, A];
          PreMulArray[X].B := DivTable[B, A];
          inc(Q);
        end;

      for X := RecLeft to RecRight do
      begin
        SumRec.A := 0;
        SumRec.R := 0;
        SumRec.G := 0;
        SumRec.B := 0;
        SumRec.Sum := 0;

        I := Max(X - RadiusI, RecLeft);
        Q := I - (X - RadiusI);
        for I := I to Min(X + RadiusI, RecRight) do
          with PreMulArray[I] do
          begin
            Inc(SumRec.A, GaussLUT[Q][A]);
            Inc(SumRec.R, GaussLUT[Q][R]);
            Inc(SumRec.G, GaussLUT[Q][G]);
            Inc(SumRec.B, GaussLUT[Q][B]);
            Inc(SumRec.Sum, GaussLUT[Q][1]);
            Inc(Q);
          end;
        Q := RowOffset + X;
        SumArray[Q].A := SumRec.A div SumRec.Sum;
        SumArray[Q].R := SumRec.R div SumRec.Sum;
        SumArray[Q].G := SumRec.G div SumRec.Sum;
        SumArray[Q].B := SumRec.B div SumRec.Sum;
      end;
      Inc(RowOffset, ImageWidth);
    end;

    RowOffset := RecTop * ImageWidth;
    for Y := RecTop to RecBottom do
    begin
      for X := RecLeft to RecRight do
      begin
        MaskClr.ARGB := Mask.Pixel[X - RecLeft, Y - RecTop];
        if (MaskClr.A = 0) then continue;

        SumRec.A := 0;
        SumRec.R := 0;
        SumRec.G := 0;
        SumRec.B := 0;
        SumRec.Sum := 0;

        I := Max(Y - RadiusI, RecTop);
        Q := I - (Y - RadiusI);
        for I := I to Min(Y + RadiusI, RecBottom) do
          with SumArray[X + I * ImageWidth] do
          begin
            Inc(SumRec.A, GaussLUT[Q][A]);
            Inc(SumRec.R, GaussLUT[Q][R]);
            Inc(SumRec.G, GaussLUT[Q][G]);
            Inc(SumRec.B, GaussLUT[Q][B]);
            Inc(SumRec.Sum, GaussLUT[Q][1]);
            Inc(Q);
          end;

        with ImagePixels[RowOffset + X] do
          if (MaskClr.A < 255) then
          begin
            Clr.A := SumRec.A div SumRec.Sum;
            Clr.R := RcTable[Clr.A, SumRec.R div SumRec.Sum];
            Clr.G := RcTable[Clr.A, SumRec.G div SumRec.Sum];
            Clr.B := RcTable[Clr.A, SumRec.B div SumRec.Sum];
            BlendMemEx(Clr.ARGB, ARGB, MaskClr.A);
          end else
          begin
            A := SumRec.A div SumRec.Sum;
            R := RcTable[A, SumRec.R div SumRec.Sum];
            G := RcTable[A, SumRec.G div SumRec.Sum];
            B := RcTable[A, SumRec.B div SumRec.Sum];
          end;
      end;
      Inc(RowOffset, ImageWidth);
    end;
    EMMS;
  finally
    Mask.Free;
  end;
end;


{ FastBlur }

procedure FastBlur(Bmp32: TBitmap32; Radius: TFloat);
begin
  FastBlur(Bmp32, Radius, Bmp32.BoundsRect);
end;

procedure FastBlur(Bmp32: TBitmap32; Radius: TFloat; const Rec: TRect);
var
  LL, RR, TT, BB, XX, YY, I, J, X, Y, RadiusI, Passes: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixel: PColor32Entry;
  SumRec: TSumRecord;
  ImgPixel: PColor32Entry;
  Pixels: array of TColor32Entry;
begin
  if Radius < 1 then
    Exit
  else if Radius > 256 then
    Radius := 256;

  RadiusI := Round(Sqrt(-Radius * Radius / (2 * ln(1 / 255))));
  if RadiusI < 2 then
  begin
    Passes := Round(Radius);
    RadiusI := 1;
  end else
    Passes := 3;

  RecLeft := Max(Rec.Left, 0);
  RecTop := Max(Rec.Top, 0);
  RecRight := Min(Rec.Right, Bmp32.Width -1);
  RecBottom := Min(Rec.Bottom, Bmp32.Height -1);

  SetLength(Pixels, Max(Bmp32.Width, Bmp32.Height) +1);
  // pre-multiply alphas ...
  for Y := RecTop to RecBottom do
  begin
    ImgPixel := PColor32Entry(Bmp32.ScanLine[Y]);
    Inc(ImgPixel, RecLeft);
    for X := RecLeft to RecRight do
      with ImgPixel^ do
      begin
        R := DivTable[R, A];
        G := DivTable[G, A];
        B := DivTable[B, A];
        inc(ImgPixel);
      end;
  end;

  for I := 1 to Passes do
  begin

    // horizontal pass...
    for Y := RecTop to RecBottom do
    begin
      ImagePixel := PColor32Entry(@Bmp32.ScanLine[Y][RecLeft]);
      // fill the Pixels buffer with a copy of the row's pixels ...
      MoveLongword(ImagePixel^, Pixels[RecLeft], RecRight - RecLeft + 1);

      SumRec.A := 0; SumRec.R := 0; SumRec.G := 0;
      SumRec.B := 0; SumRec.Sum := 0;

      LL := RecLeft;
      RR := RecLeft + RadiusI;
      if RR > RecRight then RR := RecRight;
      // update first in row ...
      for XX := LL to RR do
        with Pixels[XX] do
        begin
          Inc(SumRec.A, A);
          Inc(SumRec.R, R);
          Inc(SumRec.G, G);
          Inc(SumRec.B, B);
          Inc(SumRec.Sum);
        end;
      with ImagePixel^ do
      begin
        A := SumRec.A div SumRec.Sum;
        R := SumRec.R div SumRec.Sum;
        G := SumRec.G div SumRec.Sum;
        B := SumRec.B div SumRec.Sum;
      end;
      // update the remaining pixels in the row ...
      for X := RecLeft + 1 to RecRight do
      begin
        Inc(ImagePixel);
        LL := X - RadiusI -1;
        RR := X + RadiusI;
        if LL >= RecLeft then
          with Pixels[LL] do
          begin
            Dec(SumRec.A, A);
            Dec(SumRec.R, R);
            Dec(SumRec.G, G);
            Dec(SumRec.B, B);
            Dec(SumRec.Sum);
          end;
        if RR <= RecRight then
          with Pixels[RR] do
          begin
            Inc(SumRec.A, A);
            Inc(SumRec.R, R);
            Inc(SumRec.G, G);
            Inc(SumRec.B, B);
            Inc(SumRec.Sum);
          end;
        with ImagePixel^ do
        begin
          A := SumRec.A div SumRec.Sum;
          R := SumRec.R div SumRec.Sum;
          G := SumRec.G div SumRec.Sum;
          B := SumRec.B div SumRec.Sum;
        end;
      end;
    end;

    // vertical pass...
    for X := RecLeft to RecRight do
    begin
      ImagePixel := PColor32Entry(@Bmp32.ScanLine[RecTop][X]);
      for J := RecTop to RecBottom do
      begin
        Pixels[J] := ImagePixel^;
        inc(ImagePixel, Bmp32.Width);
      end;
      ImagePixel := PColor32Entry(@Bmp32.ScanLine[RecTop][X]);

      TT := RecTop;
      BB := RecTop + RadiusI;
      if BB > RecBottom then BB := RecBottom;
      SumRec.A := 0; SumRec.R := 0; SumRec.G := 0;
      SumRec.B := 0; SumRec.Sum := 0;
      // update first in col ...
      for YY := TT to BB do
        with Pixels[YY] do
        begin
          Inc(SumRec.A, A);
          Inc(SumRec.R, R);
          Inc(SumRec.G, G);
          Inc(SumRec.B, B);
          Inc(SumRec.Sum);
        end;
      with ImagePixel^ do
      begin
        A := SumRec.A div SumRec.Sum;
        R := SumRec.R div SumRec.Sum;
        G := SumRec.G div SumRec.Sum;
        B := SumRec.B div SumRec.Sum;
      end;
      // update remainder in col ...
      for Y := RecTop + 1 to RecBottom do
      begin
        Inc(ImagePixel, Bmp32.Width);
        TT := Y - RadiusI -1;
        BB := Y + RadiusI;

        if TT >= RecTop then
          with Pixels[TT] do
          begin
            Dec(SumRec.A, A);
            Dec(SumRec.R, R);
            Dec(SumRec.G, G);
            Dec(SumRec.B, B);
            Dec(SumRec.Sum);
          end;
        if BB <= RecBottom then
          with Pixels[BB] do
          begin
            Inc(SumRec.A, A);
            Inc(SumRec.R, R);
            Inc(SumRec.G, G);
            Inc(SumRec.B, B);
            Inc(SumRec.Sum);
          end;
        with ImagePixel^ do
        begin
          A := SumRec.A div SumRec.Sum;
          R := SumRec.R div SumRec.Sum;
          G := SumRec.G div SumRec.Sum;
          B := SumRec.B div SumRec.Sum;
        end;
      end;
    end;
  end;

  // extract alphas ...
  for Y := RecTop to RecBottom do
  begin
    ImgPixel := PColor32Entry(@Bmp32.ScanLine[Y][RecLeft]);
    for X := RecLeft to RecRight do
    begin
      ImgPixel.R := RcTable[ImgPixel.A, ImgPixel.R];
      ImgPixel.G := RcTable[ImgPixel.A, ImgPixel.G];
      ImgPixel.B := RcTable[ImgPixel.A, ImgPixel.B];
      Inc(ImgPixel);
    end;
  end;
end;

procedure FastBlur(Bmp32: TBitmap32; Radius: TFloat; const BlurRegion: TArrayOfFloatPoint);
var
  LL, RR, TT, BB, XX, YY, I, J, X, Y, RadiusI, Passes: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixel: PColor32Entry;
  SumRec: TSumRecord;
  ImgPixel: PColor32Entry;
  Pixels: array of TSumRecord;

  Mask: TBitmap32;
  Clr, MaskClr: TColor32Entry;
  Pts: TArrayOfFloatPoint;
  Rec: TRect;
begin
  if Radius < 1 then
    Exit
  else if Radius > 256 then
    Radius := 256;

  RadiusI := Round(Sqrt(-Radius * Radius / (2 * ln(1 / 255))));
  if RadiusI < 2 then
  begin
    Passes := Round(Radius);
    RadiusI := 1;
  end else
    Passes := 3;

  with PolygonBounds(BlurRegion) do
    Rec := Rect(Floor(Left), Floor(Top), Ceil(Right), Ceil(Bottom));
  if Rec.Left < 0 then Rec.Left := 0;
  if Rec.Top < 0 then Rec.Top := 0;
  if Rec.Right >= Bmp32.Width then Rec.Right := Bmp32.Width -1;
  if Rec.Bottom >= Bmp32.Height then Rec.Bottom := Bmp32.Height -1;
  RecLeft := Max(Rec.Left, 0);
  RecTop := Max(Rec.Top, 0);
  RecRight := Min(Rec.Right, Bmp32.Width -1);
  RecBottom := Min(Rec.Bottom, Bmp32.Height -1);

  SetLength(Pixels, Max(Bmp32.Width, Bmp32.Height) +1);
  // pre-multiply alphas ...
  for Y := RecTop to RecBottom do
  begin
    ImgPixel := PColor32Entry(Bmp32.ScanLine[Y]);
    Inc(ImgPixel, RecLeft);
    for X := RecLeft to RecRight do
    begin
      ImgPixel.R := DivTable[ImgPixel.R, ImgPixel.A];
      ImgPixel.G := DivTable[ImgPixel.G, ImgPixel.A];
      ImgPixel.B := DivTable[ImgPixel.B, ImgPixel.A];
      inc(ImgPixel);
    end;
  end;

  Mask := TBitmap32.Create;
  try
    Mask.SetSize(Rec.Right - Rec.Left +1, Rec.Bottom - Rec.Top +1);
    SetLength(Pts, Length(BlurRegion));
    for I := 0 to High(BlurRegion) do
    begin
      Pts[I].X := BlurRegion[I].X - Rec.Left;
      Pts[I].Y := BlurRegion[I].Y - Rec.Top;
    end;
    PolygonFS(Mask, Pts, clWhite32);

    for I := 1 to Passes do
    begin
      // horizontal pass...
      for Y := RecTop to RecBottom do
      begin
        ImagePixel := PColor32Entry(@Bmp32.ScanLine[Y][RecLeft]);
        // fill the Pixels buffer with a copy of the row's pixels ...
        for J := RecLeft to RecRight do
        begin
          MaskClr.ARGB := Mask.Pixel[J - RecLeft, Y - RecTop];
          if (MaskClr.A = 0) then
          begin
            Pixels[J].A := 0;
            Pixels[J].R := 0;
            Pixels[J].G := 0;
            Pixels[J].B := 0;
            Pixels[J].Sum := 0;
          end else
          with ImagePixel^ do
          begin
            Pixels[J].A := A;
            Pixels[J].R := R;
            Pixels[J].G := G;
            Pixels[J].B := B;
            Pixels[J].Sum := 1;
          end;
          Inc(ImagePixel);
        end;
        LL := RecLeft;
        RR := RecLeft + RadiusI;
        if RR > RecRight then RR := RecRight;
        SumRec.A := 0; SumRec.R := 0; SumRec.G := 0;
        SumRec.B := 0; SumRec.Sum := 0;
        // update first in row ...
        for XX := LL to RR do
          with Pixels[XX] do
          begin
            Inc(SumRec.A, A);
            Inc(SumRec.R, R);
            Inc(SumRec.G, G);
            Inc(SumRec.B, B);
            Inc(SumRec.Sum, Sum);
          end;

        ImagePixel := PColor32Entry(@Bmp32.ScanLine[Y][RecLeft]);
        MaskClr.ARGB := Mask.Pixel[0, Y - RecTop];
        if (MaskClr.A > 0) and (SumRec.Sum > 0) then
          with ImagePixel^ do
          begin
            A := SumRec.A div SumRec.Sum;
            R := SumRec.R div SumRec.Sum;
            G := SumRec.G div SumRec.Sum;
            B := SumRec.B div SumRec.Sum;
          end;
        // update the remaining pixels in the row ...
        for X := RecLeft + 1 to RecRight do
        begin
          Inc(ImagePixel);
          LL := X - RadiusI -1;
          RR := X + RadiusI;
          if LL >= RecLeft then
            with Pixels[LL] do
            begin
              Dec(SumRec.A, A);
              Dec(SumRec.R, R);
              Dec(SumRec.G, G);
              Dec(SumRec.B, B);
              Dec(SumRec.Sum, Sum);
            end;
          if RR <= RecRight then
            with Pixels[RR] do
            begin
              Inc(SumRec.A, A);
              Inc(SumRec.R, R);
              Inc(SumRec.G, G);
              Inc(SumRec.B, B);
              Inc(SumRec.Sum, Sum);
            end;

          MaskClr.ARGB := Mask.Pixel[X - RecLeft, Y - RecTop];
          with ImagePixel^ do
            if (SumRec.Sum > 0) and (MaskClr.A = 255) then
            begin
              A := SumRec.A div SumRec.Sum;
              R := SumRec.R div SumRec.Sum;
              G := SumRec.G div SumRec.Sum;
              B := SumRec.B div SumRec.Sum;
            end;
        end;
      end;

      // vertical pass...
      for X := RecLeft to RecRight do
      begin
        // fill the Pixels buffer with a copy of the col's pixels ...
        ImagePixel := PColor32Entry(@Bmp32.ScanLine[RecTop][X]);
        for J := RecTop to RecBottom do
        begin
          MaskClr.ARGB := Mask.Pixel[X - RecLeft, J - RecTop];
          if (MaskClr.A = 0) then
          begin
            Pixels[J].A := 0;
            Pixels[J].R := 0;
            Pixels[J].G := 0;
            Pixels[J].B := 0;
            Pixels[J].Sum := 0;
          end else
          with ImagePixel^ do
          begin
            Pixels[J].A := A;
            Pixels[J].R := R;
            Pixels[J].G := G;
            Pixels[J].B := B;
            Pixels[J].Sum := 1;
          end;
          Inc(ImagePixel, Bmp32.Width);
        end;
        ImagePixel := PColor32Entry(@Bmp32.ScanLine[RecTop][X]);

        TT := RecTop;
        BB := RecTop + RadiusI;
        if BB > RecBottom then BB := RecBottom;
        SumRec.A := 0; SumRec.R := 0; SumRec.G := 0;
        SumRec.B := 0; SumRec.Sum := 0;
        // update first in col ...
        for YY := TT to BB do
          with Pixels[YY] do
          begin
            Inc(SumRec.A, A);
            Inc(SumRec.R, R);
            Inc(SumRec.G, G);
            Inc(SumRec.B, B);
            Inc(SumRec.Sum, Sum);
          end;
        MaskClr.ARGB := Mask.Pixel[X - RecLeft, 0];
        if (MaskClr.A > 0) and (SumRec.Sum > 0) then
          with ImagePixel^ do
          begin
            A := SumRec.A div SumRec.Sum;
            R := SumRec.R div SumRec.Sum;
            G := SumRec.G div SumRec.Sum;
            B := SumRec.B div SumRec.Sum;
          end;
        // update remainder in col ...
        for Y := RecTop + 1 to RecBottom do
        begin
          Inc(ImagePixel, Bmp32.Width);
          TT := Y - RadiusI -1;
          BB := Y + RadiusI;

          if TT >= RecTop then
            with Pixels[TT] do
            begin
              Dec(SumRec.A, A);
              Dec(SumRec.R, R);
              Dec(SumRec.G, G);
              Dec(SumRec.B, B);
              Dec(SumRec.Sum, Sum);
            end;
          if BB <= RecBottom then
            with Pixels[BB] do
            begin
              Inc(SumRec.A, A);
              Inc(SumRec.R, R);
              Inc(SumRec.G, G);
              Inc(SumRec.B, B);
              Inc(SumRec.Sum, Sum);
            end;
          MaskClr.ARGB := Mask.Pixel[X - RecLeft, Y - RecTop];
          with ImagePixel^ do
            if (SumRec.Sum = 0) or (MaskClr.A = 0) then
              // do nothing
            else if (I = Passes) then
            begin
              Clr.A := SumRec.A div SumRec.Sum;
              Clr.R := SumRec.R div SumRec.Sum;
              Clr.G := SumRec.G div SumRec.Sum;
              Clr.B := SumRec.B div SumRec.Sum;
              BlendMemEx(Clr.ARGB, ARGB, MaskClr.A);
            end
            else if (MaskClr.A = 255) then
            begin
              A := SumRec.A div SumRec.Sum;
              R := SumRec.R div SumRec.Sum;
              G := SumRec.G div SumRec.Sum;
              B := SumRec.B div SumRec.Sum;
            end
        end;
        EMMS;
      end;
    end;

    // extract alphas ...
    for Y := RecTop to RecBottom do
    begin
      ImgPixel := PColor32Entry(Bmp32.ScanLine[Y]);
      Inc(ImgPixel, RecLeft);
      for X := RecLeft to RecRight do
      begin
        ImgPixel.R := RcTable[ImgPixel.A, ImgPixel.R];
        ImgPixel.G := RcTable[ImgPixel.A, ImgPixel.G];
        ImgPixel.B := RcTable[ImgPixel.A, ImgPixel.B];
        Inc(ImgPixel);
      end;
    end;
  finally
    Mask.Free;
  end;
end;

procedure MotionBlur(Bmp32: TBitmap32; Dist, AngleDeg: TFloat;
  const Rec: TRect; Bidirectional: Boolean = true);
var
  Pts: TArrayOfFloatPoint;
begin
  SetLength(Pts, 4);
  with Rec do
  begin
    Pts[0] := FloatPoint(Left, Top);
    Pts[1] := FloatPoint(Right, Top);
    Pts[2] := FloatPoint(Right, Bottom);
    Pts[3] := FloatPoint(Left, Bottom);
  end;
  MotionBlur(Bmp32, Dist, AngleDeg, Pts, Bidirectional);
end;

procedure MotionBlur(Bmp32: TBitmap32;
  Dist, AngleDeg: TFloat; Bidirectional: Boolean = true);
var
  Pts: TArrayOfFloatPoint;
begin
  SetLength(Pts, 4);
  with Bmp32.BoundsRect do
  begin
    Pts[0] := FloatPoint(Left, Top);
    Pts[1] := FloatPoint(Right, Top);
    Pts[2] := FloatPoint(Right, Bottom);
    Pts[3] := FloatPoint(Left, Bottom);
  end;
  MotionBlur(Bmp32, Dist, AngleDeg, Pts, Bidirectional);
end;

procedure MotionBlur(Bmp32: TBitmap32; Dist, AngleDeg: TFloat;
  const BlurRegion: TArrayOfFloatPoint; Bidirectional: Boolean = true);
var
  LL, RR, XX, I, X, Y, RadiusI, Passes: Integer;
  ImagePixel, ImagePixel2, ImagePixel3: PColor32Entry;
  ImagePixels, ImagePixels2: PColor32EntryArray;
  SumRec: TSumRecord;
  Pixels: array of TSumRecord;
  Mask: TBitmap32;
  Clr, MaskClr: TColor32Entry;
  Pts: TArrayOfFloatPoint;
  Rec: TRect;
  Dx, Dy: Double;
  Affine: TAffineTransformation;
  BmpCutout: TBitmap32;
  BmpRotated: TBitmap32;
  PrevIsBlank, ThisIsBlank: boolean;
begin
  if Dist < 1 then
    Exit
  else if Dist > 256 then
    Dist := 256;

  RadiusI := Round(Sqrt(-Dist * Dist / (2 * ln(1 / 255))));
  if RadiusI < 2 then
  begin
    Passes := Round(Dist);
    RadiusI := 1;
  end else
    Passes := 3;


  with PolygonBounds(BlurRegion) do
    Rec := Rect(Floor(Left), Floor(Top), Ceil(Right), Ceil(Bottom));
  Rec.Left := Max(Rec.Left, 0);
  Rec.Top := Max(Rec.Top, 0);
  Rec.Right := Min(Rec.Right, Bmp32.Width -1);
  Rec.Bottom := Min(Rec.Bottom, Bmp32.Height -1);

  Affine := TAffineTransformation.Create;
  BmpCutout := TBitmap32.create;
  BmpRotated := TBitmap32.create;
  BmpRotated.ResamplerClassName := 'TLinearResampler';
  Mask := TBitmap32.Create;
  try
    // copy the region to be blurred into the BmpCutout image buffer ...
    BmpCutout.SetSize(Rec.Right - Rec.Left, Rec.Bottom - Rec.Top);
    for Y := 0 to BmpCutout.Height -1 do
    begin
      ImagePixel := PColor32Entry(@Bmp32.ScanLine[Y + Rec.Top][Rec.Left]);
      ImagePixel2 := PColor32Entry(BmpCutout.ScanLine[Y]);
      MoveLongword(ImagePixel^, ImagePixel2^, BmpCutout.Width);
    end;

    // pre-multiply alphas in BmpCutout ...
    for Y := 0 to BmpCutout.Height -1 do
    begin
      ImagePixel := PColor32Entry(BmpCutout.ScanLine[Y]);
      for X := 0 to BmpCutout.Width -1 do
      begin
        ImagePixel.R := DivTable[ImagePixel.R, ImagePixel.A];
        ImagePixel.G := DivTable[ImagePixel.G, ImagePixel.A];
        ImagePixel.B := DivTable[ImagePixel.B, ImagePixel.A];
        inc(ImagePixel);
      end;
    end;

    // Rotate BmpCutout into BmpRotated ...
    Affine.SrcRect := FloatRect(BmpCutout.BoundsRect);
    Affine.Rotate(180 - AngleDeg);
    with Affine.GetTransformedBounds do
    begin
      Mask.SetSize(Round(Right - Left) +1, Round(Bottom - Top) +1);
      BmpRotated.SetSize(Mask.Width, Mask.Height);
      Dx := Left; Dy := Top;
      Affine.Translate(-Dx, -Dy);
    end;
    transform(BmpRotated, BmpCutout, Affine);

    // Create a rotated mask ...
    Affine.Clear;
    Affine.Translate(-Rec.Left, -Rec.Top);
    Affine.SrcRect := FloatRect(BmpCutout.BoundsRect);
    Affine.Rotate(180 - AngleDeg);
    Affine.Translate(-Dx, -Dy);
    Pts := TransformPolygon(BlurRegion, Affine);
    PolygonFS(Mask, Pts, clWhite32);
    SetLength(Pixels, BmpRotated.Width);

    // Now blur horizontally the rotated image ...
    for I := 1 to Passes do
      // Horizontal blur only ...
      for Y := 0 to BmpRotated.Height -1 do
      begin
        ImagePixel := PColor32Entry(BmpRotated.ScanLine[Y]);
        // fill the Pixels buffer with a copy of the row's pixels ...
        for X := 0 to BmpRotated.Width -1 do
        begin
          MaskClr.ARGB := Mask.Pixel[X, Y];
          if (MaskClr.A = 0) then
          begin
            Pixels[X].A := 0;
            Pixels[X].R := 0;
            Pixels[X].G := 0;
            Pixels[X].B := 0;
            Pixels[X].Sum := 0;
          end else
          with ImagePixel^ do
          begin
            Pixels[X].A := A;
            Pixels[X].R := R;
            Pixels[X].G := G;
            Pixels[X].B := B;
            Pixels[X].Sum := 1;
          end;
          Inc(ImagePixel);
        end;

        LL := 0;
        RR := RadiusI;
        if RR >= BmpRotated.Width then RR := BmpRotated.Width -1;
        SumRec.A := 0; SumRec.R := 0; SumRec.G := 0;
        SumRec.B := 0; SumRec.Sum := 0;
        // update first in row ...
        for XX := LL to RR do
          with Pixels[XX] do
          begin
            Inc(SumRec.A, A);
            Inc(SumRec.R, R);
            Inc(SumRec.G, G);
            Inc(SumRec.B, B);
            Inc(SumRec.Sum, Sum);
          end;

        ImagePixel := PColor32Entry(BmpRotated.ScanLine[Y]);
        MaskClr.ARGB := Mask.Pixel[0, Y];
        if (MaskClr.A > 0) and (SumRec.Sum > 0) then
          with ImagePixel^ do
          begin
            A := SumRec.A div SumRec.Sum;
            R := SumRec.R div SumRec.Sum;
            G := SumRec.G div SumRec.Sum;
            B := SumRec.B div SumRec.Sum;
          end;

        // update the remaining pixels in the row ...
        for X := 1 to BmpRotated.Width -1 do
        begin
          Inc(ImagePixel);
          if Bidirectional then
            LL := X - RadiusI -1
          else
            LL := X - 1;
          RR := X + RadiusI;
          if LL >= 0 then
            with Pixels[LL] do
            begin
              Dec(SumRec.A, A);
              Dec(SumRec.R, R);
              Dec(SumRec.G, G);
              Dec(SumRec.B, B);
              Dec(SumRec.Sum, Sum);
            end;
          if RR < BmpRotated.Width then
            with Pixels[RR] do
            begin
              Inc(SumRec.A, A);
              Inc(SumRec.R, R);
              Inc(SumRec.G, G);
              Inc(SumRec.B, B);
              Inc(SumRec.Sum, Sum);
            end;

          MaskClr.ARGB := Mask.Pixel[X, Y];

          with ImagePixel^ do
            if (SumRec.Sum = 0) or (MaskClr.A = 0) then
              continue
            else if (I = Passes) then
            begin
              Clr.A := SumRec.A div SumRec.Sum;
              Clr.R := SumRec.R div SumRec.Sum;
              Clr.G := SumRec.G div SumRec.Sum;
              Clr.B := SumRec.B div SumRec.Sum;
              BlendMemEx(Clr.ARGB, ARGB, MaskClr.A);
            end
            else if (MaskClr.A = 255) then
            begin
              A := SumRec.A div SumRec.Sum;
              R := SumRec.R div SumRec.Sum;
              G := SumRec.G div SumRec.Sum;
              B := SumRec.B div SumRec.Sum;
            end;
        end;
        EMMS;
      end;

    // un-rotate the now blurred image back into BmpCutout ...
    Affine.Clear;
    Affine.SrcRect := FloatRect(BmpRotated.BoundsRect);
    Affine.Translate(Dx, Dy);
    Affine.Rotate(AngleDeg + 180);
    transform(BmpCutout, BmpRotated, Affine);

    // extract alphas ...
    for Y := 0 to BmpCutout.Height -1 do
    begin
      ImagePixel := PColor32Entry(BmpCutout.ScanLine[Y]);
      for X := 0 to BmpCutout.Width -1 do
      begin
        ImagePixel.R := RcTable[ImagePixel.A, ImagePixel.R];
        ImagePixel.G := RcTable[ImagePixel.A, ImagePixel.G];
        ImagePixel.B := RcTable[ImagePixel.A, ImagePixel.B];
        Inc(ImagePixel);
      end;
    end;

    // Create an un-rotated mask and copy masked pixels from BmpCutout
    // back to the original image (Bmp32) ...
    Mask.SetSize(BmpCutout.Width, BmpCutout.Height);
    Pts := TranslatePolygon(BlurRegion, -Rec.Left, - Rec.Top);
    PolygonFS(Mask, Pts, clWhite32);

    for Y := 0 to BmpCutout.Height -1 do
    begin
      ImagePixel := PColor32Entry(BmpCutout.ScanLine[Y]);
      ImagePixel2 := PColor32Entry(Mask.ScanLine[Y]);
      ImagePixel3 := PColor32Entry(@Bmp32.ScanLine[Y + Rec.Top][Rec.Left]);
      for X := 0 to BmpCutout.Width -1 do
      begin
        if ImagePixel2.A > 0 then
          ImagePixel3.ARGB := ImagePixel.ARGB;
        Inc(ImagePixel);
        Inc(ImagePixel2);
        Inc(ImagePixel3);
      end;
    end;

  finally
    Affine.Free;
    BmpCutout.Free;
    BmpRotated.Free;
    Mask.Free;
  end;
end;

end.
