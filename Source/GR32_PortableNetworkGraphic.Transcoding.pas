unit GR32_PortableNetworkGraphic.Transcoding;

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
 * The Original Code is GR32PNG for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Christian-W. Budde
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}
{$include GR32_PngCompilerSwitches.inc}

uses
  Classes,
  SysUtils,
  GR32_PortableNetworkGraphic.Types,
  GR32_PortableNetworkGraphic.Encoding,
  GR32_PortableNetworkGraphic.Chunks,
  GR32_PortableNetworkGraphic.Chunks.PLTE,
  GR32_PortableNetworkGraphic.Chunks.gAMA,
  GR32_PortableNetworkGraphic.Chunks.tRNS;

//------------------------------------------------------------------------------
//
//      TCustomPngTranscoder
//
//------------------------------------------------------------------------------
type
  TCustomPngTranscoder = class abstract(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); override;

  public
    constructor Create(Stream: TStream; Header: TPngChunkImageHeader;
      Gamma: TPngChunkGamma = nil; Palette: TPngChunkPalette = nil;
      Transparency: TCustomPngTransparency = nil); override;
    destructor Destroy; override;

    procedure Transcode; virtual; abstract;
  end;

  TCustomPngTranscoderClass = class of TCustomPngTranscoder;


//------------------------------------------------------------------------------
//
//      TPngNonInterlacedToAdam7Transcoder
//
//------------------------------------------------------------------------------
type
  TPngNonInterlacedToAdam7Transcoder = class(TCustomPngTranscoder)
  public
    procedure Transcode; override;
  end;


//------------------------------------------------------------------------------
//
//      TPngAdam7ToNonInterlacedTranscoder
//
//------------------------------------------------------------------------------
type
  TPngAdam7ToNonInterlacedTranscoder = class(TCustomPngTranscoder)
  public
    procedure Transcode; override;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

const
  CRowStart        : array[0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  CColumnStart     : array[0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  CRowIncrement    : array[0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  CColumnIncrement : array[0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);

//------------------------------------------------------------------------------
//
//      TCustomPngTranscoder
//
//------------------------------------------------------------------------------
constructor TCustomPngTranscoder.Create(Stream: TStream;
  Header: TPngChunkImageHeader; Gamma: TPngChunkGamma = nil;
  Palette: TPngChunkPalette = nil; Transparency: TCustomPngTransparency = nil);
begin
  inherited;
  GetMem(FRowBuffer[0], FHeader.BytesPerRow + 1);
  GetMem(FRowBuffer[1], FHeader.BytesPerRow + 1);
end;

destructor TCustomPngTranscoder.Destroy;
begin
  Dispose(FRowBuffer[0]);
  Dispose(FRowBuffer[1]);
  inherited;
end;

procedure TCustomPngTranscoder.DecodeFilterRow(
  FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
begin
  case FilterMethod of
    afmNone    : ;

    afmSub     : DecodeFilterSub(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmUp      : DecodeFilterUp(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmAverage : DecodeFilterAverage(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmPaeth   : DecodeFilterPaeth(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
  else
    raise EPngError.Create(RCStrUnsupportedFilter);
  end;
end;

procedure TCustomPngTranscoder.EncodeFilterRow(CurrentRow, PreviousRow,
  OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer);
var
  PixelIndex : Integer;
  CurrentSum : Cardinal;
  BestSum    : Cardinal;
begin
  BestSum := 0;
  OutputRow^[0] := 0;
  for PixelIndex := 1 to BytesPerRow do
    BestSum := BestSum + CurrentRow[PixelIndex];
  Move(CurrentRow^[1], OutputRow^[1], BytesPerRow);

  // check whether sub pre filter shall be used
  if aafmSub in FHeader.AdaptiveFilterMethods then
  begin
    // calculate sub filter
    EncodeFilterSub(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if sub filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 1;
    end;
  end;

  // check whether up pre filter shall be used
  if aafmUp in FHeader.AdaptiveFilterMethods then
  begin
    // calculate up filter
    EncodeFilterUp(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if up filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 2;
    end;
  end;

  // check whether average pre filter shall be used
  if aafmAverage in FHeader.AdaptiveFilterMethods then
  begin
    // calculate average filter
    EncodeFilterAverage(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if average filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 3;
    end;
  end;

  // check whether paeth pre filter shall be used
  if aafmPaeth in FHeader.AdaptiveFilterMethods then
  begin
    // calculate paeth filter
    EncodeFilterPaeth(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if paeth filter is the current best filter
    if CurrentSum < BestSum then
    begin
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 4;
    end;
  end;
end;


//------------------------------------------------------------------------------
//
//      TPngNonInterlacedToAdam7Transcoder
//
//------------------------------------------------------------------------------
procedure TPngNonInterlacedToAdam7Transcoder.Transcode;
var
  CurrentRow    : Integer;
  RowByteSize   : Integer;
  PixelPerRow   : Integer;
  PixelByteSize : Integer;
  CurrentPass   : Integer;
  Index         : Integer;
  PassRow       : Integer;
  Source        : PByte;
  Destination   : PByte;
  TempData      : PByteArray;
  OutputRow     : PByteArray;
  TempBuffer    : PByteArray;
begin
  // initialize variables
  CurrentRow := 0;
  PixelByteSize := FHeader.PixelByteSize;

  GetMem(TempData, FHeader.Height * FHeader.BytesPerRow);
  Destination := PByte(TempData);
  try

    ///////////////////////////////////
    // decode image (non-interlaced) //
    ///////////////////////////////////

    // clear previous row
    FillChar(FRowBuffer[1 - CurrentRow]^[0], FHeader.BytesPerRow + 1, 0);

    for Index := 0 to FHeader.Height - 1 do
    begin
      // read data from stream
      if FStream.Read(FRowBuffer[CurrentRow][0], FHeader.BytesPerRow + 1) <> FHeader.BytesPerRow + 1 then
        raise EPngError.Create(RCStrDataIncomplete);

      // filter current row
      DecodeFilterRow(TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]),
        FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow], FHeader.BytesPerRow,
        PixelByteSize);

      // transfer data from row to temp data
      Move(FRowBuffer[CurrentRow][1], Destination^, PixelByteSize * FHeader.Width);
      Inc(Destination, FHeader.Width * PixelByteSize);

      // flip current row
      CurrentRow := 1 - CurrentRow;
    end;

    // reset position to zero
    FStream.Position := 0;

    // The Adam7 interlacer uses 7 passes to create the complete image
    for CurrentPass := 0 to 6 do
    begin
      // calculate some intermediate variables
      PixelPerRow := (FHeader.Width - CColumnStart[CurrentPass] +
        CColumnIncrement[CurrentPass] - 1) div CColumnIncrement[CurrentPass];

      case FHeader.ColorType of
        ctGrayscale      : RowByteSize := (PixelPerRow * FHeader.BitDepth + 7) div 8;
        ctIndexedColor   : RowByteSize := (PixelPerRow * FHeader.BitDepth + 7) div 8;
        ctTrueColor      : RowByteSize := (PixelPerRow * FHeader.BitDepth * 3) div 8;
        ctGrayscaleAlpha : RowByteSize := (PixelPerRow * FHeader.BitDepth * 2) div 8;
        ctTrueColorAlpha : RowByteSize := (PixelPerRow * FHeader.BitDepth * 4) div 8;
      else
        Continue;
      end;

      PassRow := CRowStart[CurrentPass];

      // clear previous row
      FillChar(FRowBuffer[1 - CurrentRow]^[0], RowByteSize + 1, 0);

      // check if pre filter is used and eventually calculate pre filter
      if (FHeader.ColorType <> ctIndexedColor) and
        not (FHeader.AdaptiveFilterMethods = []) then
      begin
        GetMem(OutputRow, RowByteSize + 1);
        GetMem(TempBuffer, RowByteSize + 1);
        try
          while PassRow < FHeader.Height do
          begin
            Index := CColumnStart[CurrentPass];
            Source := @TempData[PassRow * FHeader.BytesPerRow + Index * PixelByteSize];
            Destination := @FRowBuffer[CurrentRow][1];

            repeat
              // copy bytes per pixels
              Move(Source^, Destination^, PixelByteSize);

              Inc(Source, CColumnIncrement[CurrentPass] * PixelByteSize);
              Inc(Destination, PixelByteSize);
              Inc(Index, CColumnIncrement[CurrentPass]);
            until Index >= FHeader.Width;

            // filter current row
            EncodeFilterRow(FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow],
              OutputRow, TempBuffer, RowByteSize, FHeader.PixelByteSize);
            Assert(OutputRow[0] in [0..4]);

            // write data to data stream
            FStream.Write(OutputRow[0], RowByteSize + 1);

            // prepare for the next pass
            Inc(PassRow, CRowIncrement[CurrentPass]);
            CurrentRow := 1 - CurrentRow;
          end;
        finally
          Dispose(OutputRow);
          Dispose(TempBuffer);
        end;
      end
      else
        while PassRow < FHeader.Height do
        begin
          Index := CColumnStart[CurrentPass];
          Source := @TempData[PassRow * FHeader.BytesPerRow + Index * PixelByteSize];
          Destination := @FRowBuffer[CurrentRow][1];

          repeat
            // copy bytes per pixels
            Move(Source^, Destination^, PixelByteSize);

            Inc(Source, CColumnIncrement[CurrentPass] * PixelByteSize);
            Inc(Destination, PixelByteSize);
            Inc(Index, CColumnIncrement[CurrentPass]);
          until Index >= FHeader.Width;

          // set filter method 0
          FRowBuffer[CurrentRow][0] := 0;

          // write data to data stream
          FStream.Write(FRowBuffer[CurrentRow][0], RowByteSize + 1);

          // prepare for the next pass
          Inc(PassRow, CRowIncrement[CurrentPass]);
          CurrentRow := 1 - CurrentRow;
        end;
    end;
  finally
    Dispose(TempData);
  end;
end;


//------------------------------------------------------------------------------
//
//      TPngAdam7ToNonInterlacedTranscoder
//
//------------------------------------------------------------------------------
procedure TPngAdam7ToNonInterlacedTranscoder.Transcode;
var
  CurrentRow    : Integer;
  RowByteSize   : Integer;
  PixelPerRow   : Integer;
  PixelByteSize : Integer;
  CurrentPass   : Integer;
  Index         : Integer;
  PassRow       : Integer;
  Source        : PByte;
  Destination   : PByte;
  TempData      : PByteArray;
  OutputRow     : PByteArray;
  TempBuffer    : PByteArray;
begin
  // initialize variables
  CurrentRow := 0;
  PixelByteSize := FHeader.PixelByteSize;

  GetMem(TempData, FHeader.Height * FHeader.BytesPerRow);
  try

    /////////////////////////////////////
    // decode image (Adam7-interlaced) //
    /////////////////////////////////////

    // The Adam7 deinterlacer uses 7 passes to create the complete image
    for CurrentPass := 0 to 6 do
    begin
      // calculate some intermediate variables
      PixelPerRow := (FHeader.Width - CColumnStart[CurrentPass] +
        CColumnIncrement[CurrentPass] - 1) div CColumnIncrement[CurrentPass];

      case FHeader.ColorType of
        ctGrayscale      : RowByteSize := (PixelPerRow * FHeader.BitDepth + 7) div 8;
        ctIndexedColor   : RowByteSize := (PixelPerRow * FHeader.BitDepth + 7) div 8;
        ctTrueColor      : RowByteSize := (PixelPerRow * FHeader.BitDepth * 3) div 8;
        ctGrayscaleAlpha : RowByteSize := (PixelPerRow * FHeader.BitDepth * 2) div 8;
        ctTrueColorAlpha : RowByteSize := (PixelPerRow * FHeader.BitDepth * 4) div 8;
      else
        Continue;
      end;

      PassRow := CRowStart[CurrentPass];

      // clear previous row
      FillChar(FRowBuffer[1 - CurrentRow]^[0], RowByteSize, 0);

      // process pixels
      while PassRow < FHeader.Height do
      begin
        // get interlaced row data
        if FStream.Read(FRowBuffer[CurrentRow][0], RowByteSize + 1) <> (RowByteSize + 1) then
          raise EPngError.Create(RCStrDataIncomplete);

        DecodeFilterRow(TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]),
          FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow], RowByteSize, PixelByteSize);

        Index := CColumnStart[CurrentPass];
        Source := @FRowBuffer[CurrentRow][1];
        Destination := @TempData[PassRow * FHeader.BytesPerRow + Index * PixelByteSize];
        repeat
          // copy bytes per pixels
          Move(Source^, Destination^, PixelByteSize);

          Inc(Source, PixelByteSize);
          Inc(Destination, CColumnIncrement[CurrentPass] * PixelByteSize);
          Inc(Index, CColumnIncrement[CurrentPass]);
        until Index >= FHeader.Width;

        // prepare for the next pass
        Inc(PassRow, CRowIncrement[CurrentPass]);
        CurrentRow := 1 - CurrentRow;
      end;
    end;


    // reset position to zero
    FStream.Position := 0;


    /////////////////////////////////
    // encode image non-interlaced //
    /////////////////////////////////

    // clear previous row buffer
    FillChar(FRowBuffer[1 - CurrentRow]^[0], FHeader.BytesPerRow, 0);
    Source := PByte(TempData);

    // check if pre filter is used and eventually calculate pre filter
    if (FHeader.ColorType <> ctIndexedColor) and
      not (FHeader.AdaptiveFilterMethods = []) then
    begin
      GetMem(OutputRow, FHeader.BytesPerRow + 1);
      GetMem(TempBuffer, FHeader.BytesPerRow + 1);
      try
        for Index := 0 to FHeader.Height - 1 do
        begin
          // copy bytes per pixels
          Move(Source^, FRowBuffer[CurrentRow][1], FHeader.Width * PixelByteSize);
          Inc(Source, FHeader.Width * PixelByteSize);

          // filter current row
          EncodeFilterRow(FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow],
            OutputRow, TempBuffer, FHeader.BytesPerRow, FHeader.PixelByteSize);

          // write data to data stream
          FStream.Write(OutputRow[0], FHeader.BytesPerRow + 1);

          // flip current row used
          CurrentRow := 1 - CurrentRow;
        end;
      finally
        Dispose(OutputRow);
        Dispose(TempBuffer);
      end;
    end
    else
      for Index := 0 to FHeader.Height - 1 do
      begin
        // copy bytes per pixels
        Move(Source^, FRowBuffer[CurrentRow][1], FHeader.Width * PixelByteSize);
        Inc(Source, FHeader.Width * PixelByteSize);

        // set filter method to none
        FRowBuffer[CurrentRow][0] := 0;

        // write data to data stream
        FStream.Write(FRowBuffer[CurrentRow][0], FHeader.BytesPerRow + 1);

        // flip current row used
        CurrentRow := 1 - CurrentRow;
      end;

  finally
    Dispose(TempData);
  end;
end;

end.
