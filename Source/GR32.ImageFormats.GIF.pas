unit GR32.ImageFormats.GIF;

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
 * The Original Code is GIF support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2022
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

implementation

uses
{$ifdef FPC}
  Graphics,
{$else FPC}
  Classes,
  Generics.Defaults,
  Generics.Collections,
  GIFImg,
  GIFConsts,
  GR32,
{$endif FPC}
  GR32.ImageFormats,
  GR32.ImageFormats.TGraphic;

{$ifdef FPC}
resourcestring
  sGIFImageFile	= 'GIF Image';
{$endif FPC}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

type
{$ifdef FPC}
  // FPC TGIFImage is read-only
  TImageFormatAdapterTGIFImage = TImageFormatReaderTGraphic;
{$else FPC}
  TImageFormatAdapterTGIFImage = class(TImageFormatReaderWriterTGraphic)
  strict protected
    // IImageFormatWriter
    procedure SaveToStream(ASource: TCustomBitmap32; AStream: TStream); override;
  end;
{$endif FPC}


{ TImageFormatAdapterTGIFImage }

{$ifndef FPC}
procedure TImageFormatAdapterTGIFImage.SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
type
  TColorPair = TPair<TColor32, Cardinal>;
var
  Colors: TDictionary<TColor32, Cardinal>;
  Histogram: TArray<TColorPair>;
  ColorMap: TDictionary<TColor32, integer>;
  HasTransparency: boolean;
  i: integer;
  Color: TColor32;
  FirstColorIndex, LastColorIndex: integer;
  TransparetIndex: integer;
  ColorIndex: integer;
  GIF: TGIFImage;
  Frame: TGIFFrame;
  CommentExtension: TGIFCommentExtension;
  GraphicControlExtension: TGIFGraphicControlExtension;
  FramePixel: PByte;
  Count: Cardinal;
const
  MaxColorTableSize = 256; // From 2 to 256
  MinAlpha = 64;
begin
  // Build histogram...
  Colors := TDictionary<TColor32, Cardinal>.Create;
  try
    HasTransparency := False;
    for i := 0 to ASource.Height * ASource.Width - 1 do
    begin
      Color := ASource.Bits[i];

      if (AlphaComponent(Color) < MinAlpha) then
      begin
        HasTransparency := True;
        continue;
      end;

      Color := Color or $FF000000;

      if (Colors.TryGetValue(Color, Count)) then
      begin
        Inc(Count);
        Colors[Color] := Count;
      end else
        Colors.Add(Color, 1);
    end;

    Histogram := Colors.ToArray;

  finally
    Colors.Free;
  end;

  // ...and sort the histogram by count
  TArray.Sort<TColorPair>(Histogram, TComparer<TColorPair>.Construct(
    function(const A, B: TColorPair): integer
    begin
      Result := (B.Value - A.Value);
    end));

  ColorMap := TDictionary<TColor32, integer>.Create;
  try

    // Build color-to-index map
    for i := 0 to High(Histogram) do
      ColorMap.Add(Histogram[i].Key, i);

    GIF := TGIFImage.Create;
    try
      GIF.SetSize(ASource.Width, ASource.Height);

      // Each frame can contain up to 255 colors and a "transparent color".
      FirstColorIndex := 0;

      while (FirstColorIndex <= High(Histogram)) do
      begin
        LastColorIndex := FirstColorIndex + MaxColorTableSize - 2;
        // If first frame has no transparency then there's room for one more actual
        // color in the color map.
        if (FirstColorIndex = 0) and (not HasTransparency) then
          Inc(LastColorIndex);

        if (LastColorIndex > High(Histogram)) then
          LastColorIndex := High(Histogram);

        Frame := TGIFFrame.Create(GIF);
        Frame.Width := GIF.Width;
        Frame.Height := GIF.Height;

        // Add the colors of this frame to the frame color map
        for i := FirstColorIndex to LastColorIndex do
          Frame.ColorMap.Add(WinColor(Histogram[i].Key));

        if (FirstColorIndex = 0) then
        begin
          CommentExtension := TGIFCommentExtension.Create(Frame);
          CommentExtension.Text.Text := 'Generated by Graphics32 via TGIFImage for Delphi';
        end;

        // First frame has no transparency
        if (HasTransparency) or (FirstColorIndex > 0) then
        begin
          // Add transparent color.
          // The actual color doesn't matter. We just need the index of it.
          TransparetIndex := Frame.ColorMap.Add(WinColor(clFuchsia32));

          GraphicControlExtension := TGIFGraphicControlExtension.Create(Frame);
          GraphicControlExtension.Transparent := True;
          GraphicControlExtension.TransparentColorIndex := TransparetIndex;
          GraphicControlExtension.Disposal := dmNoDisposal;
        end else
          // Just default to 0. Pixel will be overwritten by later pixels
          TransparetIndex := 0;

        Frame.ColorMap.Optimized := True;

        FramePixel := Frame.Data;

        for i := 0 to ASource.Height * ASource.Width - 1 do
        begin
          Color := ASource.Bits[i];

          if (AlphaComponent(Color) >= MinAlpha) then
          begin
            ColorIndex := ColorMap[Color or $FF000000]; // Ignore alpha
            if (ColorIndex < FirstColorIndex) or (ColorIndex > LastColorIndex) then
              ColorIndex := TransparetIndex // Transparent in this frame
            else
              Dec(ColorIndex, FirstColorIndex);
          end else
            ColorIndex := TransparetIndex; // Alpha=0 -> Transparent

          FramePixel^ := ColorIndex;

          Inc(FramePixel);
        end;

        FirstColorIndex := LastColorIndex+1;
      end;

      GIF.Optimize([ooCrop]);

      GIF.SaveToStream(AStream);
    finally
      GIF.Free;
    end;

  finally
    ColorMap.Free;
  end;
end;
{$endif FPC}

var
  ImageFormatHandle: integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(
    TImageFormatAdapterTGIFImage.Create(TGIFImage, sGIFImageFile, ['gif']),
    ImageFormatPriorityNormal);
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

