unit GR32.ImageFormats.PNG;

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
 * The Original Code is PNG Image Format support for Graphics32
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

{$ifdef FPC}
{$message WARN 'GR32.ImageFormats.PNG does not support FreePascal. Use the GR32.ImageFormats.PNG32 unit instead'}
{$endif FPC}


implementation

{$ifdef FPC}
// Make sure ImageFormats.PNG32 is referenced so the adapters are registered.
// Beyond that, this unit does nothing on FPC.
uses
  GR32.ImageFormats.PNG32;
{$else FPC}

uses
  Classes,
  PngImage,
  Graphics,
  GR32,
  GR32.ImageFormats;

const
  PngSignature: AnsiString        = #$89#$50#$4e#$47#$0d#$0a#$1a#$0a;
  PngSignatureMask: AnsiString    = #$ff#$ff#$ff#$ff#$ff#$ff#$ff#$ff;

//------------------------------------------------------------------------------
//
//      TImageFormatAdapterPNG
//
//------------------------------------------------------------------------------
// Implements IImageFormatAdapter for the PNG image format using the standard
// Delphi TPNGImage class.
//------------------------------------------------------------------------------
type
  TImageFormatAdapterPNG = class(TCustomImageFormatAdapter,
    IImageFormatAdapter,
    IImageFormatFileInfo,
    IImageFormatReader,
    IImageFormatWriter)
  strict protected
    // IImageFormatAdapter
    function CanAssignFrom(Source: TPersistent): boolean; override;
    function AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean; override;
    function CanAssignTo(Dest: TPersistent): boolean; override;
    function AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean; override;
  private
    // IImageFormatFileInfo
    function ImageFormatDescription: string;
    function ImageFormatFileTypes: TFileTypes;
  private
    // IImageFormatReader
    function CanLoadFromStream(AStream: TStream): boolean;
    function LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
  private
    // IImageFormatWriter
    procedure SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
  end;

//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatAdapterPNG.CanAssignFrom(Source: TPersistent): boolean;
begin
  Result := (Source is TPNGImage);
end;

function TImageFormatAdapterPNG.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
var
  Bitmap: TBitmap;
  Row, Col: integer;
  Alpha: PByte;
  Src: PColor32Entry;
  Dst: PColor32Entry;
begin
  if (not (Source is TPNGImage)) then
  begin
    Result := inherited;
    exit;
  end;

  if (TPNGImage(Source).Header.ColorType <> COLOR_RGBALPHA) then
  begin

    // Defer to default assign mechanism via TBitmap
    Bitmap := TBitmap.Create;
    try
      Bitmap.Assign(Source);
      Dest.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;

  end else
  begin

    Bitmap := TBitmap.Create;
    try
      // Unfortunately the conversion from TPNGImage to TBitmap sets the
      // AlphaFormat to afDefined which in turn premultiplies the bitmap.
      // We need it unpremultiplied but changing AlphaFormat to unpremultiply
      // unavoidably loses information. The only way to avoid this is to
      // not use TBitmap...
      Bitmap.Assign(Source);

      // Make sure bitmap is 32-bits
      Bitmap.PixelFormat := pf32bit;
      // Unpremultiply :-(
      Bitmap.AlphaFormat := afIgnored;

      Dest.SetSize(Bitmap.Width, Bitmap.Height);

      // Copy RGB values. We will copy the Alpha separately below.
      Dst := PColor32Entry(Dest.Bits);
      for Row := 0 to Dest.Height-1 do
      begin
        Src := PColor32Entry(Bitmap.Scanline[Row]);
        Move(Src^, Dst^, SizeOf(TColor32)*Dest.Width);
        Inc(Dst, Dest.Width);
      end;
    finally
      Bitmap.Free;
    end;

    // Copy Alpha from PNG
    if (TPNGImage(Source).TransparencyMode = ptmPartial) then
    begin
      Dst := PColor32Entry(Dest.Bits);
      for Row := 0 to Dest.Height-1 do
      begin
        Alpha := PByte(TPNGImage(Source).AlphaScanline[Row]);
        for Col := 0 to Dest.Width-1 do
        begin
          Dst.A := Alpha^;
          Inc(Alpha);
          Inc(Dst);
        end;
      end;
    end;

  end;
  Result := True;
end;

//------------------------------------------------------------------------------

function TImageFormatAdapterPNG.CanAssignTo(Dest: TPersistent): boolean;
begin
  Result := (Dest is TPNGImage);
end;

function TImageFormatAdapterPNG.AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean;
var
  Bitmap: TBitmap;
  Row, Col: integer;
  Dst: PByte;
  Src: PColor32Entry;
begin
  if (not(Dest is TPNGImage)) then
  begin
    Result := inherited;
    exit;
  end;

  // Convert to TPNGImage via TBitmap
  Bitmap := TBitmap.Create;
  try
    Bitmap.Assign(Source);
    TPNGImage(Dest).Assign(Bitmap);
  finally
    Bitmap.Free;
  end;

  // Copy alpha
  TPNGImage(Dest).CreateAlpha;
  Src := PColor32Entry(Source.Bits);
  for Row := 0 to Source.Height-1 do
  begin
    Dst := PByte(TPNGImage(Dest).AlphaScanline[Row]);
    for Col := 0 to TPNGImage(Dest).Width-1 do
    begin
      Dst^ := Src.A;
      inc(Dst);
      inc(Src);
    end;
  end;

  Result := True;
end;

//------------------------------------------------------------------------------
// IImageFormatFileInfo
//------------------------------------------------------------------------------

function TImageFormatAdapterPNG.ImageFormatFileTypes: TFileTypes;
begin
  Result := ['png'];
end;

function TImageFormatAdapterPNG.ImageFormatDescription: string;
resourcestring
  sImageFormatPNGName = 'PNG images';
begin
  Result := sImageFormatPNGName;
end;

//------------------------------------------------------------------------------
// IImageFormatReader
//------------------------------------------------------------------------------
function TImageFormatAdapterPNG.CanLoadFromStream(AStream: TStream): boolean;
begin
{$ifdef LOADFROMSTREAM}
  Result := TPNGImage.CanLoadFromStream(AStream);
{$else LOADFROMSTREAM}
  Result := CheckFileSignature(AStream, FileSignaturePNG, FileSignaturePNGMask);
{$endif LOADFROMSTREAM}
end;

function TImageFormatAdapterPNG.LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
var
  PNGImage: TPNGImage;
begin
  PNGImage := TPNGImage.Create;
  try
    PNGImage.LoadFromStream(AStream);
    ADest.Assign(PNGImage);
  finally
    PNGImage.Free;
  end;
  Result := True;
end;

//------------------------------------------------------------------------------
// IImageFormatWriter
//------------------------------------------------------------------------------
procedure TImageFormatAdapterPNG.SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
var
  PNGImage: TPNGImage;
begin
  PNGImage := TPNGImage.Create;
  try
    PNGImage.Assign(ASource);
    PNGImage.SaveToStream(AStream);
  finally
    PNGImage.Free;
  end;
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
{$endif FPC}

var
  ImageFormatHandle: integer = 0;

initialization
{$ifndef FPC}
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(TImageFormatAdapterPNG.Create, ImageFormatPriorityWorse);
{$endif FPC}
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

