unit GR32.ImageFormats.TWICImage;

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
 * The Original Code is image format support for Graphics32
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
  Classes,
  Graphics,
  SysUtils,
{$ifndef FPC}
  Consts,
{$endif FPC}
  GR32,
  GR32.ImageFormats.TGraphic,
  GR32.ImageFormats;

//------------------------------------------------------------------------------
//
//      TImageFormatAdapterTWICImage
//
//------------------------------------------------------------------------------
// Implements IImageFormatAdapter for the TIFF file format via the TWICImage
// class.
//------------------------------------------------------------------------------
type
  TImageFormatAdapterTWICImage = class(TImageFormatReaderTGraphic,
    IImageFormatAdapter)
  strict protected
    // IImageFormatAdapter
    function AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean; override;
  strict protected
    // IImageFormatReader
    function CanLoadFromStream(AStream: TStream): boolean; override;
  end;

const
{$if defined(DynArrayOps)}
  FileSignatureTIFFLittle: TBytes               = [$49, $49, $2A, $00]; // Little endian
  FileSignatureTIFFBig: TBytes                  = [$4D, $4D, $00, $2A]; // Big endian
  FileSignatureTIFFMask: TBytes                 = [$ff, $ff, $ff, $ff];
{$else}
  FileSignatureTIFFLittle: array[0..3] of byte  = ($49, $49, $2A, $00); // Little endian
  FileSignatureTIFFBig: array[0..3] of byte     = ($4D, $4D, $00, $2A); // Big endian
  FileSignatureTIFFMask: array[0..3] of byte    = ($ff, $ff, $ff, $ff);
{$ifend}


//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatAdapterTWICImage.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
begin
{$IFDEF PLATFORM_INDEPENDENT}
  Result := inherited;
{$ELSE}
  if (not (Source is TWICImage)) then
    Exit(False);

  Result := True;

  AssignFromGraphicPlain(Dest, TGraphic(Source), 0, False);
{$ENDIF}
end;


//------------------------------------------------------------------------------
// IImageFormatReader
//------------------------------------------------------------------------------
function TImageFormatAdapterTWICImage.CanLoadFromStream(AStream: TStream): boolean;
begin
  // TWICImage does not implement CanLoadFromStream
  Result := CheckFileSignature(AStream, FileSignatureTIFFLittle, FileSignatureTIFFMask) or
    CheckFileSignature(AStream, FileSignatureTIFFBig, FileSignatureTIFFMask);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
{$IFNDEF PLATFORM_INDEPENDENT}
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(
    TImageFormatAdapterTWICImage.Create(TWICImage, SVTIFFImages, ['tif', 'tiff']),
    ImageFormatPriorityNormal);
{$ENDIF}
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

