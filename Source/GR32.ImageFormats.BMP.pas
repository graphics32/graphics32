unit GR32.ImageFormats.BMP;

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
 * The Original Code is BMP Image Format support for Graphics32
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
  Windows,
{$else FPC}
  LCLType,
{$endif FPC}
  GR32,
  GR32_Clipboard,
  GR32.ImageFormats;

const
{$if defined(DynArrayOps)}
  FileSignatureBMP: TBytes                  = [$42, $4d];  // 'BM'
  FileSignatureBMPMask: TBytes              = [$ff, $ff];
{$else}
  FileSignatureBMP: array[0..1] of byte     = ($42, $4d);  // 'BM'
  FileSignatureBMPMask: array[0..1] of byte = ($ff, $ff);
{$ifend}

resourcestring
  sImageFormatBMPName = 'Bitmaps';

//------------------------------------------------------------------------------
//
//      TImageFormatAdapterBMP
//
//------------------------------------------------------------------------------
// Implements reader and writer for the BMP image format.
// Uses the built-in bitmap support of TCustomBitmap32.
//------------------------------------------------------------------------------
type
  TImageFormatAdapterBMP = class(TCustomImageFormat,
    IImageFormat,
    IImageFormatFileInfo,
    IImageFormatReader,
    IImageFormatWriter,
    IImageFormatResourceReader,
    IImageFormatClipboardFormat)
  strict private
    // IImageFormatFileInfo
    function ImageFormatDescription: string;
    function ImageFormatFileTypes: TFileTypes;
  strict private
    // IImageFormatReader
    function CanLoadFromStream(AStream: TStream): boolean;
    function LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
  strict private
    // IImageFormatWriter
    procedure SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
  strict private
    // IImageFormatClipboardFormat
    function SupportsClipboardFormat(AFormat: TClipboardFormat): Boolean;
    function PasteFromClipboard(ADest: TCustomBitmap32): boolean;
    function LoadFromClipboardFormat(ADest: TCustomBitmap32; AFormat: TClipboardFormat; AData: THandle; APalette: THandle): boolean;
  strict private
    // IImageFormatResourceReader
    function LoadFromResource(ADest: TCustomBitmap32; AResourceType: TResourceType; AStream: TStream): boolean;
  end;

  TBitmap32Cracker = class(TCustomBitmap32);

//------------------------------------------------------------------------------
// IImageFormatFileInfo
//------------------------------------------------------------------------------

function TImageFormatAdapterBMP.ImageFormatFileTypes: TFileTypes;
begin
{$if defined(DynArrayOps)}
  Result := ['bmp', 'dib', 'rle'];
{$else}
  MakeFileTypes(['bmp', 'dib', 'rle']);
{$ifend}
end;

function TImageFormatAdapterBMP.ImageFormatDescription: string;
begin
  Result := sImageFormatBMPName;
end;

//------------------------------------------------------------------------------
// IImageFormatReader
//------------------------------------------------------------------------------
function TImageFormatAdapterBMP.CanLoadFromStream(AStream: TStream): boolean;
begin
  Result := CheckFileSignature(AStream, FileSignatureBMP, FileSignatureBMPMask);
end;

function TImageFormatAdapterBMP.LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
begin
  // Use LoadFromBMPStream instead of LoadFromStream to avoid recursion
  Result := TBitmap32Cracker(ADest).LoadFromBMPStream(AStream, AStream.Size - AStream.Position);
end;

//------------------------------------------------------------------------------
// IImageFormatWriter
//------------------------------------------------------------------------------
procedure TImageFormatAdapterBMP.SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
begin
  ASource.SaveToStream(AStream);
end;

//------------------------------------------------------------------------------
// IImageFormatClipboard
//------------------------------------------------------------------------------
function TImageFormatAdapterBMP.SupportsClipboardFormat(AFormat: TClipboardFormat): Boolean;
begin
{$ifdef FPC}
  Result := (TPredefinedClipboardFormat(AFormat) = pcfBitmap);
{$else FPC}
  Result := (AFormat in [CF_BITMAP, CF_DIBV5]);
{$endif FPC}
end;

function TImageFormatAdapterBMP.PasteFromClipboard(ADest: TCustomBitmap32): boolean;
begin
  Result := PasteBitmap32FromClipboard(ADest);
end;

function TImageFormatAdapterBMP.LoadFromClipboardFormat(ADest: TCustomBitmap32; AFormat: TClipboardFormat;
  AData: THandle; APalette: THandle): boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------
// IImageFormatResourceReader
//------------------------------------------------------------------------------
function TImageFormatAdapterBMP.LoadFromResource(ADest: TCustomBitmap32; AResourceType: TResourceType;
  AStream: TStream): boolean;
begin
  Result := TBitmap32Cracker(ADest).LoadFromDIBStream(AStream, AStream.Size - AStream.Position);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(TImageFormatAdapterBMP.Create, ImageFormatPriorityBest);
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

