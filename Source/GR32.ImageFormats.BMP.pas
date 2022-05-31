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

// WEAKPACKAGEUNIT so we can include the unit in the GR32 design time
// package in order to have the design time editor support the various formats.
{$WEAKPACKAGEUNIT ON}

interface

implementation

{$I GR32.inc}

uses
  Classes,
  Graphics,
  GR32,
  GR32.ImageFormats;

{$ifndef LOADFROMSTREAM}
const
  FileSignatureBMP : AnsiString    = #$42#$4d;//#$00#$00#$00#$00#$00#$00#$00#$00;  // BM...and then some
  FileSignatureBMPMask: AnsiString = #$ff#$ff;//#$00#$00#$00#$00#$ff#$ff#$ff#$ff;
{$endif LOADFROMSTREAM}

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
    IImageFormatWriter)
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

  TBitmap32Cracker = class(TCustomBitmap32);

//------------------------------------------------------------------------------
// IImageFormatFileInfo
//------------------------------------------------------------------------------

function TImageFormatAdapterBMP.ImageFormatFileTypes: TFileTypes;
begin
  Result := ['bmp', 'dib', 'rle'];
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
{$ifdef LOADFROMSTREAM}
  Result := TBitmap.CanLoadFromStream(AStream);
{$else LOADFROMSTREAM}
  Result := CheckFileSignature(AStream, FileSignatureBMP, FileSignatureBMPMask);
{$endif LOADFROMSTREAM}
end;

function TImageFormatAdapterBMP.LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
begin
  // Use LoadFromBMPStream instead of LoadFromStream to avoid recursion
  Result := TBitmap32Cracker(ADest).LoadFromBMPStream(AStream, AStream.Size);
end;

//------------------------------------------------------------------------------
// IImageFormatWriter
//------------------------------------------------------------------------------
procedure TImageFormatAdapterBMP.SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
begin
  ASource.SaveToStream(AStream);
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  ImageFormatManager.RegisterImageFormat(TImageFormatAdapterBMP.Create, ImageFormatPriorityBest);
end.

