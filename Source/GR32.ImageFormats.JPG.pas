unit GR32.ImageFormats.JPG;

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
 * The Original Code is Jpeg Image Format support for Graphics32
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
{$if defined(DynArrayOps)}
  SysUtils,
{$ifend}
{$ifdef FPC}
  Graphics,
{$else FPC}
  Jpeg,
  JConsts,
{$endif FPC}
  GR32.ImageFormats,
  GR32.ImageFormats.TGraphic;

const
{$if defined(DynArrayOps)}
  FileSignatureJPEG: TBytes = [
                                $FF, $D8,               // SOI marker
                                $FF, $E0,               // JFIF-APP0 marker
                                $00, $00,               // Length (masked out)
                                $4A, $46, $49, $46, $00,// 'JFIF'#0
                                $01];                   // Major version: 1

  FileSignatureJPEGMask: TBytes = [
                                $ff, $ff,
                                $ff, $ff,
                                $00, $00,
                                $ff, $ff, $ff, $ff, $ff,
                                $ff];
{$else}
  FileSignatureJPEG: array[0..11] of byte = (
                                $FF, $D8,               // SOI marker
                                $FF, $E0,               // JFIF-APP0 marker
                                $00, $00,               // Length (masked out)
                                $4A, $46, $49, $46, $00,// 'JFIF'#0
                                $01);                   // Major version: 1

  FileSignatureJPEGMask: array[0..11] of byte = (
                                $ff, $ff,
                                $ff, $ff,
                                $00, $00,
                                $ff, $ff, $ff, $ff, $ff,
                                $ff);
{$ifend}

{$ifdef FPC}
resourcestring
  sJPEGImageFile = 'JPEG Image File';
{$endif FPC}

type
  TImageFormatAdapterJPEG = class(TImageFormatReaderWriterTGraphic)
  strict protected
    // IImageFormatReader
    function CanLoadFromStream(AStream: TStream): boolean; override;
  end;

function TImageFormatAdapterJPEG.CanLoadFromStream(AStream: TStream): boolean;
begin
{$ifdef LOADFROMSTREAM}
  Result := inherited;
{$else LOADFROMSTREAM}
  Result := CheckFileSignature(AStream, FileSignatureJPEG, FileSignatureJPEGMask);
{$endif LOADFROMSTREAM}
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(
    TImageFormatAdapterJPEG.Create(TJPEGImage, sJPEGImageFile, ['jpg', 'jpeg']),
    ImageFormatPriorityNormal);
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

