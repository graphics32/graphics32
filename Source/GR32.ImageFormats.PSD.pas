unit GR32.ImageFormats.PSD;

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
 * The Original Code is PSD Image Format support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Lamdalili
 *
 * Portions created by the Initial Developer are Copyright (C) 2023
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Anders Melander <anders@melander.dk>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  SysUtils,
  Classes,
  Math,
  Types,
  GR32,
  GR32_Backends_Generic,
  GR32.ImageFormats,
  GR32.ImageFormats.PSD.Model,
  GR32.ImageFormats.PSD.Writer,
  GR32.ImageFormats.PSD.Reader;

const
{$if defined(DynArrayOps)}
  FileSignaturePsd: TBytes        = [$38, $42, $50, $53, $00, $01]; // '8BPS'#00#01;
  FileSignaturePsdMask: TBytes    = [$ff, $ff, $ff, $ff, $ff, $ff];
{$else}
  FileSignaturePsd: array[0..5] of byte     = ($38, $42, $50, $53, $00, $01); // '8BPS'#00#01;
  FileSignaturePsdMask: array[0..5] of byte = ($ff, $ff, $ff, $ff, $ff, $ff);
{$ifend}

//------------------------------------------------------------------------------
//
//      TImageFormatAdapterPSD
//
//------------------------------------------------------------------------------
// Implements IImageFormatAdapter for the PSD image format using
// TPhotoshopDocument.
//------------------------------------------------------------------------------
type
  TImageFormatAdapterPSD = class(TCustomImageFormatAdapter,
    IImageFormatAdapter,
    IImageFormatFileInfo,
    IImageFormatWriter,
    IImageFormatReader)
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
    // IImageFormatWriter
    procedure SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
  private
    // IImageFormatReader
    function CanLoadFromStream(AStream: TStream): boolean;
    function LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
  end;


//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatAdapterPSD.CanAssignFrom(Source: TPersistent): boolean;
begin
  Result := False;
end;

function TImageFormatAdapterPSD.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
var
  PSD: TPhotoshopDocument;
  i: Integer;
begin
  if (Source is TPhotoshopDocument) then
  begin
    PSD := TPhotoshopDocument(Source);

    Dest.SetSize(PSD.Width, PSD.Height);
    Dest.Clear;

    for i := 0 to PSD.Layers.Count - 1 do
      if PSD.Layers[i] is TCustomPhotoshopBitmapLayer32 then
        TCustomPhotoshopBitmapLayer32(PSD.Layers[i]).Bitmap.DrawTo(Dest, PSD.Layers[i].Left, PSD.Layers[i].Top);

    Result := True;
  end else
    Result := inherited;
end;

//------------------------------------------------------------------------------

function TImageFormatAdapterPSD.CanAssignTo(Dest: TPersistent): boolean;
begin
  Result := (Dest is TPhotoshopDocument);
end;

function TImageFormatAdapterPSD.AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean;
begin
  if (Dest is TPhotoshopDocument) then
  begin
    CreatePhotoshopDocument(Source, TPhotoshopDocument(Dest));
    Result := True;
  end else
    Result := inherited;
end;

//------------------------------------------------------------------------------
// IImageFormatFileInfo
//------------------------------------------------------------------------------

function TImageFormatAdapterPSD.ImageFormatFileTypes: TFileTypes;
begin
{$if defined(DynArrayOps)}
  Result := ['psd'];
{$else}
  MakeFileTypes(['psd']);
{$ifend}
end;

resourcestring
  sImageFormatPSDName = 'PSD images';

function TImageFormatAdapterPSD.ImageFormatDescription: string;
begin
  Result := sImageFormatPSDName;
end;

//------------------------------------------------------------------------------
// IImageFormatReader
//------------------------------------------------------------------------------
function TImageFormatAdapterPSD.CanLoadFromStream(AStream: TStream): boolean;
begin
  Result := CheckFileSignature(AStream, FileSignaturePsd, FileSignaturePsdMask);
end;

function TImageFormatAdapterPSD.LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
var
  PSD: TPhotoshopDocument;
  I: Integer;
begin
  if (not CanLoadFromStream(AStream)) then
    Exit(False);

  PSD := TPhotoshopDocument.Create;
  try
    PhotoshopDocumentReader.LoadFromStream(PSD, AStream);

    ADest.SetSize(PSD.Width, PSD.Height);
    ADest.Clear;

    for I := 0 to PSD.Layers.Count - 1 do
      if PSD.Layers[I] is TCustomPhotoshopBitmapLayer32 then
        TCustomPhotoshopBitmapLayer32(PSD.Layers[I]).Bitmap.DrawTo(ADest, PSD.Layers[I].Left, PSD.Layers[I].Top);
  finally
    PSD.Free;
  end;

  Result := True;
end;

//------------------------------------------------------------------------------
// IImageFormatWriter
//------------------------------------------------------------------------------
procedure TImageFormatAdapterPSD.SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
var
  PSD: TPhotoshopDocument;
begin
  PSD := TPhotoshopDocument.Create;
  try
    CreatePhotoshopDocument(ASource, PSD);
    PhotoshopDocumentWriter.SaveToStream(PSD, AStream);
  finally
    PSD.Free;
  end;
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(TImageFormatAdapterPSD.Create, ImageFormatPriorityNormal);
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

