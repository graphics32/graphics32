unit GR32.ImageFormats.SVG;

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
 * The Original Code is SVG Image Format support for Graphics32
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

{$I GR32.inc}

implementation

uses
  Classes,
  Graphics,

  // Image32 must be in your library search path
  Img32,
  Img32.Fmt.SVG,

  GR32,
  GR32.ImageFormats;

resourcestring
  sImageFormatSVGName = 'SVG images';

//------------------------------------------------------------------------------
//
//      TImage32Backend
//
//------------------------------------------------------------------------------
// Minimal TBitmap32 backend using a TImage32 as storage.
//------------------------------------------------------------------------------
type
  TImage32Backend = class(TCustomBackend)
  private
    FImage32: TImage32;
  private
    procedure Image32Changed(Sender: TObject);
  public
    constructor Create(AOwner: TCustomBitmap32; AImage32: TImage32);
    destructor Destroy; override;

    function Empty: Boolean; override;

    property Image32: TImage32 read FImage32;
  end;

constructor TImage32Backend.Create(AOwner: TCustomBitmap32; AImage32: TImage32);
begin
  inherited Create(AOwner);

  FImage32 := AImage32;
  FImage32.OnChange := Image32Changed;
end;

destructor TImage32Backend.Destroy;
begin
  FImage32.OnChange := nil;

  inherited;
end;

procedure TImage32Backend.Image32Changed(Sender: TObject);
begin
  BeginUpdate;
  try
    FBits := pointer(FImage32.PixelBase);
    // TImage32 doesn't fire OnResized when being resized from within SVG.LoadFromStream
    // so we have to handle the resize manually.
    FOwner.SetSize(FImage32.Width, FImage32.Height);

    Changed;
  finally
    EndUpdate;
  end;
end;

function TImage32Backend.Empty: Boolean;
begin
  Result := FImage32.IsEmpty;
end;

//------------------------------------------------------------------------------
//
//      TImageFormatAdapterSVG
//
//------------------------------------------------------------------------------
// Implements IImageFormatReader for the SVG image format using the Image32
// library's SVG reading and rendering capabilities.
//------------------------------------------------------------------------------
type
  TImageFormatAdapterSVG = class(TCustomImageFormat,
    IImageFormatFileInfo,
    IImageFormatReader)
  private
    // IImageFormatFileInfo
    function ImageFormatDescription: string;
    function ImageFormatFileTypes: TFileTypes;
  private
    // IImageFormatReader
    function CanLoadFromStream(AStream: TStream): boolean;
    function LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
  end;

//------------------------------------------------------------------------------
// IImageFormatFileInfo
//------------------------------------------------------------------------------

function TImageFormatAdapterSVG.ImageFormatFileTypes: TFileTypes;
begin
  Result := ['svg'];
end;

function TImageFormatAdapterSVG.ImageFormatDescription: string;
begin
  Result := sImageFormatSVGName;
end;

//------------------------------------------------------------------------------
// IImageFormatReader
//------------------------------------------------------------------------------
function TImageFormatAdapterSVG.CanLoadFromStream(AStream: TStream): boolean;
begin
  Result := TImageFormat_SVG.IsValidImageStream(AStream);
end;

function TImageFormatAdapterSVG.LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
var
  SVG: TImageFormat_SVG;
  Bitmap: TBitmap32;
  Image32: TImage32;
begin
  if (not TImageFormat_SVG.IsValidImageStream(AStream)) then
    Exit(False);

  Image32 := TImage32.Create;
  try

    Bitmap := TBitmap32.Create;
    try

      TImage32Backend.Create(Bitmap, Image32); // Bitmap now owns the backend

      SVG := TImageFormat_SVG.Create;
      try
        SVG.LoadFromStream(AStream, Image32);
      finally
        SVG.Free;
      end;

      ADest.Assign(Bitmap);

    finally
      Bitmap.Free;
    end;

  finally
    Image32.Free;
  end;

  Result := True;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(TImageFormatAdapterSVG.Create, ImageFormatPriorityBetter);
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

