unit GR32.ImageFormats.PNG32;

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

{$I GR32.inc}

implementation

uses
  Classes,
  Graphics,
  GR32,
  GR32_Png,
  GR32_PortableNetworkGraphic,
  GR32.ImageFormats;

resourcestring
  sImageFormatPNGName = 'PNG images';

//------------------------------------------------------------------------------
//
//      TImageFormatAdapterPNG32
//
//------------------------------------------------------------------------------
// Implements IImageFormatAdapter for the PNG image format using the GR32
// TPortableNetworkGraphic32 class.
//------------------------------------------------------------------------------
type
  TImageFormatAdapterPNG32 = class(TCustomImageFormatAdapter,
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

  TPortableNetworkGraphic32Cracker = class(TPortableNetworkGraphic32);

//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatAdapterPNG32.CanAssignFrom(Source: TPersistent): boolean;
begin
  Result := (Source is TPortableNetworkGraphic);
end;

function TImageFormatAdapterPNG32.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
var
  PNG32: TPortableNetworkGraphic32;
begin
  if (Source is TPortableNetworkGraphic32) then
  begin
    // AssignTo avoids recursion
    TPortableNetworkGraphic32Cracker(Source).AssignTo(Dest);
    Result := True;
  end else
  if (Source is TPortableNetworkGraphic) then
  begin
    PNG32 := TPortableNetworkGraphic32.Create;
    try
      PNG32.Assign(Source);
      TPortableNetworkGraphic32Cracker(PNG32).AssignTo(Dest);
    finally
      PNG32.Free;
    end;
    Result := True;
  end else
    Result := inherited;
end;

//------------------------------------------------------------------------------

function TImageFormatAdapterPNG32.CanAssignTo(Dest: TPersistent): boolean;
begin
  Result := (Dest is TPortableNetworkGraphic);
end;

function TImageFormatAdapterPNG32.AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean;
var
  PNG32: TPortableNetworkGraphic32;
begin
  if (Dest is TPortableNetworkGraphic32) then
  begin
    Dest.Assign(Source);
    Result := True;
  end else
  if (Dest is TPortableNetworkGraphic) then
  begin
    PNG32 := TPortableNetworkGraphic32.Create;
    try
      PNG32.Assign(Source);
      Dest.Assign(PNG32);
    finally
      PNG32.Free;
    end;
    Result := True;
  end else
    Result := inherited;
end;

//------------------------------------------------------------------------------
// IImageFormatFileInfo
//------------------------------------------------------------------------------

function TImageFormatAdapterPNG32.ImageFormatFileTypes: TFileTypes;
begin
  Result := ['png'];
end;

function TImageFormatAdapterPNG32.ImageFormatDescription: string;
begin
  Result := sImageFormatPNGName;
end;

//------------------------------------------------------------------------------
// IImageFormatReader
//------------------------------------------------------------------------------
function TImageFormatAdapterPNG32.CanLoadFromStream(AStream: TStream): boolean;
begin
  Result := IsValidPNG(AStream);
end;

function TImageFormatAdapterPNG32.LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
var
  PNG32: TPortableNetworkGraphic32;
begin
  if (not IsValidPNG(AStream)) then
    Exit(False);

  PNG32 := TPortableNetworkGraphic32.Create;
  try
    PNG32.LoadFromStream(AStream);
    TPortableNetworkGraphic32Cracker(PNG32).AssignTo(ADest);
  finally
    PNG32.Free;
  end;
  Result := True;
end;

//------------------------------------------------------------------------------
// IImageFormatWriter
//------------------------------------------------------------------------------
procedure TImageFormatAdapterPNG32.SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
var
  PNG32: TPortableNetworkGraphic32;
begin
  PNG32 := TPortableNetworkGraphic32.Create;
  try
    PNG32.Assign(ASource);
    PNG32.SaveToStream(AStream);
  finally
    PNG32.Free;
  end;
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(TImageFormatAdapterPNG32.Create, ImageFormatPriorityBetter);
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

