unit GR32.ImageFormats.TPicture;

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

// WEAKPACKAGEUNIT so we can include the unit in the GR32 design time
// package in order to have the design time editor support the various formats.
{$WEAKPACKAGEUNIT ON}

interface

{$I GR32.inc}

implementation

uses
  Classes,
  Graphics,
  GR32,
  GR32.ImageFormats;

//------------------------------------------------------------------------------
//
//      TImageFormatAdapterTPicture
//
//------------------------------------------------------------------------------
// Implements IImageFormatAdapter for the TPicture class.
//------------------------------------------------------------------------------
type
  TImageFormatAdapterTPicture = class(TCustomImageFormatAdapter,
    IImageFormatAdapter)
  strict protected
    // IImageFormatAdapter
    function CanAssignFrom(Source: TPersistent): boolean; override;
    function AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean; override;
    function CanAssignTo(Dest: TPersistent): boolean; override;
    function AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean; override;
  end;

//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatAdapterTPicture.CanAssignFrom(Source: TPersistent): boolean;
begin
  Result := (Source is TPicture) and (TPicture(Source).Graphic <> nil) and
    ImageFormatManager.Adapters.CanAssignFrom(TPicture(Source).Graphic);
end;

function TImageFormatAdapterTPicture.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
begin
  Result := (Source is TPicture) and (TPicture(Source).Graphic <> nil) and
    // Recurse and try to assign from the TGraphic
    ImageFormatManager.Adapters.AssignFrom(Dest, TPicture(Source).Graphic);
end;

//------------------------------------------------------------------------------

function TImageFormatAdapterTPicture.CanAssignTo(Dest: TPersistent): boolean;
begin
  Result := (Dest is TPicture) and (TPicture(Dest).Graphic <> nil) and
    ImageFormatManager.Adapters.CanAssignTo(TPicture(Dest).Graphic);
end;

function TImageFormatAdapterTPicture.AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean;
begin
  Result := (Dest is TPicture) and (TPicture(Dest).Graphic <> nil) and
    // Recurse and try to assign to the TGraphic
    ImageFormatManager.Adapters.AssignTo(Source, TPicture(Dest).Graphic);
end;


//------------------------------------------------------------------------------
//
//      TImageFormatReaderTPicture
//
//------------------------------------------------------------------------------
// Implements IImageFormatReader for the TPicture class.
// Basically this reader will support all TGraphic implementations that can
// read from a stream.
// Additionally IImageFormatFileReader is implemented to allow TPicture to
// determine the image format based on the file type.
//------------------------------------------------------------------------------
type
  TImageFormatReaderTPicture = class(TCustomImageFormat,
    IImageFormatReader,
    IImageFormatFileReader)
  strict private
    // IImageFormatReader
    function CanLoadFromStream(AStream: TStream): boolean;
    function LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
  strict private
    // IImageFormatFileReader
    function LoadFromFile(ADest: TCustomBitmap32; const AFilename: string): boolean;
  end;

//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatReaderTPicture.CanLoadFromStream(AStream: TStream): boolean;
begin
  // TPicture does not have a CanLoadFromStream so this is a last-ditch effort.
  Result := True;
end;

function TImageFormatReaderTPicture.LoadFromFile(ADest: TCustomBitmap32; const AFilename: string): boolean;
var
  Picture: TPicture;
begin
  Picture := TPicture.Create;
  try
    try
      Picture.LoadFromFile(AFilename);
    except
      on E: EInvalidGraphic do
        Exit(False);
    end;
    ADest.Assign(Picture.Graphic);
  finally
    Picture.Free;
  end;
  Result := True;
end;

function TImageFormatReaderTPicture.LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
{$ifdef LOADFROMSTREAM}
var
  Picture: TPicture;
{$endif LOADFROMSTREAM}
begin
{$ifdef LOADFROMSTREAM}
  // TPicture.LoadFromStream requires TGraphic.CanLoadFromStream.
  // Introduced in Delphi 10.2 and present in FPC as well
  // See issue #145

  Picture := TPicture.Create;
  try
    try
      Picture.LoadFromStream(AStream);
    except
      on E: EInvalidGraphic do
        Exit(False);
    end;
    ADest.Assign(Picture.Graphic);
  finally
    Picture.Free;
  end;
  Result := True;
{$else LOADFROMSTREAM}
  Result := False;
{$endif LOADFROMSTREAM}
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  ImageFormatManager.RegisterImageFormat(TImageFormatAdapterTPicture.Create, ImageFormatPriorityNormal);
{$ifdef LOADFROMSTREAM}
  ImageFormatManager.RegisterImageFormat(TImageFormatReaderTPicture.Create, ImageFormatPriorityWorst);
{$endif LOADFROMSTREAM}
end.

