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

// Define GR32_CLIPBOARD_PNG to enable the "PNG" clipboard format.
//
// When this clipboard format is enabled, copying a TBitmap32 to the clipboard
// will also place a copy of the bitmap in PNG format onto the clipboard and
// pasting from the clipboard will also support the PNG format.
{$define GR32_CLIPBOARD_PNG}

const
  ClipboardFormatNamePNG = 'PNG';

var
  CF_PNG: Word = 0;

//------------------------------------------------------------------------------

implementation

uses
  Classes,
  Graphics,
{$if defined(GR32_CLIPBOARD_PNG)}
  Clipbrd,
  Windows,
  GR32_Clipboard,
{$ifend}
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
    IImageFormatWriter,
    IImageFormatAux)
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
  private
    // IImageFormatAux
    function IsAuxFormat(Source: TCustomBitmap32; Dest: TPersistent): boolean;
  end;

  TPortableNetworkGraphic32Cracker = class(TPortableNetworkGraphic32);

//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatAdapterPNG32.CanAssignFrom(Source: TPersistent): boolean;
begin
  Result := (Source is TPortableNetworkGraphic);
{$if defined(GR32_CLIPBOARD_PNG)}
  Result := Result or ((Source is TClipboard) and (TClipboard(Source).HasFormat(CF_PNG)));
{$ifend}
end;

function TImageFormatAdapterPNG32.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
var
  PNG32: TPortableNetworkGraphic32;
{$if defined(GR32_CLIPBOARD_PNG)}
  Stream: TStream;
{$ifend}
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
{$if defined(GR32_CLIPBOARD_PNG)}
  if (Source is TClipboard) then
  begin
    Dest.BeginUpdate;
    try
{$ifndef FPC}
      Clipboard.Open;
{$endif FPC}
      try
        Stream := TClipboardMemoryStream.Create(CF_PNG);
        try
          PNG32 := TPortableNetworkGraphic32.Create;
          try
            PNG32.LoadFromStream(Stream);
            TPortableNetworkGraphic32Cracker(PNG32).AssignTo(Dest);
          finally
            PNG32.Free;
          end;
        finally
          Stream.Free;
        end;
      finally
{$ifndef FPC}
        Clipboard.Close;
{$endif FPC}
      end;
    finally
      Dest.EndUpdate;
    end;
    Result := True;
  end else
{$ifend}
    Result := inherited;
end;

//------------------------------------------------------------------------------

function TImageFormatAdapterPNG32.CanAssignTo(Dest: TPersistent): boolean;
begin
  Result := (Dest is TPortableNetworkGraphic);
{$if defined(GR32_CLIPBOARD_PNG)}
  Result := Result or (Dest is TClipboard);
{$ifend}
end;

function TImageFormatAdapterPNG32.AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean;
var
  PNG32: TPortableNetworkGraphic32;
{$if defined(GR32_CLIPBOARD_PNG)}
  Stream: TStream;
{$ifend}
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
{$if defined(GR32_CLIPBOARD_PNG)}
  if (Dest is TClipboard) then
  begin
{$ifndef FPC}
    Stream := TOwnedGlobalMemoryStream.Create(1024);
{$else FPC}
    Stream := TMemoryStream.Create;
{$endif FPC}
    try
{$ifdef FPC}
      TMemoryStreamCracker(Stream).Capacity := 1024;
{$endif FPC}

      SaveToStream(Source, Stream);

{$ifndef FPC}
      Clipboard.SetAsHandle(CF_PNG, TGlobalMemoryStream(Stream).ReleaseHandle);
{$else FPC}
      Clipboard.AddFormat(CF_PNG, Stream);
{$endif FPC}
    finally
      Stream.Free;
    end;
    Result := True;
  end else
{$ifend}
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
// IImageFormatAux
//------------------------------------------------------------------------------
function TImageFormatAdapterPNG32.IsAuxFormat(Source: TCustomBitmap32; Dest: TPersistent): boolean;
begin
  Result := (Dest is TClipboard);
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(TImageFormatAdapterPNG32.Create, ImageFormatPriorityBetter);
{$if defined(GR32_CLIPBOARD_PNG)}
  CF_PNG := RegisterClipboardFormat(ClipboardFormatNamePNG);
{$ifend}
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

