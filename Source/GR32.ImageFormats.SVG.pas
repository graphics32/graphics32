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
 * The Original Code is Native SVG Image Format support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  Classes,
  GR32,
  GR32.ImageFormats;

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
    function CanLoadFromStream(AStream: TStream): Boolean;
    function LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): Boolean;
  end;

implementation

uses
  Types,
  SysUtils,
  GR32.SVG;

resourcestring
  sImageFormatSVGName = 'Scalable Vector Graphics';

{ TImageFormatAdapterSVG }

function TImageFormatAdapterSVG.ImageFormatDescription: string;
begin
  Result := sImageFormatSVGName;
end;

function TImageFormatAdapterSVG.ImageFormatFileTypes: TFileTypes;
begin
  Result := ['svg'];
end;

function TImageFormatAdapterSVG.CanLoadFromStream(AStream: TStream): Boolean;
var
  SavedPos: Int64;
  BytesRead: Integer;
  Buffer: AnsiString;
begin
  Result := False;
  if (AStream = nil) then
    Exit;

  SavedPos := AStream.Position;
  try
    SetLength(Buffer, 255);
    BytesRead := AStream.Read(Buffer[1], Length(Buffer));

    // We need at least 4 characters to match '<svg' and 6 for a minimal valid svg: '<svg/>'
    if (BytesRead >= 6) then
    begin
      // First a quick test for the common case: Document starts with '<svg'
      if (Buffer[1] = '<') and (Buffer[2] = 's') and (Buffer[3] = 'v') and (Buffer[4] = 'g') then
        Exit(True);

      // Then the more generic case
      SetLength(Buffer, BytesRead);
      Buffer := LowerCase(Buffer);
      if (Pos('<svg', Buffer) > 0) or (Pos('<?xml', Buffer) > 0) then
        Result := True;
    end;
  finally
    AStream.Position := SavedPos;
  end;
end;

function TImageFormatAdapterSVG.LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): Boolean;
var
  doc: TSvgDocument;
  w, h: Integer;
begin
  Result := False;
  if (ADest = nil) or (AStream = nil) then
    Exit;

  doc := TSvgDocument.Create;
  try
    doc.LoadFromStream(AStream);
    if doc.Root <> nil then
    begin
      w := Round(doc.Width.ToPixels(800));
      h := Round(doc.Height.ToPixels(600));

      if (w <= 0) and doc.ViewBox.IsValid then
        w := Round(doc.ViewBox.Width);
      if (h <= 0) and doc.ViewBox.IsValid then
        h := Round(doc.ViewBox.Height);

      if w <= 0 then
        w := 800;
      if h <= 0 then
        h := 600;

      ADest.SetSize(w, h);
      doc.Draw(ADest, FloatRect(0, 0, w, h));

      Result := True;
    end;
  finally
    doc.Free;
  end;
end;

var
  ImageFormatHandle: Integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(TImageFormatAdapterSVG.Create, ImageFormatPriorityBetter);

finalization
  if ImageFormatHandle <> 0 then
    ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);

end.
