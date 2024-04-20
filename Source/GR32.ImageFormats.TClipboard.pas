unit GR32.ImageFormats.TClipboard;

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

{$I GR32.inc}

implementation

uses
  Classes,
  SysUtils,
  ClipBrd,
  GR32,
  GR32_Clipboard,
  GR32.ImageFormats;

//------------------------------------------------------------------------------
//
//      TImageFormatAdapterTClipboard
//
//------------------------------------------------------------------------------
// Implements IImageFormatAdapter for the TClipboard class.
//------------------------------------------------------------------------------
type
  TImageFormatAdapterTClipboard = class(TCustomImageFormatAdapter,
    IImageFormatAdapter,
    IImageFormatWriteNotification)
  strict protected
    // IImageFormatAdapter
    function CanAssignFrom(Source: TPersistent): boolean; override;
    function AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean; override;
    function CanAssignTo(Dest: TPersistent): boolean; override;
    function AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean; override;
  private
    // IImageFormatWriteNotification
    procedure BeginWriting(Source: TCustomBitmap32; Dest: TPersistent);
    procedure EndWriting(Source: TCustomBitmap32; Dest: TPersistent);
  end;

//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatAdapterTClipboard.CanAssignFrom(Source: TPersistent): boolean;
begin
  Result := (Source is TClipboard) and (ImageFormatManager.ClipboardFormats.CanPasteFromClipboard);
end;

function TImageFormatAdapterTClipboard.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
begin
  if (not (Source is TClipboard)) then
    Exit(False);

  Result := ImageFormatManager.ClipboardFormats.PasteFromClipboard(Dest);
end;

//------------------------------------------------------------------------------

function TImageFormatAdapterTClipboard.CanAssignTo(Dest: TPersistent): boolean;
begin
  Result := (Dest is TClipboard);
end;

function TImageFormatAdapterTClipboard.AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean;
begin
  if (Dest is TClipboard) then
  begin
    TClipboard(Dest).Open;
    try

      Result := CopyBitmap32ToClipboard(Source);

      // Note that, if PNG format is enabled, we also place a copy of the bitmap
      // in PNG format on the clipboard.
      // See comment in CopyBitmap32ToClipboard.

    finally
      TClipboard(Dest).Close;
    end;
  end else
    Result := False;
end;

//------------------------------------------------------------------------------
// IImageFormatWriteNotification
//------------------------------------------------------------------------------
procedure TImageFormatAdapterTClipboard.BeginWriting(Source: TCustomBitmap32; Dest: TPersistent);
begin
  if (Dest is TClipboard) then
    TClipboard(Dest).Open;
end;

procedure TImageFormatAdapterTClipboard.EndWriting(Source: TCustomBitmap32; Dest: TPersistent);
begin
  if (Dest is TClipboard) then
    TClipboard(Dest).Close;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(TImageFormatAdapterTClipboard.Create, ImageFormatPriorityNormal);
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

