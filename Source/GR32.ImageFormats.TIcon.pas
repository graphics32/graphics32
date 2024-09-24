unit GR32.ImageFormats.TIcon;

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

{$include GR32.inc}

implementation

uses
  Classes,
  Graphics,
  SysUtils,
{$ifndef FPC}
  Consts,
{$endif FPC}
  GR32,
  GR32.ImageFormats.TGraphic,
  GR32.ImageFormats;

//------------------------------------------------------------------------------
//
//      FPC compatibility
//
//------------------------------------------------------------------------------
{$ifdef FPC}
resourcestring
  SVIcons = 'Icons';
{$endif FPC}

//------------------------------------------------------------------------------
//
//      TImageFormatAdapterTIcon
//
//------------------------------------------------------------------------------
// Implements IImageFormatAdapter for the TIcon class.
//------------------------------------------------------------------------------
type
  TImageFormatAdapterTIcon = class(TImageFormatReaderWriterTGraphic)
  strict protected
    // IImageFormatAdapter
    function AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean; override;
  end;

//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatAdapterTIcon.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
var
  I: Integer;
  P: PColor32Entry;
  ReassignFromMasked: Boolean;
begin
  if (not (Source is TIcon)) then
    Exit(False);

  Result := True;

  AssignFromGraphicPlain(Dest, TIcon(Source), 0, False);
  if Dest.Empty then
    Exit;

  // Check if the icon was painted with a merged alpha channel.
  // That happens transparently for new-style 32-bit icons.
  // For all other bit depths GDI will reset our alpha channel to opaque.
  ReassignFromMasked := True;
  P := PColor32Entry(@Dest.Bits[0]);
  for I := 0 to Dest.Height * Dest.Width - 1 do
  begin
    if P.A > 0 then
    begin
      ReassignFromMasked := False;
      Break;
    end;
    Inc(P);
  end;

  // No alpha values found? Use masked approach...
  if ReassignFromMasked then
    AssignFromGraphicMasked(Dest, TIcon(Source));
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(
    TImageFormatAdapterTIcon.Create(TIcon, SVIcons, ['ico']),
    ImageFormatPriorityNormal);
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

