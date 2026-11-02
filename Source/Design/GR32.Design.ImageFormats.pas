unit GR32.Design.ImageFormats;

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
 * ***** END LICENSE BLOCK ***** *)


interface

{$include GR32.inc}

implementation

//------------------------------------------------------------------------------
//
//      Graphics32 design-time image format support
//
//------------------------------------------------------------------------------
// Just references all the image formats we would like to support at design-time.
//------------------------------------------------------------------------------

uses
  GR32.ImageFormats.BMP,
  GR32.ImageFormats.PNG32,
  GR32.ImageFormats.TClipboard,
  GR32.ImageFormats.TBitmap,
{$IFNDEF PLATFORM_INDEPENDENT}
  GR32.ImageFormats.TWICImage,
  GR32.ImageFormats.TMetaFile,
{$ENDIF}
  GR32.ImageFormats.JPG,
  GR32.ImageFormats.GIF,
  GR32.ImageFormats.PSD,
  GR32.ImageFormats.SVG,
  GR32.ImageFormats.TIcon,
  GR32.ImageFormats.TGraphic,
  GR32.ImageFormats.TPicture;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.

