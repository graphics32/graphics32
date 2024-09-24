unit GR32.ImageFormats.Default;

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

{$include GR32.inc}

implementation

//------------------------------------------------------------------------------
//
//      Image formats included by default.
//
//------------------------------------------------------------------------------
// These are the image format adapters that are required(*) by TBitmap32:
//
//   - BMP file format
//   - TBitmap support
//   - TClipboard support
//
// Additional format adapters for backward compatibility (i.e. previous versions
// supported assign to/from these formats):
//
//   - TPicture
//   - TIcon
//   - TMetaFile
//   - TWICImage
//
// Finally, PNG support has been enabled by default since it is the most common
// format when working with 32-bit bitmaps.
//
// [*] Actually not "required" per se:
// Without TClipoard support you cannot assign to and from TClipboard.
// Without TBitmap support, you cannot assign to and from TBitmap.
// Without BMP support, you cannot load from BMP files.
// If all or some of these aren't required by you, then go ahead and remove
// them.
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
  GR32.ImageFormats.TIcon,
  GR32.ImageFormats.TPicture;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.

