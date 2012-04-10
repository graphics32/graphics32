unit GR32_MediaPathLocator;

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
 * The Original Code is GR32 Examples
 *
 * The Initial Developer of the Original Code is
 * Christian-W. Budde
 *
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)
interface

{$I GR32.inc}

uses
  {$IFDEF Darwin}
    MacOSAll,
  {$ENDIF}
  SysUtils;

function GetMediaPath: TFileName;

implementation

function GetMediaPath: TFileName;
{$IFDEF Darwin}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
begin
  if ParamStr(1) <> '' then
    Result := ParamStr(1)
  else
  begin
    // Under Mac OS X we need to get the location of the bundle
  {$IFDEF Darwin}
    pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle);
    pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
    CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
    CFRelease(pathRef);
    CFRelease(pathCFStr);
  {$ENDIF}

    // Different platforms store resource files on different locations
  {$IFDEF Windows}
    Result := '..\..\..\Media\';
    {$IFDEF FPC}
    Result := '..\' + Result;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF UNIX}
    {$IFDEF Darwin}
      Result := pathStr + '/Contents/Resources/Media/';
    {$ELSE}
      Result := '../../../Media/';
    {$ENDIF}
  {$ENDIF}
  end;
end;

end.

