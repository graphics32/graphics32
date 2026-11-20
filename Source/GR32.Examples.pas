unit GR32.Examples;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

//------------------------------------------------------------------------------
//
//      Utilities for use by the Graphics32 examples
//
//------------------------------------------------------------------------------
type
  Graphics32Examples = record
    // The location of the examples Media files (bitmaps, etc)
    class function MediaFolder: string; static;

    // Look for a file in the examples Media folder
    class function MediaFileExists(const Filename: string): boolean; static;
  end;


implementation

uses
  SysUtils,
{$ifndef FPC}
  IOUtils,
{$endif}
  GR32.Platform;

var
  FGraphics32MediaFolder: string;
  FGraphics32MediaFolderFailed: boolean = False;

function GetGraphics32MediaFolder(RaiseOnFail: boolean): boolean;
const
  sFolderName = 'Media';
var
  ParentFolder: string;
  NewParentFolder: string;
begin
  if (not FGraphics32MediaFolderFailed) and (FGraphics32MediaFolder = '') then
  begin
    ParentFolder := TPath.GetDirectoryName(ParamStr(0));
    FGraphics32MediaFolder := TPath.Combine(ParentFolder, sFolderName);

    while (not TDirectory.Exists(FGraphics32MediaFolder)) do
    begin
      NewParentFolder := TDirectory.GetParent(ParentFolder);

      if (NewParentFolder = ParentFolder) then
      begin
        FGraphics32MediaFolderFailed := True;
        break;
      end;

      ParentFolder := NewParentFolder;
      FGraphics32MediaFolder := TPath.Combine(ParentFolder, sFolderName);
    end;
  end;

  if (RaiseOnFail and FGraphics32MediaFolderFailed) then
    raise Exception.CreateFmt('Graphics32 examples %s folder not found', [sFolderName]);

  Result := (not FGraphics32MediaFolderFailed);
end;

class function Graphics32Examples.MediaFolder: string;
begin
  GetGraphics32MediaFolder(True);
  Result := FGraphics32MediaFolder;
end;

class function Graphics32Examples.MediaFileExists(const Filename: string): boolean;
begin
  Result := GetGraphics32MediaFolder(False) and TFile.Exists(FGraphics32MediaFolder + '\' + Filename);
end;

end.

