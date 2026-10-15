unit GR32.Platform;

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
 * The Original Code is Cross platform utilities for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{$ifndef FPC}
uses
  IOUtils;
{$endif}

//------------------------------------------------------------------------------
//
//      File system utilities
//
//------------------------------------------------------------------------------
// FreePascal support
//------------------------------------------------------------------------------
{$if defined(FPC)}
type
  TPath = record
    class function GetDirectoryName(const APath: string): string; static;
    class function Combine(const APath, BPath: string): string; static;
    class function ChangeExtension(const Path, Extension: string): string; static;
  end;

type
  TDirectory = record
    class function Exists(const AFoldername: string): boolean; static;
    class function GetParent(const AFoldername: string): string; static;
  end;

type
  TFile = record
    class function Exists(const AFilename: string): boolean; static;
  end;
{$else}

type
  TPath = IOUtils.TPath;
  TDirectory = IOUtils.TDirectory;
  TFile = IOUtils.TFile;

{$ifend}

implementation

uses
  SysUtils;

//------------------------------------------------------------------------------
//
//      File system utilities
//
//------------------------------------------------------------------------------
// FreePascal support
//------------------------------------------------------------------------------
{$if defined(FPC)}
//------------------------------------------------------------------------------
// TPath
//------------------------------------------------------------------------------
class function TPath.GetDirectoryName(const APath: string): string;
begin
  Result := ExtractFileDir(APath);
end;

class function TPath.Combine(const APath, BPath: string): string;
begin
  Result := ConcatPaths([APath, BPath]);
end;

class function TPath.ChangeExtension(const Path, Extension: string): string;
begin
  Result := ChangeFileExt(Path, Extension);
end;

//------------------------------------------------------------------------------
// TDirectory
//------------------------------------------------------------------------------
class function TDirectory.Exists(const AFoldername: string): boolean;
begin
  Result := DirectoryExists(AFoldername);
end;

class function TDirectory.GetParent(const AFoldername: string): string;
begin
  Result := ExtractFileDir(ExcludeTrailingPathDelimiter(AFoldername));
end;

//------------------------------------------------------------------------------
// TFile
//------------------------------------------------------------------------------
class function TFile.Exists(const AFilename: string): boolean;
begin
  Result := FileExists(AFilename);
end;
{$ifend}

end.

