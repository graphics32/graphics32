unit GR32_Backends_Generic;

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
 * The Original Code is Backend Extension for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Andre Beckedorf - metaException
 * Andre@metaException.de
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
{$ifdef MSWINDOWS}
  Windows,
{$ENDIF}
{$ifndef FPC}
  System.IOUtils,
{$endif}
  SysUtils,
  Classes,
  GR32;

type
  { TMemoryBackend }
  { A backend that keeps the backing buffer entirely in memory.}

  TMemoryBackend = class(TCustomBackend)
  protected
    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;
  end;

{$ifdef MSWINDOWS}

  { TMMFBackend }
  { A backend that uses memory mapped files or mapped swap space for the
    backing buffer.}

  TMMFBackend = class(TMemoryBackend)
  private
    FMapHandle: THandle;
    FMapIsTemporary: boolean;
    FMapFileHandle: THandle;
    FMapFileName: string;
  protected
    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;
  public
    constructor Create(Owner: TCustomBitmap32; IsTemporary: Boolean = True; const MapFileName: string = ''); virtual;
    destructor Destroy; override;

    class procedure InitializeFileMapping(var MapHandle, MapFileHandle: THandle; var MapFileName: string);
    class procedure DeinitializeFileMapping(MapHandle, MapFileHandle: THandle; const MapFileName: string);
    class procedure CreateFileMapping(var MapHandle, MapFileHandle: THandle; var MapFileName: string; IsTemporary: Boolean; NewWidth, NewHeight: Integer);
  end;

{$ENDIF}

implementation

uses
  GR32_LowLevel;

{$ifdef MSWINDOWS}
resourcestring
  RCStrFailedToMapFile = 'Failed to map file';
  RCStrFailedToCreateMapFile = 'Failed to create map file (%s)';
  RCStrFailedToMapViewOfFile = 'Failed to map view of file.';
{$ENDIF}

{ TMemoryBackend }

procedure TMemoryBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  GetMem(FBits, NewWidth * NewHeight * 4);
  if ClearBuffer then
    FillLongword(FBits[0], NewWidth * NewHeight, clBlack32);
end;

procedure TMemoryBackend.FinalizeSurface;
begin
  if (FBits <> nil) then
  begin
    FreeMem(FBits);
    FBits := nil;
  end;
end;

{$ifdef MSWINDOWS}

{ TMMFBackend }

constructor TMMFBackend.Create(Owner: TCustomBitmap32; IsTemporary: Boolean; const MapFileName: string);
begin
  FMapFileName := MapFileName;
  FMapIsTemporary := IsTemporary;
  InitializeFileMapping(FMapHandle, FMapFileHandle, FMapFileName);
  inherited Create(Owner);
end;

destructor TMMFBackend.Destroy;
begin
  DeinitializeFileMapping(FMapHandle, FMapFileHandle, FMapFileName);
  inherited;
end;

procedure TMMFBackend.FinalizeSurface;
begin
  if (FBits <> nil) then
  begin
    UnmapViewOfFile(FBits);
    FBits := nil;
  end;
end;

procedure TMMFBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  CreateFileMapping(FMapHandle, FMapFileHandle, FMapFileName, FMapIsTemporary, NewWidth, NewHeight);
  FBits := MapViewOfFile(FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

  if (FBits = nil) then
    raise Exception.Create(RCStrFailedToMapViewOfFile);

  if ClearBuffer then
    FillLongword(FBits[0], NewWidth * NewHeight, clBlack32);
end;


class procedure TMMFBackend.InitializeFileMapping(var MapHandle, MapFileHandle: THandle; var MapFileName: string);
begin
  MapHandle := 0;
  MapFileHandle := INVALID_HANDLE_VALUE;
  if (MapFileName <> '') then
    ForceDirectories(IncludeTrailingPathDelimiter(ExtractFilePath(MapFileName)));
end;

class procedure TMMFBackend.DeinitializeFileMapping(MapHandle, MapFileHandle: THandle; const MapFileName: string);
begin
  // Note that MapHandle and MapFileHandle can be valid even though
  // MapFileName is empty in case the mapping was created in the system
  // page file.

  if (MapHandle <> 0) then
    CloseHandle(MapHandle);

  if (MapFileHandle <> INVALID_HANDLE_VALUE) then
    CloseHandle(MapFileHandle);

  if (MapFileName <> '') and FileExists(MapFileName) then
    DeleteFile(MapFileName);
end;

class procedure TMMFBackend.CreateFileMapping(var MapHandle, MapFileHandle: THandle;
  var MapFileName: string; IsTemporary: Boolean; NewWidth, NewHeight: Integer);

{$IFDEF USE_GUIDS_IN_MMF}
  function GetTempFileName(const Prefix: string): string;
  var
    PathAndPrefix: string;
  begin
{$ifdef FPC}
    PathAndPrefix := GetTempDir + Prefix;
{$else}
    PathAndPrefix := TPath.GetTempPath + Prefix;
{$endif}
    repeat
      Result := PathAndPrefix + TGUID.NewGuid.ToString;
    until not FileExists(Result);
  end;
{$ELSE}
  function GetTempFileName(const Prefix: string): string;
  var
    PathAndPrefix: string;
    n: integer;
  begin
{$ifdef FPC}
    PathAndPrefix := GetTempDir + Prefix;
{$else}
    PathAndPrefix := TPath.GetTempPath + Prefix;
{$endif}
    n := 0;
    repeat
      Result := PathAndPrefix + IntToHex(n, 8);
      Inc(n);
    until not FileExists(Result);
  end;
{$ENDIF}

var
  Flags: Cardinal;
  MaxSize: LARGE_INTEGER;
begin
  // Close previous handles

  if (MapHandle <> 0) then
  begin
    CloseHandle(MapHandle);
    MapHandle := 0;
  end;

  if (MapFileHandle <> INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(MapFileHandle);
    MapFileHandle := INVALID_HANDLE_VALUE;
  end;

  // Do we want to use an external map file?
  if (MapFileName <> '') or IsTemporary then
  begin
    if (MapFileName = '') then
      MapFileName := GetTempFileName(IntToStr(NativeUInt(Self)));

    // delete file if exists
    if FileExists(MapFileName) then
      DeleteFile(MapFileName);

    // open file
    if IsTemporary then
      Flags := FILE_ATTRIBUTE_TEMPORARY OR FILE_FLAG_DELETE_ON_CLOSE
    else
      Flags := FILE_ATTRIBUTE_NORMAL;

    MapFileHandle := CreateFile(PChar(MapFileName), GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, Flags, 0);

    if (MapFileHandle = INVALID_HANDLE_VALUE) then
    begin
      if not IsTemporary then
        raise Exception.CreateFmt(RCStrFailedToCreateMapFile, [MapFileName]);

      // Reset and fall back to allocating in the system's paging file...

      // delete file if exists
      if FileExists(MapFileName) then
        DeleteFile(MapFileName);

      MapFileName := '';
    end;
  end else
    // use the system's paging file
    MapFileHandle := INVALID_HANDLE_VALUE;

  // create map
  MaxSize.QuadPart := Int64(NewWidth) * Int64(NewHeight) * SizeOf(DWORD);
  MapHandle := Windows.CreateFileMapping(MapFileHandle, nil, PAGE_READWRITE, MaxSize.HighPart, MaxSize.LowPart, nil);

  if (MapHandle = 0) then
  begin
    if (MapFileHandle <> INVALID_HANDLE_VALUE) then
    begin
      CloseHandle(MapFileHandle);
      MapFileHandle := INVALID_HANDLE_VALUE;
    end;

    raise Exception.Create(RCStrFailedToMapFile);
  end;
end;

{$ENDIF}

end.
