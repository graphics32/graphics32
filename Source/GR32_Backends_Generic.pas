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

{$I GR32.inc}

uses
{$IFDEF FPC}
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF USE_GUIDS_IN_MMF}
  ActiveX,
{$ENDIF}
  SysUtils, Classes, GR32, GR32_Backends;

type
  { TMemoryBackend }
  { A backend that keeps the backing buffer entirely in memory.}

  TMemoryBackend = class(TCustomBackend)
  protected
    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;
  end;

{$IFDEF Windows}

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

{ TMemoryBackend }

procedure TMemoryBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  GetMem(FBits, NewWidth * NewHeight * 4);
  if ClearBuffer then
    FillLongword(FBits[0], NewWidth * NewHeight, clBlack32);
end;

procedure TMemoryBackend.FinalizeSurface;
begin
  if Assigned(FBits) then
  begin
    FreeMem(FBits);
    FBits := nil;
  end;
end;

{$IFDEF Windows}

{ TMMFBackend }

constructor TMMFBackend.Create(Owner: TCustomBitmap32; IsTemporary: Boolean = True; const MapFileName: string = '');
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
  if Assigned(FBits) then
  begin
    UnmapViewOfFile(FBits);
    FBits := nil;
  end;
end;

procedure TMMFBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  CreateFileMapping(FMapHandle, FMapFileHandle, FMapFileName, FMapIsTemporary, NewWidth, NewHeight);
  FBits := MapViewOfFile(FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

  if not Assigned(FBits) then
    raise Exception.Create('Failed to map view of file.');

  if ClearBuffer then
    FillLongword(FBits[0], NewWidth * NewHeight, clBlack32);
end;


class procedure TMMFBackend.InitializeFileMapping(var MapHandle, MapFileHandle: THandle; var MapFileName: string);
begin
  MapHandle := INVALID_HANDLE_VALUE;
  MapFileHandle := INVALID_HANDLE_VALUE;
  if MapFileName <> '' then
    ForceDirectories(IncludeTrailingPathDelimiter(ExtractFilePath(MapFileName)));
end;

class procedure TMMFBackend.DeinitializeFileMapping(MapHandle, MapFileHandle: THandle; const MapFileName: string);
begin
  if MapFileName <> '' then
  begin
    CloseHandle(MapHandle);
    CloseHandle(MapFileHandle);
    if FileExists(MapFileName) then
      DeleteFile(MapFileName);
  end;
end;

class procedure TMMFBackend.CreateFileMapping(var MapHandle, MapFileHandle: THandle;
  var MapFileName: string; IsTemporary: Boolean; NewWidth, NewHeight: Integer);
var
  Flags: Cardinal;

  function GetTempPath: string;
  var
    PC: PChar;
  begin
    PC := StrAlloc(MAX_PATH + 1);
    Windows.GetTempPath(MAX_PATH, PC);
    Result := string(PC);
    StrDispose(PC);
  end;

{$IFDEF USE_GUIDS_IN_MMF}

  function GetTempFileName(const Prefix: string): string;
  var
    GUID: TGUID;
  begin
    repeat
      CoCreateGuid(GUID);
      Result := IncludeTrailingPathDelimiter(GetTempPath) + Prefix + GUIDToString(GUID);
    until not FileExists(Result);
  end;

{$ELSE}

  function GetTempFileName(const Prefix: string): string;
  var
    PC: PChar;
  begin
    PC := StrAlloc(MAX_PATH + 1);
    Windows.GetTempFileName(PChar(GetTempPath), PChar(Prefix), 0, PC);
    Result := string(PC);
    StrDispose(PC);
  end;

{$ENDIF}

begin
  // close previous handles
  if MapHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(MapHandle);
    MapHandle := INVALID_HANDLE_VALUE;
  end;

  if MapFileHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(MapFileHandle);
    MapHandle := INVALID_HANDLE_VALUE;
  end;

  // Do we want to use an external map file?
  if (MapFileName <> '') or IsTemporary then
  begin
    if MapFileName = '' then
      MapFileName := GetTempFileName(IntToStr(Integer(Self)));

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

    if MapFileHandle = INVALID_HANDLE_VALUE then
    begin
      if not IsTemporary then
        raise Exception.Create('Failed to create map file (' + MapFileName + ')')
      else
      begin
        // Reset and fall back to allocating in the system's paging file...

        // delete file if exists
        if FileExists(MapFileName) then
          DeleteFile(MapFileName);
          
        MapFileName := '';
      end;
    end;
  end
  else // use the system's paging file
    MapFileHandle := INVALID_HANDLE_VALUE;

  // create map
  MapHandle := Windows.CreateFileMapping(MapFileHandle, nil, PAGE_READWRITE, 0, NewWidth * NewHeight * 4, nil);

  if MapHandle = 0 then
    raise Exception.Create('Failed to map file');
end;

{$ENDIF}

end.
