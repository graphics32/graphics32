unit GR32_Backends_Generic;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is Backend Extension for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Andre Beckedorf - metaException OHG
 * Andre@metaException.de
 *
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC}
    Types,
    {$IFDEF Windows}
      Windows,
    {$ENDIF}
  {$ELSE}
    {$IFDEF CLX}
      Qt, Types,
    {$ELSE}
      Windows,
    {$ENDIF}
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
    FMapFileHandle: THandle;
    FMapFileName: string;
  protected
    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;
  public
    constructor Create(Owner: TCustomBitmap32; const MapFileName: string = ''); virtual;
    destructor Destroy; override;

    class procedure InititializeFileMapping(var MapHandle, MapFileHandle: THandle; var MapFileName: string);
    class procedure DeinititializeFileMapping(MapHandle, MapFileHandle: THandle; const MapFileName: string);
    class procedure CreateFileMapping(var MapHandle, MapFileHandle: THandle; const MapFileName: string; NewWidth, NewHeight: Integer);
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

constructor TMMFBackend.Create(Owner: TCustomBitmap32; const MapFileName: string = '');
begin
  FMapFileName := MapFileName;
  InititializeFileMapping(FMapHandle, FMapFileHandle, FMapFileName);
  inherited Create(Owner);
end;

destructor TMMFBackend.Destroy;
begin
  DeinititializeFileMapping(FMapHandle, FMapFileHandle, FMapFileName);
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
  CreateFileMapping(FMapHandle, FMapFileHandle, FMapFileName, NewWidth, NewHeight);
  FBits := MapViewOfFile(FMapHandle, FILE_MAP_COPY, 0, 0, 0);

  if not Assigned(FBits) then
    raise Exception.Create('Failed to map view of file.');

  if ClearBuffer then
    FillLongword(FBits[0], NewWidth * NewHeight, clBlack32);
end;


class procedure TMMFBackend.InititializeFileMapping(var MapHandle, MapFileHandle: THandle; var MapFileName: string);
begin
  MapHandle := INVALID_HANDLE_VALUE;
  MapFileHandle := INVALID_HANDLE_VALUE;
  if MapFileName <> '' then
    ForceDirectories(IncludeTrailingPathDelimiter(ExtractFilePath(MapFileName)));
end;

class procedure TMMFBackend.DeinititializeFileMapping(MapHandle, MapFileHandle: THandle; const MapFileName: string);
begin
  if MapFileName <> '' then
  begin
    CloseHandle(MapHandle);
    CloseHandle(MapFileHandle);
    if FileExists(MapFileName) then
      DeleteFile(MapFileName);
  end;
end;

class procedure TMMFBackend.CreateFileMapping(var MapHandle, MapFileHandle: THandle; const MapFileName: string; NewWidth, NewHeight: Integer);
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
  if MapFileName <> '' then
  begin
    // delete file if exists
    if FileExists(MapFileName) then
      DeleteFile(MapFileName);

    // open file
    MapFileHandle := FileCreate(MapFileName, fmShareExclusive);

    if MapFileHandle = INVALID_HANDLE_VALUE then
      raise Exception.Create('Failed to create map file (' + MapFileName + ')');
  end
  else // use swap space
    MapFileHandle := INVALID_HANDLE_VALUE;

  // create map
  MapHandle := Windows.CreateFileMapping(MapFileHandle, nil, PAGE_READWRITE, 0, NewWidth * NewHeight * 4, nil);

  if MapHandle = 0 then
    raise Exception.Create('Failed to map file');
end;

{$ENDIF}

end.
