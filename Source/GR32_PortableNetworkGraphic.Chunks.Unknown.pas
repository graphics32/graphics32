unit GR32_PortableNetworkGraphic.Chunks.Unknown;

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
 * The Original Code is GR32PNG for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Christian-W. Budde
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}
{$include GR32_PngCompilerSwitches.inc}

uses
  Generics.Collections,
  Classes,
  Graphics,
  SysUtils,
  GR32_PortableNetworkGraphic.Types,
  GR32_PortableNetworkGraphic.Chunks;

//------------------------------------------------------------------------------
//
//      TPngChunkUnknown
//
//------------------------------------------------------------------------------
type
  TPngChunkUnknown = class(TCustomChunk)
  private
    function GetData(index: Integer): Byte;
    procedure SetData(index: Integer; const Value: Byte);
  protected
    FChunkName  : TChunkName;
    FDataStream : TMemoryStream;
    function GetChunkName: TChunkName; override;
    function GetChunkNameAsString: AnsiString; override;
    function GetChunkSize: Cardinal; override;
    function GetChunkData: pointer; override;
    function CalculateChecksum: Integer;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ChunkName: TChunkName); virtual;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Data[index : Integer]: Byte read GetData write SetData;
    property DataStream: TMemoryStream read FDataStream;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TPngChunkUnknown
//
//------------------------------------------------------------------------------
constructor TPngChunkUnknown.Create(ChunkName: TChunkName);
begin
  FChunkName := ChunkName;
  FDataStream := TMemoryStream.Create;
end;

destructor TPngChunkUnknown.Destroy;
begin
  FDataStream.Free;
  inherited;
end;

function TPngChunkUnknown.CalculateChecksum: Integer;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt-1] of Byte;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FDataStream.Size-1 do
    Inc(Result, PByteArray(FDataStream.Memory)[i]);
end;

procedure TPngChunkUnknown.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngChunkUnknown then
  begin
    TPngChunkUnknown(Dest).FDataStream.CopyFrom(FDataStream, 0);
    TPngChunkUnknown(Dest).FChunkName := FChunkName;
  end else
    inherited AssignTo(Dest);
end;

function TPngChunkUnknown.GetData(Index: Integer): Byte;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt-1] of Byte;
begin
  if (Index < 0) or (Index >= FDataStream.Size) then
    raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [index]);

  Result := PByteArray(FDataStream.Memory)[Index];
end;

function TPngChunkUnknown.GetChunkSize: Cardinal;
begin
  Result := FDataStream.Size;
end;

function TPngChunkUnknown.GetChunkNameAsString: AnsiString;
begin
  Result := FChunkName;
end;

function TPngChunkUnknown.GetChunkData: pointer;
begin
  Result := FDataStream.Memory;
end;

function TPngChunkUnknown.GetChunkName: TChunkName;
begin
  Result := FChunkName;
end;

procedure TPngChunkUnknown.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  Assert(Stream.Position+ChunkSize <= Stream.Size);

  FDataStream.Clear;
  FDataStream.Position := 0;
  if ChunkSize > 0 then
    FDataStream.CopyFrom(Stream, ChunkSize);
end;

procedure TPngChunkUnknown.WriteToStream(Stream: TStream);
begin
  Stream.CopyFrom(FDataStream, 0);
end;

procedure TPngChunkUnknown.SetData(Index: Integer; const Value: Byte);
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt-1] of Byte;
begin
  if (Index < 0) or (Index >= FDataStream.Size) then
    raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  PByteArray(FDataStream.Memory)[Index] := Value;
end;

end.
