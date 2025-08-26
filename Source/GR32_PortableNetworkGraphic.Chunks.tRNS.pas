unit GR32_PortableNetworkGraphic.Chunks.tRNS;

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
  GR32_PortableNetworkGraphic.Types,
  GR32_PortableNetworkGraphic.Chunks;

//------------------------------------------------------------------------------
//
//      TCustomPngTransparency
//
//------------------------------------------------------------------------------
type
  TCustomPngTransparency = class abstract(TPersistent)
  protected
    function GetChunkSize: Cardinal; virtual; abstract;
  public
    procedure ReadFromStream(Stream: TStream); virtual; abstract;
    procedure WriteToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Cardinal read GetChunkSize;
  end;


//------------------------------------------------------------------------------
//
//      TPngTransparencyFormat0
//
//------------------------------------------------------------------------------
type
  TPngTransparencyFormat0 = class(TCustomPngTransparency)
  private
    FGraySampleValue : Word;
  protected
    function GetChunkSize: Cardinal; override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;


//------------------------------------------------------------------------------
//
//      TPngTransparencyFormat2
//
//------------------------------------------------------------------------------
type
  TPngTransparencyFormat2 = class(TCustomPngTransparency)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    function GetChunkSize: Cardinal; override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;


//------------------------------------------------------------------------------
//
//      TPngTransparencyFormat3
//
//------------------------------------------------------------------------------
type
  TPngTransparencyFormat3 = class(TCustomPngTransparency)
  private
    function GetCount: Cardinal;
    function GetTransparency(Index: Cardinal): Byte;
  protected
    FTransparency : array of Byte;
    function GetChunkSize: Cardinal; override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property Count: Cardinal read GetCount;
    property Transparency[Index: Cardinal]: Byte read GetTransparency;
  end;


//------------------------------------------------------------------------------
//
//      TPngChunkTransparency
//
//------------------------------------------------------------------------------
type
  TPngChunkTransparency = class(TCustomDefinedChunkWithHeader)
  protected
    FTransparency : TCustomPngTransparency;
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create(Header: TPngChunkImageHeader); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    procedure HeaderChanged; override;

    property Transparency: TCustomPngTransparency read FTransparency;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32.BigEndian;

//------------------------------------------------------------------------------
//
//      TPngChunkTransparency
//
//------------------------------------------------------------------------------
procedure TPngChunkTransparency.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkTransparency) then
  begin
    Assert(FTransparency <> nil);
    Assert(FTransparency.ClassType = TPngChunkTransparency(Source).Transparency.ClassType);

    FTransparency.Assign(TPngChunkTransparency(Source).Transparency);
  end;
end;

constructor TPngChunkTransparency.Create(Header: TPngChunkImageHeader);
begin
  inherited;
  case Header.ColorType of
    ctGrayscale:
      FTransparency := TPngTransparencyFormat0.Create;

    ctTrueColor:
      FTransparency := TPngTransparencyFormat2.Create;

    ctIndexedColor:
      FTransparency := TPngTransparencyFormat3.Create;
  end;
end;

destructor TPngChunkTransparency.Destroy;
begin
  FTransparency.Free;
  inherited;
end;

class function TPngChunkTransparency.GetClassChunkName: TChunkName;
begin
  Result := 'tRNS';
end;

procedure TPngChunkTransparency.HeaderChanged;
var
  OldTransparency : TCustomPngTransparency;
begin
  inherited;

  // store old transparency object
  OldTransparency := FTransparency;

  // change transparency object class
  case FHeader.ColorType of
    ctGrayscale:
      if not (FTransparency is TPngTransparencyFormat0) then
        FTransparency := TPngTransparencyFormat0.Create;

    ctTrueColor:
      if not (FTransparency is TPngTransparencyFormat2) then
        FTransparency := TPngTransparencyFormat2.Create;

    ctIndexedColor:
      if not (FTransparency is TPngTransparencyFormat3) then
        FTransparency := TPngTransparencyFormat3.Create;
  else
    FTransparency := nil;
  end;

  if (OldTransparency <> nil) and (OldTransparency <> FTransparency) then
  begin
    if (FTransparency <> nil) then
      FTransparency.Assign(OldTransparency);
    OldTransparency.Free;
  end;
end;

function TPngChunkTransparency.GetChunkSize: Cardinal;
begin
  if (FTransparency <> nil) then
    Result := FTransparency.ChunkSize
  else
    Result := 0;
end;

procedure TPngChunkTransparency.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (FTransparency <> nil) then
    FTransparency.ReadFromStream(Stream);
end;

procedure TPngChunkTransparency.WriteToStream(Stream: TStream);
begin
  // check consistency
  case FHeader.ColorType of
    ctGrayscale:
      if not (FTransparency is TPngTransparencyFormat0) then
        raise EPngError.Create(RCStrWrongTransparencyFormat);

    ctTrueColor:
      if not (FTransparency is TPngTransparencyFormat2) then
        raise EPngError.Create(RCStrWrongTransparencyFormat);

    ctIndexedColor:
      if not (FTransparency is TPngTransparencyFormat3) then
        raise EPngError.Create(RCStrWrongTransparencyFormat);
  end;

  if (FTransparency <> nil) then
    FTransparency.WriteToStream(Stream);
end;


//------------------------------------------------------------------------------
//
//      TPngTransparencyFormat0
//
//------------------------------------------------------------------------------
procedure TPngTransparencyFormat0.Assign(Source: TPersistent);
begin
  if (Source is TPngTransparencyFormat0) then
    FGraySampleValue := TPngTransparencyFormat0(Source).GraySampleValue
  else
    inherited;
end;

function TPngTransparencyFormat0.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngTransparencyFormat0.ReadFromStream(Stream: TStream);
begin
  inherited;

  FGraySampleValue := BigEndian.ReadWord(Stream);
end;

procedure TPngTransparencyFormat0.WriteToStream(Stream: TStream);
begin
  inherited;

  BigEndian.WriteWord(Stream, FGraySampleValue);
end;


//------------------------------------------------------------------------------
//
//      TPngTransparencyFormat2
//
//------------------------------------------------------------------------------
procedure TPngTransparencyFormat2.Assign(Source: TPersistent);
begin
  if (Source is TPngTransparencyFormat2) then
  begin
    FRedSampleValue := TPngTransparencyFormat2(Source).RedSampleValue;
    FBlueSampleValue := TPngTransparencyFormat2(Source).BlueSampleValue;
    FGreenSampleValue := TPngTransparencyFormat2(Source).GreenSampleValue;
  end else
    inherited;
end;

function TPngTransparencyFormat2.GetChunkSize: Cardinal;
begin
  Result := 6;
end;

procedure TPngTransparencyFormat2.ReadFromStream(Stream: TStream);
begin
  inherited;

  FRedSampleValue  := BigEndian.ReadWord(Stream);
  FBlueSampleValue  := BigEndian.ReadWord(Stream);
  FGreenSampleValue  := BigEndian.ReadWord(Stream);
end;

procedure TPngTransparencyFormat2.WriteToStream(Stream: TStream);
begin
  inherited;

  BigEndian.WriteWord(Stream, FRedSampleValue);
  BigEndian.WriteWord(Stream, FBlueSampleValue);
  BigEndian.WriteWord(Stream, FGreenSampleValue);
end;


//------------------------------------------------------------------------------
//
//      TPngTransparencyFormat3
//
//------------------------------------------------------------------------------
procedure TPngTransparencyFormat3.Assign(Source: TPersistent);
begin
  if (Source is TPngTransparencyFormat3) then
    FTransparency := Copy(TPngTransparencyFormat3(Source).FTransparency)
  else
    inherited;
end;

function TPngTransparencyFormat3.GetChunkSize: Cardinal;
begin
  Result := Count;
end;

function TPngTransparencyFormat3.GetCount: Cardinal;
begin
  Result := Length(FTransparency);
end;

function TPngTransparencyFormat3.GetTransparency(Index: Cardinal): Byte;
begin
  if Index < Count then
    Result := FTransparency[Index]
  else
    raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TPngTransparencyFormat3.ReadFromStream(Stream: TStream);
begin
  inherited;

  SetLength(FTransparency, Stream.Size - Stream.Position);
  Stream.Read(FTransparency[0], Length(FTransparency));
end;

procedure TPngTransparencyFormat3.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FTransparency[0], Length(FTransparency));
end;

initialization
  RegisterPngChunk(TPngChunkTransparency);
end.
