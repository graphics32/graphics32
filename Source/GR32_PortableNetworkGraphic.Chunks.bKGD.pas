unit GR32_PortableNetworkGraphic.Chunks.bKGD;

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
  Classes,
  GR32_PortableNetworkGraphic.Types,
  GR32_PortableNetworkGraphic.Chunks;

//------------------------------------------------------------------------------
//
//      TPngBackgroundColorFormat*
//
//------------------------------------------------------------------------------
type
  TCustomPngBackgroundColor = class abstract(TPersistent)
  protected
    class function GetChunkSize: Cardinal; virtual; abstract;
  public
    procedure ReadFromStream(Stream: TStream); virtual; abstract;
    procedure WriteToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Cardinal read GetChunkSize;
  end;

  TPngBackgroundColorFormat04 = class(TCustomPngBackgroundColor)
  private
    FGraySampleValue : Word;
  protected
    class function GetChunkSize: Cardinal; override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;

  TPngBackgroundColorFormat26 = class(TCustomPngBackgroundColor)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    class function GetChunkSize: Cardinal; override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;

  TPngBackgroundColorFormat3 = class(TCustomPngBackgroundColor)
  private
    FIndex : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property PaletteIndex: Byte read FIndex write FIndex;
  end;

//------------------------------------------------------------------------------
//
//      TPngChunkBackgroundColor
//
//------------------------------------------------------------------------------
type
  TPngChunkBackgroundColor = class(TCustomDefinedChunkWithHeader)
  private
    FBackground : TCustomPngBackgroundColor;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

  public
    constructor Create(Header: TPngChunkImageHeader); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    procedure HeaderChanged; override;

    property Background: TCustomPngBackgroundColor read FBackground;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32.BigEndian;

//------------------------------------------------------------------------------
//
//      TPngBackgroundColorFormat*
//
//------------------------------------------------------------------------------
procedure TPngBackgroundColorFormat04.Assign(Source: TPersistent);
begin
  if (Source is TPngBackgroundColorFormat04) then
    FGraySampleValue := TPngBackgroundColorFormat04(Source).GraySampleValue
  else
    inherited;
end;

class function TPngBackgroundColorFormat04.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngBackgroundColorFormat04.ReadFromStream(Stream: TStream);
begin
  FGraySampleValue := BigEndian.ReadWord(Stream);
end;

procedure TPngBackgroundColorFormat04.WriteToStream(Stream: TStream);
begin
  BigEndian.WriteWord(Stream, FGraySampleValue);
end;


//------------------------------------------------------------------------------

procedure TPngBackgroundColorFormat26.Assign(Source: TPersistent);
begin
  if (Source is TPngBackgroundColorFormat26) then
  begin
    FRedSampleValue := TPngBackgroundColorFormat26(Source).RedSampleValue;
    FBlueSampleValue := TPngBackgroundColorFormat26(Source).BlueSampleValue;
    FGreenSampleValue := TPngBackgroundColorFormat26(Source).GreenSampleValue;
  end else
    inherited;
end;

class function TPngBackgroundColorFormat26.GetChunkSize: Cardinal;
begin
  Result := 6;
end;

procedure TPngBackgroundColorFormat26.ReadFromStream(Stream: TStream);
begin
  FRedSampleValue := BigEndian.ReadWord(Stream);
  FGreenSampleValue := BigEndian.ReadWord(Stream);
  FBlueSampleValue := BigEndian.ReadWord(Stream);
end;

procedure TPngBackgroundColorFormat26.WriteToStream(Stream: TStream);
begin
  BigEndian.WriteWord(Stream, FRedSampleValue);
  BigEndian.WriteWord(Stream, FGreenSampleValue);
  BigEndian.WriteWord(Stream, FBlueSampleValue);
end;

//------------------------------------------------------------------------------

procedure TPngBackgroundColorFormat3.Assign(Source: TPersistent);
begin
  if (Source is TPngBackgroundColorFormat3) then
    FIndex := TPngBackgroundColorFormat3(Source).PaletteIndex
  else
    inherited;
end;

class function TPngBackgroundColorFormat3.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TPngBackgroundColorFormat3.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FIndex, 1);
end;

procedure TPngBackgroundColorFormat3.WriteToStream(Stream: TStream);
begin
  Stream.Write(FIndex, 1);
end;


//------------------------------------------------------------------------------
//
//      TPngChunkBackgroundColor
//
//------------------------------------------------------------------------------
procedure TPngChunkBackgroundColor.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkBackgroundColor) then
    FBackground.Assign(TPngChunkBackgroundColor(Source).Background);
end;

constructor TPngChunkBackgroundColor.Create(Header: TPngChunkImageHeader);
begin
  inherited;

  case Header.ColorType of
    ctGrayscale, ctGrayscaleAlpha:
      FBackground := TPngBackgroundColorFormat04.Create;

    ctTrueColor, ctTrueColorAlpha:
      FBackground := TPngBackgroundColorFormat26.Create;

    ctIndexedColor:
      FBackground := TPngBackgroundColorFormat3.Create;
  end;
end;

destructor TPngChunkBackgroundColor.Destroy;
begin
  FBackground.Free;
  inherited;
end;

class function TPngChunkBackgroundColor.GetClassChunkName: TChunkName;
begin
  Result := 'bKGD';
end;

procedure TPngChunkBackgroundColor.HeaderChanged;
var
  OldBackground : TCustomPngBackgroundColor;
begin
  inherited;

  // store old background object
  OldBackground := FBackground;

  // change background object class
  case FHeader.ColorType of
    ctGrayscale, ctGrayscaleAlpha:
      if not (FBackground is TPngBackgroundColorFormat04) then
        FBackground := TPngBackgroundColorFormat04.Create;

    ctTrueColor, ctTrueColorAlpha :
      if not (FBackground is TPngBackgroundColorFormat26) then
        FBackground := TPngBackgroundColorFormat26.Create;

    ctIndexedColor :
      if not (FBackground is TPngBackgroundColorFormat3) then
        FBackground := TPngBackgroundColorFormat3.Create;
  else
    FBackground := nil;
  end;

  if (OldBackground <> nil) and (OldBackground <> FBackground)  then
  begin
    if (FBackground <> nil) then
      FBackground.Assign(OldBackground);
    OldBackground.Free;
  end;
end;

function TPngChunkBackgroundColor.GetChunkSize: Cardinal;
begin
  if (FBackground <> nil) then
    Result := FBackground.GetChunkSize
  else
    Result := 0;
end;

procedure TPngChunkBackgroundColor.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  if (FBackground <> nil) then
    FBackground.ReadFromStream(Stream);
end;

procedure TPngChunkBackgroundColor.WriteToStream(Stream: TStream);
begin
  if (FBackground <> nil) then
    FBackground.WriteToStream(Stream);
end;

initialization
  RegisterPngChunk(TPngChunkBackgroundColor);
end.
