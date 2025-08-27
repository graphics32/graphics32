unit GR32_PortableNetworkGraphic.Chunks.sBIT;

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
//      TPngSignificantBitsFormat*
//
//------------------------------------------------------------------------------
type
  TCustomPngSignificantBits = class abstract(TPersistent)
  protected
    class function GetChunkSize: Cardinal; virtual; abstract;
  public
    constructor Create(BitDepth: Integer = 8); virtual; abstract;

    procedure ReadFromStream(Stream: TStream); virtual; abstract;
    procedure WriteToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Cardinal read GetChunkSize;
  end;

  TPngSignificantBitsFormat0 = class(TCustomPngSignificantBits)
  private
    FGrayBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure Assign(Source: TPersistent); override;
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GrayBits: Byte read FGrayBits write FGrayBits;
  end;

  TPngSignificantBitsFormat23 = class(TCustomPngSignificantBits)
  private
    FRedBits   : Byte;
    FBlueBits  : Byte;
    FGreenBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure Assign(Source: TPersistent); override;
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
  end;

  TPngSignificantBitsFormat4 = class(TCustomPngSignificantBits)
  private
    FGrayBits  : Byte;
    FAlphaBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure Assign(Source: TPersistent); override;
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GrayBits: Byte read FGrayBits write FGrayBits;
    property AlphaBits: Byte read FAlphaBits write FAlphaBits;
  end;

  TPngSignificantBitsFormat6 = class(TCustomPngSignificantBits)
  private
    FRedBits   : Byte;
    FBlueBits  : Byte;
    FGreenBits : Byte;
    FAlphaBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure Assign(Source: TPersistent); override;
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
    property AlphaBits: Byte read FAlphaBits write FAlphaBits;
  end;


//------------------------------------------------------------------------------
//
//      TPngChunkSignificantBits
//
//------------------------------------------------------------------------------
type
  TPngChunkSignificantBits = class(TCustomDefinedChunkWithHeader)
  private
    FSignificantBits : TCustomPngSignificantBits;
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

    property SignificantBits: TCustomPngSignificantBits read FSignificantBits;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TPngSignificantBitsFormat*
//
//------------------------------------------------------------------------------
{ TPngSignificantBitsFormat0 }

constructor TPngSignificantBitsFormat0.Create(BitDepth: Integer = 8);
begin
  FGrayBits := BitDepth;
end;

procedure TPngSignificantBitsFormat0.Assign(Source: TPersistent);
begin
  if (Source is TPngSignificantBitsFormat0) then
    FGrayBits := TPngSignificantBitsFormat0(Source).GrayBits
  else
    inherited;
end;

class function TPngSignificantBitsFormat0.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TPngSignificantBitsFormat0.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FGrayBits, 1);
end;

procedure TPngSignificantBitsFormat0.WriteToStream(Stream: TStream);
begin
  Stream.Write(FGrayBits, 1);
end;


{ TPngSignificantBitsFormat23 }

constructor TPngSignificantBitsFormat23.Create(BitDepth: Integer = 8);
begin
  FRedBits := BitDepth;
  FGreenBits := BitDepth;
  FBlueBits := BitDepth;
end;

procedure TPngSignificantBitsFormat23.Assign(Source: TPersistent);
begin
  if (Source is TPngSignificantBitsFormat23) then
  begin
    FRedBits   := TPngSignificantBitsFormat23(Source).RedBits;
    FBlueBits  := TPngSignificantBitsFormat23(Source).BlueBits;
    FGreenBits := TPngSignificantBitsFormat23(Source).GreenBits;
  end else
    inherited;
end;

class function TPngSignificantBitsFormat23.GetChunkSize: Cardinal;
begin
  Result := 3;
end;

procedure TPngSignificantBitsFormat23.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FRedBits, 1);
  Stream.Read(FGreenBits, 1);
  Stream.Read(FBlueBits, 1);
end;

procedure TPngSignificantBitsFormat23.WriteToStream(Stream: TStream);
begin
  Stream.Write(FRedBits, 1);
  Stream.Write(FGreenBits, 1);
  Stream.Write(FBlueBits, 1);
end;


{ TPngSignificantBitsFormat4 }

constructor TPngSignificantBitsFormat4.Create(BitDepth: Integer = 8);
begin
  FGrayBits := BitDepth;
  FAlphaBits := BitDepth;
end;

procedure TPngSignificantBitsFormat4.Assign(Source: TPersistent);
begin
  if (Source is TPngSignificantBitsFormat4) then
  begin
    FGrayBits  := TPngSignificantBitsFormat4(Source).GrayBits;
    FAlphaBits := TPngSignificantBitsFormat4(Source).AlphaBits;
  end else
  if (SOurce is TPngSignificantBitsFormat0) then
    FGrayBits  := TPngSignificantBitsFormat0(Source).GrayBits
  else
    inherited;
end;

class function TPngSignificantBitsFormat4.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngSignificantBitsFormat4.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FGrayBits, 1);
  Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat4.WriteToStream(Stream: TStream);
begin
  Stream.Write(FGrayBits, 1);
  Stream.Write(FAlphaBits, 1);
end;


{ TPngSignificantBitsFormat6 }

constructor TPngSignificantBitsFormat6.Create(BitDepth: Integer = 8);
begin
  FRedBits := BitDepth;
  FGreenBits := BitDepth;
  FBlueBits := BitDepth;
  FAlphaBits := BitDepth;
end;

procedure TPngSignificantBitsFormat6.Assign(Source: TPersistent);
begin
  if (Source is TPngSignificantBitsFormat6) then
  begin
    FRedBits   := TPngSignificantBitsFormat6(Source).RedBits;
    FBlueBits  := TPngSignificantBitsFormat6(Source).BlueBits;
    FGreenBits := TPngSignificantBitsFormat6(Source).GreenBits;
    FAlphaBits := TPngSignificantBitsFormat6(Source).AlphaBits;
  end else
  if (Source is TPngSignificantBitsFormat23) then
  begin
    FRedBits   := TPngSignificantBitsFormat23(Source).RedBits;
    FBlueBits  := TPngSignificantBitsFormat23(Source).BlueBits;
    FGreenBits := TPngSignificantBitsFormat23(Source).GreenBits;
  end else
    inherited;
end;

class function TPngSignificantBitsFormat6.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TPngSignificantBitsFormat6.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FRedBits, 1);
  Stream.Read(FGreenBits, 1);
  Stream.Read(FBlueBits, 1);
  Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat6.WriteToStream(Stream: TStream);
begin
  Stream.Write(FRedBits, 1);
  Stream.Write(FGreenBits, 1);
  Stream.Write(FBlueBits, 1);
  Stream.Write(FAlphaBits, 1);
end;


//------------------------------------------------------------------------------
//
//      TPngChunkSignificantBits
//
//------------------------------------------------------------------------------
constructor TPngChunkSignificantBits.Create(Header: TPngChunkImageHeader);
begin
  inherited;

  case Header.ColorType of
    ctGrayscale:
      FSignificantBits := TPngSignificantBitsFormat0.Create(Header.BitDepth);

    ctTrueColor,
    ctIndexedColor:
      FSignificantBits := TPngSignificantBitsFormat23.Create(Header.BitDepth);

    ctGrayscaleAlpha:
      FSignificantBits := TPngSignificantBitsFormat4.Create(Header.BitDepth);

    ctTrueColorAlpha:
      FSignificantBits := TPngSignificantBitsFormat6.Create(Header.BitDepth);
  end;
end;

destructor TPngChunkSignificantBits.Destroy;
begin
  FSignificantBits.Free;

  inherited;
end;

procedure TPngChunkSignificantBits.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkSignificantBits) then
    FSignificantBits.Assign(TPngChunkSignificantBits(Source).SignificantBits);
end;

class function TPngChunkSignificantBits.GetClassChunkName: TChunkName;
begin
  Result := 'sBIT';
end;

procedure TPngChunkSignificantBits.HeaderChanged;
var
  OldSignificantBits : TCustomPngSignificantBits;
begin
  inherited;

  // store old SignificantBits object
  OldSignificantBits := FSignificantBits;

  // change SignificantBits object class
  case FHeader.ColorType of
    ctGrayscale:
      if not (FSignificantBits is TPngSignificantBitsFormat0) then
        FSignificantBits := TPngSignificantBitsFormat0.Create(FHeader.BitDepth);

    ctTrueColor, ctIndexedColor:
      if not (FSignificantBits is TPngSignificantBitsFormat23) then
        FSignificantBits := TPngSignificantBitsFormat23.Create(FHeader.BitDepth);

    ctTrueColorAlpha:
      if not (FSignificantBits is TPngSignificantBitsFormat4) then
        FSignificantBits := TPngSignificantBitsFormat4.Create(FHeader.BitDepth);

    ctGrayscaleAlpha :
      if not (FSignificantBits is TPngSignificantBitsFormat6) then
        FSignificantBits := TPngSignificantBitsFormat6.Create(FHeader.BitDepth);
  else
    FSignificantBits := nil;
  end;

  if (OldSignificantBits <> nil) and (OldSignificantBits <> FSignificantBits) then
  begin
    if (FSignificantBits <> nil) then
      FSignificantBits.Assign(OldSignificantBits);
    OldSignificantBits.Free;
  end;
end;

function TPngChunkSignificantBits.GetChunkSize: Cardinal;
begin
  if (FSignificantBits <> nil) then
    Result := FSignificantBits.GetChunkSize
  else
    Result := 0;
end;

procedure TPngChunkSignificantBits.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  if (FSignificantBits <> nil) then
    FSignificantBits.ReadFromStream(Stream);
end;

procedure TPngChunkSignificantBits.WriteToStream(Stream: TStream);
begin
  if (FSignificantBits <> nil) then
    FSignificantBits.WriteToStream(Stream);
end;



initialization
  RegisterPngChunk(TPngChunkSignificantBits);
end.
