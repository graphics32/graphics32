unit GR32_PortableNetworkGraphic.Chunks.iTXt;

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
//      TPngChunkInternationalText
//
//------------------------------------------------------------------------------
{$ifdef PNG_CHUNK_INTERNATIONAL_TEXT}
type
  TPngChunkInternationalText = class(TCustomChunkPngText)
  private
    FCompressionMethod : Byte;
    FCompressionFlag   : Byte;
    FLanguageString    : AnsiString;
    FTranslatedKeyword : string;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
    property CompressionFlag: Byte read FCompressionFlag write FCompressionFlag;
    property LanguageString: AnsiString read FLanguageString write FLanguageString;
    property TranslatedKeyword: string read FTranslatedKeyword write FTranslatedKeyword;
  end;
{$endif PNG_CHUNK_INTERNATIONAL_TEXT}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TPngChunkInternationalText
//
//------------------------------------------------------------------------------
{$ifdef PNG_CHUNK_INTERNATIONAL_TEXT}
procedure TPngChunkInternationalText.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkInternationalText) then
  begin
    FCompressionMethod := TPngChunkInternationalText(Source).CompressionMethod;
    FCompressionFlag   := TPngChunkInternationalText(Source).CompressionFlag;
    FLanguageString    := TPngChunkInternationalText(Source).LanguageString;
    FTranslatedKeyword := TPngChunkInternationalText(Source).TranslatedKeyword;
  end;
end;

class function TPngChunkInternationalText.GetClassChunkName: TChunkName;
begin
  Result := 'iTXt';
end;

function TPngChunkInternationalText.GetChunkSize: Cardinal;
begin
  Result := 0;
end;

procedure TPngChunkInternationalText.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
var
  Index : Integer;
begin
  inherited;

  // read keyword
  Index := 1;
  SetLength(FKeyword, 80);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FKeyword[Index], SizeOf(Byte));
    if FKeyword[Index] = #0 then
    begin
      SetLength(FKeyword, Index - 1);
      Break;
    end;
   Inc(Index);
  end;

  // read compression flag
  Stream.Read(FCompressionFlag, SizeOf(Byte));

  // read compression method
  Stream.Read(FCompressionMethod, SizeOf(Byte));

  // read language string
  Index := 1;
  SetLength(FLanguageString, 10);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FLanguageString[Index], SizeOf(Byte));
    if FLanguageString[Index] = #0 then
    begin
      SetLength(FLanguageString, Index - 1);
      Break;
    end;
    Inc(Index);
  end;

  // yet todo!
  Exit;
end;

procedure TPngChunkInternationalText.WriteToStream(Stream: TStream);
begin
  // TODO
  raise EPngError.CreateFmt(RCStrChunkNotImplemented, [ChunkNameAsString]);
end;
{$endif PNG_CHUNK_INTERNATIONAL_TEXT}

initialization
{$ifdef PNG_CHUNK_INTERNATIONAL_TEXT}
  RegisterPngChunk(TPngChunkInternationalText);
{$endif PNG_CHUNK_INTERNATIONAL_TEXT}
end.

