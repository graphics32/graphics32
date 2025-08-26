unit GR32_PortableNetworkGraphic.Chunks.tEXt;

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
//      TCustomChunkPngText
//
//------------------------------------------------------------------------------
type
  TCustomChunkPngText = class(TCustomDefinedChunkWithHeader)
  private
    procedure SetKeyword(const Value: AnsiString);
    procedure SetText(const Value: AnsiString);
  protected
    FKeyword : AnsiString;
    FText    : AnsiString;
    procedure KeywordChanged; virtual;
    procedure TextChanged; virtual;
  public
    procedure Assign(Source: TPersistent); override;

    property Keyword: AnsiString read FKeyword write SetKeyword;
    property Text: AnsiString read FText write SetText;
  end;


//------------------------------------------------------------------------------
//
//      TPngChunkText
//
//------------------------------------------------------------------------------
type
  TPngChunkText = class(TCustomChunkPngText)
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TCustomChunkPngText
//
//------------------------------------------------------------------------------
procedure TCustomChunkPngText.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TCustomChunkPngText) then
  begin
    FKeyword := TCustomChunkPngText(Source).Keyword;
    FText    := TCustomChunkPngText(Source).Text;
  end;
end;

procedure TCustomChunkPngText.SetKeyword(const Value: AnsiString);
begin
  if FKeyword <> Value then
  begin
    FKeyword := Value;
    KeywordChanged;
  end;
end;

procedure TCustomChunkPngText.SetText(const Value: AnsiString);
begin
  if FText <> Value then
  begin
    FText := Value;
    TextChanged;
  end;
end;

procedure TCustomChunkPngText.KeywordChanged;
begin
  // yet empty
end;

procedure TCustomChunkPngText.TextChanged;
begin
  // yet empty
end;


//------------------------------------------------------------------------------
//
//      TPngChunkText
//
//------------------------------------------------------------------------------
class function TPngChunkText.GetClassChunkName: TChunkName;
begin
  Result := 'tEXt';
end;

function TPngChunkText.GetChunkSize: Cardinal;
begin
  Result := Length(FKeyword) + Length(FText) + 1;
end;

procedure TPngChunkText.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
var
  Index : Integer;
begin
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
    if (Index > High(FKeyword)) then
      raise EPngError.Create(RCStrChunkInvalid);
  end;

  // read text
  SetLength(FText, Stream.Size - Stream.Position);
  if (Stream.Position < Stream.Size) then
    Stream.Read(FText[1], SizeOf(Byte)*(Stream.Size-Stream.Position));
end;

procedure TPngChunkText.WriteToStream(Stream: TStream);
var
  Temp  : Byte;
begin
  // write keyword
  Stream.Write(FKeyword[1], Length(FKeyword));

  // write separator
  Temp := 0;
  Stream.Write(Temp, 1);

  // write text
  if (Length(FText) > 0) then
    Stream.Write(FText[1], Length(FText));
end;

initialization
  RegisterPngChunk(TPngChunkText);
end.
