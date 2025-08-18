unit GR32_PortableNetworkGraphic.Chunks.zTXt;

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
  GR32_PortableNetworkGraphic.Chunks,
  GR32_PortableNetworkGraphic.Chunks.tEXt;

//------------------------------------------------------------------------------
//
//      TPngChunkCompressedText
//
//------------------------------------------------------------------------------
type
  TPngChunkCompressedText = class(TCustomChunkPngText)
  private
    FCompressionMethod : Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    procedure SetCompressionMethod(const Value: Byte);

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property CompressionMethod: Byte read FCompressionMethod write SetCompressionMethod;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32_PortableNetworkGraphic.ZLib;

//------------------------------------------------------------------------------
//
//      TPngChunkCompressedText
//
//------------------------------------------------------------------------------
procedure TPngChunkCompressedText.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngChunkCompressedText then
    with TPngChunkCompressedText(Dest) do
    begin
      FCompressionMethod := Self.FCompressionMethod;
    end
  else
    inherited;
end;

class function TPngChunkCompressedText.GetClassChunkName: TChunkName;
begin
  Result := 'zTXt';
end;

function TPngChunkCompressedText.GetChunkSize: Cardinal;
var
  OutputStream: TMemoryStream;
begin
  // calculate chunk size
  Result := Length(FKeyword) + 1 + 1; // +1 = separator, +1 = compression method

  if (Length(FText) > 0) then
  begin
    OutputStream := TMemoryStream.Create;
    try
      // compress text
      ZCompress(@FText[1], Length(FText), OutputStream);

      Inc(Result, OutputStream.Size);
    finally
      OutputStream.Free;
    end;
  end;
end;

procedure TPngChunkCompressedText.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
var
  DataIn     : Pointer;
  DataInSize : Integer;
  Output     : TMemoryStream;
  Index      : Integer;
begin
  inherited;

  // read keyword and null separator
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

  // read compression method
  Stream.Read(FCompressionMethod, SizeOf(Byte));
  if FCompressionMethod <> 0 then
    raise EPngError.Create(RCStrUnsupportedCompressMethod);

  // read text
  DataInSize := Stream.Size - Stream.Position;
  GetMem(DataIn, DataInSize);
  try
    Stream.Read(DataIn^, DataInSize);

    Output := TMemoryStream.Create;
    try
      ZDecompress(DataIn, DataInSize, Output);
      SetLength(FText, Output.Size);
      Move(Output.Memory^, FText[1], Output.Size);
    finally
      Output.Free;
    end;
  finally
    FreeMem(DataIn);
  end;
end;

procedure TPngChunkCompressedText.SetCompressionMethod(const Value: Byte);
begin
  if Value <> 0 then
    raise EPngError.Create(RCStrUnsupportedCompressMethod);

  FCompressionMethod := Value;
end;

procedure TPngChunkCompressedText.WriteToStream(Stream: TStream);
var
  OutputStream: TMemoryStream;
  Temp         : Byte;
begin
  if (Length(FKeyword) = 0) then
    raise EPngError.Create(RCStrChunkInvalid);

  // write keyword
  Stream.Write(FKeyword[1], Length(FKeyword));

  // write separator
  Temp := 0;
  Stream.Write(Temp, 1);

  // write compression method
  Stream.Write(FCompressionMethod, SizeOf(Byte));

  if (Length(FText) > 0) then
  begin
    OutputStream := TMemoryStream.Create;
    try
      // compress text
      ZCompress(@FText[1], Length(FText), OutputStream);

      // write text
      Stream.Write(OutputStream.Memory^, OutputStream.Size);
    finally
      OutputStream.Free;
    end;
  end;
end;

initialization
  RegisterPngChunk(TPngChunkCompressedText);
end.
