unit GR32_PortableNetworkGraphic.ZLib;

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
  SysUtils,
{$IFDEF FPC}
  ZBase, ZDeflate, ZInflate;
{$ELSE}
  {$IFDEF ZLibEx}
    ZLibEx, ZLibExApi,
  {$ELSE}
    {$if (CompilerVersion >= 32)} System.zlib, {$else} zlib, {$ifend}
  {$ENDIF}
{$ENDIF}
  GR32_PortableNetworkGraphic.Types;

//------------------------------------------------------------------------------
//
//      ZCompress
//
//------------------------------------------------------------------------------
procedure ZCompress(Data: Pointer; Size: Integer; const Output: TStream; Level: Byte = Z_BEST_COMPRESSION); overload;
procedure ZCompress(const Input: TMemoryStream; const Output: TStream; Level: Byte = Z_BEST_COMPRESSION); overload;


//------------------------------------------------------------------------------
//
//      ZDecompress
//
//------------------------------------------------------------------------------
procedure ZDecompress(Data: Pointer; Size: Integer; const Output: TStream); overload;
procedure ZDecompress(const Input: TMemoryStream; const Output: TStream); overload;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

{$IFDEF FPC}
type
  TZStreamRec = z_stream;
{$ENDIF}

//------------------------------------------------------------------------------
//
//      ZCompress
//
//------------------------------------------------------------------------------
procedure ZCompress(Data: Pointer; Size: Integer; const Output: TStream; Level: Byte); overload;
const
  CBufferSize = $8000;
var
  ZStreamRecord : TZStreamRec;
  ZResult       : Integer;
  TempBuffer    : Pointer;
begin
  FillChar(ZStreamRecord, SizeOf(TZStreamRec), 0);

  ZStreamRecord.next_in := Data;
  ZStreamRecord.avail_in := Size;
  {$IFNDEF FPC}
  {$IFNDEF ZLibEx}
  ZStreamRecord.zalloc := zlibAllocMem;
  ZStreamRecord.zfree := zlibFreeMem;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC}
  if DeflateInit_(@ZStreamRecord, Level, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during compression');
  {$ELSE}
  if DeflateInit_(ZStreamRecord, Level, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during compression');
  {$ENDIF}

  GetMem(TempBuffer, CBufferSize);
  try
    while ZStreamRecord.avail_in > 0 do
    begin
      ZStreamRecord.next_out := TempBuffer;
      ZStreamRecord.avail_out := CBufferSize;

      deflate(ZStreamRecord, Z_NO_FLUSH);

      Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
    end;

    repeat
      ZStreamRecord.next_out := TempBuffer;
      ZStreamRecord.avail_out := CBufferSize;

      ZResult := deflate(ZStreamRecord, Z_FINISH);

      Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
    until (ZResult = Z_STREAM_END) and (ZStreamRecord.avail_out > 0);
  finally
    FreeMem(TempBuffer);
  end;

  if deflateEnd(ZStreamRecord) > 0 then
    raise EPngError.Create('Error on stream validation');
end;

procedure ZCompress(const Input: TMemoryStream; const Output: TStream; Level: Byte); overload;
begin
  ZCompress(Input.Memory, Input.Size, Output, Level);
end;


//------------------------------------------------------------------------------
//
//      ZDecompress
//
//------------------------------------------------------------------------------
procedure ZDecompress(Data: Pointer; Size: Integer; const Output: TStream); overload;
const
  CBufferSize = $8000;
var
  ZStreamRecord : TZStreamRec;
  ZResult       : Integer;
  TempBuffer    : Pointer;
begin
  FillChar(ZStreamRecord, SizeOf(TZStreamRec), 0);

  ZStreamRecord.next_in := Data;
  ZStreamRecord.avail_in := Size;
  {$IFNDEF FPC}
  {$IFNDEF ZLibEx}
  ZStreamRecord.zalloc := zlibAllocMem;
  ZStreamRecord.zfree := zlibFreeMem;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC}
  if inflateInit_(@ZStreamRecord, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during decompression');
  {$ELSE}
  if inflateInit_(ZStreamRecord, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during decompression');
  {$ENDIF}
  try

    GetMem(TempBuffer, CBufferSize);
    try
      ZResult := Z_OK;

      while (ZStreamRecord.avail_in > 0) and (ZResult <> Z_STREAM_END) do
      begin
        ZStreamRecord.next_out := TempBuffer;
        ZStreamRecord.avail_out := CBufferSize;

        ZResult := inflate(ZStreamRecord, Z_NO_FLUSH);

        if ZResult < 0 then
          raise EPngError.CreateFmt('Error during decompression: %d', [ZResult]);

        Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
      end;

    finally
      FreeMem(TempBuffer);
    end;

  finally
    if inflateEnd(ZStreamRecord) > 0 then
      raise EPngError.Create('Error on stream validation');
  end;
end;

procedure ZDecompress(const Input: TMemoryStream; const Output: TStream); overload;
begin
  ZDecompress(Input.Memory, Input.Size, Output);
end;


end.
