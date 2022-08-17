unit GR32_Clipboard;


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
 * The Original Code is Clipboard support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2022
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  GR32;

//------------------------------------------------------------------------------
//
//      Clipboard functions
//
//------------------------------------------------------------------------------
function CopyBitmap32ToClipboard(const Source: TCustomBitmap32): boolean;
function PasteBitmap32FromClipboard(const Dest: TCustomBitmap32): boolean;
function CanPasteBitmap32: boolean;
function CanPasteBitmap32Alpha: boolean;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$IFDEF FPC}
  LCLType,
  LCLIntf,
{$ELSE FPC}
  Windows,
{$ENDIF FPC}
  Classes,
  Graphics,
  Clipbrd,
  SysUtils,
  GR32_Resamplers;

{$IFDEF FPC}
const
  CF_DIBV5 = 17;
{$ENDIF}

{$IFNDEF FPC}
type
  TGlobalMemoryStream = class(TCustomMemoryStream)
  private
    FHandle: HGlobal;
    FPointer: pointer;
  protected
  public
    constructor Create(const AHandle: HGlobal); overload;
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Handle: HGlobal read FHandle;
  end;

constructor TGlobalMemoryStream.Create(const AHandle: HGlobal);
begin
  inherited Create;
  FHandle := AHandle;
  FPointer := GlobalLock(Handle);
  if (FPointer = nil) then
    RaiseLastOSError;

  SetPointer(FPointer, GlobalSize(Handle));
end;

destructor TGlobalMemoryStream.Destroy;
begin
  if (FPointer <> nil) then
    GlobalUnlock(Handle);

  inherited Destroy;
end;

function TGlobalMemoryStream.Write(const Buffer; Count: Integer): Longint;
var
  Pos: Longint;
begin
  Result := 0;
  if (Position >= 0) and (Count >= 0) then
  begin
    Pos := Position + Count;

    if Pos > 0 then
    begin
      if Pos > Size then
      begin
        FHandle := GlobalReAlloc(FHandle, Pos, GMEM_MOVEABLE);
        if (FHandle = 0) then
          RaiseLastOSError;

        FPointer := GlobalLock(FHandle);
        if (FPointer = nil) then
          RaiseLastOSError;

        SetPointer(FPointer, Pos);
      end;

      System.Move(Buffer, Pointer(Longint(FPointer) + Position)^, Count);
      Seek(Pos, soFromBeginning);

      Result := Count;
    end;
  end;
end;
{$ENDIF FPC}

//------------------------------------------------------------------------------
//
//      Clipboard functions
//
//------------------------------------------------------------------------------

type
  TBitmap32Cracker = class(TCustomBitmap32);

function CopyBitmap32ToClipboard(const Source: TCustomBitmap32): boolean;
var
  Stream: TStream;
{$IFNDEF FPC}
  Matte: TBitmap32;
  Bitmap: TBitmap;
  Size: integer;
  Handle: HGlobal;
{$ENDIF FPC}
begin
  Result := True;

  (*
  ** We place the following data on the clipboard:
  **
  ** - CF_BITMAP
  **   This is the source bitmap rendered onto a white background.
  **   Transparency is not retained.
  **   For use by applications that doesn't support Alpha.
  **
  ** - CF_DIBV5
  **   A 32 bit DIB with alpha. This alone can be used to recreate the original
  **   32 bit bitmap, including alpha.
  **   This format provides round trip support.
  **
  ** Since Windows can synthesize between any of CF_DIB, CF_BITMAP and CF_DIBV5
  ** theoretically we could just supply the most capable format (CF_DIBV5) and
  ** let Windows supply the others. Unfortunately we need to supply both CF_DIBV5
  ** and CF_BITMAP/CF_DIB in order to work around various Windows bugs:
  **
  ** - When the clipboard synthesizes CF_DIBV5 from CF_DIB it uses BI_BITFIELDS.
  **   However, if the clipboard synthesizes CF_DIB from CF_DIBV5 with
  **   BI_BITFIELDS compression, the clipboard apparently forgets to take the
  **   extra 3 mask DWORDs into account which messes up the resulting DIB.
  **
  ** - When the clipboard synthesizes CF_DIB or CF_BITMAP from CF_DIBV5 it
  **   appears to require 68 extra bytes after the bitmap header.
  **   Inserting these 68 bytes would fix that but would also make the bitmap
  **   stream invalid for everything else.
  **   FWIW, 68 = SizeOf(BITMAPV4HEADER)-SizeOf(BITMAPINFOHEADER)...
  **
  ** As a bonus we get to control the background color of the CF_DIB/CF_BITMAP
  ** bitmap instead of the black one Windows would use.
  *)

  Clipboard.Open;
  try
    Clipboard.Clear;

    if (Source.Empty) then
      exit(False);

{$IFNDEF FPC}
    // Render the bitmap onto a white background and copy it as CF_BITMAP.
    // Note: In some older versions of Windows it appears that the
    // clipboard gives priority to the synthesized CF_BITMAP over the
    // explicit CF_BITMAP.
    Bitmap := TBitmap.Create;
    try
      Matte := TBitmap32.Create;
      try
        Matte.SetSize(Source.Width, Source.Height);
        Matte.Clear(clWhite32);
        BlockTransfer(Matte, 0, 0, Matte.ClipRect, Source, Source.BoundsRect, dmBlend);

        Bitmap.Assign(Matte);
      finally
        Matte.Free;
      end;

      Clipboard.Assign(Bitmap);

    finally
      Bitmap.Free;
    end;

    // Allocate room for BI_BITFIELDS whether we use it or not. It's just 12 bytes.
    Size := SizeOf(TBitmapV5Header) + 3 * SizeOf(DWORD) + Source.Width * Source.Height * SizeOf(DWORD);
    Handle := GlobalAlloc(GMEM_MOVEABLE, Size);
    if (Handle = 0) then
      RaiseLastOSError;
    try
      // Copy the unaltered image as CF_DIBV5
      Stream := TGlobalMemoryStream.Create(Handle);
      try

        TBitmap32Cracker(Source).SaveToDIBStream(Stream);

      finally
        Stream.Free;
      end;

      Clipboard.SetAsHandle(CF_DIBV5, Handle);
      Handle := 0;

    except
      if (Handle <> 0) then
        GlobalFree(Handle);
      raise;
    end;
{$ELSE FPC}
    Stream := TMemoryStream.Create;
    try
      Source.SaveToStream(Stream);
      Clipboard.AddFormat(PredefinedClipboardFormat(pcfBitmap), Stream);
    finally
      Stream.Free;
    end;
{$ENDIF FPC}

  finally
    Clipboard.Close;
  end;
end;

//------------------------------------------------------------------------------

function PasteBitmap32FromClipboard(const Dest: TCustomBitmap32): boolean;
var
  Stream: TStream;
{$IFNDEF FPC}
  Handle: HGlobal;
  Bitmap: TBitmap;
{$ENDIF FPC}
begin
{$IFNDEF FPC}
  Result := False;

  if (Clipboard.HasFormat(CF_DIBV5)) then
  begin
    Dest.BeginUpdate;
    try
      Win32Check(OpenClipboard(0));
      try
        Handle := GetClipboardData(CF_DIBV5);
        if (Handle = 0) then
          RaiseLastOSError;

        Stream := TGlobalMemoryStream.Create(Handle);
        try

          Result := TBitmap32Cracker(Dest).LoadFromDIBStream(Stream, Stream.Size);

        finally
          Stream.Free;
        end;

      finally
        CloseClipboard;
      end;

    finally
      Dest.EndUpdate;
      Dest.Changed;
    end;
  end;

  if (not Result) and (Clipboard.HasFormat(CF_BITMAP)) then
  begin
    // Fall back to CF_BITMAP format.
    // Note: We must do an explicit assign to a bitmap or we risk that the
    // clipboard retrives the data in some other compatible format.
    // E.g. if the clipboard contains CF_METAFILE and CF_BITMAP and we do a
    // TBitmap32.Assign(Clipboard), then we end grabbing the CF_METAFILE data
    // leading to a rasterized copy of a metafile capture of a bitmap... Ugh!
    Dest.BeginUpdate;
    try
      Bitmap := TBitmap.Create;
      try
        Bitmap.Assign(Clipboard);
        Dest.Assign(Bitmap);
      finally
        Bitmap.Free;
      end;
    finally
      Dest.EndUpdate;
      Dest.Changed;
    end;

    Result := True;
  end;
{$ELSE FPC}
  Stream := TMemoryStream.Create;
  try
    Clipboard.GetFormat(PredefinedClipboardFormat(pcfBitmap), Stream);
    Stream.Position := 0;
    Dest.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
  Result := True;
{$ENDIF FPC}
end;

//------------------------------------------------------------------------------

function CanPasteBitmap32: boolean;
begin
{$IFNDEF FPC}
  try
    Result:= Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_DIBV5);
  except
    on E: EClipboardException do
      Result := False; // Something else has the clipboard open
  end;
{$ELSE FPC}
  Result := Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap));
{$ENDIF FPC}
end;

//------------------------------------------------------------------------------

function CanPasteBitmap32Alpha: boolean;
begin
{$IFNDEF FPC}
  try
    Result:= Clipboard.HasFormat(CF_DIBV5);
  except
    on E: EClipboardException do
      Result := False; // Something else has the clipboard open
  end;
{$ELSE FPC}
  Result := Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap));
{$ENDIF FPC}
end;

//------------------------------------------------------------------------------

end.

