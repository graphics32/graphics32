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

{$I GR32.inc}

uses
  Classes,
{$ifdef FPC}
  LCLType,
{$endif FPC}
  GR32;

type
{$ifdef FPC}
  TClipboardFormat = LCLType.TClipboardFormat;
{$else FPC}
  TClipboardFormat = Word;
{$endif FPC}

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
//
//      Global Memory stream.
//      Can be used to read and write data to the clipboard.
//
//------------------------------------------------------------------------------
{$ifndef FPC}

type
  TGlobalMemoryStream = class(TCustomMemoryStream)
  private
    FHandle: HGlobal;
    FPointer: pointer;
  public
    constructor Create(const AHandle: HGlobal);
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint; override;
    function ReleaseHandle: HGlobal;
    property Handle: HGlobal read FHandle;
  end;

  TOwnedGlobalMemoryStream = class(TGlobalMemoryStream)
  public
    constructor Create(ASize: NativeUInt);
    destructor Destroy; override;
  end;

  TClipboardMemoryStream = class(TGlobalMemoryStream)
  private
    FClipboardFormat: TClipboardFormat;
  public
    constructor Create(AClipboardFormat: TClipboardFormat);

    property ClipboardFormat: TClipboardFormat read FClipboardFormat;
  end;

{$else FPC}

type
  TClipboardMemoryStream = class(TMemoryStream)
  private
    FClipboardFormat: TClipboardFormat;
  protected
  public
    constructor Create(AClipboardFormat: TClipboardFormat);
    property ClipboardFormat: TClipboardFormat read FClipboardFormat;
  end;

{$endif FPC}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$IFDEF FPC}
  LCLIntf,
{$ELSE FPC}
{$IFDEF WINDOWS}
  Windows,
{$ENDIF WINDOWS}
{$ENDIF FPC}
{$if defined(FRAMEWORK_VCL)}
  Vcl.Graphics,
  Vcl.Clipbrd,
{$elseif defined(FRAMEWORK_FMX)}
  FMX.Graphics,
  FMX.Clipboard,
  FMX.Platform,
  FMX.Surfaces,
{$elseif defined(FRAMEWORK_LCL)}
  Graphics,
  Clipbrd,
{$ifend}
  SysUtils,
  GR32_Resamplers;

{$if defined(FRAMEWORK_FMX)}
type
  EClipboardException = EClipboardError;
{$elseif defined(FRAMEWORK_LCL)}
const
  CF_DIBV5 = 17;

type
  EClipboardException = Exception;
{$ifend}

//------------------------------------------------------------------------------
//
//      TGlobalMemoryStream
//
//------------------------------------------------------------------------------
{$if defined(WINDOWS) and not defined(FPC)}
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
  ReleaseHandle;

  inherited Destroy;
end;

function TGlobalMemoryStream.ReleaseHandle: HGlobal;
begin
  if (FPointer <> nil) then
  begin
    if (FHandle <> 0) then
      GlobalUnlock(FHandle);
    FPointer := nil;
  end;
  Result := FHandle;
  FHandle := 0;
end;

function TGlobalMemoryStream.Write(const Buffer; Count: Integer): Longint;
var
  Pos: Int64;
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

      System.Move(Buffer, Pointer(NativeUInt(FPointer) + NativeUInt(Position))^, Count);
      Seek(Pos, soFromBeginning);

      Result := Count;
    end;
  end;
end;

//------------------------------------------------------------------------------
//
//      TOwnedGlobalMemoryStream
//
//------------------------------------------------------------------------------
constructor TOwnedGlobalMemoryStream.Create(ASize: NativeUInt);
var
  Handle: HGlobal;
begin
  Handle := GlobalAlloc(GMEM_MOVEABLE, ASize);
  if (Handle = 0) then
    RaiseLastOSError;

  try

    inherited Create(Handle);

  except
    if (Handle <> 0) then
      GlobalFree(Handle);
    raise;
  end;
end;

destructor TOwnedGlobalMemoryStream.Destroy;
var
  OwnedHandle: HGlobal;
begin
  OwnedHandle := ReleaseHandle;
  if (OwnedHandle <> 0) then
    GlobalFree(OwnedHandle);

  inherited;
end;

{$ifend}

//------------------------------------------------------------------------------
//
//      TClipboardMemoryStream
//
//------------------------------------------------------------------------------
{$ifndef FPC}
constructor TClipboardMemoryStream.Create(AClipboardFormat: TClipboardFormat);
var
  Handle: HGlobal;
begin
  FClipboardFormat := AClipboardFormat;

  Handle := GetClipboardData(FClipboardFormat);
  if (Handle = 0) then
    RaiseLastOSError;

  inherited Create(Handle);
end;
{$else FPC}
constructor TClipboardMemoryStream.Create(AClipboardFormat: TClipboardFormat);
begin
  inherited Create;
  FClipboardFormat := AClipboardFormat;

  Clipboard.GetFormat(FClipboardFormat, Self);
  Position := 0;
end;
{$endif FPC}

//------------------------------------------------------------------------------
//
//      Clipboard functions
//
//------------------------------------------------------------------------------

type
  TBitmap32Cracker = class(TCustomBitmap32);
  TMemoryStreamCracker = class(TMemoryStream);

function CopyBitmap32ToClipboard(const Source: TCustomBitmap32): boolean;
var
  Stream: TStream;
  Matte: TBitmap32;
  Bitmap: TBitmap;
  Size: integer;
{$if defined(FRAMEWORK_FMX)}
  ClipboardService: IFMXExtendedClipboardService;
{$ifend}
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

{$if defined(FRAMEWORK_FMX)}
  if (not TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, ClipboardService)) then
    exit;
{$ifend}

{$if not defined(FRAMEWORK_FMX)}
  Clipboard.Open;
{$ifend}
  try

    if (Source.Empty) then
      exit(False);

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

{$if not defined(FRAMEWORK_FMX)}
      Clipboard.Assign(Bitmap);
{$else}
      ClipboardService.SetClipboard(Bitmap);
{$ifend}

    finally
      Bitmap.Free;
    end;

    // Preallocate the minimum that we might need and no more.
    Size := 124 {124=SizeOf(TBitmapV5Header)} + Source.Width * Source.Height * SizeOf(DWORD);
{$if defined(FRAMEWORK_VCL)}
    // Copy the unaltered image as CF_DIBV5
    Stream := TOwnedGlobalMemoryStream.Create(Size);
{$else}
    Stream := TMemoryStream.Create;
{$ifend}
    try
{$if not defined(FRAMEWORK_VCL)}
      TMemoryStreamCracker(Stream).Capacity := Size;
{$ifend}

      // The clipboard needs a v5 DIB *without* a color table.
      // Note that Firefox, at the time of writing, expects a color table for v4 and v5 DIBs
      // so it will not be able to correctly read what we put on the clipboard. Our position
      // is that this is a bug in Firefox.
      //
      // See:
      // - https://bugzilla.mozilla.org/show_bug.cgi?id=1866655
      // - https://forums.getpaint.net/topic/124628-1-px-line-on-top-of-every-image-pasted-into-firefox-from-paintnet/
      // - https://github.com/graphics32/graphics32/issues/257
      //
      // See also:
      // - https://github.com/chromium/chromium/commit/e6f56636f365bdb210874bdbe63272f783792c7d
      //
      // A possible workaround for this problem is to *also* place the bitmap as a PNG on
      // the clipboard. It doesn't help with Firefox but apparently some other applications
      // give priority to the PNG format when reading from the clipboard.
      //
      TBitmap32Cracker(Source).SaveToDIBStream(Stream, False, TCustomBitmap32.TInfoHeaderVersion.InfoHeaderVersion5, False);

{$if defined(FRAMEWORK_VCL)}
      Clipboard.SetAsHandle(CF_DIBV5, TGlobalMemoryStream(Stream).ReleaseHandle);
{$elseif defined(FRAMEWORK_FMX)}
      ClipboardService.SetCustomFormat('CF_DIBV5', Stream);
{$else}
      Clipboard.AddFormat(CF_DIBV5, Stream);
{$ifend}
    finally
      Stream.Free;
    end;

  finally
{$if not defined(FRAMEWORK_FMX)}
    Clipboard.Close;
{$ifend}
  end;
end;

//------------------------------------------------------------------------------

function PasteBitmap32FromClipboard(const Dest: TCustomBitmap32): boolean;
var
  Stream: TStream;
  Bitmap: TBitmap;
{$if defined(FRAMEWORK_FMX)}
  ClipboardService: IFMXExtendedClipboardService;
  BitmapSurface: TBitmapSurface;
{$ifend}
begin
  Result := False;

{$if defined(FRAMEWORK_FMX)}
  if (not TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, ClipboardService)) then
    exit;
{$ifend}

{$if not defined(FRAMEWORK_FMX)}
  if (Clipboard.HasFormat(CF_DIBV5)) then
{$else}
  if (ClipboardService.HasCustomFormat('CF_DIBV5')) then
{$ifend}
  begin
    Dest.BeginUpdate;
    try
{$if defined(FRAMEWORK_VCL)}
      Clipboard.Open;
{$ifend}
      try
{$if not defined(FRAMEWORK_FMX)}
        Stream := TClipboardMemoryStream.Create(CF_DIBV5);
{$else}
        Stream := TMemoryStream.Create;
{$ifend}
        try
{$if defined(FRAMEWORK_FMX)}
          ClipboardService.GetCustomFormat('CF_DIBV5', Stream);
          Stream.Position := 0;
{$ifend}

          Result := TBitmap32Cracker(Dest).LoadFromDIBStream(Stream, Stream.Size);

        finally
          Stream.Free;
        end;

      finally
{$if defined(FRAMEWORK_VCL)}
        Clipboard.Close;
{$ifend}
      end;
    finally
      Dest.EndUpdate;
    end;
    Dest.Changed;
  end;

  // There's no need to fall back to CF_DIB since the clipboard will
  //synthesize CF_DIBV5 from CF_DIB.

{$if not defined(FRAMEWORK_FMX)}
  if (not Result) and (Clipboard.HasFormat(CF_BITMAP)) then
{$else}
  if (not Result) and (ClipboardService.HasImage) then
{$ifend}
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
{$if not defined(FRAMEWORK_FMX)}
        Bitmap.Assign(Clipboard);
{$else}
        BitmapSurface := ClipboardService.GetImage;
        Bitmap.Assign(BitmapSurface);
{$ifend}
        Dest.Assign(Bitmap);
      finally
        Bitmap.Free;
      end;
    finally
      Dest.EndUpdate;
    end;
    Dest.Changed;

    Result := True;
  end;
end;

//------------------------------------------------------------------------------

function CanPasteBitmap32: boolean;
{$if defined(FRAMEWORK_FMX)}
var
  ClipboardService: IFMXExtendedClipboardService;
{$ifend}
begin
  try
{$if not defined(FRAMEWORK_FMX)}
    Result:= Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_DIBV5);
{$else}
    Result := (TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, ClipboardService)) and
      (ClipboardService.HasImage) or (ClipboardService.HasCustomFormat('CF_DIBV5'));
{$ifend}
  except
    on E: EClipboardException do
      Result := False; // Something else has the clipboard open
  end;
end;

//------------------------------------------------------------------------------

function CanPasteBitmap32Alpha: boolean;
{$if defined(FRAMEWORK_FMX)}
var
  ClipboardService: IFMXExtendedClipboardService;
{$ifend}
begin
  try
{$if not defined(FRAMEWORK_FMX)}
    Result:= Clipboard.HasFormat(CF_DIBV5);
{$else}
    Result := (TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, ClipboardService)) and
      (ClipboardService.HasCustomFormat('CF_DIBV5'));
{$ifend}
  except
    on E: EClipboardException do
      Result := False; // Something else has the clipboard open
  end;
end;

//------------------------------------------------------------------------------

end.

