unit GR32.ImageFormats.TBitmap;

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
 * The Original Code is image format support for Graphics32
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

{$I GR32.inc}

implementation

uses
  Classes,
{$ifndef FPC}
  Windows,
{$else FPC}
  LCLType,
{$endif FPC}
  Graphics,
  SysUtils,
  GR32,
  GR32_Backends,
  GR32.ImageFormats.TGraphic,
  GR32.ImageFormats;

//------------------------------------------------------------------------------
//
//      TImageFormatAdapterTBitmap
//
//------------------------------------------------------------------------------
// Implements IImageFormatAdapter for the TBitmap class.
//------------------------------------------------------------------------------
type
  TImageFormatAdapterTBitmap = class(TCustomImageFormatAdapterTGraphic,
    IImageFormatAdapter,
    IImageFormatResourceReader)
  strict protected
    // IImageFormatAdapter
    function AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean; override;
    function AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean; override;
  strict private
    // IImageFormatResourceReader
    function LoadFromResource(ADest: TCustomBitmap32; AResourceType: TResourceType; AStream: TStream): boolean;
  end;

//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatAdapterTBitmap.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
var
  TransparentColor: TColor32;
  DstP: PColor32;
  I: integer;
  DstColor: TColor32;
  FontSupport: IFontSupport;
{$if defined(FRAMEWORK_FMX)}
  Data: TBitmapData;
  SrcP: PColor32;
{$ifend}
begin
  if (not (Source is TBitmap)) then
    Exit(False);

  Result := True;

{$if not defined(FRAMEWORK_FMX)}

  AssignFromGraphicPlain(Dest, TBitmap(Source), 0, TBitmap(Source).PixelFormat <> pf32bit);

  if Dest.Empty then
    Exit;

  if TBitmap(Source).Transparent then
  begin
    TransparentColor := Color32(TBitmap(Source).TransparentColor) and $00FFFFFF;
    DstP := @Dest.Bits[0];
    for I := 0 to Dest.Width * Dest.Height - 1 do
    begin
      DstColor := DstP^ and $00FFFFFF;
      if DstColor = TransparentColor then
        DstP^ := DstColor;
      Inc(DstP);
    end;
  end;

  if Supports(Dest.Backend, IFontSupport, FontSupport) then // this is optional
    FontSupport.Font.Assign(TBitmap(Source).Canvas.Font);

{$else}

  Dest.SetSize(TBitmap(Source).Width, TBitmap(Source).Height);

  TBitmap(Source).Map(TMapAccess.Read, Data);
  try
    for I := 0 to TBitmap(Source).Height-1 do
    begin
      SrcP := Data.GetScanline(I);
      DstP := Dest.GetScanline(I);
      Move(SrcP^, DstP^, Data.BytesPerLine);
    end;
  finally
    TBitmap(Source).Unmap(Data);
  end;

{$ifend}

end;

//------------------------------------------------------------------------------

function TImageFormatAdapterTBitmap.AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean;
var
  SavedBackend: TCustomBackend;
  FontSupport: IFontSupport;
{$if defined(FRAMEWORK_FMX)}
  Data: TBitmapData;
  SrcP: PColor32;
{$ifend}
begin
  if (not (Dest is TBitmap)) then
    Exit(False);

  Result := True;

  RequireBackendSupport(Source, [IDeviceContextSupport], romOr, False, SavedBackend);
  try
    TBitmap(Dest).SetSize(0, 0);

    TBitmap(Dest).PixelFormat := pf32Bit;

    TBitmap(Dest).SetSize(Source.Width, Source.Height);

    if Supports(Source.Backend, IFontSupport, FontSupport) then // this is optional
    begin
      TBitmap(Dest).Canvas.Font.Assign(FontSupport.Font);
      FontSupport := nil;
    end;

    if Source.Empty then
      Exit;

{$if not defined(FRAMEWORK_FMX)}

    TBitmap(Dest).Canvas.Lock;
    try
      (Source.Backend as IDeviceContextSupport).DrawTo(TBitmap(Dest).Canvas.Handle,
        Source.BoundsRect, Source.BoundsRect)
    finally
      TBitmap(Dest).Canvas.UnLock;
    end;

{$else}

  TBitmap(Dest).SetSize(Source.Width, Source.Height);

  TBitmap(Dest).Map(TMapAccess.Write, Data);
  try
    for I := 0 to Source.Height-1 do
    begin
      SrcP := Source.GetScanline(I);
      DstP := Data.GetScanline(I);
      Move(SrcP^, DstP^, Data.BytesPerLine);
    end;
  finally
    TBitmap(Dest).Unmap(Data);
  end;

{$ifend}
  finally
    RestoreBackend(Source, SavedBackend);
  end;
end;

//------------------------------------------------------------------------------
// IImageFormatResourceReader
//------------------------------------------------------------------------------
type
  TBitmapFileHeader = packed record
    bfType: Word;
    bfSize: DWORD;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: DWORD;
  end;

function TImageFormatAdapterTBitmap.LoadFromResource(ADest: TCustomBitmap32; AResourceType: TResourceType;
  AStream: TStream): boolean;
var
  Bitmap: TBitmap;
  BitmapFileHeader: TBitmapFileHeader;
  BitmapStream: TStream;
begin
  if (AResourceType = RT_BITMAP) then
  begin
    // TBitmap does not have any (accesible) methods to read a DIB, so we have to
    // "make believe" that the stream contains a BMP file.
    BitmapFileHeader := Default(TBitmapFileHeader);
    BitmapFileHeader.bfType := $4D42;
    BitmapStream := TMemoryStream.Create;
    try
      TMemoryStream(BitmapStream).Size := AStream.Size + SizeOf(TBitmapFileHeader);

      BitmapStream.Write(BitmapFileHeader, SizeOf(TBitmapFileHeader));
      BitmapStream.CopyFrom(AStream, 0);
      BitmapStream.Position := 0;

      Bitmap := TBitmap.Create;
      try
        Bitmap.LoadFromStream(BitmapStream);
        ADest.Assign(Bitmap);
      finally
        Bitmap.Free;
      end;

    finally
      BitmapStream.Free;
    end;
    Result := True;
  end else
    Result := False;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(TImageFormatAdapterTBitmap.Create(TBitmap), ImageFormatPriorityNormal);;
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

