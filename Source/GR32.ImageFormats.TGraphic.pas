unit GR32.ImageFormats.TGraphic;

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

{$include GR32.inc}

uses
  Classes,
  Graphics,
  SysUtils,
  GR32,
  GR32_Backends,
  GR32.ImageFormats;

//------------------------------------------------------------------------------
//
//      TCustomImageFormatAdapterTGraphic
//
//------------------------------------------------------------------------------
// Implements IImageFormatAdapter for the TGraphic class.
//------------------------------------------------------------------------------
type
  TCustomImageFormatAdapterTGraphic = class(TCustomImageFormatAdapter,
    IImageFormatAdapter)
  strict private
    FGraphicClass: TGraphicClass;
  protected
    class procedure AssignFromGraphicMasked(TargetBitmap: TCustomBitmap32; SrcGraphic: TGraphic);
    class procedure AssignFromGraphicPlain(TargetBitmap: TCustomBitmap32; SrcGraphic: TGraphic; FillColor: TColor32;
      ResetAlphaAfterDrawing: Boolean);
  strict protected
    // IImageFormatAdapter
    function CanAssignFrom(Source: TPersistent): boolean; override;
    function AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean; override;
    function CanAssignTo(Dest: TPersistent): boolean; override;
    function AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean; override;
  protected
    property GraphicClass: TGraphicClass read FGraphicClass;
  public
    constructor Create; overload;
    constructor Create(AGraphicClass: TGraphicClass); overload;
  end;


//------------------------------------------------------------------------------
//
//      TImageFormatReaderTGraphic
//      TImageFormatReaderWriterTGraphic
//
//------------------------------------------------------------------------------
// Implements file related interfaces for the TGraphic class.
//------------------------------------------------------------------------------
// Note: A default implementation of the IImageFormatWriter interface is
// provided but descedant classes has to declare implicit support for the
// interface if they actually support it. The TImageFormatReaderWriterTGraphic
// class does this.
// This is because not all TGraphic implementations support writing data (e.g.
// most of the GraphicEx library).
//------------------------------------------------------------------------------
type
  TImageFormatReaderTGraphic = class(TCustomImageFormatAdapterTGraphic,
    IImageFormatAdapter,
    IImageFormatFileInfo,
    IImageFormatReader)
  strict private
    FDescription: string;
    FFileTypes: TFileTypes;
  private
    // IImageFormatFileInfo
    function ImageFormatDescription: string;
    function ImageFormatFileTypes: TFileTypes;
  strict protected
    // IImageFormatReader
    function CanLoadFromStream(AStream: TStream): boolean; virtual;
    function LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean; virtual;
  strict protected
    // IImageFormatWriter
    procedure SaveToStream(ASource: TCustomBitmap32; AStream: TStream); virtual;
  public
{$if defined(DynArrayOps)}
    constructor Create(AGraphicClass: TGraphicClass; const ADescription: string; const AFileTypes: TFileTypes = nil);
    constructor CreateEx(AGraphicClass: TGraphicClass; const ADescription: string; const AFileTypes: array of string); deprecated 'Use Create with dynamic array';
{$else}
    constructor CreateEx(AGraphicClass: TGraphicClass; const ADescription: string; const AFileTypes: TFileTypes = nil);
    constructor Create(AGraphicClass: TGraphicClass; const ADescription: string; const AFileTypes: array of string);
{$ifend}
  end;

  TImageFormatReaderWriterTGraphic = class(TImageFormatReaderTGraphic, IImageFormatWriter)
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Types;

type
  TGraphicCracker = class(TGraphic);

//------------------------------------------------------------------------------
//
//      TCustomImageFormatAdapterTGraphic
//
//------------------------------------------------------------------------------
constructor TCustomImageFormatAdapterTGraphic.Create(AGraphicClass: TGraphicClass);
begin
  inherited Create;
  FGraphicClass := AGraphicClass;
end;

constructor TCustomImageFormatAdapterTGraphic.Create;
begin
  Create(TGraphic);
end;

//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TCustomImageFormatAdapterTGraphic.CanAssignFrom(Source: TPersistent): boolean;
begin
  Result := Source.ClassType.InheritsFrom(GraphicClass);
end;

function TCustomImageFormatAdapterTGraphic.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
begin
  if (Source.ClassType.InheritsFrom(GraphicClass)) then
  begin
    Result := True;
    AssignFromGraphicPlain(Dest, TGraphic(Source), clWhite32, True);
  end else
    Result := inherited;
end;

//------------------------------------------------------------------------------

class procedure TCustomImageFormatAdapterTGraphic.AssignFromGraphicPlain(TargetBitmap: TCustomBitmap32;
  SrcGraphic: TGraphic; FillColor: TColor32; ResetAlphaAfterDrawing: Boolean);
var
  SavedBackend: TCustomBackend;
  Canvas: TCanvas;
  DeviceContextSupport: IDeviceContextSupport;
  CanvasSupport: ICanvasSupport;
  InteroperabilitySupport: IInteroperabilitySupport;
begin
  if not Assigned(SrcGraphic) then
    Exit;
  RequireBackendSupport(TargetBitmap, [IDeviceContextSupport, ICanvasSupport,
    IInteroperabilitySupport], romOr, True, SavedBackend);
  try
    TargetBitmap.SetSize(SrcGraphic.Width, SrcGraphic.Height);
    if TargetBitmap.Empty then Exit;

    TargetBitmap.Clear(FillColor);

    if Supports(TargetBitmap.Backend, IInteroperabilitySupport, InteroperabilitySupport) then
    begin
      InteroperabilitySupport.CopyFrom(SrcGraphic);
      InteroperabilitySupport := nil;
    end else
    if Supports(TargetBitmap.Backend, ICanvasSupport, CanvasSupport) then
    begin
      TGraphicCracker(SrcGraphic).Draw(CanvasSupport.Canvas,
        MakeRect(0, 0, TargetBitmap.Width, TargetBitmap.Height));
      CanvasSupport := nil;
    end else
    if Supports(TargetBitmap.Backend, IDeviceContextSupport, DeviceContextSupport) then
    begin
      Canvas := TCanvas.Create;
      try
        Canvas.Lock;
        try
          Canvas.Handle := DeviceContextSupport.Handle;

          TGraphicCracker(SrcGraphic).Draw(Canvas,
            MakeRect(0, 0, TargetBitmap.Width, TargetBitmap.Height));
        finally
          Canvas.Unlock;
        end;
      finally
        Canvas.Free;
      end;
      DeviceContextSupport := nil;
    end else
      raise Exception.Create(RCStrInpropriateBackend);

    if ResetAlphaAfterDrawing then
      TargetBitmap.ResetAlpha;
  finally
    RestoreBackend(TargetBitmap, SavedBackend);
  end;
end;

class procedure TCustomImageFormatAdapterTGraphic.AssignFromGraphicMasked(TargetBitmap: TCustomBitmap32; SrcGraphic: TGraphic);
var
  TempBitmap: TCustomBitmap32;
  I: integer;
  DstP, SrcP: PColor32;
  DstColor: TColor32;
begin
  AssignFromGraphicPlain(TargetBitmap, SrcGraphic, clWhite32, False); // mask on white
  if TargetBitmap.Empty then
  begin
    TargetBitmap.Clear;
    Exit;
  end;

  if TargetBitmap.Backend <> nil then
    // Use the same backend type as the target. See Issue #127
    TempBitmap := TCustomBitmap32.Create(TCustomBackendClass(TargetBitmap.Backend.ClassType))
  else
    TempBitmap := TCustomBitmap32.Create;
  try
    AssignFromGraphicPlain(TempBitmap, SrcGraphic, clRed32, False); // mask on red

    DstP := @TargetBitmap.Bits[0];
    SrcP := @TempBitmap.Bits[0];
    for I := 0 to TargetBitmap.Width * TargetBitmap.Height - 1 do
    begin
      DstColor := DstP^ and $00FFFFFF;
      // this checks for transparency by comparing the pixel-color of the
      // temporary bitmap (red masked) with the pixel of our
      // bitmap (white masked). if they match, make that pixel opaque
      if DstColor = (SrcP^ and $00FFFFFF) then
        DstP^ := DstColor or $FF000000
      else
      // if the colors do not match (that is the case if there is a
      // match "is clRed32 = clWhite32 ?"), just make that pixel
      // transparent:
        DstP^ := DstColor;

       Inc(SrcP); Inc(DstP);
    end;
  finally
    TempBitmap.Free;
  end;
end;

//------------------------------------------------------------------------------

function TCustomImageFormatAdapterTGraphic.CanAssignTo(Dest: TPersistent): boolean;
begin
  // Assume we can't assign unless we have an explicit class (i.e. not the default TGraphic)
  Result := (GraphicClass <> TGraphic) and Dest.ClassType.InheritsFrom(GraphicClass);
end;

function TCustomImageFormatAdapterTGraphic.AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean;
var
  Bitmap: TBitmap;
begin
  if (Dest.ClassType.InheritsFrom(GraphicClass)) then
  begin
    // Give it a go via TBitmap
    Bitmap := TBitmap.Create;
    try
      Bitmap.Assign(Source);
      Dest.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
    Result := True;
  end else
    Result := inherited;
end;


//------------------------------------------------------------------------------
//
//      TImageFormatReaderTGraphic
//
//------------------------------------------------------------------------------
{$if defined(DynArrayOps)}
constructor TImageFormatReaderTGraphic.Create(AGraphicClass: TGraphicClass;
  const ADescription: string; const AFileTypes: TFileTypes);
{$else}
constructor TImageFormatReaderTGraphic.CreateEx(AGraphicClass: TGraphicClass;
  const ADescription: string; const AFileTypes: TFileTypes);
{$ifend}
var
  FileType: string;
begin
  inherited Create(AGraphicClass);

  FDescription := ADescription;
  FFileTypes := AFileTypes;

  if (Length(FFileTypes) = 0) then
  begin
    FileType := GraphicExtension(GraphicClass);
    if (FileType <> '') then
    begin
      SetLength(FFileTypes, 1);
      FFileTypes[0] := FileType;
    end;
  end;
end;

{$if (not defined(DynArrayOps))}
constructor TImageFormatReaderTGraphic.Create(AGraphicClass: TGraphicClass;
  const ADescription: string; const AFileTypes: array of string);
{$else}
constructor TImageFormatReaderTGraphic.CreateEx(AGraphicClass: TGraphicClass;
  const ADescription: string; const AFileTypes: array of string);
{$ifend}
var
  FileTypes: TFileTypes;
  i: integer;
begin
  SetLength(FileTypes, Length(AFileTypes));
  for i := 0 to High(AFileTypes) do
    FileTypes[i] := AFileTypes[i];
{$if defined(DynArrayOps)}
  Create(AGraphicClass, ADescription, FileTypes);
{$else}
  CreateEx(AGraphicClass, ADescription, FileTypes);
{$ifend}
end;

//------------------------------------------------------------------------------
// IImageFormatFileInfo
//------------------------------------------------------------------------------

function TImageFormatReaderTGraphic.ImageFormatFileTypes: TFileTypes;
begin
  Result := FFileTypes;
end;

function TImageFormatReaderTGraphic.ImageFormatDescription: string;
begin
  Result := FDescription;
end;

//------------------------------------------------------------------------------
// IImageFormatReader
//------------------------------------------------------------------------------
function TImageFormatReaderTGraphic.CanLoadFromStream(AStream: TStream): boolean;
begin
{$ifdef LOADFROMSTREAM}
  Result := GraphicClass.CanLoadFromStream(AStream);
{$else LOADFROMSTREAM}
  Result := False;
{$endif LOADFROMSTREAM}
end;

function TImageFormatReaderTGraphic.LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
var
  Graphic: TGraphic;
begin
{$ifdef LOADFROMSTREAM}
  if (not GraphicClass.CanLoadFromStream(AStream)) then
    Exit(False);
{$endif LOADFROMSTREAM}

  Graphic := GraphicClass.Create;
  try
{$ifdef LOADFROMSTREAM}
    Graphic.LoadFromStream(AStream);
{$else LOADFROMSTREAM}
    try

      Graphic.LoadFromStream(AStream);

    except
      on E: EInvalidGraphic do
        Exit(False);
    end;
{$endif LOADFROMSTREAM}

    ADest.Assign(Graphic);
  finally
    Graphic.Free;
  end;
  Result := True;
end;

//------------------------------------------------------------------------------
// IImageFormatWriter
//------------------------------------------------------------------------------
procedure TImageFormatReaderTGraphic.SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
var
  Graphic: TGraphic;
begin
  Graphic := GraphicClass.Create;
  try
    Graphic.Assign(ASource);
    Graphic.SaveToStream(AStream);
  finally
    Graphic.Free;
  end;
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(TCustomImageFormatAdapterTGraphic.Create, ImageFormatPriorityWorse);
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

