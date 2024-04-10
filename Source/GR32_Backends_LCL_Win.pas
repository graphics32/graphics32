unit GR32_Backends_LCL_Win;

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
 * The Original Code is Backend Extension for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Andre Beckedorf - metaException
 * Andre@metaException.de
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Christian Budde
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF LCLWin32} Windows, {$ENDIF} LCLIntf, LCLType, Types, Controls,
  SysUtils, Classes, Graphics, GR32, GR32_Backends, GR32_Backends_Generic,
  GR32_Containers, GR32_Image, GR32_Paths;

type
  { TLCLBackend }
  { This backend uses the LCL to manage and provide the buffer and additional
    graphics sub system features. The backing buffer is kept in memory. }

  TLCLBackend = class(TCustomBackend,
      IPaintSupport,
      IBitmapContextSupport,
      IDeviceContextSupport,
      ITextSupport,
      IFontSupport,
      ITextToPathSupport,
      ICanvasSupport,
      IInteroperabilitySupport,
      IUpdateRectSupport
    )
  private
    procedure FontChangedHandler(Sender: TObject);
    procedure CanvasChangedHandler(Sender: TObject);
    procedure CanvasChanged;
    procedure FontChanged;
  protected
    FBitmapInfo: TBitmapInfo;
    FBitmapHandle: HBITMAP;
    FHDC: HDC;
    FFont: TFont;
    FCanvas: TCanvas;
    FFontHandle: HFont;
    FMapHandle: THandle;

    FOnFontChange: TNotifyEvent;
    FOnCanvasChange: TNotifyEvent;

    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;

    procedure PrepareFileMapping(NewWidth, NewHeight: Integer); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Changed; override;

    function Empty: Boolean; override;
  public
    { IPaintSupport }
    procedure ImageNeeded;
    procedure CheckPixmap;
    procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList;
      ACanvas: TCanvas; APaintBox: TCustomPaintBox32);

    { IBitmapContextSupport }
    function GetBitmapInfo: TBitmapInfo;
    function GetBitmapHandle: THandle;

    property BitmapInfo: TBitmapInfo read GetBitmapInfo;
    property BitmapHandle: THandle read GetBitmapHandle;

    { IDeviceContextSupport }
    function GetHandle: HDC;

    procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;

    property Handle: HDC read GetHandle;

    { ITextSupport }
    procedure Textout(X, Y: Integer; const Text: string); overload;
    procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: string); overload;
    procedure Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string); overload;
    function  TextExtent(const Text: string): TSize;

    { IFontSupport }
    function GetOnFontChange: TNotifyEvent;
    procedure SetOnFontChange(Handler: TNotifyEvent);
    function GetFont: TFont;
    procedure SetFont(const Font: TFont);

    procedure UpdateFont;
    property Font: TFont read GetFont write SetFont;
    property OnFontChange: TNotifyEvent read FOnFontChange write FOnFontChange;

    { ITextToPathSupport }
    procedure TextToPath(Path: TCustomPath; const X, Y: TFloat; const Text: string); overload;
    procedure TextToPath(Path: TCustomPath; const DstRect: TFloatRect; const Text: string; Flags: Cardinal); overload;
    function MeasureText(const DstRect: TFloatRect; const Text: string; Flags: Cardinal): TFloatRect;

    { IInteroperabilitySupport }
    function CopyFrom(Graphic: TGraphic): Boolean; overload;

    { ICanvasSupport }
    function GetCanvasChange: TNotifyEvent;
    procedure SetCanvasChange(Handler: TNotifyEvent);
    function GetCanvas: TCanvas;

    procedure DeleteCanvas;
    function CanvasAllocated: Boolean;

    property Canvas: TCanvas read GetCanvas;
    property OnCanvasChange: TNotifyEvent read GetCanvasChange write SetCanvasChange;

    { IUpdateRectSupport }
    procedure InvalidateRect(AControl: TWinControl; const ARect: TRect);
    procedure GetUpdateRects(AControl: TWinControl; AUpdateRects: TRectList; AReservedCapacity: integer; var AFullUpdate: boolean); overload;
  end;

  { TLCLGDIMMFBackend }
  { Same as TGDIBackend but relies on memory mapped files or mapped swap space
    for the backing buffer. }

  TLCLMMFBackend = class(TLCLBackend)
  private
    FMapFileHandle: THandle;
    FMapIsTemporary: Boolean;
    FMapFileName: string;
  protected
    procedure PrepareFileMapping(NewWidth, NewHeight: Integer); override;
  public
    constructor Create(Owner: TBitmap32; IsTemporary: Boolean = True; const MapFileName: string = ''); virtual;
    destructor Destroy; override;
  end;

  { TGDIMemoryBackend }
  { A backend that keeps the backing buffer entirely in memory and offers
    IPaintSupport without allocating a GDI handle }

  { TLCLMemoryBackend }

  TLCLMemoryBackend = class(TMemoryBackend, IPaintSupport, IDeviceContextSupport)
  private
    procedure DoPaintRect(ABuffer: TBitmap32; ARect: TRect; ACanvas: TCanvas);

    function GetHandle: HDC; // Dummy
  protected
    FBitmapInfo: TBitmapInfo;

    procedure InitializeSurface(NewWidth: Integer; NewHeight: Integer;
      ClearBuffer: Boolean); override;
  public
    constructor Create; override;

    { IPaintSupport }
    procedure ImageNeeded;
    procedure CheckPixmap;
    procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);

    { IInteroperabilitySupport }
    function CopyFrom(Graphic: TGraphic): Boolean; overload;

    { IDeviceContextSupport }
    procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;
  end;

implementation

uses
  GR32_Text_LCL_Win;

var
  StockFont: HFONT;

{ TLCLBackend }

constructor TLCLBackend.Create;
begin
  inherited;

  FillChar(FBitmapInfo, SizeOf(TBitmapInfo), 0);
  with FBitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
  end;

  FMapHandle := 0;

  FFont := TFont.Create;
  FFont.OnChange := FontChangedHandler;
end;

destructor TLCLBackend.Destroy;
begin
  DeleteCanvas;
  FFont.Free;

  inherited;
end;

procedure TLCLBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  with FBitmapInfo.bmiHeader do
  begin
    biWidth := NewWidth;
    biHeight := -NewHeight;
    biSizeImage := NewWidth * NewHeight * 4;
  end;

  PrepareFileMapping(NewWidth, NewHeight);

  FBitmapHandle := LCLIntf.CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBits), FMapHandle, 0);

  if FBits = nil then
    raise Exception.Create(RCStrCannotAllocateDIBHandle);

  FHDC := CreateCompatibleDC(0);
  if FHDC = 0 then
  begin
    DeleteObject(FBitmapHandle);
    FBitmapHandle := 0;
    FBits := nil;
    raise Exception.Create(RCStrCannotCreateCompatibleDC);
  end;

  if SelectObject(FHDC, FBitmapHandle) = 0 then
  begin
    DeleteDC(FHDC);
    DeleteObject(FBitmapHandle);
    FHDC := 0;
    FBitmapHandle := 0;
    FBits := nil;
    raise Exception.Create(RCStrCannotSelectAnObjectIntoDC);
  end;
end;

procedure TLCLBackend.FinalizeSurface;
begin
  if FHDC <> 0 then DeleteDC(FHDC);
  FHDC := 0;
  if FBitmapHandle <> 0 then DeleteObject(FBitmapHandle);
  FBitmapHandle := 0;

  FBits := nil;
end;

procedure TLCLBackend.PrepareFileMapping(NewWidth, NewHeight: Integer);
begin
  // to be implemented by descendants
end;

procedure TLCLBackend.Changed;
begin
  if FCanvas <> nil then FCanvas.Handle := Self.Handle;
  inherited;
end;

function TLCLBackend.Empty: Boolean;
begin
  Result := FBitmapHandle = 0;
end;


{ IPaintSupport }

procedure TLCLBackend.ImageNeeded;
begin

end;

procedure TLCLBackend.CheckPixmap;
begin

end;

procedure TLCLBackend.DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList;
  ACanvas: TCanvas; APaintBox: TCustomPaintBox32);
var
  i: Integer;
begin
  if AInvalidRects.Count > 0 then
    for i := 0 to AInvalidRects.Count - 1 do
      with AInvalidRects[i]^ do
        Windows.BitBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top, ABuffer.Handle, Left, Top, SRCCOPY)
  else
    with APaintBox.GetViewportRect do
      Windows.BitBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top, ABuffer.Handle, Left, Top, SRCCOPY);
end;


{ IFontSupport }

function TLCLBackend.GetOnFontChange: TNotifyEvent;
begin
  Result := FOnFontChange;
end;

procedure TLCLBackend.InvalidateRect(AControl: TWinControl; const ARect: TRect);
begin
  if (AControl.HandleAllocated) then
    Windows.InvalidateRect(AControl.Handle, ARect, False);
end;

procedure TLCLBackend.GetUpdateRects(AControl: TWinControl; AUpdateRects: TRectList; AReservedCapacity: integer; var AFullUpdate: boolean);
var
  DC: HDC;
  RegionType: integer;
  UpdateRegion: HRGN;
  RegionSize: integer;
  RegionData: PRgnData;
  Offset: TPoint;
  i: integer;
begin
  UpdateRegion := CreateRectRgn(0,0,0,0);
  try
    DC := GetDC(AControl.Handle);
    try

      // On Lazarus the WM_PAINT handler is called from within the BeginPaint/EndPaint
      // block so we cannot use GetUpdateRgb (BeginPaint) clears it. Instead we use
      // GetRandomRgn which can be used within BeginPaint/EndPaint.
      RegionType := GetRandomRgn(DC, UpdateRegion, SYSRGN);

    finally
      ReleaseDC(AControl.Handle, DC);
    end;

    case RegionType of

      1: // Complex region
        begin
          RegionSize := GetRegionData(UpdateRegion, 0, nil);

          if (RegionSize > 0) then
          begin
            GetMem(RegionData, RegionSize);
            try

              RegionSize := GetRegionData(UpdateRegion, RegionSize, RegionData);
              Assert(RegionSize <> 0);

              // GetRandomRgn returns coordinates relative to the screen.
              // Make them relative to the control.
              Offset := Point(0, 0);
              MapWindowPoints(0, AControl.Handle, Offset, 1);
              for i := 0 to RegionData.rdh.nCount-1 do
                OffsetRect(PPolyRects(@RegionData.Buffer)[i], Offset.X, Offset.Y);

              if (RegionData.rdh.nCount = 1) and (GR32.EqualRect(PPolyRects(@RegionData.Buffer)[0], AControl.ClientRect)) then
                AFullUpdate := True
              else
              begin
                  // Final count is known so set capacity to avoid reallocation
                AUpdateRects.Capacity := Max(AUpdateRects.Capacity, AUpdateRects.Count + AReservedCapacity + integer(RegionData.rdh.nCount));

                for i := 0 to RegionData.rdh.nCount-1 do
                  AUpdateRects.Add(PPolyRects(@RegionData.Buffer)[i]);
              end;

            finally
              FreeMem(RegionData);
            end;
          end;
        end;

      0: // Null region
        AFullUpdate := True;

    else
      // Error - Ignore it
      AFullUpdate := True
    end;
  finally
    DeleteObject(UpdateRegion);
  end;
end;

function TLCLBackend.GetFont: TFont;
begin
  Result := FFont;
end;

procedure TLCLBackend.SetFont(const Font: TFont);
begin
  FFont.Assign(Font);
  FontChanged;
end;

procedure TLCLBackend.SetOnFontChange(Handler: TNotifyEvent);
begin
  FOnFontChange := Handler;
end;

procedure TLCLBackend.UpdateFont;
begin
  if (FFontHandle = 0) and (Handle <> 0) then
  begin
    SelectObject(Handle, Font.Handle);
    SetTextColor(Handle, ColorToRGB(Font.Color));
    SetBkMode(Handle, TRANSPARENT);
    FFontHandle := Font.Handle;
  end
  else
  begin
    SelectObject(Handle, FFontHandle);
    SetTextColor(Handle, ColorToRGB(Font.Color));
    SetBkMode(Handle, TRANSPARENT);
  end;
end;

procedure TLCLBackend.FontChanged;
begin
  if Assigned(FOnFontChange) then
    FOnFontChange(Self);
end;

procedure TLCLBackend.FontChangedHandler(Sender: TObject);
begin
  if FFontHandle <> 0 then
  begin
    if Handle <> 0 then SelectObject(Handle, StockFont);
    FFontHandle := 0;
  end;

  FontChanged;
end;


{ ITextSupport }

procedure TLCLBackend.Textout(X, Y: Integer; const Text: string);
var
  Extent: TSize;
begin
  UpdateFont;

  if not FOwner.MeasuringMode then
  begin
    if FOwner.Clipping then
      ExtTextout(Handle, X, Y, ETO_CLIPPED, @FOwner.ClipRect, PChar(Text), Length(Text), nil)
    else
      ExtTextout(Handle, X, Y, 0, nil, PChar(Text), Length(Text), nil);
  end;

  Extent := TextExtent(Text);
  FOwner.Changed(MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1));
end;

procedure TLCLBackend.Textout(X, Y: Integer; const ClipRect: TRect; const Text: string);
var
  Extent: TSize;
begin
  UpdateFont;

  if not FOwner.MeasuringMode then
    ExtTextout(Handle, X, Y, ETO_CLIPPED, @ClipRect, PChar(Text), Length(Text), nil);

  Extent := TextExtent(Text);
  FOwner.Changed(MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1));
end;

procedure TLCLBackend.Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string);
begin
  UpdateFont;

  if not FOwner.MeasuringMode then
    DrawText(Handle, PChar(Text), Length(Text), DstRect, Flags);

  FOwner.Changed(DstRect);
end;

function TLCLBackend.TextExtent(const Text: string): TSize;
var
  DC: HDC;
  OldFont: HGDIOBJ;
begin
  UpdateFont;
  Result.cX := 0;
  Result.cY := 0;
  if Handle <> 0 then
    GetTextExtentPoint32(Handle, PChar(Text), Length(Text), Result)
  else
  begin
    StockBitmap.Canvas.Lock;
    try
      DC := StockBitmap.Canvas.Handle;
      OldFont := SelectObject(DC, Font.Handle);
      GetTextExtentPoint32(DC, PChar(Text), Length(Text), Result);
      SelectObject(DC, OldFont);
    finally
      StockBitmap.Canvas.Unlock;
    end;
  end;
end;

{ ITextToPathSupport }

procedure TLCLBackend.TextToPath(Path: TCustomPath; const X, Y: TFloat;
  const Text: string);
var
  R: TFloatRect;
begin
  R := FloatRect(X, Y, X, Y);
  GR32_Text_LCL_Win.TextToPath(Font.Handle, Path, R, Text, 0);
end;

procedure TLCLBackend.TextToPath(Path: TCustomPath; const DstRect: TFloatRect;
  const Text: string; Flags: Cardinal);
begin
  GR32_Text_LCL_Win.TextToPath(Font.Handle, Path, DstRect, Text, Flags);
end;

function TLCLBackend.MeasureText(const DstRect: TFloatRect;
  const Text: string; Flags: Cardinal): TFloatRect;
begin
  Result := GR32_Text_LCL_Win.MeasureText(Font.Handle, DstRect, Text, Flags);
end;

procedure TLCLBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
begin
  Windows.BitBlt(hDst, DstX, DstY, FOwner.Width, FOwner.Height, Handle, 0, 0,
    SRCCOPY);
(*
StretchDIBits(
    hDst, DstX, DstY, FOwner.Width, FOwner.Height,
    0, 0, FOwner.Width, FOwner.Height, Bits, FBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
*)
end;

procedure TLCLBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
begin
  Windows.StretchBlt(hDst,
    DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top, Handle,
    SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, SRCCOPY);
end;

function TLCLBackend.GetBitmapHandle: THandle;
begin
  Result := FBitmapHandle;
end;

function TLCLBackend.GetBitmapInfo: TBitmapInfo;
begin
  Result := FBitmapInfo;
end;


{ IInteroperabilitySupport }

type
  TGraphicAccess = class(TGraphic);

function TLCLBackend.CopyFrom(Graphic: TGraphic): Boolean;
begin
  TGraphicAccess(Graphic).Draw(Canvas, MakeRect(0, 0, Canvas.Width, Canvas.Height));
  Result := True;
end;


{ ICanvasSupport }

procedure TLCLBackend.DeleteCanvas;
begin
  if Assigned(FCanvas) then
  begin
    FCanvas.Handle := 0;
    FCanvas.Free;
    FCanvas := nil;
  end;
end;

function TLCLBackend.CanvasAllocated: Boolean;
begin
  Result := Assigned(FCanvas);
end;

procedure TLCLBackend.CanvasChanged;
begin
  if Assigned(FOnCanvasChange) then
    FOnCanvasChange(Self);
end;

function TLCLBackend.GetCanvas: TCanvas;
begin
  if not Assigned(FCanvas) then
  begin
    FCanvas := TCanvas.Create;
    FCanvas.Handle := Handle;
    FCanvas.OnChange := CanvasChangedHandler;
  end;
  Result := FCanvas;
end;

function TLCLBackend.GetCanvasChange: TNotifyEvent;
begin
  Result := FOnCanvasChange;
end;

procedure TLCLBackend.CanvasChangedHandler(Sender: TObject);
begin
  CanvasChanged;
end;


{ IDeviceContextSupport }

function TLCLBackend.GetHandle: HDC;
begin
  Result := FHDC;
end;

procedure TLCLBackend.SetCanvasChange(Handler: TNotifyEvent);
begin
  FOnCanvasChange := Handler;
end;

procedure TLCLBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
begin
  if FOwner.Empty then Exit;

  if not FOwner.MeasuringMode then
    Windows.StretchBlt(Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, hSrc, SrcRect.Left, SrcRect.Top,
      SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, SRCCOPY);

  FOwner.Changed(DstRect);
end;


{ TLCLMMFBackend }

constructor TLCLMMFBackend.Create(Owner: TBitmap32; IsTemporary: Boolean = True; const MapFileName: string = '');
begin
  FMapFileName := MapFileName;
  FMapIsTemporary := IsTemporary;
  TMMFBackend.InitializeFileMapping(FMapHandle, FMapFileHandle, FMapFileName);
  inherited Create(Owner);
end;

destructor TLCLMMFBackend.Destroy;
begin
  TMMFBackend.DeinitializeFileMapping(FMapHandle, FMapFileHandle, FMapFileName);
  inherited;
end;

procedure TLCLMMFBackend.PrepareFileMapping(NewWidth, NewHeight: Integer);
begin
  TMMFBackend.CreateFileMapping(FMapHandle, FMapFileHandle, FMapFileName, FMapIsTemporary, NewWidth, NewHeight);
end;


{ TLCLMemoryBackend }

constructor TLCLMemoryBackend.Create;
begin
  inherited;
  FillChar(FBitmapInfo, SizeOf(TBitmapInfo), 0);
  with FBitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biXPelsPerMeter := 96;
    biYPelsPerMeter := 96;
    biClrUsed := 0;
  end;
end;

procedure TLCLMemoryBackend.InitializeSurface(NewWidth: Integer;
  NewHeight: Integer; ClearBuffer: Boolean);
begin
  inherited;
  with FBitmapInfo.bmiHeader do
  begin
    biWidth := NewWidth;
    biHeight := -NewHeight;
  end;
end;


{ IPaintSupport }

procedure TLCLMemoryBackend.ImageNeeded;
begin

end;

procedure TLCLMemoryBackend.CheckPixmap;
begin

end;

procedure TLCLMemoryBackend.DoPaintRect(ABuffer: TBitmap32;
  ARect: TRect; ACanvas: TCanvas);
var
  Bitmap        : HBITMAP;
  DeviceContext : HDC;
  Buffer        : Pointer;
  OldObject     : HGDIOBJ;
begin
  {$IFDEF LCLWin32}
  if SetDIBitsToDevice(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right -
    ARect.Left, ARect.Bottom - ARect.Top, ARect.Left, ARect.Top, 0,
    ARect.Bottom - ARect.Top, ABuffer.Bits, Windows.BITMAPINFO(FBitmapInfo), DIB_RGB_COLORS) = 0 then
  begin
    // create compatible device context
    DeviceContext := CreateCompatibleDC(ACanvas.Handle);
    if DeviceContext <> 0 then
    try
      Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo, DIB_RGB_COLORS,
        Buffer, 0, 0);

      if Bitmap <> 0 then
      begin
        OldObject := SelectObject(DeviceContext, Bitmap);
        try
          Move(ABuffer.Bits^, Buffer^, FBitmapInfo.bmiHeader.biWidth *
            FBitmapInfo.bmiHeader.biHeight * SizeOf(Cardinal));
          Windows.BitBlt(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right -
            ARect.Left, ARect.Bottom - ARect.Top, DeviceContext, 0, 0, SRCCOPY);
        finally
          if OldObject <> 0 then
            SelectObject(DeviceContext, OldObject);
          DeleteObject(Bitmap);
        end;
      end else
        raise Exception.Create(RCStrCannotCreateCompatibleDC);
    finally
      DeleteDC(DeviceContext);
    end;
  end;
  {$ELSE}
  raise Exception.Create('"SetDIBitsToDevice" is only included in windows unit!')
  {$ENDIF}
end;

procedure TLCLMemoryBackend.DoPaint(ABuffer: TBitmap32;
  AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);
var
  i : Integer;
begin
  if AInvalidRects.Count > 0 then
    for i := 0 to AInvalidRects.Count - 1 do
      DoPaintRect(ABuffer, AInvalidRects[i]^, ACanvas)
  else
    DoPaintRect(ABuffer, APaintBox.GetViewportRect, ACanvas);
end;


{ IInteroperabilitySupport }

function TLCLMemoryBackend.CopyFrom(Graphic: TGraphic): Boolean;
begin
  // yet todo
  Result := False;
end;


{ IDeviceContextSupport }

procedure TLCLMemoryBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
begin
  if FOwner.Empty then Exit;

  if not FOwner.MeasuringMode then
    raise Exception.Create('Not yet supported!');

  FOwner.Changed(DstRect);
end;

procedure TLCLMemoryBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
var
  Bitmap: HBITMAP;
  DeviceContext: HDC;
  Buffer: Pointer;
  OldObject: HGDIOBJ;
begin
  {$IFDEF LCLWin32}
  if SetDIBitsToDevice(hDst, DstX, DstY,
    FOwner.Width, FOwner.Height, 0, 0, 0, FOwner.Height, FBits,
    Windows.BITMAPINFO(FBitmapInfo), DIB_RGB_COLORS) = 0 then
  begin
    // create compatible device context
    DeviceContext := CreateCompatibleDC(hDst);
    if DeviceContext <> 0 then
    try
      Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo, DIB_RGB_COLORS,
        Buffer, 0, 0);

      if Bitmap <> 0 then
      begin
        OldObject := SelectObject(DeviceContext, Bitmap);
        try
          Move(FBits^, Buffer^, FBitmapInfo.bmiHeader.biWidth *
            FBitmapInfo.bmiHeader.biHeight * SizeOf(Cardinal));
          Windows.BitBlt(hDst, DstX, DstY, FOwner.Width, FOwner.Height, DeviceContext,
            0, 0, SRCCOPY);
        finally
          if OldObject <> 0 then
            SelectObject(DeviceContext, OldObject);
          DeleteObject(Bitmap);
        end;
      end else
        raise Exception.Create('Can''t create compatible DC''');
    finally
      DeleteDC(DeviceContext);
    end;
  end;
  {$ELSE}
  raise Exception.Create('"SetDIBitsToDevice" is only included in windows unit!')
  {$ENDIF}
end;

procedure TLCLMemoryBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
var
  Bitmap: HBITMAP;
  DeviceContext: HDC;
  Buffer: Pointer;
  OldObject: HGDIOBJ;
begin
  {$IFDEF LCLWin32}
  if SetDIBitsToDevice(hDst, DstRect.Left, DstRect.Top,
    DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top, SrcRect.Left,
    SrcRect.Top, 0, SrcRect.Bottom - SrcRect.Top, FBits,
    Windows.BITMAPINFO(FBitmapInfo), DIB_RGB_COLORS) = 0 then
  begin
    // create compatible device context
    DeviceContext := CreateCompatibleDC(hDst);
    if DeviceContext <> 0 then
    try
      Buffer := nil;
      Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo, DIB_RGB_COLORS,
        Buffer, 0, 0);

      if Bitmap <> 0 then
      begin
        OldObject := SelectObject(DeviceContext, Bitmap);
        try
          Move(FBits^, Buffer^, FBitmapInfo.bmiHeader.biWidth *
            FBitmapInfo.bmiHeader.biHeight * SizeOf(Cardinal));
          Windows.BitBlt(hDst, DstRect.Left, DstRect.Top, DstRect.Right -
            DstRect.Left, DstRect.Bottom - DstRect.Top, DeviceContext, 0, 0, SRCCOPY);
        finally
          if OldObject <> 0 then
            SelectObject(DeviceContext, OldObject);
          DeleteObject(Bitmap);
        end;
      end else
        raise Exception.Create('Can''t create compatible DC''');
    finally
      DeleteDC(DeviceContext);
    end;
  end;
  {$ELSE}
  raise Exception.Create('"SetDIBitsToDevice" is only included in windows unit!')
  {$ENDIF}
end;

function TLCLMemoryBackend.GetHandle: HDC;
begin
  Result := 0;
end;

initialization
  StockFont := GetStockObject(SYSTEM_FONT);

finalization

end.
