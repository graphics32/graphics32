unit GR32_Backends_LCL_Win;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is Backend Extension for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Andre Beckedorf - metaException OHG
 * Andre@metaException.de
 *
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Christian Budde
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  LCLIntf, LCLType, types, Controls, SysUtils, Classes,
  Graphics, GR32, GR32_Backends;

type
  { TLCLBackend }
  { This backend uses the LCL to manage and provide the buffer and additional
    graphics sub system features. The backing buffer is kept in memory. }

  TLCLBackend = class(TCustomBackend,
    ICopyFromBitmapSupport, IBitmapContextSupport,
    IDeviceContextSupport, ITextSupport, IFontSupport, ICanvasSupport)
  private
    procedure FontChangedHandler(Sender: TObject);
    procedure CanvasChangedHandler(Sender: TObject);
    procedure CanvasChanged;
    procedure FontChanged;
  protected
    FBits: Pointer;
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
    { ICopyFromBitmapSupport }
    procedure CopyFromBitmap(SrcBmp: TBitmap);

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
    procedure Textout(X, Y: Integer; const Text: String); overload;
    procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: String); overload;
    procedure Textout(DstRect: TRect; const Flags: Cardinal; const Text: String); overload;
    function  TextExtent(const Text: String): TSize;

    procedure TextoutW(X, Y: Integer; const Text: Widestring); overload;
    procedure TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring); overload;
    procedure TextoutW(DstRect: TRect; const Flags: Cardinal; const Text: Widestring); overload;
    function  TextExtentW(const Text: Widestring): TSize;

    { IFontSupport }
    function GetOnFontChange: TNotifyEvent;
    procedure SetOnFontChange(Handler: TNotifyEvent);
    function GetFont: TFont;
    procedure SetFont(const Font: TFont);

    procedure UpdateFont;
    property Font: TFont read GetFont write SetFont;
    property OnFontChange: TNotifyEvent read FOnFontChange write FOnFontChange;

    { ICanvasSupport }
    function GetCanvasChange: TNotifyEvent;
    procedure SetCanvasChange(Handler: TNotifyEvent);
    function GetCanvas: TCanvas;

    procedure DeleteCanvas;
    function CanvasAllocated: Boolean;

    property Canvas: TCanvas read GetCanvas;
    property OnCanvasChange: TNotifyEvent read GetCanvasChange write SetCanvasChange;
  end;

implementation

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
  end;

  PrepareFileMapping(NewWidth, NewHeight);

  FBitmapHandle := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBits), FMapHandle, 0);

  if FBits = nil then
    raise Exception.Create('Can''t allocate the DIB handle');

  FHDC := CreateCompatibleDC(0);
  if FHDC = 0 then
  begin
    DeleteObject(FBitmapHandle);
    FBitmapHandle := 0;
    FBits := nil;
    raise Exception.Create('Can''t create compatible DC');
  end;

  if SelectObject(FHDC, FBitmapHandle) = 0 then
  begin
    DeleteDC(FHDC);
    DeleteObject(FBitmapHandle);
    FHDC := 0;
    FBitmapHandle := 0;
    FBits := nil;
    raise Exception.Create('Can''t select an object into DC');
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

procedure TLCLBackend.DeleteCanvas;
begin
  if Assigned(FCanvas) then
  begin
    FCanvas.Handle := 0;
    FCanvas.Free;
    FCanvas := nil;
  end;
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

procedure TLCLBackend.CopyFromBitmap(SrcBmp: TBitmap);
begin
  SrcBmp.Canvas.Lock; // lock to avoid GDI memory leaks, eg. when calling from threads
  try
    BitBlt(Handle, 0, 0, FOwner.Width, FOwner.Height, SrcBmp.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    SrcBmp.Canvas.UnLock;
  end;
end;

procedure TLCLBackend.CanvasChanged;
begin
  if Assigned(FOnCanvasChange) then
    FOnCanvasChange(Self);
end;

procedure TLCLBackend.FontChanged;
begin
  if Assigned(FOnFontChange) then
    FOnFontChange(Self);
end;

function TLCLBackend.TextExtent(const Text: String): TSize;
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

function TLCLBackend.TextExtentW(const Text: Widestring): TSize;
var
  DC: HDC;
  OldFont: HGDIOBJ;
begin
  Result := TextExtent(Text);
  
  {TODO: implement Widestring capable text method here...}
  (*
  UpdateFont;
  Result.cX := 0;
  Result.cY := 0;

  if Handle <> 0 then
    GetTextExtentPoint32W(Handle, PWideChar(Text), Length(Text), Result)
  else
  begin
    StockBitmap.Canvas.Lock;
    try
      DC := StockBitmap.Canvas.Handle;
      OldFont := SelectObject(DC, Font.Handle);
      GetTextExtentPoint32W(DC, PWideChar(Text), Length(Text), Result);
      SelectObject(DC, OldFont);
    finally
      StockBitmap.Canvas.Unlock;
    end;
  end;
  *)
end;

procedure TLCLBackend.Textout(X, Y: Integer; const Text: String);
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

procedure TLCLBackend.TextoutW(X, Y: Integer; const Text: Widestring);
var
  Extent: TSize;
begin
  Textout(X, Y, Text);

  {TODO: implement Widestring capable text method here...}

  (*
  UpdateFont;

  if not FOwner.MeasuringMode then
  begin
    if FOwner.Clipping then
      ExtTextoutW(Handle, X, Y, ETO_CLIPPED, @FOwner.ClipRect, PWideChar(Text), Length(Text), nil)
    else
      ExtTextoutW(Handle, X, Y, 0, nil, PWideChar(Text), Length(Text), nil);
  end;

  Extent := TextExtentW(Text);
  FOwner.Changed(MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1));
  *)
end;

procedure TLCLBackend.TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring);
var
  Extent: TSize;
begin
  Textout(X, Y, ClipRect, Text);
  
  {TODO: implement Widestring capable text method here...}

  (*
  UpdateFont;

  if not FOwner.MeasuringMode then
    ExtTextoutW(Handle, X, Y, ETO_CLIPPED, @ClipRect, PWideChar(Text), Length(Text), nil);

  Extent := TextExtentW(Text);
  FOwner.Changed(MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1));
  *)
end;

procedure TLCLBackend.Textout(X, Y: Integer; const ClipRect: TRect; const Text: String);
var
  Extent: TSize;
begin
  UpdateFont;

  if not FOwner.MeasuringMode then
    ExtTextout(Handle, X, Y, ETO_CLIPPED, @ClipRect, PChar(Text), Length(Text), nil);

  Extent := TextExtent(Text);
  FOwner.Changed(MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1));
end;

procedure TLCLBackend.TextoutW(DstRect: TRect; const Flags: Cardinal; const Text: Widestring);
begin
  Textout(DstRect, Flags, Text);
  
  {TODO: implement Widestring capable text method here...}

  (*
  UpdateFont;

  if not FOwner.MeasuringMode then
    DrawTextW(Handle, PWideChar(Text), Length(Text), DstRect, Flags);

  FOwner.Changed(DstRect);
  *)
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

procedure TLCLBackend.Textout(DstRect: TRect; const Flags: Cardinal; const Text: String);
begin
  UpdateFont;

  if not FOwner.MeasuringMode then
    DrawText(Handle, PChar(Text), Length(Text), DstRect, Flags);

  FOwner.Changed(DstRect);
end;

procedure TLCLBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
begin
  StretchDIBits(
    hDst, DstX, DstY, FOwner.Width, FOwner.Height,
    0, 0, FOwner.Width, FOwner.Height, Bits, FBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
end;

procedure TLCLBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
begin
  StretchBlt(
    hDst,
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

function TLCLBackend.GetFont: TFont;
begin
  Result := FFont;
end;

function TLCLBackend.GetHandle: HDC;
begin
  Result := FHDC;
end;

function TLCLBackend.GetOnFontChange: TNotifyEvent;
begin
  Result := FOnFontChange;
end;

procedure TLCLBackend.SetCanvasChange(Handler: TNotifyEvent);
begin
  FOnCanvasChange := Handler;
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

procedure TLCLBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
begin
  if FOwner.Empty then Exit;

  if not FOwner.MeasuringMode then
    StretchBlt(Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, hSrc, SrcRect.Left, SrcRect.Top,
      SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, SRCCOPY);

  FOwner.Changed(DstRect);
end;

function TLCLBackend.CanvasAllocated: Boolean;
begin
  Result := Assigned(FCanvas);
end;

function TLCLBackend.Empty: Boolean;
begin
  Result := FBitmapHandle = 0;
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

procedure TLCLBackend.CanvasChangedHandler(Sender: TObject);
begin
  CanvasChanged;
end;

initialization
  StockFont := GetStockObject(SYSTEM_FONT);

finalization

end.
