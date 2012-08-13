unit GR32_Backends_LCL_Gtk;

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
 * Felipe Monteiro de Carvalho <felipemonteiro.carvalho@gmail.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

{$DEFINE VerboseGR32GTK}

uses
  LCLIntf, LCLType, types, Controls, SysUtils, Classes,
{$IFDEF LCLGtk2}
  gdk2, gtk2, gdk2pixbuf, glib2, gtk2Def,
{$ELSE}
  gdk, gtk, gdkpixbuf, glib, gtkdef,
{$ENDIF}
  Graphics, GR32, GR32_Backends, GR32_Containers, GR32_Image;

type

  { TLCLBackend }

  TLCLBackend = class(TCustomBackend,
    IPaintSupport, ITextSupport, IFontSupport, ICanvasSupport)
  private
    FFont: TFont;
    FCanvas: TCanvas;
    FCanvasHandle: TGtkDeviceContext;
    FOnFontChange: TNotifyEvent;
    FOnCanvasChange: TNotifyEvent;

    { Gtk specific variables }
    FPixbuf: PGdkPixBuf;

    procedure CanvasChangedHandler(Sender: TObject);
    procedure FontChangedHandler(Sender: TObject);
    procedure CanvasChanged;
    procedure FontChanged;
  protected
    FFontHandle: HFont;
    FBitmapInfo: TBitmapInfo;
    FHDC: HDC;

    { BITS_GETTER }
    function GetBits: PColor32Array; override;

    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Changed; override;

    function Empty: Boolean; override;
  public
    { IPaintSupport }
    procedure ImageNeeded;
    procedure CheckPixmap;
    procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);

    { ITextSupport }
    procedure Textout(X, Y: Integer; const Text: string); overload;
    procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: string); overload;
    procedure Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string); overload;
    function  TextExtent(const Text: string): TSize;

    procedure TextoutW(X, Y: Integer; const Text: Widestring); overload;
    procedure TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring); overload;
    procedure TextoutW(var DstRect: TRect; const Flags: Cardinal; const Text: Widestring); overload;
    function  TextExtentW(const Text: Widestring): TSize;

    { IDeviceContextSupport }
    function GetHandle: HDC;

    procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;

    property Handle: HDC read GetHandle;

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

uses
  GR32_LowLevel;

resourcestring
  RCStrCannotAllocateMemory = 'Can''t allocate memory for the DIB';
  RCStrCannotAllocateThePixBuf = 'Can''t allocate the Pixbuf';

var
  StockFont: TFont;

{ TLCLBackend }

constructor TLCLBackend.Create;
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.Create]', ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  inherited;

  FFont := TFont.Create;
  FFont.OnChange := FontChangedHandler;
end;

destructor TLCLBackend.Destroy;
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.Destroy]',
    ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  DeleteCanvas;
  FFont.Free;

  inherited;
end;

function TLCLBackend.GetBits: PColor32Array;
begin
  Result := FBits;
end;

procedure TLCLBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
var
  Stride: Integer;
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.InitializeSurface] BEGIN',
     ' Self: ', IntToHex(PtrUInt(Self), 8),
     ' NewWidth: ', NewWidth,
     ' NewHeight: ', NewHeight
     );
  {$ENDIF}

  { We allocate our own memory for the image, because otherwise it's
    not guaranteed which Stride Gdk will use. }
  Stride := NewWidth * 4;
  FBits := GetMem(NewHeight * Stride);

  FHDC := CreateCompatibleDC(0);
  if FHDC = 0 then
  begin
    FBits := nil;
    raise Exception.Create(RCStrCannotCreateCompatibleDC);
  end;

  if FBits = nil then
    raise Exception.Create(RCStrCannotAllocateMemory);

  { We didn't pass a memory freeing function, so we will have to take
    care of that ourselves }
  FPixbuf := gdk_pixbuf_new_from_data(pguchar(FBits),
   GDK_COLORSPACE_RGB, True, 8, NewWidth, NewHeight, Stride, nil, nil);

  if FPixbuf = nil then
    raise Exception.Create(RCStrCannotAllocateThePixBuf);

  { clear the image }
  if ClearBuffer then
    FillLongword(FBits[0], NewWidth * NewHeight, clBlack32);

  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.InitializeSurface] END');
  {$ENDIF}
end;

procedure TLCLBackend.FinalizeSurface;
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.FinalizeSurface]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

{$IFDEF LCLGtk2}
  if Assigned(FPixbuf) then g_object_unref(FPixbuf);
  FPixbuf := nil;
{$ELSE}
  if Assigned(FPixbuf) then gdk_pixbuf_unref(FPixbuf);
  FPixbuf := nil;
{$ENDIF}

  if FHDC <> 0 then DeleteDC(FHDC);
  FHDC := 0;

  if Assigned(FBits) then FreeMem(FBits);
  FBits := nil;
end;

procedure TLCLBackend.Changed;
begin
  if FCanvas <> nil then FCanvas.Handle := Self.Handle;
  inherited;
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

function TLCLBackend.Empty: Boolean;
begin
  Result := (FPixBuf = nil) or (FBits = nil);
end;

procedure TLCLBackend.FontChangedHandler(Sender: TObject);
begin
  if FFontHandle <> 0 then
  begin
//    if Handle <> 0 then SelectObject(Handle, StockFont);
    FFontHandle := 0;
  end;

  FontChanged;
end;

procedure TLCLBackend.CanvasChangedHandler(Sender: TObject);
begin
  CanvasChanged;
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
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.DoPaint]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  gdk_draw_rgb_32_image(
    TGtkDeviceContext(ACanvas.Handle).Drawable,
    TGtkDeviceContext(ACanvas.Handle).GC,
    0,
    0,
    ABuffer.Width,
    ABuffer.Height,
    GDK_RGB_DITHER_NORMAL,
    Pguchar(FBits),
    ABuffer.Width * 4
  );

(*
gdk_pixbuf_render_to_drawable(
    FPixbuf,
    TGtkDeviceContext(ACanvas.Handle).Drawable,
    TGtkDeviceContext(ACanvas.Handle).GC,
    0,                     // src_x
    0,                     // src_y
    0,                     // dest_x
    0,                     // dest_y
    ABuffer.Width,         // width
    ABuffer.Height,        // height
    GDK_RGB_DITHER_NONE,  // dither
    0,                     // x_dither
    0);                    // y_dither
*)
end;

{ ITextSupport }

procedure TLCLBackend.Textout(X, Y: Integer; const Text: string);
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.Textout]', ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if not Assigned(FCanvas) then GetCanvas;

  UpdateFont;

  if not FOwner.MeasuringMode then
    FCanvas.TextOut(X, Y, Text);

  FOwner.Changed;
end;

procedure TLCLBackend.Textout(X, Y: Integer; const ClipRect: TRect; const Text: string);
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.Textout with ClipRect]', ' Self: ',
      IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if not Assigned(FCanvas) then GetCanvas;

  UpdateFont;

  LCLIntf.ExtTextOut(FCanvas.Handle, X, Y, ETO_CLIPPED, @ClipRect, PChar(Text),
    Length(Text), nil);
end;

procedure TLCLBackend.Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string);
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.Textout with Flags]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if not Assigned(FCanvas) then GetCanvas;

  UpdateFont;

  LCLIntf.DrawText(FCanvas.Handle, PChar(Text), Length(Text), DstRect, Flags);
end;

function TLCLBackend.TextExtent(const Text: string): TSize;
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.TextExtent]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if not Assigned(FCanvas) then GetCanvas;

//  UpdateFont;

  Result := FCanvas.TextExtent(Text);
end;

{ Gtk uses UTF-8, so all W functions are converted to UTF-8 ones }

procedure TLCLBackend.TextoutW(X, Y: Integer; const Text: Widestring);
begin
  TextOut(X, Y, Utf8Encode(Text));
end;

procedure TLCLBackend.TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring);
begin
  TextOut(X, Y, ClipRect, Utf8Encode(Text));
end;

procedure TLCLBackend.TextoutW(var DstRect: TRect; const Flags: Cardinal; const Text: Widestring);
begin
  TextOut(DstRect, Flags, Utf8Encode(Text));
end;

function TLCLBackend.TextExtentW(const Text: Widestring): TSize;
begin
  Result := TextExtent(Utf8Encode(Text));
end;

{ IFontSupport }

function TLCLBackend.GetOnFontChange: TNotifyEvent;
begin
  Result := FOnFontChange;
end;

procedure TLCLBackend.SetOnFontChange(Handler: TNotifyEvent);
begin
  FOnFontChange := Handler;
end;

function TLCLBackend.GetFont: TFont;
begin
  Result := FFont;
end;

function TLCLBackend.GetHandle: HDC;
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.GetHandle]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if not Assigned(FCanvas) then GetCanvas;

  Result := FCanvas.Handle;
end;

procedure TLCLBackend.SetFont(const Font: TFont);
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.SetFont]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  FFont.Assign(Font);
end;

procedure TLCLBackend.UpdateFont;
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.UpdateFont]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  FFontHandle := Font.Handle;
  FFont.OnChange := FOnFontChange;

  if Assigned(FCanvas) then FCanvas.Font := FFont;
end;

{ IDeviceContextSupport }

procedure TLCLBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.Draw]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if FOwner.Empty then Exit;

  if not FOwner.MeasuringMode then
    LclIntf.StretchBlt(Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, hSrc, SrcRect.Left, SrcRect.Top,
      SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, SRCCOPY);

  FOwner.Changed(DstRect);
end;

procedure TLCLBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.DrawTo]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  LclIntf.BitBlt(hDst, DstX, DstY, FOwner.Width, FOwner.Height, Handle, DstX,
    DstY, SRCCOPY);
  (*
  LclIntf.StretchDIBits(
    hDst, DstX, DstY, FOwner.Width, FOwner.Height,
    0, 0, FOwner.Width, FOwner.Height, Bits, FBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
*)
end;

procedure TLCLBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.DrawTo with rects]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  LclIntf.StretchBlt(hDst,
    DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top, Handle,
    SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, SRCCOPY);
  (*
*)
end;


{ ICanvasSupport }

function TLCLBackend.GetCanvasChange: TNotifyEvent;
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.GetCanvasChange]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  Result := FOnCanvasChange;
end;

procedure TLCLBackend.SetCanvasChange(Handler: TNotifyEvent);
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.SetCanvasChange]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  FOnCanvasChange := Handler;
end;

function TLCLBackend.GetCanvas: TCanvas;
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.GetCanvas] BEGIN',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if not Assigned(FCanvas) then
  begin
    FCanvas := TCanvas.Create;

    FCanvasHandle := TGtkDeviceContext.Create;

    FCanvas.Handle := HDC(FCanvasHandle);
    FCanvas.OnChange := CanvasChangedHandler;
  end;
  Result := FCanvas;
end;

procedure TLCLBackend.DeleteCanvas;
begin
  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.DeleteCanvas]',
     ' Self: ', IntToHex(PtrUInt(Self), 8),
     ' FCanvas: ', PtrUInt(FCanvas));
  {$ENDIF}

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

  {$IFDEF VerboseGR32GTK}
    WriteLn('[TLCLBackend.CanvasAllocated]',
     ' Self: ', IntToHex(PtrUInt(Self), 8),
     ' FCanvas: ', PtrUInt(FCanvas));
  {$ENDIF}
end;

initialization
  StockFont := TFont.Create;

finalization
  StockFont.Free;

end.
