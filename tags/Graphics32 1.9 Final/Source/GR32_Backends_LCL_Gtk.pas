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
 * Portions created by the Initial Developer are Copyright (C) 2007-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  LCLIntf, LCLType, types, Controls, SysUtils, Classes,
{$IFDEF LCLGtk2}
  gdk2, gtk2, gdk2pixbuf, glib2,
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
    FOnFontChange: TNotifyEvent;
    FOnCanvasChange: TNotifyEvent;

    { Gtk specific variables }
    FPixbuf: PGdkPixBuf;

    procedure FontChangedHandler(Sender: TObject);
  protected
    FontHandle: HFont;

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
    procedure Textout(DstRect: TRect; const Flags: Cardinal; const Text: string); overload;
    function  TextExtent(const Text: string): TSize;

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

uses
  GR32_LowLevel;

var
  StockFont: TFont;

{ TLCLBackend }

constructor TLCLBackend.Create;
begin
  inherited;

  FFont := TFont.Create;
  FFont.OnChange := FontChangedHandler;
end;

destructor TLCLBackend.Destroy;
begin
  FFont.Free;

  inherited;
end;

procedure TLCLBackend.FontChangedHandler(Sender: TObject);
begin
  FontHandle := 0;
end;

function TLCLBackend.GetBits: PColor32Array;
begin
  Result := FBits;
end;

procedure TLCLBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
var
  Stride: Integer;
begin
  { We allocate our own memory for the image, because otherwise it's
    not guaranteed which Stride Gdk will use. }
  Stride := NewWidth * 4;
  FBits := GetMem(NewHeight * Stride);

  if FBits = nil then
    raise Exception.Create('Can''t allocate memory for the DIB');

  { We didn't pass a memory freeing function, so we will have to take
    care of that ourselves }
  FPixbuf := gdk_pixbuf_new_from_data(pguchar(FBits),
   GDK_COLORSPACE_RGB, True, 8, NewWidth, NewHeight, Stride, nil, nil);

  if FPixbuf = nil then
    raise Exception.Create('Can''t allocate the Pixbuf');

  { clear the image }
  if ClearBuffer then
    FillLongword(FBits[0], NewWidth * NewHeight, clBlack32);
end;

procedure TLCLBackend.FinalizeSurface;
begin
{$IFDEF LCLGtk2}
  if Assigned(FPixbuf) then g_object_unref(FPixbuf);
  FPixbuf := nil;
{$ELSE}
  if Assigned(FPixbuf) then gdk_pixbuf_unref(FPixbuf);
  FPixbuf := nil;
{$ENDIF}

  if Assigned(FBits) then FreeMem(FBits);
  FBits := nil;
end;

procedure TLCLBackend.Changed;
begin
  inherited;
end;

function TLCLBackend.Empty: Boolean;
begin
  Result := (FPixBuf = nil) or (FBits = nil);
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
  WriteLn('[TLCLBackend.DoPaint]');

{  gdk_pixbuf_render_to_drawable(
    FPixbuf,
    TDeviceContext(ACanvas.Handle).Drawable,
    TDeviceContext(ACanvas.Handle).GetGC,
    0,                     // src_x
    0,                     // src_y
    0,                     // dest_x
    0,                     // dest_y
    ABuffer.Width,         // width
    ABuffer.Height,        // height
    GDK_RGB_DITHER_NORMAL, // dither
    0,                     // x_dither
    0);    }                // y_dither
end;

{ ITextSupport }

procedure TLCLBackend.Textout(X, Y: Integer; const Text: string);
begin

end;

procedure TLCLBackend.Textout(X, Y: Integer; const ClipRect: TRect; const Text: string);
begin

end;

procedure TLCLBackend.Textout(DstRect: TRect; const Flags: Cardinal; const Text: string);
begin

end;

function TLCLBackend.TextExtent(const Text: string): TSize;
begin

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

procedure TLCLBackend.TextoutW(DstRect: TRect; const Flags: Cardinal; const Text: Widestring);
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

procedure TLCLBackend.SetFont(const Font: TFont);
begin
  FFont.Assign(Font);
end;

procedure TLCLBackend.UpdateFont;
begin
  FontHandle := Font.Handle;
end;

{ ICanvasSupport }

function TLCLBackend.GetCanvasChange: TNotifyEvent;
begin
  Exit;
end;

procedure TLCLBackend.SetCanvasChange(Handler: TNotifyEvent);
begin
  Exit;
end;

function TLCLBackend.GetCanvas: TCanvas;
begin
  Exit;
end;

procedure TLCLBackend.DeleteCanvas;
begin
  Exit;
end;

function TLCLBackend.CanvasAllocated: Boolean;
begin
  Exit;
end;

initialization
  StockFont := TFont.Create;

finalization
  StockFont.Free;

end.
