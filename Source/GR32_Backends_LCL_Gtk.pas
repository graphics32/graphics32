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
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

{$DEFINE VerboseGR32GTK}

uses
  LCLIntf, LCLType,
  Types, Controls, SysUtils, Classes,
{$IFDEF LCLGtk2}
  gdk2, gdk2pixbuf, glib2, gtk2Def, gtk2,
{$ELSE}
  gdk, gdkpixbuf, glib, gtkdef,
{$ENDIF}
  Graphics, GraphType, FPImage, IntfGraphics,
  GR32, GR32_Backends, GR32_Containers, GR32_Image;

type

  { TLCLBackend }

  TLCLBackend = class(TCustomBackend,
      IPaintSupport,
      ITextSupport,
      IFontSupport,
      ICanvasSupport,
      IDeviceContextSupport,
      IInteroperabilitySupport,
      IUpdateRectSupport
    )
  private
    FFont: TFont;
    FOnFontChange: TNotifyEvent;
    FOnCanvasChange: TNotifyEvent;
    FWidth, FHeight: Cardinal;
    FRawImage: TRawImage;
    FBitmap: TBitmap;
    FPixmapDirty: boolean;
    FCanvasDirty: boolean;
  private
    procedure CanvasChangedHandler(Sender: TObject);
    procedure CanvasChanged;

    procedure CopyPixmapToCanvas;
    procedure CopyCanvasToPixmap;

    procedure NeedBits; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure NeedCanvas; {$IFDEF USEINLINING} inline; {$ENDIF}
  protected
{$IFDEF BITS_GETTER}
    function GetBits: PColor32Array; override;
{$ELSE BITS_GETTER}
  {$MESSAGE FATAL LCL GTK backend requires that BITS_GETTER is defined}
{$ENDIF BITS_GETTER}
    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;
  protected
    // IPaintSupport
    procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);
    procedure ImageNeeded;
    procedure CheckPixmap;
  protected
    // IDeviceContextSupport
    function GetHandle: HDC;
    procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;
  protected
    // ITextSupport
    procedure Textout(X, Y: Integer; const Text: string); overload;
    procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: string); overload;
    procedure Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string); overload;
    function  TextExtent(const Text: string): TSize;
  protected
    // IFontSupport
    function GetOnFontChange: TNotifyEvent;
    procedure SetOnFontChange(Handler: TNotifyEvent);
    function GetFont: TFont;
    procedure SetFont(const AFont: TFont);
    procedure UpdateFont;
    property Font: TFont read GetFont;
  protected
    // IInteroperabilitySupport
    function CopyFrom(ImageBitmap: TFPImageBitmap): Boolean; overload;
    function CopyFrom(Graphic: TGraphic): Boolean; overload;
  protected
    // ICanvasSupport
    function GetCanvasChange: TNotifyEvent;
    procedure SetCanvasChange(Handler: TNotifyEvent);
    function GetCanvas: TCanvas;
    function CanvasAllocated: Boolean;
    procedure DeleteCanvas;
  protected
    { IUpdateRectSupport }
    procedure InvalidateRect(AControl: TWinControl; const ARect: TRect);
    procedure GetUpdateRects(AControl: TWinControl; AUpdateRects: TRectList; AReservedCapacity: integer; var AFullUpdate: boolean);
  protected
    property Canvas: TCanvas read GetCanvas;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Changed; override;

    function Empty: Boolean; override;
  end;

implementation

uses
  Math,
  GR32_LowLevel;

{ TLCLBackend }

constructor TLCLBackend.Create;
begin
  inherited;
end;

destructor TLCLBackend.Destroy;
begin
  FFont.Free;
  FBitmap.Free;

  inherited;
end;

procedure TLCLBackend.NeedBits;
begin
  // If canvas was modified, copy it back to the pixmap
  if (FCanvasDirty) then
    CopyCanvasToPixmap;
end;

procedure TLCLBackend.NeedCanvas;
begin
  if (FBitmap = nil) then
  begin
    FBitmap := TBitmap.Create;
    FBitmap.Canvas.Brush.Style := bsClear; // Otherwise text is drawn opaque
    FBitmap.Canvas.OnChange := CanvasChangedHandler;

    FBitmap.LoadFromRawImage(FRawImage, False);

    FPixmapDirty := False;
    FCanvasDirty := False;
  end;

  // If pixmap was modified ensure that canvas is up to date
  if (FPixmapDirty) then
    CopyPixmapToCanvas;
end;

procedure TLCLBackend.CopyCanvasToPixmap;
var
  PixBuf: PGdkPixbuf;
  P: TPoint;
  SourceBits: pointer;
begin
  // Allocate a new pixbuf, 8 bits per channel with alpha.
  PixBuf := gdk_pixbuf_new(GDK_COLORSPACE_RGB, True, 8, FWidth, FHeight);
  try

    P := TGtkDeviceContext(Canvas.Handle).Offset;

    // Fill the pixbuf with the canvas pixel data
    if (gdk_pixbuf_get_from_drawable(PixBuf,
      TGtkDeviceContext(Canvas.Handle).Drawable, nil,
      P.X,P.Y, 0,0, FWidth, FHeight) = nil) then
      raise Exception.Create('[TLCLBackend.CopyCanvasToPixmap] gdk_pixbuf_get_from_drawable failed');

    // Note: we cant directly assign data pointer to FBits here, 
    // because the pointer will be soon disposed (see 'finally' below).
    // Instead, we should do copy the pixels (pointer content) to FBits to keep it accesible later.

    // Get a pointer to the pixbuf pixel data
    SourceBits := gdk_pixbuf_get_pixels(PixBuf);

    // Copy data (pointer content) from pixbuf to pixmap
    MoveLongword(SourceBits^, FBits^, FWidth*FHeight);

  finally
    g_object_unref(PixBuf);
  end;

  FPixmapDirty := False;
  FCanvasDirty := False;
end;

procedure TLCLBackend.CopyPixmapToCanvas;
var
  P: TPoint;
begin
  P := TGtkDeviceContext(FBitmap.Canvas.Handle).Offset;

  // Draw the pixbuf data onto the canvas.
  gdk_draw_rgb_32_image(TGtkDeviceContext(FBitmap.Canvas.Handle).Drawable,
    TGtkDeviceContext(FBitmap.Canvas.Handle).GC, P.X, P.Y, FWidth, FHeight,
    GDK_RGB_DITHER_NONE, pguchar(FBits), FWidth * SizeOf(TColor32));

  FPixmapDirty := False;
  FCanvasDirty := False;
end;

{$IFDEF BITS_GETTER}
function TLCLBackend.GetBits: PColor32Array;
begin
  NeedBits;

  Result := FBits;

  // Since caller now has direct access to the pixmap, assume it will be modified
  FPixmapDirty := True;
end;
{$ENDIF}

procedure TLCLBackend.CanvasChangedHandler(Sender: TObject);
begin
  CanvasChanged;
end;

procedure TLCLBackend.CanvasChanged;
begin
  FCanvasDirty := True;

  if Assigned(FOnCanvasChange) then
    FOnCanvasChange(Self);
end;

procedure TLCLBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  { We allocate our own memory for the image }
  FRawImage.Init;
{$ifdef RGBA_FORMAT}
  FRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(NewWidth, NewHeight);
{$else RGBA_FORMAT}
  FRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(NewWidth, NewHeight);
{$endif RGBA_FORMAT}

  FRawImage.CreateData(ClearBuffer);
  FBits := PColor32Array(FRawImage.Data);

  if (FBits = nil) then
    raise Exception.Create('[TLCLBackend.InitializeSurface] FBits = nil');

  FWidth := NewWidth;
  FHeight := NewHeight;

  FPixmapDirty := True;
end;

procedure TLCLBackend.FinalizeSurface;
begin
  if (FBits <> nil) then
  begin
    FRawImage.FreeData;
    FBits := nil;

    FreeAndNil(FBitmap);
  end;
end;

procedure TLCLBackend.Changed;
begin
  inherited;
end;

function TLCLBackend.Empty: Boolean;
begin
  Result := (FBits = nil);
end;

{ IPaintSupport }

procedure TLCLBackend.ImageNeeded;
begin
  // empty by purpose
end;

procedure TLCLBackend.CheckPixmap;
begin
  // empty by purpose
end;

procedure TLCLBackend.DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList;
  ACanvas: TCanvas; APaintBox: TCustomPaintBox32);
var
  P: TPoint;
begin
  P := TGtkDeviceContext(ACanvas.Handle).Offset;

  gdk_draw_rgb_32_image(TGtkDeviceContext(ACanvas.Handle).Drawable,
    TGtkDeviceContext(ACanvas.Handle).GC, P.X, P.Y,
    ABuffer.Width, ABuffer.Height,
    GDK_RGB_DITHER_NONE, pguchar(ABuffer.Bits), ABuffer.Width * SizeOf(TColor32)
  );
end;


{ IDeviceContextSupport }

function TLCLBackend.GetHandle: HDC;
begin
  Result := Canvas.Handle;
end;

procedure TLCLBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
begin
  NeedCanvas;

  StretchMaskBlt(FBitmap.Canvas.Handle, DstRect.Left, DstRect.Top,
    DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
    hSrc, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left,
    SrcRect.Bottom - SrcRect.Top, 0, 0, 0, FBitmap.Canvas.CopyMode);
end;

procedure TLCLBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
var
  P: TPoint;
begin
  NeedBits;

  P := TGtkDeviceContext(hDst).Offset;

  Inc(DstX, P.X);
  Inc(DstY, P.Y);

  gdk_draw_rgb_32_image(TGtkDeviceContext(hDst).Drawable,
    TGtkDeviceContext(hDst).GC, DstX, DstY, FWidth, FHeight,
    GDK_RGB_DITHER_NONE, PGuChar(FBits), FWidth * SizeOf(TColor32)
  );
end;

procedure TLCLBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
var
  P: TPoint;
  DR: TRect;
begin
  NeedBits;

  P := TGtkDeviceContext(hDst).Offset;
  DR := DstRect;

  Inc(DR.Left , P.X);
  Inc(DR.Right, P.X);

  gdk_draw_rgb_32_image(TGtkDeviceContext(hDst).Drawable,
    TGtkDeviceContext(hDst).GC, DR.Left, DR.Top, SrcRect.Right - SrcRect.Left,
    SrcRect.Bottom - SrcRect.Top, GDK_RGB_DITHER_NONE, PGuChar(FBits),
    FWidth * SizeOf(TColor32)
  );
end;


{ ITextSupport }

procedure TLCLBackend.Textout(X, Y: Integer; const Text: string);
var
  Extent: TSize;
  ChangeRect: TRect;
begin
  if Empty then
    Exit;

  UpdateFont;

  if (not FOwner.MeasuringMode) then
  begin
    if FOwner.Clipping then
    begin
      LCLIntf.ExtTextOut(Canvas.Handle, X, Y, ETO_CLIPPED, @FOwner.ClipRect, PChar(Text), Length(Text), nil);
      CanvasChanged;
    end else
      Canvas.TextOut(X, Y, Text);
  end;

  Extent := TextExtent(Text);
  ChangeRect := MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1);
  if FOwner.Clipping then
    ChangeRect.Intersect(FOwner.ClipRect);
  FOwner.Changed(ChangeRect);
end;

procedure TLCLBackend.Textout(X, Y: Integer; const ClipRect: TRect; const Text: string);
var
  Extent: TSize;
  ActualClipRect: TRect;
  ChangeRect: TRect;
begin
  if Empty then
    Exit;

  UpdateFont;

  ActualClipRect := ClipRect;
  if FOwner.Clipping then
    ActualClipRect.Intersect(FOwner.ClipRect);

  if (not FOwner.MeasuringMode) then
  begin
    LCLIntf.ExtTextOut(Canvas.Handle, X, Y, ETO_CLIPPED, @ActualClipRect, PChar(Text), Length(Text), nil);

    CanvasChanged;
  end;

  Extent := TextExtent(Text);
  ChangeRect := MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1);
  ChangeRect.Intersect(ActualClipRect);
  FOwner.Changed(ChangeRect);
end;

procedure TLCLBackend.Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string);
var
  ChangeRect: TRect;
  TextCopy: string;
begin
  if Empty then
    Exit;

  UpdateFont;

  if (not FOwner.MeasuringMode) then
  begin
    TextCopy := Text;
    if (Flags and DT_MODIFYSTRING <> 0) then
      UniqueString(TextCopy); // string must be writable

    LCLIntf.DrawText(Canvas.Handle, PChar(TextCopy), Length(TextCopy), DstRect, Flags);

    CanvasChanged;
  end else
    LCLIntf.DrawText(Canvas.Handle, PChar(TextCopy), Length(TextCopy), DstRect, (Flags or DT_CALCRECT) and (not DT_MODIFYSTRING));

  ChangeRect := DstRect;
  if FOwner.Clipping then
    ChangeRect.Intersect(FOwner.ClipRect);
  FOwner.Changed(ChangeRect);
end;

function TLCLBackend.TextExtent(const Text: string): TSize;
begin
  Result.cx := 0;
  Result.cy := 0;

  UpdateFont;

  Result := Canvas.TextExtent(Text);
end;

{ IFontSupport }

function TLCLBackend.GetOnFontChange: TNotifyEvent;
begin
  Result := Font.OnChange;
end;

procedure TLCLBackend.SetOnFontChange(Handler: TNotifyEvent);
begin
  Font.OnChange := Handler;
end;

function TLCLBackend.GetFont: TFont;
begin
  if (FFont = nil) then
    FFont := TFont.Create;
  Result := FFont;
end;

procedure TLCLBackend.SetFont(const AFont: TFont);
begin
  Font.Assign(AFont);
end;

procedure TLCLBackend.UpdateFont;
begin
  Font.OnChange := FOnFontChange;
  Canvas.Font := FFont;
end;


{ IUpdateRectSupport }
procedure TLCLBackend.InvalidateRect(AControl: TWinControl; const ARect: TRect);
var
  Widget: PGtkWidget;
  UpdateRect: TGdkRectangle;
begin
  // https://lazka.github.io/pgi-docs/Gdk-3.0/classes/Window.html#Gdk.Window.invalidate_rect
  // https://developer-old.gnome.org/pygtk/stable/class-gdkwindow.html#method-gdkwindow--invalidate-rect

  // procedure gdk_window_invalidate_rect(window:PGdkWindow; rect:PGdkRectangle; invalidate_children:gboolean); cdecl; external gdklib;

  Widget := PGtkWidget(AControl.Handle);

  UpdateRect.x := ARect.Left;
  UpdateRect.y := ARect.Top;
  UpdateRect.Width := ARect.Right-ARect.Left;
  UpdateRect.Height := ARect.Bottom-ARect.Top;

  gdk_window_invalidate_rect(Widget.window, @UpdateRect, False);
end;

procedure TLCLBackend.GetUpdateRects(AControl: TWinControl; AUpdateRects: TRectList; AReservedCapacity: integer;
  var AFullUpdate: boolean);
var
  Widget: PGtkWidget;
  UpdateRegion: PGdkRegion;
  UpdateRects: PGdkRectangle;
  UpdateRect: PGdkRectangle;
  Count: integer;
  r: TRect;
  i: integer;
begin
  // TODO : How do we get the update rect with GTK?
  // TGdkWindow.get_update_area ?
  // https://developer-old.gnome.org/pygtk/stable/class-gdkwindow.html#method-gdkwindow--get-update-area

  // function gdk_window_get_update_area(window:PGdkWindow):PGdkRegion; cdecl; external gdklib;

  Widget := PGtkWidget(AControl.Handle);

  UpdateRegion := gdk_window_get_update_area(Widget.window);
  if (UpdateRegion = PGdkRegion(GDK_NONE)) then
    exit;
  try

    if (gdk_region_empty(UpdateRegion)) then
      exit;

    gdk_region_get_rectangles(UpdateRegion, UpdateRects, @Count);
    try
      if (Count = 0) then
        exit;

      // Final count is known so set capacity to avoid reallocation
      AUpdateRects.Capacity := Math.Max(AUpdateRects.Capacity, AUpdateRects.Count + AReservedCapacity + Count);

      UpdateRect := UpdateRects;
      for i := 0 to Count-1 do
      begin
        r := MakeRect(UpdateRect.x, UpdateRect.y, UpdateRect.x+UpdateRect.Width, UpdateRect.y+UpdateRect.Height);
        AUpdateRects.Add(r);
        Inc(UpdateRect);
      end;

    finally
      g_free(UpdateRects)
    end;
  finally
    gdk_region_destroy(UpdateRegion);
  end;

end;


{ IInteroperabilitySupport }

type
  TGraphicAccess = class(TGraphic);

function TLCLBackend.CopyFrom(ImageBitmap: TFPImageBitmap): Boolean;
var
  Src: TLazIntfImage;
  X, Y: Integer;
  SrcLine: PCardinalArray;
  DestLine: PByte;
begin
  NeedBits; // Ensure that pixmap is up to date

  Src := ImageBitmap.CreateIntfImage;
  try
    if ImageBitmap.Transparent then
    begin
      for Y := 0 to Src.Height - 1 do
      begin
        SrcLine := Src.GetDataLineStart(Y);
        DestLine := FRawImage.GetLineStart(Y);
        for X := 0 to Src.Width - 1 do
        begin
          DestLine^ := BlueComponent(SrcLine^[X]);
          Inc(DestLine);
          DestLine^ := GreenComponent(SrcLine^[X]);
          Inc(DestLine);
          DestLine^ := RedComponent(SrcLine^[X]);
          Inc(DestLine);
          DestLine^ := AlphaComponent(SrcLine^[X]);
          Inc(DestLine);
        end;
      end;
    end
    else
    begin
      for Y := 0 to Src.Height - 1 do
      begin
        SrcLine := Src.GetDataLineStart(Y);
        DestLine := FRawImage.GetLineStart(Y);
        for X := 0 to Src.Width - 1 do
        begin
          DestLine^ := BlueComponent(SrcLine^[X]);
          Inc(DestLine);
          DestLine^ := GreenComponent(SrcLine^[X]);
          Inc(DestLine);
          DestLine^ := RedComponent(SrcLine^[X]);
          Inc(DestLine);
          DestLine^ := $FF;
          Inc(DestLine);
        end;
      end;
    end;
  finally
    Src.Free;
  end;

  Result := True;
end;

function TLCLBackend.CopyFrom(Graphic: TGraphic): Boolean;
begin
  Result := (Graphic is TFPImageBitmap) and CopyFrom(TFPImageBitmap(Graphic));

  if not Result then
  begin
    NeedCanvas;

    TGraphicAccess(Graphic).Draw(FBitmap.Canvas, MakeRect(0, 0, FBitmap.Canvas.Width, FBitmap.Canvas.Height));
    Result := True;
  end;
end;


{ ICanvasSupport }

function TLCLBackend.GetCanvasChange: TNotifyEvent;
begin
  Result := FOnCanvasChange;
end;

procedure TLCLBackend.SetCanvasChange(Handler: TNotifyEvent);
begin
  FOnCanvasChange := Handler;
end;

function TLCLBackend.GetCanvas: TCanvas;
begin
  NeedCanvas;

  Result := FBitmap.Canvas;
end;

function TLCLBackend.CanvasAllocated: Boolean;
begin
  result := GetCanvas() <> nil;
end;

procedure TLCLBackend.DeleteCanvas;
begin

end;

end.
