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
  gdk2, gdk2pixbuf, glib2, gtk2Def,
{$ELSE}
  gdk, gdkpixbuf, glib, gtkdef,
{$ENDIF}
  Graphics, GraphType, FPImage, IntfGraphics,
  GR32, GR32_Backends, GR32_Containers, GR32_Image;

type

  { TLCLBackend }

  TLCLBackend = class(TCustomBackend, IPaintSupport, ITextSupport,
    IFontSupport, ICanvasSupport, IDeviceContextSupport,
    IInteroperabilitySupport)
  private
    FFont: TFont;
    FOnFontChange: TNotifyEvent;
    FOnCanvasChange: TNotifyEvent;

    FWidth, FHeight: Cardinal;
    FRawImage: TRawImage;
    FBitmap: TBitmap;
    FPixBuf: PGdkPixbuf;

    procedure CanvasToPixBuf;
    procedure PixBufToCanvas;
    procedure CanvasChangedHandler(Sender: TObject);
  protected
    FFontHandle: HFont;
    FBitmapInfo: TBitmapInfo;
    FHDC: HDC;

{$IFDEF BITS_GETTER}
    function GetBits: PColor32Array; override;
{$ENDIF}

    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;

    procedure CanvasChanged;
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
    procedure SetFont(const Font: TFont);
    procedure UpdateFont;
  protected
    // IInteroperabilitySupport
    function CopyFrom(ImageBitmap: TFPImageBitmap): Boolean; overload;
    function CopyFrom(Graphic: TGraphic): Boolean; overload;
  protected
    // ICanvasSupport
    function GetCanvasChange: TNotifyEvent;
    procedure SetCanvasChange(Handler: TNotifyEvent);
    function GetCanvas: TCanvas;
  protected
    // TODO : These three can probably be deleted
    procedure DeleteCanvas;
    function CanvasAllocated: Boolean;
    property OnCanvasChange: TNotifyEvent read GetCanvasChange write SetCanvasChange;

    property Canvas: TCanvas read GetCanvas;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Changed; override;

    function Empty: Boolean; override;
  end;

implementation

uses
  GR32_LowLevel;

{ TLCLBackend }

constructor TLCLBackend.Create;
begin
  inherited;
  FBitmap := TBitmap.Create;
  FBitmap.Canvas.Brush.Style := bsClear; // Otherwise text is drawn opaque
  FBitmap.Canvas.OnChange := CanvasChangedHandler;
  FFont := TFont.Create;
end;

destructor TLCLBackend.Destroy;
begin
  FFont.Free;
  FBitmap.Free;

  inherited;
end;

{$IFDEF BITS_GETTER}
function TLCLBackend.GetBits: PColor32Array;
begin
  Result := FBits;
end;
{$ENDIF}

procedure TLCLBackend.CanvasChangedHandler(Sender: TObject);
begin
  CanvasChanged;
end;

procedure TLCLBackend.CanvasChanged;
begin
  if Assigned(FOnCanvasChange) then
    FOnCanvasChange(Self);
end;

procedure TLCLBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
var
  LazImage: TLazIntfImage;
begin
  { We allocate our own memory for the image }
{$ifdef RGBA_FORMAT}
  FRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(NewWidth, NewHeight);
{$else RGBA_FORMAT}
  FRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(NewWidth, NewHeight);
{$endif RGBA_FORMAT}

  FRawImage.CreateData(ClearBuffer);
  FBits := PColor32Array(FRawImage.Data);

  if (FBits = nil) then
    raise Exception.Create('[TLCLBackend.InitializeSurface] FBits = nil');

  // TODO : What is the purpose of this?
  // FRawImage is empty so loading it into FBitmap (via a TLazIntfImage) does nothing
  // but set the size of FBitmap...
  LazImage := TLazIntfImage.Create(FRawImage, False);
  try
    FBitmap.LoadFromIntfImage(LazImage);

    FWidth := NewWidth;
    FHeight := NewHeight;
  finally
    LazImage.Free;
  end;
end;

procedure TLCLBackend.FinalizeSurface;
begin
  if (FPixBuf <> nil) then
    g_object_unref(FPixBuf);

  if (FBits <> nil) then
  begin
    FRawImage.FreeData;
    FBits := nil;

    FBitmap.ReleaseHandle;
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
    GDK_RGB_DITHER_NONE, pguchar(ABuffer.Bits), ABuffer.Width * 4
  );
end;


{ IDeviceContextSupport }

function TLCLBackend.GetHandle: HDC;
begin
  Result := Canvas.Handle;
end;

procedure TLCLBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
begin
  StretchMaskBlt(Canvas.Handle, DstRect.Left, DstRect.Top,
    DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
    hSrc, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left,
    SrcRect.Bottom - SrcRect.Top, 0, 0, 0, Canvas.CopyMode);
end;

procedure TLCLBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
var
  P: TPoint;
begin
  P := TGtkDeviceContext(hDst).Offset;

  Inc(DstX, P.X);
  Inc(DstY, P.Y);

  gdk_draw_rgb_32_image(TGtkDeviceContext(hDst).Drawable,
    TGtkDeviceContext(hDst).GC, DstX, DstY, FWidth, FHeight,
    GDK_RGB_DITHER_NONE, PGuChar(FRawImage.Data), FWidth * 4
  );
end;

procedure TLCLBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
var
  P: TPoint;
  DR: TRect;
begin
  P := TGtkDeviceContext(hDst).Offset;
  DR := DstRect;

  Inc(DR.Left , P.X);
  Inc(DR.Right, P.X);

  gdk_draw_rgb_32_image(TGtkDeviceContext(hDst).Drawable,
    TGtkDeviceContext(hDst).GC, DR.Left, DR.Top, SrcRect.Right - SrcRect.Left,
    SrcRect.Bottom - SrcRect.Top, GDK_RGB_DITHER_NONE, PGuChar(FRawImage.Data),
    FWidth * 4
  );
end;


{ ITextSupport }

procedure TLCLBackend.CanvasToPixBuf;
var
  P: TPoint;
begin
  if (FPixBuf <> nil) then
    // Create a new GdkPixbuf structure and allocate a buffer for it.
    FPixBuf := gdk_pixbuf_new(GDK_COLORSPACE_RGB, True, 8, FWidth, FHeight);

  P := TGtkDeviceContext(Canvas.Handle).Offset;

  // Transfer image data from the Gdk drawable (Canvas.Handle) and convert it to
  // an RGBA representation inside the GdkPixbuf (FPixBuf).
  if (gdk_pixbuf_get_from_drawable(FPixBuf,
    TGtkDeviceContext(Canvas.Handle).Drawable, nil,
    P.X,P.Y, 0,0, FOwner.Width, FOwner.Height) = nil) then
    raise Exception.Create('[TLCLBackend.CanvasToPixBuf] gdk_pixbuf_get_from_drawable failed');

  // Get a pointer to the pixbuf pixel data.
  // Note: Causes an implicit copy of the pixbuf data if the pixbuf was created
  // from read-only data.
  FBits := PColor32Array(gdk_pixbuf_get_pixels(FPixBuf));
end;

procedure TLCLBackend.PixBufToCanvas;
var
  P: TPoint;
begin
  P := TGtkDeviceContext(Canvas.Handle).Offset;

  // TODO : gdk_draw_rgb_32_image is deprecated since GDK2. Isn't there something newer we can use?
  // ...or maybe that's what the LCL_Carbon is for?
  // TODO : As far as I can tell from the documentation, gdk_draw_rgb_32_image doesn't handle
  // alpha. Isn't this a problem?

  // Draw the pixbuf data onto the canvas.
  gdk_draw_rgb_32_image(TGtkDeviceContext(Canvas.Handle).Drawable,
    TGtkDeviceContext(Canvas.Handle).GC, P.X, P.Y, FOwner.Width, FOwner.Height,
    GDK_RGB_DITHER_NONE, pguchar(FBits), FOwner.Width * 4);
end;

procedure TLCLBackend.Textout(X, Y: Integer; const Text: string);
begin
  if Empty then
    Exit;

  UpdateFont;

  if not FOwner.MeasuringMode then
  begin
    PixBufToCanvas;
    Canvas.TextOut(X, Y, Text);
    CanvasToPixBuf;
  end;
end;

procedure TLCLBackend.Textout(X, Y: Integer; const ClipRect: TRect; const Text: string);
begin
  if Empty then
    Exit;

  UpdateFont;

  PixBufToCanvas;
  LCLIntf.ExtTextOut(Canvas.Handle, X, Y, ETO_CLIPPED, @ClipRect, PChar(Text), Length(Text), nil);
  CanvasToPixBuf;
end;

procedure TLCLBackend.Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string);
begin
  if Empty then
    Exit;

  UpdateFont;

  PixBufToCanvas;
  LCLIntf.DrawText(Canvas.Handle, PChar(Text), Length(Text), DstRect, Flags);
  CanvasToPixBuf;
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
  Result := FFont.OnChange;
end;

procedure TLCLBackend.SetOnFontChange(Handler: TNotifyEvent);
begin
  FFont.OnChange := Handler;
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
  FFont.OnChange := FOnFontChange;
  Canvas.Font := FFont;
end;


{ IInteroperabilitySupport }

type
  TGraphicAccess = class(TGraphic);

function TLCLBackend.CopyFrom(ImageBitmap: TFPImageBitmap): Boolean;
var
  Src: TLazIntfImage;
  Dest: TLazIntfImage;
  X, Y: Integer;
  SrcLine: PCardinalArray;
  DestLine: PByte;
begin
  Src := ImageBitmap.CreateIntfImage;
  Dest := FBitmap.CreateIntfImage;
  try
    if ImageBitmap.Transparent then
    begin
      for Y := 0 to Src.Height - 1 do
      begin
        SrcLine := Src.GetDataLineStart(Y);
        DestLine := FRawImage.GetLineStart(Y);
        for X := 0 to Src.Width - 1 do
        begin
          DestLine^ := Blue(SrcLine^[X]);
          Inc(DestLine);
          DestLine^ := Green(SrcLine^[X]);
          Inc(DestLine);
          DestLine^ := Red(SrcLine^[X]);
          Inc(DestLine);
          DestLine^ := Alpha(SrcLine^[X]);
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
          DestLine^ := Blue(SrcLine^[X]);
          Inc(DestLine);
          DestLine^ := Green(SrcLine^[X]);
          Inc(DestLine);
          DestLine^ := Red(SrcLine^[X]);
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
  if Graphic is TFPImageBitmap then
    Result := CopyFrom(TFPImageBitmap(Graphic));

  if not Result then
  begin
    TGraphicAccess(Graphic).Draw(Canvas, MakeRect(0, 0, Canvas.Width, Canvas.Height));
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
  Result := FBitmap.Canvas;
end;

procedure TLCLBackend.DeleteCanvas;
begin
end;

function TLCLBackend.CanvasAllocated: Boolean;
begin
  Result := (Canvas <> nil);
end;

end.
