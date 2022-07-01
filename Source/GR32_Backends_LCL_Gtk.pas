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

    { BITS_GETTER }
    function GetBits: PColor32Array; override;

    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;

    procedure CanvasChanged;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Changed; override;

    function Empty: Boolean; override;
  public
    { IPaintSupport }
    procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);
    procedure ImageNeeded;
    procedure CheckPixmap;

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

    { IInteroperabilitySupport }
    function CopyFrom(ImageBitmap: TFPImageBitmap): Boolean; overload;
    function CopyFrom(Graphic: TGraphic): Boolean; overload;

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

function TLCLBackend.GetBits: PColor32Array;
begin
  Result := FBits;
end;

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
    FPixBuf := gdk_pixbuf_new(GDK_COLORSPACE_RGB, True, 8, FWidth, FHeight);

  P := TGtkDeviceContext(Canvas.Handle).Offset;

  if (gdk_pixbuf_get_from_drawable(FPixBuf,
    TGtkDeviceContext(Canvas.Handle).Drawable, nil,
    P.X,P.Y, 0,0, FOwner.Width, FOwner.Height) = nil) then
    raise Exception.Create('[TLCLBackend.CanvasToPixBuf] gdk_pixbuf_get_from_drawable failed');

  FBits := PColor32Array(gdk_pixbuf_get_pixels(FPixBuf));
end;

procedure TLCLBackend.PixBufToCanvas;
var
  P: TPoint;
begin
  P := TGtkDeviceContext(Canvas.Handle).Offset;

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
