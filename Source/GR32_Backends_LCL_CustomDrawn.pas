unit GR32_Backends_LCL_CustomDrawn;

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
 * Mattias Andersson <mattias@centaurix.com>
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
  { RTL and LCL }
  LCLIntf, LCLType, types, Controls, SysUtils, Classes, Graphics,
  { Graphics 32 }
  GR32, GR32_Backends, GR32_Containers, GR32_Image,
  { CustomDrawn bindings }
  GraphType, FPImage, IntfGraphics, LCLProc, CustomDrawnProc;

type

  { TLCLBackend }

  TLCLBackend = class(
    TCustomBackend,
    IPaintSupport,
    ITextSupport,
    IFontSupport,
    IDeviceContextSupport,
    ICanvasSupport
  )
  private
    FFont: TFont;
    FCanvas: TCanvas;
    FCanvasHandle: HDC;
    FOnFontChange: TNotifyEvent;
    FOnCanvasChange: TNotifyEvent;

    FWidth, FHeight: Cardinal;

    FRawImage: TRawImage;
    FBitmap: TBitmap;

    procedure CanvasChangedHandler(Sender: TObject);
  protected
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

    procedure TextoutW(X, Y: Integer; const Text: Widestring); overload;
    procedure TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring); overload;
    procedure TextoutW(var DstRect: TRect; const Flags: Cardinal; const Text: Widestring); overload;
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

{ TLCLBackend }

constructor TLCLBackend.Create;
begin
  inherited;
  FBitmap := TBitmap.Create;
  FBitmap.Canvas.OnChange := CanvasChangedHandler;
  FFont := TFont.Create;
end;

destructor TLCLBackend.Destroy;
begin
  inherited;
  FFont.Free;
  FBitmap.Free;
end;

procedure TLCLBackend.CanvasChangedHandler(Sender: TObject);
begin
  if Assigned(FOnCanvasChange) then
    FOnCanvasChange(Sender);
end;

function TLCLBackend.GetBits: PColor32Array;
begin
  Result := FBits;
end;

procedure TLCLBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
var
  CDBitmap: TCDBitmap;
  LazImage: TLazIntfImage;
begin
  { We allocate our own memory for the image }

  FRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(NewWidth, NewHeight);
  FRawImage.CreateData(ClearBuffer);
  FBits := PColor32Array(FRawImage.Data);
  if FBits = nil then
    raise Exception.Create('[TLCLBackend.InitializeSurface] ERROR FBits = nil');

  LazImage := TLazIntfImage.Create(FRawImage, False);
  CDBitmap := TCDBitmap.Create;
  CDBitmap.Image := LazImage;
  FBitmap.Handle := HBITMAP(CDBitmap);

  FWidth := NewWidth;
  FHeight := NewHeight;
end;

procedure TLCLBackend.FinalizeSurface;
begin
  if Assigned(FBits) then
  begin
    FRawImage.FreeData;
    FBits := nil;

    FBitmap.Handle := HBITMAP(0);
  end;
  FBits := nil;
end;

procedure TLCLBackend.Changed;
begin
  inherited;
end;

function TLCLBackend.Empty: Boolean;
begin
  Result := FBits = nil;
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
  ACanvas.Draw(0, 0, FBitmap);
end;


{ IDeviceContextSupport }

function TLCLBackend.GetHandle: HDC;
begin
  Result := Canvas.Handle;
end;

procedure TLCLBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
begin
  StretchMaskBlt(
    Canvas.Handle,
    DstRect.Left,
    DstRect.Top,
    DstRect.Right - DstRect.Left,
    DstRect.Bottom - DstRect.Top,
    hSrc,
    SrcRect.Left,
    SrcRect.Top,
    SrcRect.Right - SrcRect.Left,
    SrcRect.Bottom - SrcRect.Top,
    0,
    0,
    0,
    Canvas.CopyMode
  );
end;

procedure TLCLBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
begin
  StretchMaskBlt(
    hDst,
    DstX,
    DstY,
    FWidth,
    FHeight,
    Canvas.Handle,
    0,
    0,
    FWidth,
    FHeight,
    0,
    0,
    0,
    Canvas.CopyMode
  );
end;

procedure TLCLBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
begin
  StretchMaskBlt(
    hDst,
    DstRect.Left,
    DstRect.Top,
    DstRect.Right - DstRect.Left,
    DstRect.Bottom - DstRect.Top,
    Canvas.Handle,
    SrcRect.Left,
    SrcRect.Top,
    SrcRect.Right - SrcRect.Left,
    SrcRect.Bottom - SrcRect.Top,
    0,
    0,
    0,
    Canvas.CopyMode
  );
end;

{ ITextSupport }

procedure TLCLBackend.Textout(X, Y: Integer; const Text: string);
begin
  if not Assigned(FCanvas) then GetCanvas;

  UpdateFont;

  if not FOwner.MeasuringMode then
    FCanvas.TextOut(X, Y, Text);

//  FOwner.Changed(DstRect);
end;

procedure TLCLBackend.Textout(X, Y: Integer; const ClipRect: TRect; const Text: string);
begin
  if not Assigned(FCanvas) then GetCanvas;

  UpdateFont;

  LCLIntf.ExtTextOut(FCanvas.Handle, X, Y, ETO_CLIPPED, @ClipRect, PChar(Text), Length(Text), nil);
end;

procedure TLCLBackend.Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string);
begin
  UpdateFont;

  LCLIntf.DrawText(FCanvas.Handle, PChar(Text), Length(Text), DstRect, Flags);
end;

function TLCLBackend.TextExtent(const Text: string): TSize;
begin
  if not Assigned(FCanvas) then GetCanvas;

  UpdateFont;

  Result := FCanvas.TextExtent(Text);
end;

procedure TLCLBackend.TextoutW(X, Y: Integer; const Text: Widestring);
begin
  Canvas.TextOut(X, Y, Text);
end;

procedure TLCLBackend.TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring);
begin
  Canvas.ClipRect := ClipRect;;
  Canvas.TextOut(X, Y, Text);
end;

procedure TLCLBackend.TextoutW(var DstRect: TRect; const Flags: Cardinal; const Text: Widestring);
begin
  TextOut(DstRect, Flags, Text);
end;

function TLCLBackend.TextExtentW(const Text: Widestring): TSize;
begin
  Result := TextExtent(Text);
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

  if Assigned(FCanvas) then FCanvas.Font := FFont;
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
