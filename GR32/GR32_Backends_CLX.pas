unit GR32_Backends_CLX;

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
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  SysUtils, Classes, Qt, Types, QConsts, QGraphics, QControls,
  GR32, GR32_Backends;

type
  IQtDeviceContextSupport = interface
  ['{DD1109DA-4019-4A5C-A450-3631A73CF288}']
    function GetPixmap: QPixmapH;
    function GetPixmapChanged: Boolean;
    procedure SetPixmapChanged(Value: Boolean);
    function GetImage: QImageH;
    function GetPainter: QPainterH;

    procedure Draw(const DstRect, SrcRect: TRect; hSrc: QPixmapH); overload;
    procedure DrawTo(hDst: QPainterH; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: QPainterH; const DstRect, SrcRect: TRect); overload;

    property Pixmap: QPixmapH read GetPixmap;
    property PixmapChanged: Boolean read GetPixmapChanged write SetPixmapChanged;
    property Image: QImageH read GetImage;
    property Painter: QPainterH read GetPainter;
  end;

  TCLXBackend = class(TCustomBackend, IDDBContextSupport,
    ICopyFromBitmapSupport, ICanvasSupport, ITextSupport, IFontSupport,
    IQtDeviceContextSupport)
  private
    FFont: TFont;
    FCanvas: TCanvas;
    FPixmap: QPixmapH;
    FImage: QImageH;
    FPainter: QPainterH;
    FPainterCount: Integer;
    FPixmapActive: Boolean;
    FPixmapChanged: Boolean;

    FOnFontChange: TNotifyEvent;
    FOnCanvasChange: TNotifyEvent;

    procedure CanvasChangedHandler(Sender: TObject);
    procedure FontChangedHandler(Sender: TObject);
    procedure CanvasChanged;
    procedure FontChanged;
  protected
    FontHandle: HFont;

    procedure StartPainter;
    procedure StopPainter;

    function GetBits: PColor32Array; override;

    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Changed; override;

    function Empty: Boolean; override;
  public
    { IDDBContextSupport }
    procedure PixmapNeeded;
    procedure ImageNeeded;
    procedure CheckPixmap;

    { ICopyFromBitmapSupport }
    procedure CopyFromBitmap(SrcBmp: TBitmap);

    { IQtDeviceContextSupport }
    function GetPixmap: QPixmapH;
    function GetPixmapChanged: Boolean;
    procedure SetPixmapChanged(Value: Boolean);
    function GetImage: QImageH;
    function GetPainter: QPainterH;

    procedure Draw(const DstRect, SrcRect: TRect; SrcPixmap: QPixmapH); overload;
    procedure DrawTo(hDst: QPainterH; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: QPainterH; const DstRect, SrcRect: TRect); overload;

    property Pixmap: QPixmapH read GetPixmap;
    property PixmapChanged: Boolean read GetPixmapChanged write SetPixmapChanged;
    property Image: QImageH read GetImage;
    property Painter: QPainterH read GetPainter;

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

  TBitmap32Canvas = class(TCanvas)
  private
    FBackend: TCLXBackend;
  protected
    procedure BeginPainting; override;
    procedure CreateHandle; override;
  public
    constructor Create(Backend: TCLXBackend);
  end;

implementation

uses
  GR32_LowLevel;

var
  StockFont: TFont;

function StretchPixmap(DestPainter: QPainterH; DestX, DestY, DestWidth, DestHeight,
  SrcX, SrcY, SrcWidth, SrcHeight: Integer; SrcPixmap: QPixmapH): Integer;
var
  NewMatrix: QWMatrixH;
begin
  QPainter_saveWorldMatrix(DestPainter);
  try
    NewMatrix:= QWMatrix_create(DestWidth / SrcWidth, 0, 0, DestHeight / SrcHeight, DestX, DestY);
    try
      QPainter_setWorldMatrix(DestPainter, NewMatrix, True);
      QPainter_drawPixmap(DestPainter, 0, 0, SrcPixmap, SrcX, SrcY, SrcWidth, SrcHeight);
    finally
      QWMatrix_destroy(NewMatrix);
    end;
  finally
    QPainter_restoreWorldMatrix(DestPainter);
  end;
  Result := 0;
end;

{ TCLXBackend }

constructor TCLXBackend.Create;
begin
  inherited;

  FFont := TFont.Create;
  FFont.OnChange := FontChangedHandler;
end;

destructor TCLXBackend.Destroy;
begin
  FFont.Free;

  QPixmap_destroy(FPixmap);
  QPainter_destroy(FPainter);

  inherited;
end;

procedure TCLXBackend.UpdateFont;
begin
  FontHandle := Font.Handle;
end;

procedure TCLXBackend.FontChangedHandler(Sender: TObject);
begin
  if Assigned(FontHandle) then FontHandle := nil;
end;

procedure TCLXBackend.DeleteCanvas;
begin
  if FCanvas <> nil then
  begin
    FCanvas.Handle := nil;
    FCanvas.Free;
    FCanvas := nil;
  end;
end;

procedure TCLXBackend.PixmapNeeded;
begin
  if Assigned(FPixmap) and Assigned(FImage) and not FPixmapActive then
  begin
    QPixmap_convertFromImage(FPixmap, FImage, QPixmapColorMode(QPixmapColorMode_Auto));
    FPixmapActive := True;
    FPixmapChanged := False;
  end;
end;

procedure TCLXBackend.ImageNeeded;
begin
  if Assigned(FPixmap) and Assigned(FImage) and FPixmapActive and FPixmapChanged then
  begin
    QPixmap_convertToImage(FPixmap, FImage);
    FPixmapActive := False;
    FPixmapChanged := False;
    FBits := Pointer(QImage_bits(FImage));
  end;
end;

procedure TCLXBackend.CheckPixmap;
begin
  if not FPixmapChanged then
    // try to avoid QPixmap -> QImage conversion, since we don't need that.
    FPixmapActive := False;
  // else the conversion takes place as soon as the Bits property is accessed.
end;

function TCLXBackend.GetBits: PColor32Array;
begin
  ImageNeeded;
  Result := FBits;
end;

function TCLXBackend.GetImage: QImageH;
begin
  ImageNeeded;
  Result := FImage;
end;

function TCLXBackend.GetPixmap: QPixmapH;
begin
  PixmapNeeded;
  Result := FPixmap;
end;

function TCLXBackend.GetPainter: QPainterH;
begin
  PixmapNeeded;
  Result := FPainter;
end;

procedure TCLXBackend.StartPainter;
begin
  if (FPainterCount = 0) and not QPainter_isActive(Painter) then
    if not QPainter_begin(Painter, Pixmap) then
      raise EInvalidGraphicOperation.CreateRes(@SInvalidCanvasState);

  Inc(FPainterCount);
end;

procedure TCLXBackend.StopPainter;
begin
  Dec(FPainterCount);
  If (FPainterCount = 0) then
  begin
    QPainter_end(FPainter);
    FPixmapChanged := True;
  end;
end;

procedure TCLXBackend.Textout(X, Y: Integer; const Text: string);
begin
  TextOutW(X, Y, Text);
end;

procedure TCLXBackend.Textout(X, Y: Integer; const ClipRect: TRect; const Text: string);
begin
  TextOutW(X, Y, ClipRect, Text);
end;

procedure TCLXBackend.Textout(DstRect: TRect; const Flags: Cardinal; const Text: string);
begin
  TextOutW(DstRect, Flags, Text);
end;

function TCLXBackend.TextExtent(const Text: string): TSize;
begin
  Result := TextExtentW(Text); // QT uses Unicode.
end;

procedure TCLXBackend.TextoutW(X, Y: Integer; const Text: Widestring);
var
  Extent: TSize;
  R: TRect;
begin
  UpdateFont;
  if not FOwner.MeasuringMode then
  begin
    StartPainter;
    R := MakeRect(X, Y, High(Word), High(Word));
    QPainter_setFont(Painter, Font.Handle);
    QPainter_setPen(Painter, Font.FontPen);

    if FOwner.Clipping then
    begin
      QPainter_setClipRect(Painter, @FOwner.ClipRect);
      QPainter_setClipping(Painter, True);
    end;
    QPainter_drawText(Painter, @R, 0, @Text, -1, nil, nil);
    if FOwner.Clipping then QPainter_setClipping(Painter, False);
    StopPainter;
  end;
  Extent := TextExtentW(Text);
  FOwner.Changed(MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1));
end;

procedure TCLXBackend.TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring);
var
  Extent: TSize;
  TextW: WideString;
  R: TRect;
begin
  UpdateFont;
  if not FOwner.MeasuringMode then
  begin
    StartPainter;
    TextW := WideString(Text);
    R := MakeRect(X, Y, High(Word), High(Word));
    QPainter_setFont(Painter, Font.Handle);
    QPainter_setPen(Painter, Font.FontPen);
    QPainter_setClipRect(Painter, @ClipRect);
    QPainter_setClipping(Painter, True);
    QPainter_drawText(Painter, @R, 0, @TextW, -1, nil, nil);
    QPainter_setClipping(Painter, False);
    StopPainter;
  end;
  Extent := TextExtentW(Text);
  FOwner.Changed(MakeRect(X, Y, X + Extent.cx + 1, Y + Extent.cy + 1));
end;

procedure TCLXBackend.TextoutW(DstRect: TRect; const Flags: Cardinal; const Text: Widestring);
begin
  UpdateFont;
  if not FOwner.MeasuringMode then
  begin
    StartPainter;
    QPainter_setFont(Painter, Font.Handle);
    QPainter_setPen(Painter, Font.FontPen);
    QPainter_drawText(Painter, @DstRect, Flags, @Text, -1, nil, nil);
    StopPainter;
  end;
  FOwner.Changed(DstRect);
end;

function TCLXBackend.TextExtentW(const Text: Widestring): TSize;
var
  OldFont: TFont;
begin
  UpdateFont;
  Result.cX := 0;
  Result.cY := 0;

  if Assigned(Painter) then
  begin // doing it the ugly way to avoid QImage <-> QPixMap conversion.
    with TBitmap.Create do
    try
      Width := 5;
      Height := 5;
      Canvas.Font.Assign(Font);
      Result := Canvas.TextExtent(Text);
    finally
      Free;
    end;
  end
  else
  begin
    StockBitmap.Canvas.Lock;
    try
      OldFont := TFont.Create;
      OldFont.Assign(StockBitmap.Canvas.Font);
      StockBitmap.Canvas.Font.Assign(Font);
      Result := StockBitmap.Canvas.TextExtent(Text);
      StockBitmap.Canvas.Font.Assign(OldFont);
      OldFont.Free;
    finally
      StockBitmap.Canvas.Unlock;
    end;
  end;
end;

function TCLXBackend.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
  begin
    FCanvas := TBitmap32Canvas.Create(Self);
    FCanvas.Handle := Painter;
    FCanvas.OnChange := CanvasChangedHandler;
  end;
  Result := FCanvas;
end;

procedure TCLXBackend.DrawTo(hDst: QPainterH; DstX, DstY: Integer);
begin
  if Empty then Exit;
  StretchPixmap(
    hDst, DstX, DstY, FOwner.Width, FOwner.Height,
    0, 0, FOwner.Width, FOwner.Height, GetPixmap);
end;

procedure TCLXBackend.DrawTo(hDst: QPainterH; const DstRect, SrcRect: TRect);
begin
  if Empty then Exit;
  StretchPixmap(
    hDst,
    DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
    SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
    GetPixmap);
end;

procedure TCLXBackend.Draw(const DstRect, SrcRect: TRect; SrcPixmap: QPixmapH);
var
  NewMatrix: QWMatrixH;
  SrcHeight, SrcWidth: Integer;
begin
  if Empty then Exit;
  if not FOwner.MeasuringMode then
  begin
    StartPainter;
    QPainter_saveWorldMatrix(Painter);
    try
      SrcWidth := SrcRect.Right - SrcRect.Left;
      SrcHeight := SrcRect.Bottom - SrcRect.Top;
      // use world transformation to translate and scale.
      NewMatrix:= QWMatrix_create((DstRect.Right - DstRect.Left) / SrcWidth ,
        0, 0, (DstRect.Bottom - DstRect.Top) / SrcHeight, DstRect.Left, DstRect.Top);
      try
        QPainter_setWorldMatrix(Painter, NewMatrix, True);
        QPainter_drawPixmap(Painter, 0, 0, SrcPixmap,
          SrcRect.Left, SrcRect.Top, SrcWidth, SrcHeight);
      finally
        QWMatrix_destroy(NewMatrix);
      end;
    finally
      QPainter_restoreWorldMatrix(Painter);
      StopPainter;
    end;
  end;
  FOwner.Changed(DstRect);
end;

function TCLXBackend.CanvasAllocated: Boolean;
begin
  Result := Assigned(FCanvas);
end;

function TCLXBackend.Empty: Boolean;
begin
  Result := not(Assigned(FImage) or Assigned(FPixmap) or (FBits = nil));
end;

procedure TCLXBackend.CopyFromBitmap(SrcBmp: TBitmap);
begin
  if not QPainter_isActive(Painter) then
    if not QPainter_begin(Painter, Pixmap) then
      raise EInvalidGraphicOperation.CreateRes(@SInvalidCanvasState);
      
  QPainter_drawPixmap(Painter, 0, 0, SrcBmp.Handle, 0, 0, FOwner.Width, FOwner.Height);
  QPainter_end(Painter);
  PixmapChanged := True;
end;

procedure TCLXBackend.CanvasChanged;
begin
  if Assigned(FOnCanvasChange) then
    FOnCanvasChange(Self);
end;

procedure TCLXBackend.FontChanged;
begin
  if Assigned(FOnFontChange) then
    FOnFontChange(Self);
end;

function TCLXBackend.GetCanvasChange: TNotifyEvent;
begin
  Result := FOnCanvasChange;
end;

procedure TCLXBackend.SetCanvasChange(Handler: TNotifyEvent);
begin
  FOnCanvasChange := Handler;
end;

function TCLXBackend.GetFont: TFont;
begin
  Result := FFont;
end;

procedure TCLXBackend.SetFont(const Font: TFont);
begin
  FFont.Assign(Font);
  FontChanged;
end;

function TCLXBackend.GetOnFontChange: TNotifyEvent;
begin
  Result := FOnFontChange;
end;

procedure TCLXBackend.SetOnFontChange(Handler: TNotifyEvent);
begin
  FOnFontChange := Handler;
end;

procedure TCLXBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  FImage := QImage_create(NewWidth, NewHeight, 32, 1, QImageEndian_IgnoreEndian);
  if FImage <> nil then
  begin
    FBits := Pointer(QImage_bits(FImage));
    // clear it since QT doesn't initialize the image data:
    if ClearBuffer then
      FillLongword(FBits[0], NewWidth * NewHeight, clBlack32);
  end;

  if FBits = nil then
    raise Exception.Create('Can''t allocate the DIB handle');

  FPainter := QPainter_create;
  if FPainter = nil then
  begin
    QImage_destroy(FImage);
    FBits := nil;
    raise Exception.Create('Can''t create compatible DC');
  end;

  FPixmap := QPixmap_create;
end;

procedure TCLXBackend.FinalizeSurface;
begin
  if Assigned(FPainter) then QPainter_destroy(FPainter);
  FPainter := nil;
  if Assigned(FImage) then QImage_destroy(FImage);
  FImage := nil;
  if Assigned(FPixmap) then QPixmap_destroy(FPixmap);
  FPixmap := nil;
  FPixmapChanged := False;
  FPixmapActive := False;

  FBits := nil;
end;

procedure TCLXBackend.Changed;
begin
  if FCanvas <> nil then FCanvas.Handle := Self.Painter;
  inherited;
end;

procedure TCLXBackend.CanvasChangedHandler(Sender: TObject);
begin
  Changed;
end;

function TCLXBackend.GetPixmapChanged: Boolean;
begin
  Result := FPixmapChanged;
end;

procedure TCLXBackend.SetPixmapChanged(Value: Boolean);
begin
  FPixmapChanged := Value;
end;

{ TBitmap32Canvas }

constructor TBitmap32Canvas.Create(Backend: TCLXBackend);
begin
  inherited Create;
  FBackend := Backend;
end;

procedure TBitmap32Canvas.CreateHandle;
begin
  Handle := QPainter_create;
end;

procedure TBitmap32Canvas.BeginPainting;
begin
  if not QPainter_isActive(FBackend.Painter) then
    if not QPainter_begin(FBackend.Painter, FBackend.Pixmap) then
      raise EInvalidGraphicOperation.CreateRes(@SInvalidCanvasState);

  FBackend.PixmapChanged := True; // whatever happens, we've potentially changed
                                  // the Pixmap, so propagate that status...
end;

initialization
  StockFont := TFont.Create;

finalization
  StockFont.Free;

end.
