unit GR32_Backends_LCL_Carbon;

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
  { RTL and LCL }
  LCLIntf, LCLType, types, Controls, SysUtils, Classes, Graphics,
  { Graphics 32 }
  GR32, GR32_Backends, GR32_Containers, GR32_Image,
  { Carbon bindings }
  FPCMacOSAll,
  { Carbon lcl interface }
  CarbonCanvas, CarbonPrivate;

const
  STR_GenericRGBProfilePath = '/System/Library/ColorSync/Profiles/Generic RGB Profile.icc';

type

  { TLCLBackend }

  TLCLBackend = class(TCustomBackend,
    IPaintSupport, ICopyFromBitmapSupport, IDeviceContextSupport,
    ITextSupport, IFontSupport, ICanvasSupport)
  private
    FFont: TFont;
    FCanvas: TCanvas;
    FOnFontChange: TNotifyEvent;
    FOnCanvasChange: TNotifyEvent;

    { Carbon specific variables }
    Stride: Integer;
    FWidth, FHeight: Cardinal;
    FProfile: CMProfileRef;
    FColorSpace: CGColorSpaceRef;
    FContext, OldCanvasContext: CGContextRef;
    FCanvasHandle: TCarbonDeviceContext;

    { Functions to easely generate carbon structures }
    function GetCarbonRect(Left, Top, Width, Height: Integer): FPCMacOSAll.Rect;
    function GetCGRect(Left, Top, Width, Height: Integer): FPCMacOSAll.CGRect;
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
    procedure ImageNeeded;
    procedure CheckPixmap;
    procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);

    { ICopyFromBitmapSupport }
    procedure CopyFromBitmap(SrcBmp: TBitmap);

    { IDeviceContextSupport }
    function GetHandle: HDC;

    procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;

    property Handle: HDC read GetHandle;

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

function TLCLBackend.GetCarbonRect(Left, Top, Width, Height: Integer): FPCMacOSAll.Rect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function TLCLBackend.GetCGRect(Left, Top, Width, Height: Integer): FPCMacOSAll.CGRect;
begin
  Result.Origin.X := Left;
  Result.Origin.Y := Top;
  Result.Size.Width := Width;
  Result.Size.Height := Height;
end;

constructor TLCLBackend.Create;
var
  loc: CMProfileLocation;
  status: OSStatus;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.Create]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  inherited;

  { Creates a standard font }

  FFont := TFont.Create;

  { Creates a generic color profile }

  loc.locType := cmPathBasedProfile;
  loc.u.pathLoc.path := STR_GenericRGBProfilePath;
  
  status := CMOpenProfile(FProfile, loc);
  
  if status <> noErr then raise Exception.Create('Couldn''t create the generic profile');
  
  { Creates a generic color space }
  
  FColorSpace := CGColorSpaceCreateWithPlatformColorSpace(FProfile);
  
  if FColorSpace = nil then raise Exception.Create('Couldn''t create the generic RGB color space');
end;

destructor TLCLBackend.Destroy;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.Destroy]',
    ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  FFont.Free;

  CMCloseProfile(FProfile);
  
  inherited;
end;

function TLCLBackend.GetBits: PColor32Array;
begin
  Result := FBits;
end;

procedure TLCLBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.InitializeSurface] BEGIN',
     ' Self: ', IntToHex(PtrUInt(Self), 8),
     ' NewWidth: ', NewWidth,
     ' NewHeight: ', NewHeight
     );
  {$ENDIF}

  { We allocate our own memory for the image }
  Stride := NewWidth * 4;
  FBits := System.GetMem(NewHeight * Stride);

  if FBits = nil then
  begin
    WriteLn('[TLCLBackend.InitializeSurface] ERROR FBits = nil');
    Exit;
  end;

  { Creates a device context for our raw image area }

  FContext := CGBitmapContextCreate(FBits,
   NewWidth, NewHeight, 8, Stride, FColorSpace, kCGImageAlphaNoneSkipFirst or kCGBitmapByteOrder32Little);

  if FContext = nil then
  begin
    WriteLn('[TLCLBackend.InitializeSurface] ERROR FContext = nil');
    Exit;
  end;

  { flip and offset CTM to upper left corner }
  CGContextTranslateCTM(FContext, 0, NewHeight);
  CGContextScaleCTM(FContext, 1, -1);

  FWidth := NewWidth;
  FHeight := NewHeight;

  { clear the image }
  if ClearBuffer then
    FillLongword(FBits[0], NewWidth * NewHeight, clBlack32);

  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.InitializeSurface] END');
  {$ENDIF}
end;

procedure TLCLBackend.FinalizeSurface;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.FinalizeSurface]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if Assigned(FBits) then System.FreeMem(FBits);
  FBits := nil;
  
  if Assigned(FContext) then CGContextRelease(FContext);
  FContext := nil;
end;

procedure TLCLBackend.Changed;
begin
  inherited;
end;

function TLCLBackend.Empty: Boolean;
begin
  Result := (FContext = nil) or (FBits = nil);
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
  {  CGContextDrawImage is also possible, but it doesn't flip the image }
   
  HIViewDrawCGImage(
    TCarbonDeviceContext(ACanvas.Handle).CGContext,
    GetCGRect(0, 0, FWidth, FHeight),
    CGBitmapContextCreateImage(FContext));

  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.DoPaint]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}
end;

{ ICopyFromBitmapSupport }

procedure TLCLBackend.CopyFromBitmap(SrcBmp: TBitmap);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.CopyFromBitmap]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  SrcBmp.Canvas.Lock; // lock to avoid GDI memory leaks, eg. when calling from threads
  try
    if not Assigned(FCanvas) then GetCanvas;

    LCLIntf.BitBlt(FCanvas.Handle, 0, 0, FOwner.Width, FOwner.Height, SrcBmp.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    SrcBmp.Canvas.UnLock;
  end;
end;

{ IDeviceContextSupport }

function TLCLBackend.GetHandle: HDC;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.GetHandle]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

end;

procedure TLCLBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.Draw]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

end;

procedure TLCLBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.DrawTo]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

end;

procedure TLCLBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.DrawTo with rects]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

end;

{ ITextSupport }

procedure TLCLBackend.Textout(X, Y: Integer; const Text: string);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.Textout]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if not Assigned(FCanvas) then GetCanvas;

  FCanvas.TextOut(X, Y, Text);
end;

procedure TLCLBackend.Textout(X, Y: Integer; const ClipRect: TRect; const Text: string);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.Textout with ClipRect]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}
  
  if not Assigned(FCanvas) then GetCanvas;

//  FCanvas.TextOut(DstRect.Left, DstRect.Top, Text);  simple implementation that ignores DstRect and Flags

  LCLIntf.ExtTextOut(FCanvas.Handle, X, Y, ETO_CLIPPED, @ClipRect, PChar(Text), Length(Text), nil);
end;

procedure TLCLBackend.Textout(DstRect: TRect; const Flags: Cardinal; const Text: string);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.Textout with Flags]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}
  
  if not Assigned(FCanvas) then GetCanvas;

//  FCanvas.TextOut(DstRect.Left, DstRect.Top, Text);  simple implementation that ignores DstRect and Flags

  LCLIntf.DrawText(FCanvas.Handle, PChar(Text), Length(Text), DstRect, Flags);
end;

function TLCLBackend.TextExtent(const Text: string): TSize;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.TextExtent]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if not Assigned(FCanvas) then GetCanvas;

  Result := FCanvas.TextExtent(Text);
end;

{ Carbon uses UTF-8, so all W functions are converted to UTF-8 ones }

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
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.GetOnFontChange]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  Result := FFont.OnChange;
end;

procedure TLCLBackend.SetOnFontChange(Handler: TNotifyEvent);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.SetOnFontChange]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  FFont.OnChange := Handler;
end;

function TLCLBackend.GetFont: TFont;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.GetFont]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  Result := FFont;
end;

procedure TLCLBackend.SetFont(const Font: TFont);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.SetFont]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  FFont.Assign(Font);
end;

procedure TLCLBackend.UpdateFont;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.UpdateFont]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  FFont.OnChange := FOnFontChange;

  if Assigned(FCanvas) then FCanvas.Font := FFont;
end;

{ ICanvasSupport }

function TLCLBackend.GetCanvasChange: TNotifyEvent;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.GetCanvasChange]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  Result := FOnCanvasChange;
end;

procedure TLCLBackend.SetCanvasChange(Handler: TNotifyEvent);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.SetCanvasChange]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  FOnCanvasChange := Handler;
end;

function TLCLBackend.GetCanvas: TCanvas;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.GetCanvas] BEGIN',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if FCanvas = nil then
  begin
    FCanvas := TCanvas.Create;

    FCanvasHandle := TCarbonDeviceContext.Create;
    FCanvasHandle.CGContext := FContext;

    FCanvas.Handle := HDC(FCanvasHandle);
    FCanvas.OnChange := FOnCanvasChange;

    FCanvas.Font := FFont;
  end;
  
  Result := FCanvas;

  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.GetCanvas] END');
  {$ENDIF}
end;

procedure TLCLBackend.DeleteCanvas;
begin
  {$IFDEF VerboseGR32Carbon}
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
  Result := (FCanvas <> nil);

  {$IFDEF VerboseGR32Carbon}
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
