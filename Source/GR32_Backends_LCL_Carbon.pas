unit GR32_Backends_LCL_Carbon;

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
  { RTL and LCL }
  LCLIntf, LCLType, types, Controls, SysUtils, Classes, Graphics,
  { Graphics 32 }
  GR32, GR32_Backends, GR32_Containers, GR32_Image,
  { Carbon bindings }
  MacOSAll,
  { Carbon lcl interface }
  CarbonCanvas, CarbonPrivate;

const
  STR_GenericRGBProfilePath = '/System/Library/ColorSync/Profiles/Generic RGB Profile.icc';

type

  { TLCLBackend }

  TLCLBackend = class(TCustomBackend,
    IPaintSupport, IDeviceContextSupport,
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
    function GetCarbonRect(Left, Top, Width, Height: Integer): MacOSAll.Rect;
    function GetCGRect(Left, Top, Width, Height: Integer): MacOSAll.CGRect; overload;
    function GetCGRect(SrcRect: TRect): MacOSAll.CGRect; overload;
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

function TLCLBackend.GetCarbonRect(Left, Top, Width, Height: Integer): MacOSAll.Rect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function TLCLBackend.GetCGRect(Left, Top, Width, Height: Integer): MacOSAll.CGRect;
begin
  Result.Origin.X := Left;
  Result.Origin.Y := Top;
  Result.Size.Width := Width;
  Result.Size.Height := Height;
end;

function TLCLBackend.GetCGRect(SrcRect: TRect): MacOSAll.CGRect;
begin
  Result.Origin.X := SrcRect.Left;
  Result.Origin.Y := SrcRect.Top;
  Result.Size.Width := SrcRect.Right - SrcRect.Left;
  Result.Size.Height := SrcRect.Bottom - SrcRect.Top;
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

  { Deallocates the standard font }

  FFont.Free;

  { Closes the profile }

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
    raise Exception.Create('[TLCLBackend.InitializeSurface] ERROR FBits = nil');

  { Creates a device context for our raw image area }

  FContext := CGBitmapContextCreate(FBits,
   NewWidth, NewHeight, 8, Stride, FColorSpace, kCGImageAlphaNoneSkipFirst or kCGBitmapByteOrder32Little);

  if FContext = nil then
    raise Exception.Create('[TLCLBackend.InitializeSurface] ERROR FContext = nil');

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

{ IDeviceContextSupport }

function TLCLBackend.GetHandle: HDC;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.GetHandle]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  if not Assigned(FCanvas) then GetCanvas;

  Result := FCanvas.Handle;
end;

procedure TLCLBackend.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
var
  original, subsection: CGImageRef;
  CGDstRect, CGSrcRect: CGRect;
  ExternalContext: CGContextRef;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.Draw]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  // Gets the external context
  if (hSrc = 0) then Exit;
  ExternalContext := TCarbonDeviceContext(hSrc).CGContext;

  // Converts the rectangles to CoreGraphics rectangles
  CGDstRect := GetCGRect(DstRect);
  CGSrcRect := GetCGRect(SrcRect);

  // Gets an image handle that represents the subsection
  original := CGBitmapContextCreateImage(ExternalContext);
  subsection := CGImageCreateWithImageInRect(original, CGSrcRect);
  CGImageRelease(original);

  { We need to make adjustments to the CTM so the painting is done correctly }
  CGContextSaveGState(FContext);
  try
    CGContextTranslateCTM(FContext, 0, FOwner.Height);
    CGContextScaleCTM(FContext, 1, -1);
    CGContextTranslateCTM(FContext, 0, -CGDstRect.origin.y);
    CGDstRect.origin.y := 0;

    { Draw the subsection }
    CGContextDrawImage(FContext, CGDstRect, subsection);
  finally
    { reset the CTM to the old values }
    CGContextRestoreGState(FContext);
  end;

  // Release the subsection
  CGImageRelease(subsection);
end;

procedure TLCLBackend.DrawTo(hDst: HDC; DstX, DstY: Integer);
var
  DstRect, SrcRect: TRect;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.DrawTo]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}
  
  DstRect.Left   := DstX;
  DstRect.Top    := DstY;
  DstRect.Right  := FOwner.Width + DstX;
  DstRect.Bottom := FOwner.Height + DstY;

  SrcRect.Left   := 0;
  SrcRect.Top    := 0;
  SrcRect.Right  := FOwner.Width;
  SrcRect.Bottom := FOwner.Height;

  DrawTo(hDst, DstRect, SrcRect);
end;

procedure TLCLBackend.DrawTo(hDst: HDC; const DstRect, SrcRect: TRect);
var
  original, subsection: CGImageRef;
  CGDstRect, CGSrcRect: CGRect;
  ExternalContext: CGContextRef;
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.DrawTo with rects]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}

  // Gets the external context
  if (hDst = 0) then Exit;
  ExternalContext := TCarbonDeviceContext(hDst).CGContext;

  // Converts the rectangles to CoreGraphics rectangles
  CGDstRect := GetCGRect(DstRect);
  CGSrcRect := GetCGRect(SrcRect);

  // Gets an image handle that represents the subsection
  original := CGBitmapContextCreateImage(FContext);
  subsection := CGImageCreateWithImageInRect(original, CGSrcRect);
  CGImageRelease(original);
  
  { We need to make adjustments to the CTM so the painting is done correctly }
  CGContextSaveGState(ExternalContext);
  try
    CGContextTranslateCTM(ExternalContext, 0, FOwner.Height);
    CGContextScaleCTM(ExternalContext, 1, -1);
    CGContextTranslateCTM(ExternalContext, 0, -CGDstRect.origin.y);
    CGDstRect.origin.y := 0;

    { Draw the subsection }
    CGContextDrawImage(ExternalContext, CGDstRect, subsection);
  finally
    { reset the CTM to the old values }
    CGContextRestoreGState(ExternalContext);
  end;
  
  // Release the subsection
  CGImageRelease(subsection);
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

  LCLIntf.ExtTextOut(FCanvas.Handle, X, Y, ETO_CLIPPED, @ClipRect, PChar(Text), Length(Text), nil);
end;

procedure TLCLBackend.Textout(DstRect: TRect; const Flags: Cardinal; const Text: string);
begin
  {$IFDEF VerboseGR32Carbon}
    WriteLn('[TLCLBackend.Textout with Flags]',
     ' Self: ', IntToHex(PtrUInt(Self), 8));
  {$ENDIF}
  
  if not Assigned(FCanvas) then GetCanvas;

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