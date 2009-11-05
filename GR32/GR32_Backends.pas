unit GR32_Backends;

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
 * Andre Beckedorf - metaException
 * Andre@metaException.de
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
{$IFDEF FPC}
  LCLIntf, LCLType, types, Controls, Graphics,
{$ELSE}
  {$IFDEF CLX}
    Qt, Types,
    {$IFDEF LINUX}Libc,{$ENDIF}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF}
    QControls, QGraphics, QConsts,
  {$ELSE}
    Windows, Messages, Controls, Graphics,
  {$ENDIF}
{$ENDIF}
  Classes, SysUtils, GR32, GR32_Containers, GR32_Image;
  
type
  ITextSupport = interface
  ['{225997CC-958A-423E-8B60-9EDE0D3B53B5}']
    procedure Textout(X, Y: Integer; const Text: String); overload;
    procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: String); overload;
    procedure Textout(DstRect: TRect; const Flags: Cardinal; const Text: String); overload;
    function  TextExtent(const Text: String): TSize;

    procedure TextoutW(X, Y: Integer; const Text: Widestring); overload;
    procedure TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring); overload;
    procedure TextoutW(DstRect: TRect; const Flags: Cardinal; const Text: Widestring); overload;
    function  TextExtentW(const Text: Widestring): TSize;
  end;

  IFontSupport = interface
  ['{67C73044-1EFF-4FDE-AEA2-56BFADA50A48}']
    function GetOnFontChange: TNotifyEvent;
    procedure SetOnFontChange(Handler: TNotifyEvent);
    function GetFont: TFont;
    procedure SetFont(const Font: TFont);

    procedure UpdateFont;
    property Font: TFont read GetFont write SetFont;
    property OnFontChange: TNotifyEvent read GetOnFontChange write SetOnFontChange;
  end;

  ICanvasSupport = interface
  ['{5ACFEEC7-0123-4AD8-8AE6-145718438E01}']
    function GetCanvasChange: TNotifyEvent;
    procedure SetCanvasChange(Handler: TNotifyEvent);
    function GetCanvas: TCanvas;

    procedure DeleteCanvas;
    function CanvasAllocated: Boolean;

    property Canvas: TCanvas read GetCanvas;
    property OnCanvasChange: TNotifyEvent read GetCanvasChange write SetCanvasChange;
  end;

  IDeviceContextSupport = interface
  ['{DD1109DA-4019-4A5C-A450-3631A73CF288}']
    function GetHandle: HDC;

    procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;

    property Handle: HDC read GetHandle;
  end;

  IBitmapContextSupport = interface
  ['{DF0F9475-BA13-4C6B-81C3-D138624C4D08}']
    function GetBitmapInfo: TBitmapInfo;
    function GetBitmapHandle: THandle;

    property BitmapInfo: TBitmapInfo read GetBitmapInfo;
    property BitmapHandle: THandle read GetBitmapHandle;
  end;

  IPaintSupport = interface
  ['{CE64DBEE-C4A9-4E8E-ABCA-1B1FD6F45924}']
    procedure ImageNeeded;
    procedure CheckPixmap;
    procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas; APaintBox: TCustomPaintBox32);
  end;

  { TCustomBackend }
  { This class functions as backend for the TBitmap32 class.
    It manages and provides the backing buffer as well as OS or
    graphics subsystem specific features.}

  TCustomBackend = class(TBackend)
  protected
    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); virtual; abstract;
    procedure FinalizeSurface; virtual; abstract;

{$IFDEF BITS_GETTER}
    function GetBits: PColor32Array; override;
{$ENDIF}
  public
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Empty: Boolean; override;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer; ClearBuffer: Boolean = True); override;
  end;

  TRequireOperatorMode = (romAnd, romOr);

// Helper functions to temporarily switch the back-end depending on the required interfaces

procedure RequireBackendSupport(TargetBitmap: TCustomBitmap32;
  RequiredInterfaces: array of TGUID;
  Mode: TRequireOperatorMode; UseOptimizedDestructiveSwitchMethod: Boolean;
  out ReleasedBackend: TBackend);

procedure RestoreBackend(TargetBitmap: TCustomBitmap32; const SavedBackend: TBackend);

implementation

uses
  GR32_LowLevel;

type
  TBitmap32Access = class(TBitmap32);

procedure RequireBackendSupport(TargetBitmap: TCustomBitmap32;
  RequiredInterfaces: array of TGUID;
  Mode: TRequireOperatorMode; UseOptimizedDestructiveSwitchMethod: Boolean;
  out ReleasedBackend: TBackend);
var
  I: Integer;
  Supported: Boolean;
begin
  Supported := False;
  for I := Low(RequiredInterfaces) to High(RequiredInterfaces) do
  begin
    Supported := Supports(TargetBitmap.Backend, RequiredInterfaces[I]);
    if ((Mode = romAnd) and not Supported) or
      ((Mode = romOr) and Supported) then
      Break;
  end;

  if not Supported then
  begin
    if UseOptimizedDestructiveSwitchMethod then
      TargetBitmap.SetSize(0, 0); // Reset size so we avoid the buffer copy during back-end switch

    ReleasedBackend := TargetBitmap.ReleaseBackend;

    // TODO: Try to find a back-end that supports the required interfaces
    //       instead of resorting to the default platform back-end class...
    TargetBitmap.Backend := GetPlatformBackendClass.Create;
  end
  else
    ReleasedBackend := nil;
end;

procedure RestoreBackend(TargetBitmap: TCustomBitmap32; const SavedBackend: TBackend);
begin
  if Assigned(SavedBackend) then
    TargetBitmap.Backend := SavedBackend;
end;

{ TCustomBackend }

destructor TCustomBackend.Destroy;
begin
  Clear;
  inherited;
end;

{$IFDEF BITS_GETTER}
function TCustomBackend.GetBits: PColor32Array;
begin
  Result := FBits;
end;
{$ENDIF}

procedure TCustomBackend.ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  try
    Changing;

    FinalizeSurface;

    Width := 0;
    Height := 0;

    if (NewWidth > 0) and (NewHeight > 0) then
      InitializeSurface(NewWidth, NewHeight, ClearBuffer);

    Width := NewWidth;
    Height := NewHeight;
  finally
    Changed;
  end;
end;

procedure TCustomBackend.Assign(Source: TPersistent);
var
  SrcBackend: TCustomBackend;
begin
  if Source is TCustomBackend then
  begin
    if Assigned(FOwner) then
    begin
      SrcBackend := TCustomBackend(Source);

      ChangeSize(
        TBitmap32Access(FOwner).FWidth, TBitmap32Access(FOwner).FHeight,
        SrcBackend.FOwner.Width, SrcBackend.FOwner.Height,
        False
      );

      if not SrcBackend.Empty then
        MoveLongword(
          SrcBackend.Bits[0], Bits[0],
          SrcBackend.FOwner.Width * SrcBackend.FOwner.Height
        );
    end;
  end
  else
    inherited;
end;

function TCustomBackend.Empty: Boolean;
begin
  Result := False;
end;

end.