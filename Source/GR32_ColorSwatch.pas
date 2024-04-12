unit GR32_ColorSwatch;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf, LCLType, LMessages, Types,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
{$ELSE}
  Windows, Messages,
{$ENDIF}
  Classes, Controls, Forms, GR32, GR32_Containers;

type
  TCustomColorSwatch = class(TCustomControl)
  private
    FBuffer: TBitmap32;
    FColor: TColor32;
    FBufferValid: Boolean;
    FBorder: Boolean;
    procedure SetBorder(const Value: Boolean);
    procedure SetColor(const Value: TColor32); reintroduce;
{$IFDEF FPC}
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TLMessage); message LM_GETDLGCODE;
{$ELSE}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWmGetDlgCode); message WM_GETDLGCODE;
{$ENDIF}
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Invalidate; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property Border: Boolean read FBorder write SetBorder default False;
    property Color: TColor32 read FColor write SetColor;
  end;

  TColorSwatch = class(TCustomColorSwatch)
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure SkipValue(Reader: TReader);
  published
    property Border;
    property Color;

    property Align;
    property Anchors;
    property DragCursor;
    property DragKind;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;

{$IFNDEF PLATFORM_INDEPENDENT}
    property OnCanResize;
{$ENDIF}
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnStartDrag;
  end;

implementation

uses
  Math, Graphics, GR32_Backends, GR32_Math, GR32_Blend, GR32_VectorUtils;


{ TCustomColorSwatch }

procedure TCustomColorSwatch.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TCustomColorSwatch) then
  begin
    FBorder := TCustomColorSwatch(Source).Border;
    FColor := TCustomColorSwatch(Source).Color;

    Invalidate;
  end;
end;

constructor TCustomColorSwatch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := [csOpaque];
  Width := 32;
  Height := 32;

  FBuffer := TBitmap32.Create;
  FColor := clSalmon32;
end;

destructor TCustomColorSwatch.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TCustomColorSwatch.Invalidate;
begin
  FBufferValid := False;
  inherited;
end;

procedure TCustomColorSwatch.Paint;
var
  X, Y: Integer;
  OddY: Boolean;
  ScanLine: PColor32Array;
const
  CCheckerBoardColor: array [Boolean] of TColor32 = ($FFA0A0A0, $FF5F5F5F);
begin
//  if not Assigned(Parent) then
//    Exit;


  if (FBuffer.Empty) then
    exit;

  Canvas.Pen.Color := clRed;
  Canvas.MoveTo(0, 0);
  Canvas.LineTo(FBuffer.Width, FBuffer.Height);

  if not FBufferValid then
  begin
  Canvas.Pen.Color := clGreen;
  Canvas.MoveTo(Width, 0);
  Canvas.LineTo(0, Height);

    (FBuffer.Backend as IPaintSupport).ImageNeeded;

    // draw checker board
    if not (FColor and $FF000000 = $FF000000) then
    begin
      Y := 0;
      while Y < FBuffer.Height do
      begin
        ScanLine := FBuffer.Scanline[Y];
        OddY := Odd(Y shr 2);
        for X := 0 to FBuffer.Width - 1 do
          ScanLine[X] := CCheckerBoardColor[Odd(X shr 2) = OddY];
        Inc(Y);
      end;
    end;

    // draw color
    FBuffer.FillRectT(0, 0, FBuffer.Width, FBuffer.Height, FColor);

    // eventually draw border
    if FBorder then
      FBuffer.FrameRectTS(0, 0, FBuffer.Width, FBuffer.Height, $DF000000);

    (FBuffer.Backend as IPaintSupport).CheckPixmap;
    FBufferValid := True;
  end;

  FBuffer.Lock;
  try
    (FBuffer.Backend as IDeviceContextSupport).DrawTo(Canvas.Handle, 0, 0);
  finally
    FBuffer.Unlock;
  end;
end;

procedure TCustomColorSwatch.SetBorder(const Value: Boolean);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Invalidate;
  end;
end;

procedure TCustomColorSwatch.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (FBuffer <> nil) then
    FBuffer.SetSize(Width, Height);
  FBufferValid := False;
end;

procedure TCustomColorSwatch.SetColor(const Value: TColor32);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TCustomColorSwatch.WMEraseBkgnd(var Message: {$IFDEF FPC}TLmEraseBkgnd{$ELSE}TWmEraseBkgnd{$ENDIF});
begin
  Message.Result := 1;
end;

procedure TCustomColorSwatch.WMGetDlgCode(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TWmGetDlgCode{$ENDIF});
begin
  Msg.Result := Msg.Result or DLGC_WANTARROWS;
end;

{ TColorSwatch }

procedure TColorSwatch.DefineProperties(Filer: TFiler);
begin
  inherited;

  // Previously, but no longer, published properties
  Filer.DefineProperty('ParentBackground', SkipValue, nil, False);
  Filer.DefineProperty('ParentColor', SkipValue, nil, False);
end;

procedure TColorSwatch.SkipValue(Reader: TReader);
begin
{$ifndef FPC}
  Reader.SkipValue;
{$else}
  Reader.Driver.SkipValue;
{$endif}
end;

end.
