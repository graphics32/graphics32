unit G32_ProgressBar;

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
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Windows, Classes, Graphics, Forms, GR32, GR32_Image, GR32_Blend;

type
  TG32_ProgressBar = class(TCustomPaintBox32)
  private
    FBackColor: TColor;
    FBorderStyle: TBorderStyle;
    FContrast: Integer;
    FFramed: Boolean;
    FMax: Integer;
    FMin: Integer;
    FPosition: Integer;
    procedure SetBackColor(Value: TColor);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetContrast(Value: Integer);
    procedure SetFramed(Value: Boolean);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPosition(Value: Integer);
  protected
    procedure DoPaintBuffer; override;
    function  GetBarRect: TRect;
    procedure PaintBar(Buffer: TBitmap32; ARect: TRect);
    procedure PaintFrame(Buffer: TBitmap32; ARect: TRect);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property BackColor: TColor read FBackColor write SetBackColor default clBtnShadow;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Contrast: Integer read FContrast write SetContrast default 64;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Framed: Boolean read FFramed write SetFramed default True;
    property Height default 16;
    property HelpContext;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Position: Integer read FPosition write SetPosition;
    property ShowHint;
    property Visible;
    property Width default 150;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Graphics32', [TG32_ProgressBar]);
end;

{ TG32_ProgressBar }

constructor TG32_ProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  Width := 150;
  Height := 16;
  FBackColor := clBtnShadow;
  FContrast := 64;
  FMax := 100;
  FFramed := True;
end;

procedure TG32_ProgressBar.DoPaintBuffer;
var
  Rect: TRect;
begin
  inherited;
  Rect := ClientRect;
  PaintFrame(Buffer, Rect);
  PaintBar(Buffer, GetBarRect);
end;

function TG32_ProgressBar.GetBarRect: TRect;
var
  X: Integer;
begin
  Result := ClientRect;
  if Framed then InflateRect(Result, -1, -1);
  with Result do
    if (Position > Min) and (Max > Min) then
    begin
      X := Round(Left + (Position - Min) * (Right - Left) / (Max - Min));
      if X < Right then Right := X;
    end
    else
      Right := Left; // return an empty rectagle
end;

procedure TG32_ProgressBar.PaintBar(Buffer: TBitmap32; ARect: TRect);
var
  Clr, LineColor: TColor32;
  I, CY, H: Integer;
begin
  if IsRectEmpty(ARect) then Exit;

  Clr := Color32(Color);

  if Contrast <> 0 then
  begin
    with ARect do
    try
      H := Bottom - Top;
      CY := (Top + Bottom) div 2;
      for I := Top to Bottom - 1 do
      begin
        LineColor := Lighten(Clr, (CY - I) * Contrast div H);
        Buffer.HorzLineS(Left, I, Right - 1, LineColor);
      end;
    finally
      EMMS; // the low-level blending function was used EMMS is required
    end;
  end
  else Buffer.FillRectS(ARect, Clr);

  Buffer.RaiseRectTS(ARect, 32);

  if Framed then
    with ARect do Buffer.VertLineS(Right, Top, Bottom, Color32(clWindowFrame));
end;

procedure TG32_ProgressBar.PaintFrame(Buffer: TBitmap32; ARect: TRect);
var
  Clr: TColor32;
begin
  if BorderStyle = bsSingle then
  begin
  end;

  { Paint frame border }
  if Framed then
  begin
    Clr := TColor32(clWindowFrame);
    Buffer.FrameRectS(ARect, Clr);
    InflateRect(ARect, -1, -1);
  end;

  { Fill the background }
  Clr := Color32(BackColor);
  Buffer.FillRectS(ARect, Clr);

  { Paint shadow on top of background}
  with ARect do
  begin
    Dec(Right);
    Dec(Bottom);
    Clr := Lighten(Clr, -32);
    Buffer.HorzLineS(Left, Top, Right, Clr);
    Buffer.VertLineS(Left, Top + 1, Bottom, Clr);
    if not Framed then
    begin
      Clr := Lighten(Clr, 64);
      Buffer.HorzLineS(Left, Bottom, Right, Clr);
      Buffer.VertLineS(Right, Top + 1, Bottom, Clr);
    end;
  end;
end;

procedure TG32_ProgressBar.SetBackColor(Value: TColor);
begin
  FBackColor := Value;
  Invalidate;
end;

procedure TG32_ProgressBar.SetBorderStyle(Value: TBorderStyle);
begin
  FBorderStyle := Value;
  Invalidate;
end;

procedure TG32_ProgressBar.SetContrast(Value: Integer);
begin
  FContrast := Value;
  Invalidate;
end;

procedure TG32_ProgressBar.SetFramed(Value: Boolean);
begin
  FFramed := Value;
  Invalidate;
end;

procedure TG32_ProgressBar.SetMax(Value: Integer);
begin
  FMax := Value;
  Invalidate;
end;

procedure TG32_ProgressBar.SetMin(Value: Integer);
begin
  FMin := Value;
  Invalidate;
end;

procedure TG32_ProgressBar.SetPosition(Value: Integer);
begin
  if Value <> Position then
  begin
    FPosition := Value;
    Repaint;
  end;
end;

end.
