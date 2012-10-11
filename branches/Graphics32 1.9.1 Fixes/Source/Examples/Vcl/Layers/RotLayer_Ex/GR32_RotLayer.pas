unit GR32_RotLayer;

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
 * The Original Code is Rotation Layer Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$IFNDEF FPC}
  {$DEFINE Windows}
{$ENDIF}

uses
  SysUtils, Classes, Controls, Forms, Graphics, GR32, GR32_Layers, GR32_Transforms;

type
  TCustomAffineLayer = class(TCustomLayer)
  private
    FAlphaHit: Boolean;
    FTransformation: TAffineTransformation;
    FBitmap: TBitmap32;
    procedure BitmapChanged(Sender: TObject);
    procedure SetBitmap(Value: TBitmap32);
  protected
    FBitmapCenter: TFloatPoint;
    procedure AdjustTransformation; virtual;
    function DoHitTest(X, Y: Integer): Boolean; override;
    procedure Paint(Buffer: TBitmap32); override;
    property Transformation: TAffineTransformation read FTransformation;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    property AlphaHit: Boolean read FAlphaHit write FAlphaHit;
    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
  end;

  TAffineLayer = class(TCustomAffineLayer)
  public
    property Transformation;
  end;

  TRotLayer = class(TCustomAffineLayer)
  private
    FPosition: TFloatPoint;
    FScaled: Boolean;
    FAngle: Single;
    procedure SetAngle(Value: Single);
    procedure SetPosition(const Value: TFloatPoint);
    procedure SetScaled(Value: Boolean);
    procedure SetBitmapCenter(const Value: TFloatPoint);
  protected
    procedure AdjustTransformation; override;
  public
    property Angle: Single read FAngle write SetAngle;
    property BitmapCenter: TFloatPoint read FBitmapCenter write SetBitmapCenter;
    property Scaled: Boolean read FScaled write SetScaled;
    property Position: TFloatPoint read FPosition write SetPosition;
  end;

implementation

{ TCustomAffineLayer }

type TATAccess = class(TAffineTransformation);

procedure TCustomAffineLayer.AdjustTransformation;
begin
  // do nothing here
end;

procedure TCustomAffineLayer.BitmapChanged(Sender: TObject);
begin
  Transformation.SrcRect := FloatRect(Bitmap.BoundsRect);
  Changed;
end;

constructor TCustomAffineLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FBitmap := TBitmap32.Create;
  FBitmap.OnChange := BitmapChanged;
  FTransformation := TAffineTransformation.Create;
end;

destructor TCustomAffineLayer.Destroy;
begin
  FTransformation.Free;
  FBitmap.Free;
  inherited;
end;

function TCustomAffineLayer.DoHitTest(X, Y: Integer): Boolean;
var
  BX, BY: Integer;
  Pt: TPoint;
begin
  Result := False;

  with TATAccess(Transformation) do
    Pt := ReverseTransform(Point(X, Y)); // BX,BY - in 'FBitmap' coordinates

  BX := Pt.X;
  BY := Pt.Y;

  if PtInRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), Pt) then
    Result := True;

  if Result and AlphaHit and (Bitmap.PixelS[BX, BY] and $FF000000 = 0) then
    Result := False;
end;

procedure TCustomAffineLayer.Paint(Buffer: TBitmap32);
begin
  AdjustTransformation;
  Transform(Buffer, FBitmap, Transformation);
end;

procedure TCustomAffineLayer.SetBitmap(Value: TBitmap32);
begin
  FBitmap.Assign(Value);
end;

{ TRotLayer }

procedure TRotLayer.AdjustTransformation;
var
  ScaleX, ScaleY,
  ShiftX, ShiftY: Single;
begin
  Transformation.Clear;
  Transformation.Translate(-BitmapCenter.X, -BitmapCenter.Y);
  Transformation.Rotate(0, 0, Angle);
  Transformation.Translate(Position.X, Position.Y);
  if Scaled and Assigned(LayerCollection) then
    with LayerCollection do
    begin
      GetViewportScale(ScaleX, ScaleY);
      GetViewportShift(ShiftX, ShiftY);
      Transformation.Scale(ScaleX, ScaleY);
      Transformation.Translate(ShiftX, ShiftY);
    end;
end;

procedure TRotLayer.SetAngle(Value: Single);
begin
  FAngle := Value;
  Changed;
end;

procedure TRotLayer.SetBitmapCenter(const Value: TFloatPoint);
begin
  FBitmapCenter := Value;
  Changed;
end;

procedure TRotLayer.SetPosition(const Value: TFloatPoint);
begin
  FPosition := Value;
  Changed;
end;

procedure TRotLayer.SetScaled(Value: Boolean);
begin
  FScaled := Value;
  Changed;
end;

end.
