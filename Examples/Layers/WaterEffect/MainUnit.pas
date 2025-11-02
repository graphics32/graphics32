unit MainUnit;

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
 * The Original Code is WaterEffect Example
 *
 * The Initial Developers of the Original Code is:
 *
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2025
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  GR32_Image,
  GR32_Layers,
  GR32.Layers.WaterEffect;


type
  TFormMain = class(TForm)
    Image32: TImage32;
    TimerWaterEffect: TTimer;
    TimerRaindrops: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Image32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure TimerWaterEffectTimer(Sender: TObject);
    procedure Image32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure TimerRaindropsTimer(Sender: TObject);
  private
    FWaterEffectLayer: TWaterEffectLayer32;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Types;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FWaterEffectLayer := TWaterEffectLayer32(Image32.Layers.Add(TWaterEffectLayer32));

  // We need the buffer to be the exact size of the bitmap since the
  // water effect distortion map size is based on the layer buffer size.
  Image32.BufferOversize := 0;

  Image32.Bitmap.LoadFromResourceName(HInstance, 'MonaLisa', RT_RCDATA);
end;

procedure TFormMain.Image32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if (Layer = FWaterEffectLayer) then
  begin
    if (not Image32.BoundsRect.Contains(Point(X, Y))) then
      exit;

    FWaterEffectLayer.WaterDrop(X, Y, 10, 150);
  end;
end;

procedure TFormMain.Image32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Multiplier: integer;
  Sign: integer;
begin
  if (Layer = FWaterEffectLayer) then
  begin
    if (not Image32.BoundsRect.Contains(Point(X, Y))) then
      exit;

    if (ssLeft in Shift) then
      Multiplier := 2
    else
      Multiplier := 1;

    if (ssRight in Shift) then
      Sign := -1
    else
      Sign := 1;

    FWaterEffectLayer.WaterDrop(X, Y, 10 * Multiplier, 50 * Multiplier * Sign);
  end;
end;

procedure TFormMain.TimerRaindropsTimer(Sender: TObject);
var
  DropSize: integer;
begin
  DropSize := 5 + Random(5);
  FWaterEffectLayer.WaterDrop(-1, -1, DropSize, DropSize * DropSize);

  TTimer(Sender).Interval := 100 + Random(1000);
end;

procedure TFormMain.TimerWaterEffectTimer(Sender: TObject);
begin
  FWaterEffectLayer.UpdateWater;
end;

end.
