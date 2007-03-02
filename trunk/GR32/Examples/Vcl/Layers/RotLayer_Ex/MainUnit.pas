unit MainUnit;

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
 *    Michael Hansen <dyster_tid@hotmail.com>
 *    - 2007/03/02 - pamTransparentEdge setup, minor GUI changes, image loading
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, GR32,
  GR32_Image, GR32_RotLayer, GR32_Transforms, GR32_RangeBars, GR32_Resamplers,
  Jpeg;

type
  TForm1 = class(TForm)
    ImgView: TImgView32;
    GaugeBar1: TGaugeBar;
    Label1: TLabel;
    GaugeBar2: TGaugeBar;
    Label2: TLabel;
    GaugeBar3: TGaugeBar;
    Label3: TLabel;
    GaugeBar4: TGaugeBar;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GaugeBar1Change(Sender: TObject);
    procedure GaugeBar2Change(Sender: TObject);
    procedure GaugeBar4Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    L: TRotLayer;
  end;

var
  Form1: TForm1;

implementation

uses Math;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImgView.Bitmap.SetSize(200, 200);
  ImgView.Bitmap.LoadFromFile('..\..\..\Media\stones.jpg');
  L := TRotLayer.Create(ImgView.Layers);
  L.Bitmap := TBitmap32.Create;
  with L.Bitmap do
  begin
    BeginUpdate;
    LoadFromFile('..\..\..\Media\sprite_texture.bmp');

    TLinearResampler.Create(L.Bitmap);

    //ensure good looking edge, dynamic alternative to SetBorderTransparent
    TBitmap32Resampler(L.Bitmap.Resampler).PixelAccessMode := pamTransparentEdge;

    L.BitmapCenter := FloatPoint(Width / 2, Height / 2);
    MasterAlpha := 200;
    FrameRectS(BoundsRect, $FFFFFFFF);
    DrawMode := dmBlend;
    EndUpdate;
    Changed;
  end;
  L.Scaled := True;
  L.Position := FloatPoint(100, 100);
end;

procedure TForm1.GaugeBar1Change(Sender: TObject);
begin
  L.Angle := GaugeBar1.Position;
end;

procedure TForm1.GaugeBar2Change(Sender: TObject);
var
  P: TFloatPoint;
begin
  P := L.Position;
  P.X := GaugeBar2.Position;
  P.Y := GaugeBar3.Position;
  L.Position := P;
end;

procedure TForm1.GaugeBar4Change(Sender: TObject);
begin
  ImgView.Scale := Power(10, GaugeBar4.Position / 100);
  ImgView.Update;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  L.Scaled := not L.Scaled;
end;

end.
