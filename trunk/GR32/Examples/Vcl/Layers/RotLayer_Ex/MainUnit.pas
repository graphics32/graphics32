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

{$I GR32.INC}

uses
  {$IFDEF FPC}LCLIntf, LResources, Variants, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, GR32,
  GR32_Image, GR32_RotLayer, GR32_Transforms, GR32_RangeBars, GR32_Resamplers;

type
  TForm1 = class(TForm)
    ImgView: TImgView32;
    GaugeBar1: TGaugeBar;
    GaugeBar2: TGaugeBar;
    GaugeBar3: TGaugeBar;
    GaugeBar4: TGaugeBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
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

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
{$IFDEF Darwin}
  FPCMacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG,
{$ELSE}
  LazJPG,
{$ENDIF}
  Math;

procedure TForm1.FormCreate(Sender: TObject);
var
{$IFDEF Darwin}
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
  pathMedia: string;
begin
  // Under Mac OS X we need to get the location of the bundle
{$IFDEF Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
{$ENDIF}

  // On Lazarus we don't use design-time packages because they consume time to be installed
{$IFDEF FPC}
  ImgView := TImgView32.Create(Self);
  ImgView.Parent := Self;
  ImgView.Left := 4;
  ImgView.Top := 8;
  ImgView.Width := 354;
  ImgView.Height := 294;
  ImgView.Anchors := [akLeft, akTop, akRight, akBottom];
  ImgView.Bitmap.ResamplerClassName := 'TNearestResampler';
  ImgView.BitmapAlign := baCustom;
  ImgView.Color := clAppWorkSpace;
  ImgView.ParentColor := False;
  ImgView.RepaintMode := rmOptimizer;
  ImgView.Scale := 1.000000000000000000;
  ImgView.ScaleMode := smScale;
  ImgView.ScrollBars.ShowHandleGrip := True;
  ImgView.ScrollBars.Style := rbsMac;
  ImgView.OverSize := 0;
  ImgView.TabOrder := 0;

  GaugeBar1 := TGaugeBar.Create(Self);
  GaugeBar1.Parent := Self;
  GaugeBar1.Left := 150;
  GaugeBar1.Top := 313;
  GaugeBar1.Width := 153;
  GaugeBar1.Height := 16;
  GaugeBar1.Anchors := [akLeft, akBottom];
  GaugeBar1.Backgnd := bgPattern;
  GaugeBar1.Max := 180;
  GaugeBar1.Min := -180;
  GaugeBar1.ShowHandleGrip := True;
  GaugeBar1.Style := rbsMac;
  GaugeBar1.Position := 0;
  GaugeBar1.OnChange := GaugeBar1Change;

  GaugeBar2 := TGaugeBar.Create(Self);
  GaugeBar2.Parent := Self;
  GaugeBar2.Left := 150;
  GaugeBar2.Top := 345;
  GaugeBar2.Width := 153;
  GaugeBar2.Height := 16;
  GaugeBar2.Anchors := [akLeft, akBottom];
  GaugeBar2.Backgnd := bgPattern;
  GaugeBar2.Max := 200;
  GaugeBar2.ShowHandleGrip := True;
  GaugeBar2.Style := rbsMac;
  GaugeBar2.Position := 100;
  GaugeBar2.OnChange := GaugeBar2Change;

  GaugeBar3 := TGaugeBar.Create(Self);
  GaugeBar3.Parent := Self;
  GaugeBar3.Left := 150;
  GaugeBar3.Top := 373;
  GaugeBar3.Width := 153;
  GaugeBar3.Height := 16;
  GaugeBar3.Anchors := [akLeft, akBottom];
  GaugeBar3.Backgnd := bgPattern;
  GaugeBar3.Max := 200;
  GaugeBar3.ShowHandleGrip := True;
  GaugeBar3.Style := rbsMac;
  GaugeBar3.Position := 100;
  GaugeBar3.OnChange := GaugeBar2Change;

  GaugeBar4 := TGaugeBar.Create(Self);
  GaugeBar4.Parent := Self;
  GaugeBar4.Left := 150;
  GaugeBar4.Top := 441;
  GaugeBar4.Width := 153;
  GaugeBar4.Height := 16;
  GaugeBar4.Anchors := [akLeft, akBottom];
  GaugeBar4.Backgnd := bgPattern;
  GaugeBar4.Min := -100;
  GaugeBar4.ShowHandleGrip := True;
  GaugeBar4.Style := rbsMac;
  GaugeBar4.Position := 0;
  GaugeBar4.OnChange := GaugeBar4Change;
{$ENDIF}

  ImgView.Bitmap.SetSize(200, 200);

  // Different platforms store resource files on different locations
{$IFDEF Windows}
  {$IFDEF FPC}
  pathMedia := '..\..\..\..\Media\';
  {$ELSE}
  pathMedia := '..\..\..\Media\';
  {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF Darwin}
    pathMedia := pathStr + '/Contents/Resources/Media/';
  {$ELSE}
    pathMedia := '../../../Media/';
  {$ENDIF}
{$ENDIF}

  ImgView.Bitmap.LoadFromFile(pathMedia + 'delphi.jpg');

  L := TRotLayer.Create(ImgView.Layers);
  L.Bitmap := TBitmap32.Create;
  with L.Bitmap do
  begin
    BeginUpdate;

    // Different platforms store resource files on different locations
    LoadFromFile(pathMedia + 'sprite_texture.bmp');

    TLinearResampler.Create(L.Bitmap);

    //ensure good looking edge, dynamic alternative to SetBorderTransparent
    TCustomResampler(L.Bitmap.Resampler).PixelAccessMode := pamTransparentEdge;

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
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  L.Scaled := not L.Scaled;
end;

{$IFDEF FPC}
initialization
  {$I MainUnit.lrs}
{$ENDIF}

end.
