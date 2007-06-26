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

{$MODE Delphi}

uses
  LCLIntf, LResources, Variants,
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

uses
{$IFDEF Darwin}
  FPCMacOSAll,
{$ENDIF}
  LazJPEG,
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

  // Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia := '..\..\..\Media\';
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF Darwin}
    pathMedia := pathStr + '/Contents/Resources/Media/';
  {$ELSE}
    pathMedia := '../../../Media/';
  {$ENDIF}
{$ENDIF}

  // On Lazarus we don't use design-time packages because they consume time to be installed
  ImgView := TImgView32.Create(Self);
  with ImgView do
  begin
    Parent := Self;
    Left := 4;
    Top := 8;
    Width := 354;
    Height := 294;
    Anchors := [akLeft, akTop, akRight, akBottom];
    Bitmap.ResamplerClassName := 'TNearestResampler';
    BitmapAlign := baCustom;
    Color := clAppWorkSpace;
    ParentColor := False;
    RepaintMode := rmOptimizer;
    Scale := 1.000000000000000000;
    ScaleMode := smScale;
    ScrollBars.ShowHandleGrip := True;
    ScrollBars.Style := rbsMac;
    OverSize := 0;
    TabOrder := 0;
    Bitmap.SetSize(200, 200);
    Bitmap.LoadFromFile(pathMedia + 'delphi.jpg');
  end;

  GaugeBar1 := TGaugeBar.Create(Self);
  with GaugeBar1 do
  begin
    Parent := Self;
    Left := 150;
    Top := 313;
    Width := 153;
    Height := 16;
    Anchors := [akLeft, akBottom];
    Backgnd := bgPattern;
    Max := 180;
    Min := -180;
    ShowHandleGrip := True;
    Style := rbsMac;
    Position := 0;
    OnChange := GaugeBar1Change;
  end;

  GaugeBar2 := TGaugeBar.Create(Self);
  with GaugeBar2 do
  begin
    Parent := Self;
    Left := 150;
    Top := 345;
    Width := 153;
    Height := 16;
    Anchors := [akLeft, akBottom];
    Backgnd := bgPattern;
    Max := 200;
    ShowHandleGrip := True;
    Style := rbsMac;
    Position := 100;
    OnChange := GaugeBar2Change;
  end;

  GaugeBar3 := TGaugeBar.Create(Self);
  with GaugeBar3 do
  begin
    Parent := Self;
    Left := 150;
    Top := 373;
    Width := 153;
    Height := 16;
    Anchors := [akLeft, akBottom];
    Backgnd := bgPattern;
    Max := 200;
    ShowHandleGrip := True;
    Style := rbsMac;
    Position := 100;
    OnChange := GaugeBar2Change;
  end;

  GaugeBar4 := TGaugeBar.Create(Self);
  with GaugeBar4 do
  begin
    Parent := Self;
    Left := 150;
    Top := 441;
    Width := 153;
    Height := 16;
    Anchors := [akLeft, akBottom];
    Backgnd := bgPattern;
    Min := -100;
    ShowHandleGrip := True;
    Style := rbsMac;
    Position := 0;
    OnChange := GaugeBar4Change;
  end;

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

initialization
  {$I MainUnit.lrs}

end.
