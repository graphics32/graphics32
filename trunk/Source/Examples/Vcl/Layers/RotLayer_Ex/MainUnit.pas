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

{$I GR32.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, Variants, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, GR32,
  GR32_Image, GR32_RotLayer, GR32_Transforms, GR32_RangeBars, GR32_Resamplers;

type

  { TFormRotLayer }

  TFormRotLayer = class(TForm)
    cbScaled: TCheckBox;
    gbAngle: TGaugeBar;
    gbPositionX: TGaugeBar;
    gbPositionY: TGaugeBar;
    gbScale: TGaugeBar;
    ImgView: TImgView32;
    lbAngle: TLabel;
    lbPositionX: TLabel;
    lbPositionY: TLabel;
    lbScale: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure gbAngleChange(Sender: TObject);
    procedure gbPositionChange(Sender: TObject);
    procedure gbScaleChange(Sender: TObject);
    procedure cbScaledClick(Sender: TObject);
  private
    { Private declarations }
  public
    L: TRotLayer;
  end;

var
  FormRotLayer: TFormRotLayer;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG,
{$ELSE}
  LazJPG,
{$ENDIF}
  Math;

procedure TFormRotLayer.FormCreate(Sender: TObject);
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
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding);
  CFRelease(pathRef);
  CFRelease(pathCFStr);
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

  Assert(FileExists(pathMedia + 'delphi.jpg'));
  ImgView.Bitmap.LoadFromFile(pathMedia + 'delphi.jpg');

  L := TRotLayer.Create(ImgView.Layers);
  L.Bitmap := TBitmap32.Create;
  with L.Bitmap do
  begin
    BeginUpdate;

    // Different platforms store resource files on different locations
    Assert(FileExists(pathMedia + 'sprite_texture.bmp'));
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

procedure TFormRotLayer.gbAngleChange(Sender: TObject);
begin
  L.Angle := gbAngle.Position;
end;

procedure TFormRotLayer.gbPositionChange(Sender: TObject);
var
  P: TFloatPoint;
begin
  P := L.Position;
  P.X := gbPositionX.Position;
  P.Y := gbPositionY.Position;
  L.Position := P;
end;

procedure TFormRotLayer.gbScaleChange(Sender: TObject);
begin
  ImgView.Scale := Power(10, gbScale.Position * 0.01);
end;

procedure TFormRotLayer.cbScaledClick(Sender: TObject);
begin
  L.Scaled := not L.Scaled;
end;

{$IFDEF FPC}
initialization
  {$I MainUnit.lrs}
{$ENDIF}

end.
