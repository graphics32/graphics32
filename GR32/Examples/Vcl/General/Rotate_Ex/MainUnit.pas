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
 * The Original Code is Rotate Example
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
  {$IFDEF FPC}LCLIntf, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, GR32, GR32_Image,
  GR32_Transforms, ComCtrls, Math, GR32_RangeBars;

type
  TForm1 = class(TForm)
    Src: TImage32;
    Dst: TImage32;
    Angle: TGaugeBar;
    procedure FormCreate(Sender: TObject);
    procedure AngleChange(Sender: TObject);
  public
    procedure ScaleRot(Alpha: Single);
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
  JPEG;
{$ELSE}
  LazJPEG;
{$ENDIF}

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
  Src := TImage32.Create(Self);
  Src.Parent := Self;
  Src.Left := 16;
  Src.Top := 16;
  Src.Width := 192;
  Src.Height := 192;
  Src.Bitmap.DrawMode := dmBlend;
  Src.Bitmap.ResamplerClassName := 'TLinearResampler';
  Src.BitmapAlign := baCenter;
  Src.Color := clWindowText;
  Src.ParentColor := False;
  Src.Scale := 1;
  Src.ScaleMode := smNormal;
  Src.TabOrder := 0;

  Dst := TImage32.Create(Self);
  Dst.Parent := Self;
  Dst.Left := 232;
  Dst.Top := 16;
  Dst.Width := 192;
  Dst.Height := 192;
  Dst.Bitmap.ResamplerClassName := 'TNearestResampler';
  Dst.BitmapAlign := baCenter;
  Dst.Color := clWindowText;
  Dst.ParentColor := False;
  Dst.Scale := 1;
  Dst.ScaleMode := smNormal;
  Dst.TabOrder := 1;

  Angle := TGaugeBar.Create(Self);
  Angle.Parent := Self;
  Angle.Left := 16;
  Angle.Top := 220;
  Angle.Width := 409;
  Angle.Height := 19;
  Angle.Backgnd := bgPattern;
  Angle.Max := 180;
  Angle.Min := -180;
  Angle.ShowHandleGrip := True;
  Angle.Style := rbsMac;
  Angle.Position := 0;
  Angle.OnChange := AngleChange;
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

  Src.Bitmap.LoadFromFile(pathMedia + 'delphi.jpg');

  Dst.Bitmap.SetSize(Src.Bitmap.Width, Src.Bitmap.Height);

  // a workaround to the edge antialiasing problem
  SetBorderTransparent(Src.Bitmap, Src.Bitmap.BoundsRect);

  // show the picture
  ScaleRot(0);
end;

procedure TForm1.ScaleRot(Alpha: Single);
var
  SrcR: Integer;
  SrcB: Integer;
  T: TAffineTransformation;
  Sx, Sy, Scale: Single;
begin
  SrcR := Src.Bitmap.Width - 1;
  SrcB := Src.Bitmap.Height - 1;
  T := TAffineTransformation.Create;
  T.SrcRect := FloatRect(0, 0, SrcR + 1, SrcB + 1);
  try
    // shift the origin
    T.Clear;

    // move the origin to a center for scaling and rotation
    T.Translate(-SrcR / 2, -SrcB / 2);
    T.Rotate(0, 0, Alpha);
    Alpha := Alpha * PI / 180;

    // get the width and height of rotated image (without scaling)
    Sx := Abs(SrcR * Cos(Alpha)) + Abs(SrcB * Sin(Alpha));
    Sy := Abs(SrcR * Sin(Alpha)) + Abs(SrcB * Cos(Alpha));

    // calculate a new scale so that the image fits in original boundaries
    Sx := Src.Bitmap.Width / Sx;
    Sy := Src.Bitmap.Height / Sy;
    scale := Min(Sx, Sy);

    T.Scale(Scale, Scale);

    // move the origin back
    T.Translate(SrcR / 2, SrcB / 2);

    // transform the bitmap
    Dst.BeginUpdate;
    Dst.Bitmap.Clear(clBlack32);
    Transform(Dst.Bitmap, Src.Bitmap, T);
    Dst.EndUpdate;
    Dst.Repaint;
  finally
    T.Free;
  end;
end;

procedure TForm1.AngleChange(Sender: TObject);
begin
  ScaleRot(-Angle.Position);
end;

{$IFDEF FPC}
initialization
  {$I MainUnit.lrs}
{$ENDIF}

end.
