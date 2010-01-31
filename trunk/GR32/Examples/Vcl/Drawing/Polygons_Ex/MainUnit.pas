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
 * The Original Code is Polygons Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Christian-W. Budde
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, Buttons, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GR32, GR32_Image, GR32_Layers, GR32_Polygons;

type

  { TFormPolygons }

  TFormPolygons = class(TForm)
    Antialiase: TCheckBox;
    AntialiasMode: TRadioGroup;
    BitmapList: TBitmap32List;
    Button1: TButton;
    FillAlpha: TScrollBar;
    FillMode: TRadioGroup;
    Image: TImage32;
    lbLineOpacity: TLabel;
    lbFillOpacity: TLabel;
    lbOutlineThickness: TLabel;
    lbOutlineThicknessValue: TLabel;
    LineAlpha: TScrollBar;
    LineThickness: TScrollBar;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    Pattern: TCheckBox;
    ThickOutline: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageResize(Sender: TObject);
    procedure ParamsChanged(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ThicknessChanged(Sender: TObject);
  private
    Polygon: TPolygon32;
    Outline: TPolygon32;
    UseOutlinePoly: Boolean;
    LineSize: Single;
    procedure Build;
    procedure Draw;
  end;

var
  FormPolygons: TFormPolygons;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG;
{$ELSE}
  LazJPG;
{$ENDIF}

procedure TFormPolygons.FormCreate(Sender: TObject);
var
{$IFDEF Darwin}
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
  pathMedia: string;
  Item: TBitmap32Item;
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
  {$IFDEF FPC}
  pathMedia := '..\' + pathMedia;
  {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF Darwin}
    pathMedia := pathStr + '/Contents/Resources/Media/';
  {$ELSE}
    pathMedia := '../../../Media/';
  {$ENDIF}
{$ENDIF}

  // load example images
  Assert(FileExists(pathMedia + 'delphi.jpg'));
  BitmapList.Bitmap[0].LoadFromFile(pathMedia + 'delphi.jpg');

  Assert(FileExists(pathMedia + 'texture_b.jpg'));
  BitmapList.Bitmap[1].LoadFromFile(pathMedia + 'texture_b.jpg');

  Image.SetupBitmap;
  Polygon := TPolygon32.Create;
end;

procedure TFormPolygons.FormDestroy(Sender: TObject);
begin
  Outline.Free;
  Polygon.Free;
end;

procedure TFormPolygons.Draw;
var
  MyFiller: TBitmapPolygonFiller;
begin
  Image.Bitmap.BeginUpdate;
  try
    Image.Bitmap.Clear(clWhite32);
    Image.Bitmap.Draw(50, 50, BitmapList.Bitmap[0]);

    Polygon.Antialiased := Antialiase.Checked;
    Polygon.AntialiasMode := TAntialiasMode(AntialiasMode.ItemIndex);

    if UseOutlinePoly then
    begin
      Outline.Antialiased := Antialiase.Checked;
      Outline.AntialiasMode := TAntialiasMode(AntialiasMode.ItemIndex);
    end;

    if FillMode.ItemIndex = 0 then
      Polygon.FillMode := pfAlternate
    else
      Polygon.FillMode := pfWinding;

    if Pattern.Checked then
    begin
      BitmapList.Bitmap[1].MasterAlpha := FillAlpha.Position;
      BitmapList.Bitmap[1].DrawMode := dmBlend;
      MyFiller := TBitmapPolygonFiller.Create;
      try
        MyFiller.Pattern := BitmapList.Bitmap[1];
        Polygon.DrawFill(Image.Bitmap, MyFiller);
      finally
        MyFiller.Free;
      end;
    end
    else
      Polygon.DrawFill(Image.Bitmap, SetAlpha(clGreen32, FillAlpha.Position));

    if UseOutlinePoly then
      Outline.DrawFill(Image.Bitmap, SetAlpha(clBlack32, LineAlpha.Position))
    else
      Polygon.DrawEdge(Image.Bitmap, SetAlpha(clBlack32, LineAlpha.Position));

  finally
    Image.Bitmap.EndUpdate;
  end;

  Image.Bitmap.Changed;
  Image.Refresh; // force repaint
end;

procedure TFormPolygons.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Button = mbLeft then Polygon.Add(GR32.FixedPoint(X, Y))
  else Polygon.Clear;
  Build;
  Draw;
end;

procedure TFormPolygons.ImageResize(Sender: TObject);
begin
  Image.SetupBitmap;
  Build;
  Draw;
end;

procedure TFormPolygons.ParamsChanged(Sender: TObject);
begin
  AntialiasMode.Enabled := Antialiase.Checked;
  Draw;
end;

procedure TFormPolygons.Button1Click(Sender: TObject);
begin
  Polygon.NewLine;
end;

procedure TFormPolygons.Build;
var
  TmpPoly: TPolygon32;
begin
  Outline.Free;
  Outline := nil;

  if UseOutlinePoly then
  begin
    TmpPoly := Polygon.Outline;
    Outline := TmpPoly.Grow(Fixed(LineSize * 0.5), 0.5);
    Outline.FillMode := pfWinding;
    TmpPoly.Free;
  end;

  if UseOutlinePoly then
    lbOutlineThicknessValue.Caption := Format('(%.1f)', [LineSize])
  else
    lbOutlineThicknessValue.Caption := '(1)';
end;

procedure TFormPolygons.ThicknessChanged(Sender: TObject);
begin
  AntialiasMode.Enabled := Antialiase.Checked;
  UseOutlinePoly := ThickOutline.Checked;
  LineSize := LineThickness.Position * 0.1;
  Build;
  Draw;
end;

{$IFDEF FPC}
initialization
  {$I MainUnit.lrs}
{$ENDIF}

end.
