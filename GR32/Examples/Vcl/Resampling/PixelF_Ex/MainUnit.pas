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
 * The Original Code is PixelF Example
 *
 * The Initial Developer of the Original Code is
 * Michael Hansen
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LResources, Variants,{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Math, GR32, GR32_Lowlevel, GR32_Image, GR32_RangeBars, GR32_Transforms;

type
  { TMainForm }
  TMainForm = class(TForm)
    Image32: TImage32;
    gbTwist: TGaugeBar;
    PnlSettings: TPanel;
    Label3: TLabel;
    Panel4: TPanel;
    rbGetPixelFS: TRadioButton;
    rbPixelS: TRadioButton;
    procedure FormDestroy(Sender: TObject);
    procedure Image32PaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure FormCreate(Sender: TObject);
    procedure gbTwistChange(Sender: TObject);
  private
  public
    Src: TBitmap32;
    procedure TwirlDistortion(Dst, Srcb: TBitmap32; const Value: Integer);
  end;

var
  MainForm: TMainForm;

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

procedure TMainForm.FormCreate(Sender: TObject);
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

  // load example image
  Assert(FileExists(pathMedia + 'stones.jpg'));
  Image32.Bitmap.LoadFromFile(pathMedia + 'stones.jpg');

  with Image32 do
  begin
    if PaintStages[0]^.Stage = PST_CLEAR_BACKGND then PaintStages[0]^.Stage := PST_CUSTOM;
    PaintStages.Add^.Stage := PST_CUSTOM;
  end;
  Image32.BufferOversize := 0;
  Src := TBitmap32.Create;
  with Src do
  begin
    SetBorderTransparent(Src, BoundsRect);
    Assign(Image32.Bitmap);
    OuterColor := $00000000;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
 Src.Free;
end;

procedure TMainForm.TwirlDistortion(Dst, Srcb: TBitmap32; const Value: Integer);
{twirl algoritm inspired by Patrick Quinn´s remap demo}
var
  X, Y, DstR, DstB: Integer;
  r, rx, ry, t, tt, v: Single;
begin
  rx := Srcb.Width / 2;
  ry := Srcb.Height / 2;
  v := -Value / 5 / Srcb.Height;
  DstR := Dst.Width - 1;
  DstB := Dst.Height - 1;

  if rbGetPixelFS.Checked then
   for Y := 0 to DstB do
    for X := 0 to DstR do begin
      r := Hypot(X - rx, Y - ry);
      t := ArcTan2(Y - ry, X - rx);
      tt := t + r * v;
      Dst.Pixel[X, Y] := Srcb.PixelFS[ rx + r * Cos(tt),
                                      ry + r * Sin(tt) ];
    end
  else if rbPixelS.Checked then
   for Y := 0 to DstB do
    for X := 0 to DstR do begin
      r := Hypot(X - rx, Y - ry);
      t := ArcTan2(Y - ry, X - rx);
      tt := t + r * v;
      Dst.Pixel[X, Y] := Srcb.PixelS[ Round(rx + r * Cos(tt)),
                                      Round(ry + r * Sin(tt)) ];
    end;
end;

procedure TMainForm.Image32PaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
const
  Colors: array [0..1] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  W, I, J, Parity: Integer;
  Line1, Line2: TArrayOfColor32; // a buffer for a couple of scanlines
begin
  with Image32.Buffer do
    if StageNum = 0 then
    begin
      W := Width;
      SetLength(Line1, W);
      SetLength(Line2, W);
      for I := 0 to W - 1 do
      begin
        Parity := I shr 3 and $1;
        Line1[I] := Colors[Parity];
        Line2[I] := Colors[1 - Parity];
      end;
      for J := 0 to Height - 1 do
      begin
        Parity := J shr 3 and $1;
        if Boolean(Parity) then MoveLongword(Line1[0], ScanLine[J]^, W)
        else MoveLongword(Line2[0], ScanLine[J]^, W);
      end;
    end
    else
      FrameRectS(BoundsRect , $FF000000);
end;

procedure TMainForm.gbTwistChange(Sender: TObject);
begin
 with Image32 do
  begin
   TwirlDistortion(Bitmap, Src, gbTwist.Position);
   gbTwist.Repaint;
   Repaint;
  end;
end;

{$IFDEF FPC}
initialization
  {$I MainUnit.lrs}
{$ENDIF}

end.
