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

{$MODE Delphi}

uses
  LResources, Variants,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, GR32, GR32_Lowlevel,
  GR32_Image, StdCtrls, GR32_RangeBars, ExtCtrls, Math, GR32_Transforms;

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

uses
{$IFDEF Darwin}
  FPCMacOSAll,
{$ENDIF}
  LazJPG;

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
  gbTwist := TGaugeBar.Create(PnlSettings);
  with gbTwist do
  begin
    Parent := PnlSettings;
    Left := 8;
    Height := 12;
    Top := 48;
    Width := 129;
    Color := clScrollBar;
    HandleSize := 16;
    Max := 50;
    Min := -50;
    ShowArrows := False;
    ShowHandleGrip := True;
    Style := rbsMac;
    OnChange := gbTwistChange;
  end;

  Image32 := TImage32.Create(Self);
  with Image32 do
  begin
    Parent := Self;
    Left := 8;
    Height := 244;
    Top := 8;
    Width := 300;
    Anchors := [akTop, akLeft, akRight, akBottom];
    AutoSize := True;
    Bitmap.DrawMode := dmBlend;
    Bitmap.ResamplerClassName := 'TNearestResampler';
    Bitmap.OnChange := nil;
    Bitmap.OnResize := nil;
    BitmapAlign := baCenter;
    Scale := 1;
    TabOrder := 0;
    OnPaintStage := Image32PaintStage;
    Bitmap.LoadFromFile(pathMedia + 'stones.jpg');
    if PaintStages[0]^.Stage = PST_CLEAR_BACKGND then PaintStages[0]^.Stage := PST_CUSTOM;
    PaintStages.Add^.Stage := PST_CUSTOM;
    BufferOversize := 0;
  end;

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

initialization
  {$I MainUnit.lrs}

end.
