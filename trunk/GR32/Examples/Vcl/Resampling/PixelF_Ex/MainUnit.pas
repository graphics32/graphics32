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
 * The Original Code is PixelF_Ex
 *
 * The Initial Developer of the Original Code is
 * Michael Hansen
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GR32, GR32_Lowlevel, GR32_Image, StdCtrls, GR32_RangeBars,
  ExtCtrls, Math;

type
  TMainForm = class(TForm)
    Image32: TImage32;
    PnlSettings: TPanel;
    Label3: TLabel;
    Panel4: TPanel;
    gbTwist: TGaugeBar;
    rbGetPixelFS: TRadioButton;
    rbPixelS: TRadioButton;
    procedure Image32PaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure FormCreate(Sender: TObject);
    procedure gbTwistChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    Src: TBitmap32;
    procedure TwirlDistortion(Dst, Src: TBitmap32; const Value: Integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.TwirlDistortion(Dst, Src: TBitmap32; const Value: Integer);
{twirl algoritm inspired by Patrick Quinn´s remap demo}
var
  X, Y, DstR, DstB: Integer;
  r, rx, ry, t, tt, v: Single;
begin
  rx := Src.Width / 2;
  ry := Src.Height / 2;
  v := -Value / 5 / Src.Height;
  DstR := Dst.Width - 1;
  DstB := Dst.Height - 1;

  if rbGetPixelFS.Checked then
   for Y := 0 to DstB do
    for X := 0 to DstR do begin
      r := Hypot(X - rx, Y - ry);
      t := ArcTan2(Y - ry, X - rx);
      tt := t + r * v;
      Dst.Pixel[X, Y] := Src.PixelFS[ rx + r * Cos(tt),
                                      ry + r * Sin(tt) ];
    end
  else if rbPixelS.Checked then
   for Y := 0 to DstB do
    for X := 0 to DstR do begin
      r := Hypot(X - rx, Y - ry);
      t := ArcTan2(Y - ry, X - rx);
      tt := t + r * v;
      Dst.Pixel[X, Y] := Src.PixelS[ Round(rx + r * Cos(tt)),
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

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  with Image32 do
  begin
    if PaintStages[0].Stage = PST_CLEAR_BACKGND then PaintStages[0].Stage := PST_CUSTOM;
    PaintStages.Add.Stage := PST_CUSTOM;
  end;

  Src := TBitmap32.Create;
  with Src do begin //Making distorted borders look better
   Assign(Image32.Bitmap);
   for i:= 0 to Width - 1 do begin
    Pixel[i, 0] := Pixel[i, 0] and $00FFFFFF;
    Pixel[i, Height - 1] := Pixel[i, 0] and $00FFFFFF;
   end;
   for i:= 0 to Height - 1 do begin
    Pixel[0, i] := Pixel[i, 0] and $00FFFFFF or $7F000000;
    Pixel[Width - 1, i] := Pixel[i, 0] and $00FFFFFF;
   end;
   OuterColor := $00000000;
  end;
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

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Src.Free;
end;

end.
