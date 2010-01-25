unit RGBALoaderUnit;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$MODE Delphi}

uses
  LCLIntf, LResources,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  GR32_Image, GR32_Filters, GR32_RangeBars, ExtCtrls, ExtDlgs, Buttons;

type

  { TRGBALoaderForm }

  TRGBALoaderForm = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    ImgRGB: TImgView32;
    Button1: TButton;
    Label3: TLabel;
    ImgAlpha: TImgView32;
    Button2: TButton;
    Label4: TLabel;
    Button3: TButton;
    Button4: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RGBALoaderForm: TRGBALoaderForm;

implementation

procedure TRGBALoaderForm.Button1Click(Sender: TObject);
begin
  with OpenPictureDialog do
    if Execute then ImgRGB.Bitmap.LoadFromFile(FileName);
end;

procedure TRGBALoaderForm.Button2Click(Sender: TObject);
begin
  with OpenPictureDialog, ImgAlpha do
    if Execute then
    begin
      Bitmap.LoadFromFile(FileName);
      ColorToGrayscale(Bitmap, Bitmap);
    end;
end;

procedure TRGBALoaderForm.FormCreate(Sender: TObject);
begin
  ImgRGB := TImgView32.Create(Self);
  with ImgRGB do
  begin
    Parent := Self;
    Left := 24;
    Top := 112;
    Width := 169;
    Height := 169;
    Bitmap.ResamplerClassName := 'TNearestResampler';
    Color := clAppWorkSpace;
    ParentColor := False;
    Scale := 1;
    ScrollBars.Color := clBtnShadow;
    ScrollBars.ShowHandleGrip := True;
    ScrollBars.Style := rbsMac;
    OverSize := 0;
    TabOrder := 1;
  end;

  ImgAlpha := TImgView32.Create(Self);
  with ImgAlpha do
  begin
    Parent := Self;
    Left := 216;
    Top := 112;
    Width := 169;
    Height := 169;
    Bitmap.ResamplerClassName := 'TNearestResampler';
    Color := clAppWorkSpace;
    ParentColor := False;
    Scale := 1;
    ScrollBars.Color := clBtnShadow;
    ScrollBars.ShowHandleGrip := True;
    ScrollBars.Style := rbsDefault;
    OverSize := 0;
    TabOrder := 3;
  end;
end;

procedure TRGBALoaderForm.SpeedButton1Click(Sender: TObject);
begin
  ImgRGB.Scale := ImgRGB.Scale * 1.5;
end;

procedure TRGBALoaderForm.SpeedButton2Click(Sender: TObject);
begin
  ImgRGB.Scale := ImgRGB.Scale / 1.5;
end;

procedure TRGBALoaderForm.SpeedButton3Click(Sender: TObject);
begin
  ImgAlpha.Scale := ImgAlpha.Scale * 1.5;
end;

procedure TRGBALoaderForm.SpeedButton4Click(Sender: TObject);
begin
  ImgAlpha.Scale := ImgAlpha.Scale / 1.5;
end;

procedure TRGBALoaderForm.Button5Click(Sender: TObject);
begin
  ImgRGB.Scale := 1;
  ImgAlpha.Scale := 1;
end;

initialization
  {$I RGBALoaderUnit.lrs}

end.
