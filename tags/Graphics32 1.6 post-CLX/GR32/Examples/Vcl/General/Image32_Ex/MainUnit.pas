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

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GR32, GR32_Image, GR32_RangeBars;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image: TImage32;
    rgScaleMode: TRadioGroup;
    rgStretchFilter: TRadioGroup;
    rgBitmapAlign: TRadioGroup;
    StaticText1: TStaticText;
    sbScale: TGaugeBar;
    procedure rgBitmapAlignClick(Sender: TObject);
    procedure sbScaleChange(Sender: TObject);
    procedure rgScaleModeClick(Sender: TObject);
    procedure rgStretchFilterClick(Sender: TObject);
  public
    Time: Single;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.rgBitmapAlignClick(Sender: TObject);
const
  BA_CONSTS: array [0..2] of TBitmapAlign = (baTopLeft, baCenter, baTile);
begin
  Image.BitmapAlign := BA_CONSTS[rgBitmapAlign.ItemIndex];
end;

procedure TForm1.sbScaleChange(Sender: TObject);
begin
  sbScale.Update;
  Image.Scale := sbScale.Position / 100;
end;

procedure TForm1.rgScaleModeClick(Sender: TObject);
const
  SM_CONSTS: array [0..3] of TScaleMode = (smNormal, smStretch, smScale, smResize);
begin
  Image.ScaleMode := SM_CONSTS[rgScaleMode.ItemIndex];
  sbScale.Enabled := rgScaleMode.ItemIndex = 2;
  StaticText1.Enabled := rgScaleMode.ItemIndex = 2;
end;

procedure TForm1.rgStretchFilterClick(Sender: TObject);
const
  SF_CONSTS: array [0..4] of TStretchFilter =
    (sfNearest, sfLinear, sfSpline, sfLanczos, sfMitchell);
begin
  Image.Bitmap.StretchFilter := SF_CONSTS[rgStretchFilter.ItemIndex];
end;

end.
