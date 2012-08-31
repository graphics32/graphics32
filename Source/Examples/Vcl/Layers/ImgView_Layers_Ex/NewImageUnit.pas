unit NewImageUnit;

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

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$IFNDEF FPC}
  {$DEFINE Windows}
{$ENDIF}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons;

type
  TFrmNewImage = class(TForm)
    BtnCancel: TButton;
    BtnOK: TButton;
    BtnSelect: TButton;
    BtnUpDownHeight: TUpDown;
    BtnUpDownWidth: TUpDown;
    ColorDialog: TColorDialog;
    EdtImageHeight: TEdit;
    EdtImageWidth: TEdit;
    LblBackgroundColor: TLabel;
    LblHeight: TLabel;
    LblHeightUnit: TLabel;
    LblWidth: TLabel;
    LblWidthUnit: TLabel;
    PnlColor: TPanel;
    procedure BtnSelectClick(Sender: TObject);
  end;

var
  FrmNewImage: TFrmNewImage;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TNewImageForm }

procedure TFrmNewImage.BtnSelectClick(Sender: TObject);
begin
  with ColorDialog do
  begin
    Color := PnlColor.Color;
    if Execute then PnlColor.Color := Color;
  end;
end;

end.
