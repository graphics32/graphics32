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
  TNewImageForm = class(TForm)
    btCancel: TButton;
    btOK: TButton;
    btSelect: TButton;
    btUpDownHeight: TUpDown;
    btUpDownWidth: TUpDown;
    ColorDialog: TColorDialog;
    ImageHeight: TEdit;
    ImageWidth: TEdit;
    lbBackgroundColor: TLabel;
    lbHeight: TLabel;
    lbHeightUnit: TLabel;
    lbWidth: TLabel;
    lbWidthUnit: TLabel;
    pnColor: TPanel;
    procedure btSelectClick(Sender: TObject);
  end;

var
  NewImageForm: TNewImageForm;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TNewImageForm }

procedure TNewImageForm.btSelectClick(Sender: TObject);
begin
  with ColorDialog do
  begin
    Color := pnColor.Color;
    if Execute then pnColor.Color := Color;
  end;
end;

end.
