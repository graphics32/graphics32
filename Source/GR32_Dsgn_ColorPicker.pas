unit GR32_Dsgn_ColorPicker;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Christian-W. Budde
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Classes, SysUtils,
{$IFDEF FPC}
  RTLConsts, LazIDEIntf, PropEdits, Graphics, Dialogs, Forms, Spin, ExtCtrls,
  StdCtrls, Controls,
  {$IFDEF Windows}
    Windows, Registry,
  {$ENDIF}
{$ELSE}
  Consts,
  DesignIntf, DesignEditors, VCLEditors, StdCtrls, Controls,
  Windows, Registry, Graphics, Dialogs, Forms, ExtCtrls, Spin,
{$ENDIF}
  GR32, GR32_ColorPicker, GR32_ColorSwatch;

type
  TFormColorPicker = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ButtonPickFromScreen: TButton;
    CheckBoxWebSafe: TCheckBox;
    ColorPickerAlpha: TColorPickerComponent;
    ColorPickerBlue: TColorPickerComponent;
    ColorPickerGreen: TColorPickerComponent;
    ColorPickerGTK: TColorPickerGTK;
    ColorPickerRed: TColorPickerComponent;
    ColorSwatch: TColorSwatch;
    ColorSwatchAqua: TColorSwatch;
    ColorSwatchBlack: TColorSwatch;
    ColorSwatchBlue: TColorSwatch;
    ColorSwatchFuchsia: TColorSwatch;
    ColorSwatchGreen: TColorSwatch;
    ColorSwatchRed: TColorSwatch;
    ColorSwatchWhite: TColorSwatch;
    ColorSwatchYellow: TColorSwatch;
    EditColor: TEdit;
    LabelAlpha: TLabel;
    LabelBlue: TLabel;
    LabelGreen: TLabel;
    LabelPalette: TLabel;
    LabelPreview: TLabel;
    LabelRed: TLabel;
    LabelWebColor: TLabel;
    PanelControl: TPanel;
    SpinEditAlpha: TSpinEdit;
    SpinEditBlue: TSpinEdit;
    SpinEditGreen: TSpinEdit;
    SpinEditRed: TSpinEdit;
    procedure ButtonPickFromScreenClick(Sender: TObject);
    procedure ColorPickerChanged(Sender: TObject);
    procedure SpinEditColorChange(Sender: TObject);
    procedure CheckBoxWebSafeClick(Sender: TObject);
    procedure EditColorChange(Sender: TObject);
    procedure ColorSwatchClick(Sender: TObject);
  private
    FColor: TColor32;
    FScreenColorPickerForm: TScreenColorPickerForm;
    FLockChanged: integer;

    procedure UpdateColor;
    procedure ScreenColorPickerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure SetColor32(const Value: TColor32);
  public
    property Color: TColor32 read FColor write SetColor32;
  end;

implementation

{$R *.dfm}

{ TFormColorPicker }

procedure TFormColorPicker.ButtonPickFromScreenClick(Sender: TObject);
begin
  FScreenColorPickerForm := TScreenColorPickerForm.Create(Application);
  try
    FScreenColorPickerForm.OnMouseMove := ScreenColorPickerMouseMove;
    if FScreenColorPickerForm.ShowModal = mrOk then
      Color := FScreenColorPickerForm.SelectedColor;
  finally
    FreeAndNil(FScreenColorPickerForm);
  end;
end;

procedure TFormColorPicker.CheckBoxWebSafeClick(Sender: TObject);
begin
  ColorPickerGTK.WebSafe := CheckBoxWebSafe.Checked;
  ColorPickerRed.WebSafe := CheckBoxWebSafe.Checked;
  ColorPickerGreen.WebSafe := CheckBoxWebSafe.Checked;
  ColorPickerBlue.WebSafe := CheckBoxWebSafe.Checked;
  ColorPickerAlpha.WebSafe := CheckBoxWebSafe.Checked;
end;

procedure TFormColorPicker.ColorPickerChanged(Sender: TObject);
begin
  if (FLockChanged > 0) then
    exit;

  Inc(FLockChanged);
  try
    if (Sender = ColorPickerGTK) then
      Color := ColorPickerGTK.SelectedColor or ColorPickerAlpha.SelectedColor
    else
      Color := (ColorPickerRed.SelectedColor and $00FF0000) or
               (ColorPickerGreen.SelectedColor and $0000FF00) or
               (ColorPickerBlue.SelectedColor and $000000FF) or
               (ColorPickerAlpha.SelectedColor and $FF000000);
  finally
    Dec(FLockChanged);
  end;
end;

procedure TFormColorPicker.ColorSwatchClick(Sender: TObject);
begin
  Color := TColorSwatch(Sender).Color;
end;

procedure TFormColorPicker.EditColorChange(Sender: TObject);
var
  ColorText: string;
  Value: Integer;
begin
  if (FLockChanged > 0) then
    exit;

  Inc(FLockChanged);
  try
    ColorText := StringReplace(EditColor.Text, '#', '$', []);

    if TryStrToInt(ColorText, Value) then
      Color := Value;

  finally
    Dec(FLockChanged);
  end;
end;

procedure TFormColorPicker.ScreenColorPickerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Color := FScreenColorPickerForm.SelectedColor;
end;

procedure TFormColorPicker.SetColor32(const Value: TColor32);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    UpdateColor;
  end;
end;

procedure TFormColorPicker.SpinEditColorChange(Sender: TObject);
begin
  if (FLockChanged > 0) then
    exit;

  Inc(FLockChanged);
  try
    Color :=
      SpinEditAlpha.Value shl 24 +
      SpinEditRed.Value   shl 16 +
      SpinEditGreen.Value shl 8 +
      SpinEditBlue.Value;

  finally
    Dec(FLockChanged);
  end;
end;

procedure TFormColorPicker.UpdateColor;
var
  R, G, B, A: Byte;
  SelStart: Integer;
begin
  // disable OnChange handler
  Inc(FLockChanged);
  try

    // update spin edits
    Color32ToRGBA(FColor, R, G, B, A);
    SpinEditRed.Value := R;
    SpinEditGreen.Value := G;
    SpinEditBlue.Value := B;
    SpinEditAlpha.Value := A;

    // update color edit
    SelStart := EditColor.SelStart;
    EditColor.Text := '#' + IntToHex(FColor, 8);
    EditColor.SelStart := SelStart;

    ColorPickerRed.SelectedColor := FColor and $00FF0000;
    ColorPickerGreen.SelectedColor := FColor and $0000FF00;
    ColorPickerBlue.SelectedColor := FColor and $000000FF;
    ColorPickerAlpha.SelectedColor := FColor and $FF000000;
    ColorPickerGTK.SelectedColor := FColor;
    ColorSwatch.Color := FColor;


  finally
    // re-enable OnChange handler
    Dec(FLockChanged);
  end;
end;

end.

