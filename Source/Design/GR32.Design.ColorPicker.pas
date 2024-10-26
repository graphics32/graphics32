unit GR32.Design.ColorPicker;

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
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  Classes, SysUtils,
{$IFDEF FPC}
  RTLConsts, LazIDEIntf, PropEdits, Graphics, Dialogs, Forms, Spin, ExtCtrls,
  StdCtrls, Controls,
  {$ifdef MSWINDOWS}
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
    PanelColorPickerMain: TPanel;
    EditColor: TEdit;
    LabelAlpha: TLabel;
    LabelBlue: TLabel;
    LabelGreen: TLabel;
    LabelPreview: TLabel;
    LabelRed: TLabel;
    LabelWebColor: TLabel;
    PanelControl: TPanel;
    SpinEditAlpha: TSpinEdit;
    SpinEditBlue: TSpinEdit;
    SpinEditGreen: TSpinEdit;
    SpinEditRed: TSpinEdit;
    PanelPreview: TPanel;
    PanelSwatches: TPanel;
    procedure ButtonPickFromScreenClick(Sender: TObject);
    procedure ColorPickerChanged(Sender: TObject);
    procedure SpinEditColorChange(Sender: TObject);
    procedure CheckBoxWebSafeClick(Sender: TObject);
    procedure EditColorChange(Sender: TObject);
    procedure ColorSwatchClick(Sender: TObject);
  private
    FColor: TColor32;
    FScreenColorPickerForm: TScreenColorPickerForm;
    FColorPickerAlpha: TColorPickerComponent;
    FColorPickerBlue: TColorPickerComponent;
    FColorPickerGreen: TColorPickerComponent;
    FColorPickerRed: TColorPickerComponent;
    FColorSwatch: TColorSwatch;
    FColorSwatchOpaque: TColorSwatch;
    FColorPickerGTK: TColorPickerGTK;
    FLockChanged: integer;

    procedure UpdateColor;
    procedure ScreenColorPickerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure SetColor32(const Value: TColor32);
  public
    constructor Create(AOwner: TComponent); override;

    function Execute: boolean;

    property Color: TColor32 read FColor write SetColor32;
  end;

implementation

{$R *.dfm}

{ TFormColorPicker }

constructor TFormColorPicker.Create(AOwner: TComponent);

  function CreateColorPickerComponent(ColorComponent: TColorComponent; ALabel: TLabel; AEdit: TControl): TColorPickerComponent;
  begin
    Result := TColorPickerComponent.Create(Self);
    Result.Left := ALabel.Left + ALabel.Width + 1;
    Result.Top := AEdit.Top;
    Result.Height := AEdit.Height;
    Result.Width := AEdit.Left - Result.Left - 8;

    Result.Cursor := crHandPoint;
    Result.Border := True;
    Result.ColorComponent := ColorComponent;
    Result.OnChanged := ColorPickerChanged;

    Result.Parent := Self;

    // Resize the label now that we don't need its width to align with anymore
    ALabel.AutoSize := True;
  end;

const
  SwatchColors: array[0..7] of TColor32 =
    (clBlack32, clWhite32, clRed32, clLime32, clBlue32, clYellow32, clFuchsia32, clAqua32);

var
  SwatchColor: TColor32;
  Swatch: TColorSwatch;
  PanelSpace: integer;
  NextPos: integer;
begin
  inherited;

  // Create Graphics32 controls at run-time so we don't need to
  // have the design-time package installed before the form can
  // be opened.
  // This is only really done to avoid users messing up the form
  // if they open it before the package has been installed.

  FColorPickerRed := CreateColorPickerComponent(ccRed, LabelRed, SpinEditRed);
  FColorPickerGreen := CreateColorPickerComponent(ccGreen, LabelGreen, SpinEditGreen);
  FColorPickerBlue := CreateColorPickerComponent(ccBlue, LabelBlue, SpinEditBlue);
  FColorPickerAlpha := CreateColorPickerComponent(ccAlpha, LabelAlpha, SpinEditAlpha);

  FColorSwatch := TColorSwatch.Create(Self);
  FColorSwatch.Border := False;
  FColorSwatch.Width := (PanelPreview.Width-4) div 2;
  FColorSwatch.Align := alLeft;
  FColorSwatch.Parent := PanelPreview;

  FColorSwatchOpaque := TColorSwatch.Create(Self);
  FColorSwatchOpaque.Border := False;
  FColorSwatchOpaque.Width := (PanelPreview.Width-4) div 2;
  FColorSwatchOpaque.Align := alClient;
  FColorSwatchOpaque.Parent := PanelPreview;

  // Note: Swatch.Width = Swatch.Height = PanelSwatches.Height
  PanelSpace := PanelSwatches.Height + (PanelSwatches.Width - Length(SwatchColors) * PanelSwatches.Height) div (Length(SwatchColors)-1);
  NextPos := 0;

  for SwatchColor in SwatchColors do
  begin
    Swatch := TColorSwatch.Create(Self);

    Swatch.Cursor := crHandPoint;
    Swatch.Border := True;
    Swatch.Color := SwatchColor;
    Swatch.OnClick := ColorSwatchClick;

    Swatch.Height := PanelSwatches.Height;
    Swatch.Width := Swatch.Height;
    Swatch.Left := NextPos;
    Swatch.Parent := PanelSwatches;

    Inc(NextPos, PanelSpace);
  end;

  FColorPickerGTK := TColorPickerGTK.Create(Self);
  FColorPickerGTK.Align := alClient;
  FColorPickerGTK.Parent := PanelColorPickerMain;
  FColorPickerGTK.Cursor := crHandPoint;
  FColorPickerGTK.OnChanged := ColorPickerChanged;
end;

procedure TFormColorPicker.ButtonPickFromScreenClick(Sender: TObject);
var
  SaveBounds: TRect;
begin
  Invalidate;

  SaveBounds := BoundsRect;

  FScreenColorPickerForm := TScreenColorPickerForm.Create(nil);
  try
    FScreenColorPickerForm.OnMouseMove := ScreenColorPickerMouseMove;

    if FScreenColorPickerForm.Execute then
      Color := FScreenColorPickerForm.SelectedColor;

  finally
    FreeAndNil(FScreenColorPickerForm);
  end;

  BoundsRect := SaveBounds;
end;

procedure TFormColorPicker.CheckBoxWebSafeClick(Sender: TObject);
begin
  FColorPickerGTK.WebSafe := CheckBoxWebSafe.Checked;
  FColorPickerRed.WebSafe := CheckBoxWebSafe.Checked;
  FColorPickerGreen.WebSafe := CheckBoxWebSafe.Checked;
  FColorPickerBlue.WebSafe := CheckBoxWebSafe.Checked;
  FColorPickerAlpha.WebSafe := CheckBoxWebSafe.Checked;
end;

procedure TFormColorPicker.ColorPickerChanged(Sender: TObject);
begin
  if (FLockChanged > 0) then
    exit;

  Inc(FLockChanged);
  try
    if (Sender = FColorPickerGTK) then
      Color := SetAlpha(FColorPickerGTK.SelectedColor, TColor32Entry(FColorPickerAlpha.SelectedColor).A)
    else
      Color := Color32(
        TColor32Entry(FColorPickerRed.SelectedColor).R,
        TColor32Entry(FColorPickerGreen.SelectedColor).G,
        TColor32Entry(FColorPickerBlue.SelectedColor).B,
        TColor32Entry(FColorPickerAlpha.SelectedColor).A);
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

function TFormColorPicker.Execute: boolean;
begin
  Result := (ShowModal = mrOK);
end;

procedure TFormColorPicker.ScreenColorPickerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  FormCenter: TPoint;

const
  DMZ = 20;
  MoveSize = 80;

  procedure MoveHorizontally;
  begin
    if (FormCenter.X > X) then
    begin
      // We are to the right. Can we move more toward the right?
      if (BoundsRect.Right + MoveSize < Monitor.BoundsRect.Right) then
        Left := Left + MoveSize
      else
        // Move left of center instead
        Left := Monitor.BoundsRect.CenterPoint.X - Width;
    end else
    begin
      // We are to the left. Can we move more toward the left?
      if (BoundsRect.Left - MoveSize > Monitor.BoundsRect.Left) then
        Left := Left - MoveSize
      else
        // Move right of center instead
        Left := Monitor.BoundsRect.CenterPoint.X;
    end;
  end;

  procedure MoveVertically;
  begin
    if (FormCenter.Y > Y) then
    begin
      // We are at the bottom. Can we move more toward the bottom?
      if (BoundsRect.Bottom + MoveSize < Monitor.BoundsRect.Bottom) then
        Top := Top + MoveSize
      else
        // Move above center instead
        Top := Monitor.BoundsRect.CenterPoint.Y - Height;
    end else
    begin
      // We are to the top. Can we move more toward the top?
      if (BoundsRect.Top - MoveSize > Monitor.BoundsRect.Top) then
        Top := Top - MoveSize
      else
        // Move below center instead
        Top := Monitor.BoundsRect.CenterPoint.Y;
    end;
  end;

var
  r: TRect;
  Collision: boolean;
begin
  // Move ourself if we are getting in the way of the screen color picker
  r := BoundsRect;
  InflateRect(r, DMZ, DMZ);
  if (PtInRect(r, Point(X, Y))) then
  begin
    FormCenter := BoundsRect.CenterPoint;

    // Horizontal collision?
    Collision := (Abs(FormCenter.X - X) - Width <= DMZ);

    if (Collision) and (Y >= BoundsRect.Top) and (Y <= BoundsRect.Bottom) then
    begin
      MoveHorizontally;
      FormCenter := BoundsRect.CenterPoint;
    end;

    // Vertical collision?
    Collision :=  (Abs(FormCenter.Y - Y) - Height <= DMZ);

    if (Collision) and (X >= BoundsRect.Left) and (X <= BoundsRect.Right) then
      MoveVertically;
  end;

  Color := FScreenColorPickerForm.SelectedColor;

  Update;
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

    Color := Color32(SpinEditRed.Value, SpinEditGreen.Value, SpinEditBlue.Value, SpinEditAlpha.Value);

  finally
    Dec(FLockChanged);
  end;
end;

procedure TFormColorPicker.UpdateColor;
var
  SelStart: Integer;
begin
  // disable OnChange handler
  Inc(FLockChanged);
  try

    // update spin edits
    SpinEditRed.Value := TColor32Entry(FColor).R;
    SpinEditGreen.Value := TColor32Entry(FColor).G;
    SpinEditBlue.Value := TColor32Entry(FColor).B;
    SpinEditAlpha.Value := TColor32Entry(FColor).A;

    // update color edit
    SelStart := EditColor.SelStart;
    EditColor.Text := '$' + IntToHex(FColor, 8);
    EditColor.SelStart := SelStart;

    FColorPickerRed.SelectedColor := Color32(TColor32Entry(FColor).R, 0, 0);
    FColorPickerGreen.SelectedColor := Color32(0, TColor32Entry(FColor).G, 0);
    FColorPickerBlue.SelectedColor := Color32(0, 0, TColor32Entry(FColor).B);
    FColorPickerAlpha.SelectedColor := SetAlpha(clWhite32, TColor32Entry(FColor).A);
    FColorPickerGTK.SelectedColor := FColor;
    FColorSwatch.Color := FColor;
    FColorSwatchOpaque.Color := SetAlpha(FColor, 255);

  finally
    // re-enable OnChange handler
    Dec(FLockChanged);
  end;
end;

end.

