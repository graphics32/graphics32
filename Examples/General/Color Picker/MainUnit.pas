unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ComCtrls, GR32, GR32_ColorPicker;

type
  TFormMain = class(TForm)
    ButtonFromScreen: TButton;
    ColorPickerGTK: TColorPickerGTK;
    ColorPickerHSV: TColorPickerHSV;
    EditColor: TEdit;
    LabelBlue: TLabel;
    LabelColor: TLabel;
    LabelGreen: TLabel;
    LabelRed: TLabel;
    LabelWebColor: TLabel;
    PageControlColorPicker: TPageControl;
    SpinEditBlue: TSpinEdit;
    SpinEditGreen: TSpinEdit;
    SpinEditRed: TSpinEdit;
    TabColorPickerGTK: TTabSheet;
    TabColorPickerHSV: TTabSheet;
    procedure ButtonFromScreenClick(Sender: TObject);
    procedure ColorPickerGTKChanged(Sender: TObject);
    procedure ColorPickerHSVChanged(Sender: TObject);
    procedure EditColorChange(Sender: TObject);
    procedure EditColorKeyPress(Sender: TObject; var Key: Char);
    procedure SpinEditColorChange(Sender: TObject);
  private
    FScreenColorPickerForm: TScreenColorPickerForm;
    procedure UpdateColor(Color: TColor32);
    procedure ScreenColorPickerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  public
    { Public-Deklarationen }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{ TFormMain }

procedure TFormMain.ButtonFromScreenClick(Sender: TObject);
begin
  FScreenColorPickerForm := TScreenColorPickerForm.Create(Application);
  try
    FScreenColorPickerForm.OnMouseMove := ScreenColorPickerMouseMove;
    if FScreenColorPickerForm.ShowModal = mrOk then
      UpdateColor(FScreenColorPickerForm.SelectedColor);
  finally
    FreeAndNil(FScreenColorPickerForm);
  end;
end;

procedure TFormMain.ColorPickerGTKChanged(Sender: TObject);
begin
  UpdateColor(ColorPickerGTK.SelectedColor);
end;

procedure TFormMain.ColorPickerHSVChanged(Sender: TObject);
begin
  UpdateColor(ColorPickerHSV.SelectedColor);
end;

procedure TFormMain.EditColorChange(Sender: TObject);
var
  Value: Integer;
begin
  if TryStrToInt(EditColor.Text, Value) then
    UpdateColor(Value);
end;

procedure TFormMain.EditColorKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['$', '0'..'9', 'a'..'f', 'A'..'F']) then
    Key := #0;
end;

procedure TFormMain.ScreenColorPickerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateColor(FScreenColorPickerForm.SelectedColor);
end;

procedure TFormMain.SpinEditColorChange(Sender: TObject);
begin
  EditColor.OnChange := nil;
  EditColor.Text := '#' +
    IntToHex(SpinEditRed.Value, 2) +
    IntToHex(SpinEditGreen.Value, 2) +
    IntToHex(SpinEditBlue.Value, 2);
  EditColor.OnChange := EditColorChange;
end;

procedure TFormMain.UpdateColor(Color: TColor32);
var
  R, G, B: Byte;
begin
  EditColor.OnChange := nil;
  SpinEditRed.OnChange := nil;
  SpinEditGreen.OnChange := nil;
  SpinEditBlue.OnChange := nil;

  Color32ToRGB(Color, R, G, B);
  ColorPickerGTK.SelectedColor := Color;
  ColorPickerHSV.SelectedColor := Color;

  SpinEditRed.Value := R;
  SpinEditGreen.Value := G;
  SpinEditBlue.Value := B;

  EditColor.Text := '$FF' + IntToHex(R, 2) + IntToHex(G, 2) + IntToHex(B, 2);
  EditColor.Color := WinColor(Color);
  if Intensity(Color) < 128 then
    EditColor.Font.Color := clWhite
  else
    EditColor.Font.Color := clBlack;

  SpinEditRed.OnChange := SpinEditColorChange;
  SpinEditGreen.OnChange := SpinEditColorChange;
  SpinEditBlue.OnChange := SpinEditColorChange;
  EditColor.OnChange := EditColorChange;
end;

end.

