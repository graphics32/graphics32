unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ComCtrls, ExtCtrls, GR32, GR32_ColorPicker, GR32_ColorSwatch;

type
  TFormMain = class(TForm)
    ButtonFromScreen: TButton;
    ColorPickerAlpha: TColorPickerComponent;
    ColorPickerBlue: TColorPickerComponent;
    ColorPickerGreen: TColorPickerComponent;
    ColorPickerGTK: TColorPickerGTK;
    ColorPickerHSV: TColorPickerHSV;
    ColorPickerRed: TColorPickerComponent;
    ColorPickerRGBA: TColorPickerRGBA;
    EditColor: TEdit;
    LabelAlpha: TLabel;
    LabelBlue: TLabel;
    LabelColor: TLabel;
    LabelComponentAlpha: TLabel;
    LabelComponentBlue: TLabel;
    LabelComponentGreen: TLabel;
    LabelComponentRed: TLabel;
    LabelGreen: TLabel;
    LabelRed: TLabel;
    LabelWebColor: TLabel;
    PageControlColorPicker: TPageControl;
    SpinEditAlpha: TSpinEdit;
    SpinEditBlue: TSpinEdit;
    SpinEditGreen: TSpinEdit;
    SpinEditRed: TSpinEdit;
    TabColorPickerGTK: TTabSheet;
    TabColorPickerHSV: TTabSheet;
    TabColorPickerRGBA: TTabSheet;
    TabSheetComponents: TTabSheet;
    ColorSwatch: TColorSwatch;
    procedure FormCreate(Sender: TObject);
    procedure ButtonFromScreenClick(Sender: TObject);
    procedure ColorPickerGTKChanged(Sender: TObject);
    procedure ColorPickerHSVChanged(Sender: TObject);
    procedure ColorPickerRGBAChanged(Sender: TObject);
    procedure EditColorChange(Sender: TObject);
    procedure EditColorKeyPress(Sender: TObject; var Key: Char);
    procedure SpinEditColorChange(Sender: TObject);
    procedure ColorPickerRedChanged(Sender: TObject);
    procedure ColorPickerGreenChanged(Sender: TObject);
    procedure ColorPickerBlueChanged(Sender: TObject);
    procedure ColorPickerAlphaChanged(Sender: TObject);
  private
    FColor: TColor32;
    FScreenColorPickerForm: TScreenColorPickerForm;
    procedure UpdateColor;
    procedure ScreenColorPickerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SetColor(const Value: TColor32);
  public
    property Color: TColor32 read FColor write SetColor;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Color := SetAlpha(clSalmon32, 200);
end;

procedure TFormMain.ButtonFromScreenClick(Sender: TObject);
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

procedure TFormMain.ColorPickerRedChanged(Sender: TObject);
begin
  Color := ColorPickerRed.SelectedColor;
end;

procedure TFormMain.ColorPickerGreenChanged(Sender: TObject);
begin
  Color := ColorPickerGreen.SelectedColor;
end;

procedure TFormMain.ColorPickerBlueChanged(Sender: TObject);
begin
  Color := ColorPickerBlue.SelectedColor;
end;

procedure TFormMain.ColorPickerAlphaChanged(Sender: TObject);
begin
  Color := ColorPickerAlpha.SelectedColor;
end;

procedure TFormMain.ColorPickerGTKChanged(Sender: TObject);
begin
  Color := ColorPickerGTK.SelectedColor;
end;

procedure TFormMain.ColorPickerHSVChanged(Sender: TObject);
begin
  Color := ColorPickerHSV.SelectedColor;
end;

procedure TFormMain.ColorPickerRGBAChanged(Sender: TObject);
begin
  Color := ColorPickerRGBA.SelectedColor;
end;

procedure TFormMain.SetColor(const Value: TColor32);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    UpdateColor;
  end;
end;

procedure TFormMain.EditColorChange(Sender: TObject);
var
  ColorText: string;
  Value: Integer;
begin
  ColorText := StringReplace(EditColor.Text, '#', '$', []);
  if TryStrToInt(ColorText, Value) then
    Color := Value;
end;

procedure TFormMain.EditColorKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['$', '0'..'9', 'a'..'f', 'A'..'F', #8]) then
    Key := #0;
end;

procedure TFormMain.ScreenColorPickerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Color := FScreenColorPickerForm.SelectedColor;
end;

procedure TFormMain.SpinEditColorChange(Sender: TObject);
begin
  EditColor.OnChange := nil;
  Color :=
    SpinEditAlpha.Value shl 24 +
    SpinEditRed.Value shl 16 +
    SpinEditGreen.Value shl 8 +
    SpinEditBlue.Value;
  EditColor.OnChange := EditColorChange;
end;

procedure TFormMain.UpdateColor;
var
  R, G, B, A: Byte;
  SelStart: Integer;
begin
  // disable OnChange handler
  EditColor.OnChange := nil;
  SpinEditRed.OnChange := nil;
  SpinEditGreen.OnChange := nil;
  SpinEditBlue.OnChange := nil;
  SpinEditAlpha.OnChange := nil;

  ColorPickerGTK.SelectedColor := FColor;
  ColorPickerHSV.SelectedColor := FColor;
  ColorPickerRGBA.SelectedColor := FColor;
  ColorPickerRed.SelectedColor := FColor;
  ColorPickerGreen.SelectedColor := FColor;
  ColorPickerBlue.SelectedColor := FColor;
  ColorPickerAlpha.SelectedColor := FColor;
  ColorSwatch.Color := FColor;

  // update spin edits
  Color32ToRGBA(FColor, R, G, B, A);
  SpinEditRed.Value := R;
  SpinEditGreen.Value := G;
  SpinEditBlue.Value := B;
  SpinEditAlpha.Value := A;

  // update color edit
  SelStart := EditColor.SelStart;
  EditColor.Text := '#' + IntToHex(A, 2) + IntToHex(R, 2) + IntToHex(G, 2) + IntToHex(B, 2);
  EditColor.SelStart := SelStart;

  // re-enable OnChange handler
  SpinEditRed.OnChange := SpinEditColorChange;
  SpinEditGreen.OnChange := SpinEditColorChange;
  SpinEditBlue.OnChange := SpinEditColorChange;
  SpinEditAlpha.OnChange := SpinEditColorChange;
  EditColor.OnChange := EditColorChange;
end;

end.

