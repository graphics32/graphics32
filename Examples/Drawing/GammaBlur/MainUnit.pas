unit MainUnit;

interface

uses
  {$IFNDEF FPC} Windows, {$ELSE} LCLIntf, LCLType, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Menus, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Math,
  GR32, GR32_Image, GR32_Layers, GR32_RangeBars;

type
  TFormGammaBlur = class(TForm)
    PaintBoxIncorrect: TPaintBox32;
    LabelIncorrect: TLabel;
    LabelCorrect: TLabel;
    PaintBoxCorrect: TPaintBox32;
    GaugeBarGamma: TGaugeBar;
    LabelGamma: TLabel;
    LabelGammaValue: TLabel;
    GaugeBarBlurRadius: TGaugeBar;
    LabelBlur: TLabel;
    LabelBlurValue: TLabel;
    Panel1: TPanel;
    LabelTestImage: TLabel;
    RadioButtonRedGreen: TRadioButton;
    RadioButtonCircles: TRadioButton;
    CheckBoxUseNew: TCheckBox;
    CheckBoxGammaSRGB: TCheckBox;
    procedure PaintBoxIncorrectPaintBuffer(Sender: TObject);
    procedure PaintBoxCorrectPaintBuffer(Sender: TObject);
    procedure GaugeBarGammaChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GaugeBarBlurRadiusChange(Sender: TObject);
    procedure PaintBoxResize(Sender: TObject);
    procedure RadioButtonTestImageClick(Sender: TObject);
    procedure CheckBoxUseNewClick(Sender: TObject);
    procedure CheckBoxGammaSRGBClick(Sender: TObject);
  private
    FTestBitmap: TBitmap32;
    procedure ComposeTestImage;
    procedure UpdateGamma;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormGammaBlur: TFormGammaBlur;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  GR32_Math,
  GR32_Polygons,
  GR32_VectorUtils,
  GR32_Gamma,
  GR32_System,
  GR32_Blurs,
  GR32.Blur,
  GR32_Resamplers;

{ TFrmGammaBlur }

constructor TFormGammaBlur.Create(AOwner: TComponent);
begin
  inherited;
  PaintBoxIncorrect.BufferOversize := 0;
  PaintBoxCorrect.BufferOversize := 0;
  FTestBitmap := TBitmap32.Create;
end;

destructor TFormGammaBlur.Destroy;
begin
  FTestBitmap.Free;

  inherited;
end;

procedure TFormGammaBlur.FormCreate(Sender: TObject);
begin
  GaugeBarGammaChange(nil);
  GaugeBarBlurRadiusChange(nil);
  // Ensure controls are same size in case we messed up at design-time
  PaintBoxIncorrect.Width := PaintBoxCorrect.Width;
  PaintBoxIncorrect.Height := PaintBoxCorrect.Height;
end;

procedure TFormGammaBlur.GaugeBarBlurRadiusChange(Sender: TObject);
var
  BlurRadius: Double;
begin
  BlurRadius := 0.1 * GaugeBarBlurRadius.Position;
  LabelBlurValue.Caption := Format('%.1n px', [BlurRadius]);
  PaintBoxIncorrect.Invalidate;
  PaintBoxCorrect.Invalidate;
end;

procedure TFormGammaBlur.GaugeBarGammaChange(Sender: TObject);
begin
  UpdateGamma;
end;

procedure ComposeTestImageRedGreen(Bitmap: TBitmap32);
begin
  Bitmap.Clear(clRed32);
  Bitmap.FillRect(0, 0, Bitmap.Width, Bitmap.Height div 2, clLime32);
end;

procedure ComposeTestImageCircles(Bitmap: TBitmap32);
var
  Points: TArrayOfFloatPoint;
  Index: Integer;
begin
  Bitmap.Clear(clBlack32);
  RandSeed := integer($DEADBABE);
  for Index := 0 to 70 do
  begin
    Points := Circle(Bitmap.Width * Random, Bitmap.Height * Random,
      0.5 * Min(Bitmap.Width, Bitmap.Height) * Random);
    PolygonFS(Bitmap, Points, HSLtoRGB(Random, 1, 0.5));
  end;
end;

procedure TFormGammaBlur.CheckBoxGammaSRGBClick(Sender: TObject);
begin
  UpdateGamma;
end;

procedure TFormGammaBlur.CheckBoxUseNewClick(Sender: TObject);
begin
  PaintBoxCorrect.Invalidate;
  PaintBoxIncorrect.Invalidate;
end;

procedure TFormGammaBlur.ComposeTestImage;
begin
  if RadioButtonCircles.Checked then
    ComposeTestImageCircles(FTestBitmap)
  else
    ComposeTestImageRedGreen(FTestBitmap);
end;

procedure TFormGammaBlur.PaintBoxResize(Sender: TObject);
begin
  Assert(PaintBoxCorrect.Width = PaintBoxIncorrect.Width);
  Assert(PaintBoxCorrect.Height = PaintBoxIncorrect.Height);
  FTestBitmap.SetSize(PaintBoxCorrect.Width, PaintBoxCorrect.Height);
  ComposeTestImage;
end;

procedure TFormGammaBlur.RadioButtonTestImageClick(Sender: TObject);
begin
  ComposeTestImage;
  PaintBoxCorrect.Invalidate;
  PaintBoxIncorrect.Invalidate;
end;

procedure TFormGammaBlur.UpdateGamma;
var
  GammaValue: Double;
begin
  GaugeBarGamma.Enabled := (not CheckBoxGammaSRGB.Checked);

  if (CheckBoxGammaSRGB.Checked) then
  begin
    Set_sRGB;
    LabelGammaValue.Caption := 'sRGB';
  end else
  begin
    GammaValue := 0.001 * GaugeBarGamma.Position;
    LabelGammaValue.Caption := Format('%.3n', [GammaValue]);
    SetGamma(GammaValue);
  end;

  PaintBoxIncorrect.Invalidate;
  PaintBoxCorrect.Invalidate;
end;

procedure TFormGammaBlur.PaintBoxCorrectPaintBuffer(Sender: TObject);
begin
  if CheckBoxUseNew.Checked then
    GammaBlur32(FTestBitmap, PaintBoxCorrect.Buffer, 0.1 * GaugeBarBlurRadius.Position)
  else
  begin
    FTestBitmap.DrawTo(PaintBoxCorrect.Buffer);
    GaussianBlurGamma(PaintBoxCorrect.Buffer, 0.1 * GaugeBarBlurRadius.Position);
  end;
end;

procedure TFormGammaBlur.PaintBoxIncorrectPaintBuffer(Sender: TObject);
begin
  if CheckBoxUseNew.Checked then
    Blur32(FTestBitmap, PaintBoxIncorrect.Buffer, 0.1 * GaugeBarBlurRadius.Position)
  else
  begin
    FTestBitmap.DrawTo(PaintBoxIncorrect.Buffer);
    GaussianBlur(PaintBoxIncorrect.Buffer, 0.1 * GaugeBarBlurRadius.Position);
  end;
end;

end.
