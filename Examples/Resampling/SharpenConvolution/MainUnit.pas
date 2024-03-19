unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls,

  GR32,
  GR32_Image,
  GR32_OrdinalMaps;

type
  TFormMain = class(TForm)
    PanelSource: TPanel;
    ImgViewSource: TImgView32;
    Panel2: TPanel;
    PanelOptions: TPanel;
    PanelResult: TPanel;
    ImgViewResult: TImgView32;
    Panel5: TPanel;
    PanelImages: TPanel;
    Splitter1: TSplitter;
    Label1: TLabel;
    SpinEditValue: TSpinEdit;
    ButtonApply: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FKernel: TIntegerMap;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  GR32_Resamplers;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ImgViewSource.Bitmap.LoadFromResourceName(hInstance, 'DICE', 'PNG');

  ImgViewSource.Bitmap.ResamplerClassName := TLinearResampler.ClassName;

  FKernel := TIntegerMap.Create;
  FKernel.SetSize(3, 3);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FKernel.Free;
end;

procedure TFormMain.ButtonApplyClick(Sender: TObject);
var
  Value: integer;
  z: integer;
  NormalizationFactor: Double;
  Norm_z: integer;
  Norm_zz: integer;
  Norm_One: integer;
begin
  Screen.Cursor := crHourGlass;
  try

    Value := SpinEditValue.Value;

    if (Value = 0) then
    begin
      // Nothing to do; Just copy the bitmap

      // Assign() also copies the properties...
      ImgViewResult.Bitmap.Assign(ImgViewSource.Bitmap);
      // ...so we'll need to restore some of them
      ImgViewResult.Bitmap.ResamplerClassName := TNearestResampler.ClassName;

      exit;
    end;

    // Note: Kernel is using 24:8 fixed precision numbers

    if (Value < 0) then
    begin

      z := 6 + Value;

      // Normalization: 1/<sum>
      NormalizationFactor := 1 / (z*4 + 4 + z*z);

      // Normalize values and scale
      Norm_One := Round(NormalizationFactor * 256);
      Norm_z := Round(z * NormalizationFactor * 256);
      Norm_zz := Round(z*z * NormalizationFactor * 256);

      FKernel.Value[0, 0] := Norm_One; FKernel.Value[1, 0] := Norm_z;  FKernel.Value[2, 0] := Norm_One;
      FKernel.Value[0, 1] := Norm_z;   FKernel.Value[1, 1] := Norm_zz; FKernel.Value[2, 1] := Norm_z;
      FKernel.Value[0, 2] := Norm_One; FKernel.Value[1, 2] := Norm_z;  FKernel.Value[2, 2] := Norm_One;

    end else
    begin

      // Sharpen
      z := 22 - Value * 2;

      // Normalization: 1/<sum>
      NormalizationFactor := 1 / (z - 8);

      // Normalize values and scale
      Norm_One := -Round(NormalizationFactor * 256);
      Norm_z := Round(z * NormalizationFactor * 256);

      FKernel.Value[0, 0] := Norm_One; FKernel.Value[1, 0] := Norm_One; FKernel.Value[2, 0] := Norm_One;
      FKernel.Value[0, 1] := Norm_One; FKernel.Value[1, 1] := Norm_z;   FKernel.Value[2, 1] := Norm_One;
      FKernel.Value[0, 2] := Norm_One; FKernel.Value[1, 2] := Norm_One; FKernel.Value[2, 2] := Norm_One;

    end;

    Convolve(ImgViewSource.Bitmap, ImgViewResult.Bitmap, FKernel, 1, 1);

  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
