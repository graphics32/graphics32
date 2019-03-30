object FormGammaBlur: TFormGammaBlur
  Left = 100
  Top = 189
  Caption = 'GammaBlur'
  ClientHeight = 289
  ClientWidth = 263
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  DesignSize = (
    263
    289)
  PixelsPerInch = 96
  TextHeight = 15
  object LabelIncorrect: TLabel
    Left = 8
    Top = 8
    Width = 194
    Height = 15
    Caption = 'Incorrect (without gamma handling)'
  end
  object LabelCorrect: TLabel
    Left = 8
    Top = 111
    Width = 170
    Height = 15
    Caption = 'Correct (with gamma handling)'
  end
  object LabelGamma: TLabel
    Left = 8
    Top = 207
    Width = 45
    Height = 15
    Caption = 'Gamma'
  end
  object LabelGammaValue: TLabel
    Left = 215
    Top = 206
    Width = 3
    Height = 15
    Anchors = [akTop, akRight]
    ExplicitLeft = 295
  end
  object LabelBlur: TLabel
    Left = 8
    Top = 236
    Width = 68
    Height = 15
    Caption = 'Blur Radius:'
  end
  object LabelBlurValue: TLabel
    Left = 215
    Top = 235
    Width = 3
    Height = 15
    Anchors = [akTop, akRight]
    ExplicitLeft = 295
  end
  object LabelBlurType: TLabel
    Left = 8
    Top = 266
    Width = 53
    Height = 15
    Caption = 'Blur Type:'
  end
  object PaintBoxIncorrect: TPaintBox32
    Left = 8
    Top = 29
    Width = 247
    Height = 68
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnPaintBuffer = PaintBoxIncorrectPaintBuffer
    ExplicitWidth = 249
  end
  object PaintBoxCorrect: TPaintBox32
    Left = 8
    Top = 132
    Width = 247
    Height = 68
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnPaintBuffer = PaintBoxCorrectPaintBuffer
    ExplicitWidth = 602
  end
  object GaugeBarGamma: TGaugeBar
    Left = 82
    Top = 206
    Width = 127
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Backgnd = bgPattern
    LargeChange = 100
    Max = 3000
    Min = 300
    ShowHandleGrip = True
    Position = 480
    OnChange = GaugeBarGammaChange
  end
  object GaugeBarBlurRadius: TGaugeBar
    Left = 82
    Top = 235
    Width = 127
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Backgnd = bgPattern
    LargeChange = 100
    Max = 200
    Min = 1
    ShowHandleGrip = True
    Position = 50
    OnChange = GaugeBarBlurRadiusChange
  end
  object RadioButtonGaussianBlur: TRadioButton
    Left = 82
    Top = 266
    Width = 80
    Height = 17
    Caption = 'Gaussian'
    TabOrder = 4
  end
  object RadioButtonFastBlur: TRadioButton
    Left = 168
    Top = 264
    Width = 50
    Height = 17
    Caption = 'Fast'
    Checked = True
    TabOrder = 5
    TabStop = True
  end
end
