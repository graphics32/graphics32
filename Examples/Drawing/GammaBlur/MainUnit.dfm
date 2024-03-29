object FormGammaBlur: TFormGammaBlur
  Left = 100
  Top = 189
  BorderStyle = bsDialog
  Caption = 'GammaBlur'
  ClientHeight = 314
  ClientWidth = 263
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    263
    314)
  TextHeight = 15
  object LabelIncorrect: TLabel
    Left = 8
    Top = 32
    Width = 194
    Height = 15
    Caption = 'Incorrect (without gamma handling)'
  end
  object LabelCorrect: TLabel
    Left = 8
    Top = 135
    Width = 170
    Height = 15
    Caption = 'Correct (with gamma handling)'
  end
  object LabelGamma: TLabel
    Left = 8
    Top = 231
    Width = 45
    Height = 15
    Caption = 'Gamma'
  end
  object LabelGammaValue: TLabel
    Left = 215
    Top = 230
    Width = 3
    Height = 15
    Anchors = [akTop, akRight]
  end
  object LabelBlur: TLabel
    Left = 8
    Top = 260
    Width = 68
    Height = 15
    Caption = 'Blur Radius:'
  end
  object LabelBlurValue: TLabel
    Left = 215
    Top = 259
    Width = 3
    Height = 15
    Anchors = [akTop, akRight]
  end
  object PaintBoxIncorrect: TPaintBox32
    Left = 8
    Top = 53
    Width = 247
    Height = 68
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnPaintBuffer = PaintBoxIncorrectPaintBuffer
    OnResize = PaintBoxResize
  end
  object PaintBoxCorrect: TPaintBox32
    Left = 8
    Top = 156
    Width = 247
    Height = 68
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnPaintBuffer = PaintBoxCorrectPaintBuffer
    OnResize = PaintBoxResize
  end
  object GaugeBarGamma: TGaugeBar
    Left = 82
    Top = 230
    Width = 127
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Backgnd = bgPattern
    LargeChange = 100
    Max = 3000
    Min = 300
    ShowHandleGrip = True
    Position = 2200
    OnChange = GaugeBarGammaChange
  end
  object GaugeBarBlurRadius: TGaugeBar
    Left = 82
    Top = 259
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 263
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 4
    object LabelTestImage: TLabel
      Left = 8
      Top = 8
      Width = 67
      Height = 15
      Caption = 'Test  Image:'
    end
    object RadioButtonRedGreen: TRadioButton
      Left = 82
      Top = 7
      Width = 80
      Height = 17
      Caption = 'Red/Green'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButtonTestImageClick
    end
    object RadioButtonCircles: TRadioButton
      Left = 175
      Top = 8
      Width = 66
      Height = 17
      Caption = 'Circles'
      TabOrder = 1
      OnClick = RadioButtonTestImageClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 281
    Width = 263
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 5
    ExplicitTop = 273
    object LabelBlurType: TLabel
      Left = 8
      Top = 10
      Width = 53
      Height = 15
      Caption = 'Blur Type:'
    end
    object RadioButtonGaussianBlur: TRadioButton
      Left = 82
      Top = 8
      Width = 80
      Height = 17
      Caption = 'Gaussian'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButtonFastBlur: TRadioButton
      Left = 168
      Top = 8
      Width = 50
      Height = 17
      Caption = 'Fast'
      TabOrder = 1
    end
  end
end
