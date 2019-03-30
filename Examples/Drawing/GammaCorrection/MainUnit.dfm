object FrmGammaCorrection: TFrmGammaCorrection
  Left = 0
  Top = 0
  Caption = 'Gamma Test'
  ClientHeight = 389
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox32: TPaintBox32
    Left = 0
    Top = 0
    Width = 492
    Height = 317
    Align = alClient
    TabOrder = 0
    OnPaintBuffer = PaintBox32PaintBuffer
  end
  object PnControl: TPanel
    Left = 0
    Top = 317
    Width = 492
    Height = 72
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    object LblContrast: TLabel
      Left = 8
      Top = 8
      Width = 42
      Height = 13
      Caption = 'Contrast'
    end
    object LblGamma: TLabel
      Left = 8
      Top = 30
      Width = 35
      Height = 13
      Caption = 'Gamma'
    end
    object LblThickness: TLabel
      Left = 8
      Top = 52
      Width = 46
      Height = 13
      Caption = 'Thickness'
    end
    object LblContrastValue: TLabel
      Left = 439
      Top = 8
      Width = 3
      Height = 13
    end
    object LblGammaValue: TLabel
      Left = 439
      Top = 30
      Width = 3
      Height = 13
    end
    object LblThicknessValue: TLabel
      Left = 439
      Top = 51
      Width = 3
      Height = 13
    end
    object GbrContrast: TGaugeBar
      Left = 56
      Top = 6
      Width = 377
      Height = 16
      Backgnd = bgPattern
      Max = 255
      ShowHandleGrip = True
      Position = 255
      OnChange = GbrContrastChange
    end
    object GbrGamma: TGaugeBar
      Left = 56
      Top = 30
      Width = 377
      Height = 16
      Backgnd = bgPattern
      LargeChange = 100
      Max = 3000
      Min = 300
      ShowHandleGrip = True
      Position = 1000
      OnChange = GbrGammaChange
    end
    object GbrThickness: TGaugeBar
      Left = 56
      Top = 50
      Width = 377
      Height = 16
      Backgnd = bgPattern
      Max = 300
      ShowHandleGrip = True
      Position = 200
      OnChange = GbrThicknessChange
    end
  end
end
