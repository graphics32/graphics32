object FrmClipper: TFrmClipper
  Left = 272
  Top = 153
  Width = 676
  Height = 496
  Caption = 'Clipper'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object PnlControl: TPanel
    Left = 0
    Top = 0
    Width = 169
    Height = 469
    Align = alLeft
    TabOrder = 0
    object BtnExit: TButton
      Left = 36
      Top = 420
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Exit'
      TabOrder = 2
      OnClick = BtnExitClick
    end
    object rgClipping: TRadioGroup
      Left = 16
      Top = 26
      Width = 138
      Height = 127
      Caption = 'Clipping Op'
      ItemIndex = 1
      Items.Strings = (
        '&Intersection'
        '&Union'
        '&Difference'
        '&XOR')
      TabOrder = 0
    end
    object BtnClear: TButton
      Left = 35
      Top = 384
      Width = 75
      Height = 25
      Caption = '&Clear'
      TabOrder = 1
      OnClick = BtnClearClick
    end
    object RgpObject: TRadioGroup
      Left = 15
      Top = 176
      Width = 138
      Height = 98
      Caption = 'Shape'
      ItemIndex = 0
      Items.Strings = (
        '&Star'
        '&Ellipse'
        '&Rectangle')
      TabOrder = 3
    end
  end
  object ImgView32: TImgView32
    Left = 169
    Top = 0
    Width = 499
    Height = 469
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 16
    ScrollBars.Visibility = svAuto
    OverSize = 0
    TabOrder = 1
    OnMouseDown = ImgView32MouseDown
    OnMouseLeave = ImgView32MouseLeave
    OnMouseMove = ImgView32MouseMove
  end
end
