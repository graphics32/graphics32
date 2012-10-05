object FrmClipper: TFrmClipper
  Left = 280
  Top = 166
  Caption = 'Clipper'
  ClientHeight = 508
  ClientWidth = 834
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 15
  object PnlControl: TPanel
    Left = 0
    Top = 0
    Width = 169
    Height = 508
    Align = alLeft
    TabOrder = 0
    object BtnExit: TButton
      Left = 36
      Top = 420
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Exit'
      TabOrder = 5
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
      TabOrder = 4
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
      TabOrder = 1
    end
    object BtnInflate: TButton
      Left = 35
      Top = 296
      Width = 75
      Height = 25
      Caption = 'In&flate'
      TabOrder = 2
      OnClick = BtnInflateClick
    end
    object BtnDeflate: TButton
      Left = 35
      Top = 330
      Width = 75
      Height = 25
      Caption = 'Defla&te'
      TabOrder = 3
      OnClick = BtnDeflateClick
    end
  end
  object ImgView32: TImgView32
    Left = 169
    Top = 0
    Width = 665
    Height = 508
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
