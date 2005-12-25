object Form1: TForm1
  Left = 295
  Top = 110
  Width = 524
  Height = 373
  Caption = 'PixelCombine Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    516
    344)
  PixelsPerInch = 96
  TextHeight = 13
  object ImgView: TImgView32
    Left = 16
    Top = 20
    Width = 367
    Height = 309
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TNearestResampler'
    Color = clBtnShadow
    ParentColor = False
    Scale = 1.000000000000000000
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    OverSize = 0
    TabOrder = 0
  end
  object RadioGroup1: TRadioGroup
    Left = 394
    Top = 20
    Width = 109
    Height = 185
    Anchors = [akTop, akRight]
    Caption = 'Operation'
    ItemIndex = 0
    Items.Strings = (
      'Opaque'
      'Add'
      'Sub'
      'Modulate'
      'Min'
      'Max'
      'Difference'
      'Exclusion'
      'Pattern')
    TabOrder = 1
    OnClick = RadioGroup1Click
  end
end
