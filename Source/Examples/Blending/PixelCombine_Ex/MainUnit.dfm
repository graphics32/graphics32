object FormPixelCombine: TFormPixelCombine
  Left = 295
  Top = 110
  Caption = 'PixelCombine Example'
  ClientHeight = 346
  ClientWidth = 516
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
    346)
  PixelsPerInch = 96
  TextHeight = 13
  object ImgView: TImgView32
    Left = 16
    Top = 20
    Width = 367
    Height = 309
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Color = clBtnShadow
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 16
    OverSize = 0
    TabOrder = 0
  end
  object RadioGroup: TRadioGroup
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
    OnClick = RadioGroupClick
  end
end
