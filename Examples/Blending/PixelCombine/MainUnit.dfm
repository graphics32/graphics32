object FormPixelCombine: TFormPixelCombine
  Left = 295
  Top = 110
  Caption = 'PixelCombine Example'
  ClientHeight = 346
  ClientWidth = 564
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    564
    346)
  PixelsPerInch = 96
  TextHeight = 13
  object ImgView: TImgView32
    Left = 16
    Top = 20
    Width = 393
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
    Left = 415
    Top = 20
    Width = 141
    Height = 285
    Anchors = [akTop, akRight]
    Caption = 'Operation'
    ItemIndex = 0
    Items.Strings = (
      'Opaque (none)'
      'Add / Plus / Lighter'
      'Sub'
      'Modulate / Multiply'
      'Min / Darken'
      'Max / Lighten'
      'Screen'
      'Color-Dodge'
      'Color-Burn'
      'Difference'
      'Exclusion'
      'Pattern'
      'Blend'
      'Blend Add'
      'Blend Modulate')
    TabOrder = 1
    OnClick = RadioGroupClick
  end
end
