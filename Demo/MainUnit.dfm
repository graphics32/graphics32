object FmPngDemo: TFmPngDemo
  Left = 299
  Top = 55
  Width = 483
  Height = 306
  Caption = 'PNG Demonstration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    475
    279)
  PixelsPerInch = 96
  TextHeight = 13
  object ImageDisplay: TImage32
    Left = 8
    Top = 8
    Width = 459
    Height = 262
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnClick = ImageDisplayClick
  end
end
