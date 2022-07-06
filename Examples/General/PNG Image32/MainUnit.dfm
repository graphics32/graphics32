object FmPngDemo: TFmPngDemo
  Left = 299
  Top = 55
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'PNG Demonstration'
  ClientHeight = 279
  ClientWidth = 475
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
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
    Align = alClient
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnClick = ImageDisplayClick
    OnDblClick = ImageDisplayDblClick
  end
end
