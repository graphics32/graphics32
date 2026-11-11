object FormDirectPixelAccess: TFormDirectPixelAccess
  Left = 200
  Top = 200
  Caption = 'Direct Pixel Access Example'
  ClientHeight = 256
  ClientWidth = 256
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Image32: TImage32
    Left = 0
    Top = 0
    Width = 256
    Height = 256
    Align = alClient
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
end
