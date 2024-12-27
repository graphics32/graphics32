object FormRotateExample: TFormRotateExample
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Rotate Example'
  ClientHeight = 252
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Src: TImage32
    Left = 16
    Top = 16
    Width = 192
    Height = 192
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TLinearResampler'
    BitmapAlign = baCenter
    Color = clWindowText
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
  object Dst: TImage32
    Left = 232
    Top = 16
    Width = 192
    Height = 192
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Color = clWindowText
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 1
  end
  object Angle: TGaugeBar
    Left = 16
    Top = 220
    Width = 409
    Height = 19
    Backgnd = bgPattern
    Max = 180
    Min = -180
    ShowHandleGrip = True
    Style = rbsMac
    Position = 0
    OnChange = AngleChange
  end
end
