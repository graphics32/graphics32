object FormInteractiveLayers: TFormInteractiveLayers
  Left = 200
  Top = 200
  Caption = 'Image Display and Interactive Layers Example'
  ClientHeight = 372
  ClientWidth = 442
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
    Width = 442
    Height = 372
    Align = alClient
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnMouseDown = Image32MouseDown
    ExplicitWidth = 380
    ExplicitHeight = 220
  end
end
