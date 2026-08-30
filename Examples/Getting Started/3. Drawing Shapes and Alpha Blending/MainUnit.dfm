object FormDrawingShapes: TFormDrawingShapes
  Left = 200
  Top = 200
  Caption = 'Drawing Shapes and Alpha Blending Example'
  ClientHeight = 200
  ClientWidth = 320
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
    Width = 320
    Height = 200
    Align = alClient
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
end
