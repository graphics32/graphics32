object FrmAntiAliasingTest: TFrmAntiAliasingTest
  Left = 0
  Top = 0
  Caption = 'Anti-Aliasing Test'
  ClientHeight = 350
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox32: TPaintBox32
    Left = 0
    Top = 0
    Width = 480
    Height = 350
    Align = alClient
    TabOrder = 0
    OnPaintBuffer = PaintBox32PaintBuffer
  end
end
