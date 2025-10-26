object Form48: TForm48
  Left = 0
  Top = 0
  Caption = 'Rotated Text example'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object PaintBox32: TPaintBox32
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    Align = alClient
    RepaintMode = rmOptimizer
    TabOrder = 0
    OnClick = PaintBox32Click
    OnPaintBuffer = PaintBox32PaintBuffer
  end
  object TimerRotate: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerRotateTimer
    Left = 308
    Top = 228
  end
end
