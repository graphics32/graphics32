object FormMain: TFormMain
  Left = 220
  Top = 105
  Caption = 'Particle Swarm example'
  ClientHeight = 723
  ClientWidth = 1003
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  WindowState = wsMaximized
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 13
  object PaintBox: TPaintBox32
    Left = 0
    Top = 0
    Width = 1003
    Height = 723
    Align = alClient
    TabOrder = 0
    OnResize = PaintBoxResize
  end
  object TimerFrameRate: TTimer
    Interval = 5000
    OnTimer = TimerFrameRateTimer
    Left = 304
    Top = 216
  end
end
