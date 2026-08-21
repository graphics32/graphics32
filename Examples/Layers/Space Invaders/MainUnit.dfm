object MainForm: TMainForm
  Left = 0
  Top = 0
  Cursor = -1
  Caption = 'Space Invaders'
  ClientHeight = 768
  ClientWidth = 672
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  TextHeight = 15
  object Image32: TImage32
    Left = 0
    Top = 0
    Width = 672
    Height = 768
    Cursor = -1
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Color = clBlack
    ParentColor = False
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smResize
    TabOrder = 0
  end
  object TimerGame: TTimer
    Interval = 16
    OnTimer = TimerGameTimer
    Left = 24
    Top = 24
  end
end
