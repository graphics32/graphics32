object MainForm: TMainForm
  Left = 200
  Top = 153
  Cursor = crCross
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Visualization Example'
  ClientHeight = 300
  ClientWidth = 400
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseMove = FormMouseMove
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object MovementTimer: TTimer
    Enabled = False
    Interval = 3000
    OnTimer = MovementTimerTimer
    Left = 64
    Top = 16
  end
  object RenderTimer: TTimer
    Enabled = False
    Interval = 1
    OnTimer = RenderTimerTimer
    Left = 96
    Top = 16
  end
  object ColorTimer: TTimer
    Enabled = False
    Interval = 200
    OnTimer = ColorTimerTimer
    Left = 128
    Top = 16
  end
  object FPSTimer: TTimer
    Enabled = False
    OnTimer = FPSTimerTimer
    Left = 32
    Top = 16
  end
end
