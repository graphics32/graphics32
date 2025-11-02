object FormMain: TFormMain
  Left = 0
  Top = 0
  AutoSize = True
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Water Effect layer example'
  ClientHeight = 600
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Image32: TImage32
    Left = 0
    Top = 0
    Width = 440
    Height = 600
    AutoSize = True
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnMouseDown = Image32MouseDown
    OnMouseMove = Image32MouseMove
  end
  object TimerWaterEffect: TTimer
    Interval = 50
    OnTimer = TimerWaterEffectTimer
    Left = 204
    Top = 104
  end
  object TimerRaindrops: TTimer
    OnTimer = TimerRaindropsTimer
    Left = 204
    Top = 176
  end
end
