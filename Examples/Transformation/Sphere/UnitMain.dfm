object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Spherical projection example'
  ClientHeight = 719
  ClientWidth = 1033
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  WindowState = wsMaximized
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object PaintBox32: TPaintBox32
    Left = 0
    Top = 0
    Width = 1033
    Height = 719
    Align = alClient
    TabOrder = 0
    TabStop = True
    OnDblClick = PaintBox32DblClick
    OnMouseDown = PaintBox32MouseDown
    OnMouseMove = PaintBox32MouseMove
    OnMouseUp = PaintBox32MouseUp
    OnMouseWheel = PaintBox32MouseWheel
    OnPaintBuffer = PaintBox32PaintBuffer
  end
  object TimerRotate: TTimer
    Interval = 200
    OnTimer = TimerRotateTimer
    Left = 492
    Top = 244
  end
end
