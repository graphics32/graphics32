object FormBezier: TFormBezier
  Left = 0
  Top = 0
  Caption = 'Bezier Curves'
  ClientHeight = 480
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox32: TPaintBox32
    Left = 0
    Top = 0
    Width = 640
    Height = 480
    Align = alClient
    TabOrder = 0
    OnDblClick = PaintBox32DblClick
    OnMouseDown = PaintBox32MouseDown
    OnMouseMove = PaintBox32MouseMove
    OnMouseUp = PaintBox32MouseUp
    OnPaintBuffer = PaintBox32PaintBuffer
  end
end
