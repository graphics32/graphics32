object FrmLineSimplification: TFrmLineSimplification
  Left = 0
  Top = 0
  Caption = 
    'Line Simpification - Use the mouse to draw an arbitrary polyline' +
    '; Hit [ENTER] for further reduction'
  ClientHeight = 473
  ClientWidth = 692
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
    Width = 692
    Height = 473
    Align = alClient
    TabOrder = 0
    OnMouseDown = PaintBox32MouseDown
    OnMouseUp = PaintBox32MouseUp
    OnPaintBuffer = PaintBox32PaintBuffer
  end
end
