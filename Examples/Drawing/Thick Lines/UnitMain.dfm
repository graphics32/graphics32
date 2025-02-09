object FormThickLineTest: TFormThickLineTest
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Thick Line benchmark'
  ClientHeight = 611
  ClientWidth = 969
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object PaintBoxGDIThin: TPaintBox
    Left = 8
    Top = 24
    Width = 281
    Height = 273
    Cursor = crHandPoint
    OnClick = PaintBoxClick
    OnPaint = PaintBoxGDIThinPaint
  end
  object Label1: TLabel
    Left = 12
    Top = 8
    Width = 80
    Height = 15
    Caption = 'TCanvas.LineTo'
  end
  object Label2: TLabel
    Left = 312
    Top = 8
    Width = 107
    Height = 15
    Caption = 'TBitmap32.LineToAS'
  end
  object PaintBoxGDIThick: TPaintBox
    Left = 8
    Top = 328
    Width = 281
    Height = 273
    Cursor = crHandPoint
    OnClick = PaintBoxClick
    OnPaint = PaintBoxGDIThickPaint
  end
  object Label3: TLabel
    Left = 8
    Top = 312
    Width = 80
    Height = 15
    Caption = 'TCanvas.LineTo'
  end
  object Label4: TLabel
    Left = 312
    Top = 312
    Width = 92
    Height = 15
    Caption = 'TCanvas32.LineTo'
  end
  object Label5: TLabel
    Left = 612
    Top = 312
    Width = 138
    Height = 15
    Caption = 'Graphics32 DrawThickLine'
  end
  object PaintBox32_Thin: TPaintBox32
    Left = 312
    Top = 24
    Width = 281
    Height = 273
    Cursor = crHandPoint
    TabOrder = 0
    OnClick = PaintBox32Click
    OnPaintBuffer = PaintBox32_ThinPaintBuffer
  end
  object PaintBox32_Thick: TPaintBox32
    Left = 312
    Top = 328
    Width = 281
    Height = 273
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = PaintBox32Click
    OnPaintBuffer = PaintBox32_ThickPaintBuffer
  end
  object ButtonRedraw: TButton
    Left = 612
    Top = 24
    Width = 101
    Height = 25
    Caption = 'Redraw all'
    TabOrder = 2
    OnClick = ButtonRedrawClick
  end
  object PaintBox32_ThickLine: TPaintBox32
    Left = 612
    Top = 328
    Width = 281
    Height = 273
    Cursor = crHandPoint
    TabOrder = 3
    OnClick = PaintBox32Click
    OnPaintBuffer = PaintBox32_ThickLinePaintBuffer
  end
end
