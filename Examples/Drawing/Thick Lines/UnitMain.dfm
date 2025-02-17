object FormThickLineTest: TFormThickLineTest
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Thick Line benchmark'
  ClientHeight = 697
  ClientWidth = 905
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Padding.Left = 10
  Padding.Top = 10
  Padding.Right = 10
  Padding.Bottom = 10
  Position = poScreenCenter
  TextHeight = 15
  object PaintBoxGDIThin: TPaintBox
    Left = 10
    Top = 52
    Width = 281
    Height = 273
    Cursor = crHandPoint
    OnClick = PaintBoxClick
    OnPaint = PaintBoxGDIThinPaint
  end
  object Label1: TLabel
    Left = 10
    Top = 10
    Width = 281
    Height = 40
    AutoSize = False
    Caption = 'TCanvas.LineTo'
    ShowAccelChar = False
    Layout = tlCenter
    WordWrap = True
  end
  object Label2: TLabel
    Left = 314
    Top = 10
    Width = 281
    Height = 40
    AutoSize = False
    Caption = 'TBitmap32.LineToAS'
    ShowAccelChar = False
    Layout = tlCenter
    WordWrap = True
  end
  object PaintBoxGDIThick: TPaintBox
    Left = 10
    Top = 372
    Width = 281
    Height = 273
    Cursor = crHandPoint
    OnClick = PaintBoxClick
    OnPaint = PaintBoxGDIThickPaint
  end
  object Label3: TLabel
    Left = 10
    Top = 330
    Width = 281
    Height = 40
    AutoSize = False
    Caption = 'TCanvas.LineTo'
    ShowAccelChar = False
    Layout = tlCenter
    WordWrap = True
  end
  object Label4: TLabel
    Left = 314
    Top = 330
    Width = 281
    Height = 40
    AutoSize = False
    Caption = 'TCanvas32.LineTo'
    ShowAccelChar = False
    Layout = tlCenter
    WordWrap = True
  end
  object Label5: TLabel
    Left = 614
    Top = 330
    Width = 281
    Height = 40
    AutoSize = False
    Caption = 'Graphics32 DrawThickLine'
    ShowAccelChar = False
    Layout = tlCenter
    WordWrap = True
  end
  object Label6: TLabel
    Left = 614
    Top = 10
    Width = 281
    Height = 40
    AutoSize = False
    Caption = 'TBitmap32.LineToS'
    ShowAccelChar = False
    Layout = tlCenter
    WordWrap = True
  end
  object PaintBox32_ThinAlpha: TPaintBox32
    Left = 314
    Top = 52
    Width = 281
    Height = 273
    Cursor = crHandPoint
    TabOrder = 0
    OnClick = PaintBox32Click
    OnPaintBuffer = PaintBox32_ThinAlphaPaintBuffer
  end
  object PaintBox32_Thick: TPaintBox32
    Left = 314
    Top = 372
    Width = 281
    Height = 273
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = PaintBox32Click
    OnPaintBuffer = PaintBox32_ThickPaintBuffer
  end
  object ButtonRedraw: TButton
    Left = 406
    Top = 662
    Width = 101
    Height = 25
    Caption = 'Redraw all'
    TabOrder = 2
    OnClick = ButtonRedrawClick
  end
  object PaintBox32_ThickLine: TPaintBox32
    Left = 614
    Top = 372
    Width = 281
    Height = 273
    Cursor = crHandPoint
    TabOrder = 3
    OnClick = PaintBox32Click
    OnPaintBuffer = PaintBox32_ThickLinePaintBuffer
  end
  object PaintBox32_Thin: TPaintBox32
    Left = 614
    Top = 52
    Width = 281
    Height = 273
    Cursor = crHandPoint
    TabOrder = 4
    OnClick = PaintBox32Click
    OnPaintBuffer = PaintBox32_ThinPaintBuffer
  end
end
