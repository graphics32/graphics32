object BitmapCompareDialog: TBitmapCompareDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Bitmap Comparison'
  ClientHeight = 257
  ClientWidth = 348
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  DesignSize = (
    348
    257)
  PixelsPerInch = 96
  TextHeight = 13
  object LeftBitmap: TPaintBox
    Left = 8
    Top = 128
    Width = 105
    Height = 105
    Color = clBtnFace
    ParentColor = False
    OnMouseMove = UpdateColorUnderMouse
    OnPaint = LeftBitmapPaint
  end
  object RightBitmap: TPaintBox
    Left = 119
    Top = 128
    Width = 105
    Height = 105
    Color = clBtnFace
    ParentColor = False
    OnMouseMove = UpdateColorUnderMouse
    OnPaint = RightBitmapPaint
  end
  object Overlay: TPaintBox
    Left = 230
    Top = 128
    Width = 105
    Height = 105
    Color = clBtnFace
    ParentColor = False
    OnMouseMove = UpdateColorUnderMouse
    OnPaint = OverlayPaint
  end
  object LabelWant: TLabel
    Left = 87
    Top = 109
    Width = 26
    Height = 13
    Alignment = taRightJustify
    Caption = 'Want'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LabelHave: TLabel
    Left = 119
    Top = 109
    Width = 25
    Height = 13
    Caption = 'Have'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LabelOverlay: TLabel
    Left = 230
    Top = 109
    Width = 38
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Overlay'
  end
  object LabelColorUnderMouse: TLabel
    Left = 0
    Top = 239
    Width = 348
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = '<Color Under Mouse>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object ButtonWantHave: TButton
    Left = 48
    Top = 8
    Width = 75
    Height = 25
    Caption = '[W]ant/Have'
    TabOrder = 0
    OnClick = ButtonWantHaveClick
    OnKeyDown = FormKeyDown
  end
  object ButtonOverlay: TButton
    Left = 129
    Top = 8
    Width = 75
    Height = 25
    Caption = '[O]verlay'
    TabOrder = 1
    OnClick = ButtonOverlayClick
    OnKeyDown = FormKeyDown
  end
  object Button100: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = '[1]00 %'
    TabOrder = 3
    OnClick = Button100Click
    OnKeyDown = FormKeyDown
  end
  object Button200: TButton
    Left = 89
    Top = 39
    Width = 75
    Height = 25
    Caption = '[2]00 %'
    TabOrder = 4
    OnClick = Button200Click
    OnKeyDown = FormKeyDown
  end
  object Button300: TButton
    Left = 170
    Top = 39
    Width = 75
    Height = 25
    Caption = '[3]00 %'
    TabOrder = 5
    OnClick = Button300Click
    OnKeyDown = FormKeyDown
  end
  object Button400: TButton
    Left = 251
    Top = 39
    Width = 75
    Height = 25
    Caption = '[4]00 %'
    TabOrder = 6
    OnClick = Button400Click
    OnKeyDown = FormKeyDown
  end
  object Button500: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = '[5]00 %'
    TabOrder = 7
    OnClick = Button500Click
    OnKeyDown = FormKeyDown
  end
  object Button600: TButton
    Left = 89
    Top = 70
    Width = 75
    Height = 25
    Caption = '[6]00 %'
    TabOrder = 8
    OnClick = Button600Click
    OnKeyDown = FormKeyDown
  end
  object Button700: TButton
    Left = 170
    Top = 70
    Width = 75
    Height = 25
    Caption = '[7]00 %'
    TabOrder = 9
    OnClick = Button700Click
    OnKeyDown = FormKeyDown
  end
  object Button800: TButton
    Left = 251
    Top = 70
    Width = 75
    Height = 25
    Caption = '[8]00 %'
    TabOrder = 10
    OnClick = Button800Click
    OnKeyDown = FormKeyDown
  end
  object ButtonDiff: TButton
    Left = 210
    Top = 8
    Width = 75
    Height = 25
    Caption = '[D]iff'
    TabOrder = 2
    OnClick = ButtonDiffClick
    OnKeyDown = FormKeyDown
  end
  object ToggleTimer: TTimer
    Interval = 500
    OnTimer = ToggleTimerTimer
    Left = 264
    Top = 168
  end
end
