object FormGradientLines: TFormGradientLines
  Left = 220
  Top = 105
  Caption = 'Gradient Lines Example'
  ClientHeight = 556
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    623
    556)
  PixelsPerInch = 96
  TextHeight = 13
  object lbTotal: TLabel
    Left = 518
    Top = 92
    Width = 28
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Total:'
  end
  object PaintBox: TPaintBox32
    Left = 6
    Top = 8
    Width = 495
    Height = 542
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object btAddOne: TButton
    Left = 515
    Top = 8
    Width = 98
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Add One'
    TabOrder = 1
    OnClick = btAddOneClick
  end
  object btAddTen: TButton
    Left = 515
    Top = 34
    Width = 98
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Add Ten'
    TabOrder = 2
    OnClick = btAddTenClick
  end
  object btClear: TButton
    Left = 515
    Top = 60
    Width = 98
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Clear'
    TabOrder = 3
    OnClick = btClearClick
  end
  object rgFade: TRadioGroup
    Left = 510
    Top = 224
    Width = 109
    Height = 89
    Anchors = [akTop, akRight]
    Caption = 'Fade'
    Ctl3D = True
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Slow'
      'Fast')
    ParentCtl3D = False
    TabOrder = 4
    OnClick = rgFadeClick
  end
  object rgDraw: TRadioGroup
    Left = 510
    Top = 136
    Width = 109
    Height = 81
    Anchors = [akTop, akRight]
    Caption = 'Draw'
    Ctl3D = True
    ItemIndex = 0
    Items.Strings = (
      'Slow'
      'Normal'
      'Fast')
    ParentCtl3D = False
    TabOrder = 5
    OnClick = rgDrawClick
  end
  object pnTotalLines: TPanel
    Left = 519
    Top = 108
    Width = 73
    Height = 17
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = '0'
    Color = clWindow
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 6
  end
  object RepaintOpt: TCheckBox
    Left = 511
    Top = 320
    Width = 109
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Repaint Optimization'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = RepaintOptClick
  end
  object Memo: TMemo
    Left = 510
    Top = 343
    Width = 109
    Height = 57
    TabStop = False
    Anchors = [akTop, akRight]
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'Disable fading to see '
      'effect of repaint '
      'optimization.')
    ParentFont = False
    ReadOnly = True
    TabOrder = 8
  end
end
