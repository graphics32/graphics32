object FormGradientLines: TFormGradientLines
  Left = 220
  Top = 105
  Caption = 'Gradient Lines Example'
  ClientHeight = 423
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  DesignSize = (
    623
    423)
  TextHeight = 13
  object LblTotal: TLabel
    Left = 510
    Top = 92
    Width = 28
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Total:'
  end
  object PaintBox: TPaintBox32
    Left = 8
    Top = 8
    Width = 496
    Height = 409
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object BtnAddOne: TButton
    Left = 510
    Top = 8
    Width = 105
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Add One'
    TabOrder = 1
    OnClick = BtnAddOneClick
  end
  object BtnAddTen: TButton
    Left = 510
    Top = 34
    Width = 105
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Add Ten'
    TabOrder = 2
    OnClick = BtnAddTenClick
  end
  object BtnClear: TButton
    Left = 510
    Top = 60
    Width = 105
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Clear'
    TabOrder = 3
    OnClick = BtnClearClick
  end
  object RgpFade: TRadioGroup
    Left = 510
    Top = 224
    Width = 105
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
    OnClick = RgpFadeClick
  end
  object RgpDraw: TRadioGroup
    Left = 510
    Top = 136
    Width = 105
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
    OnClick = RgpDrawClick
  end
  object PnlTotalLines: TPanel
    Left = 511
    Top = 108
    Width = 104
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
    Left = 510
    Top = 320
    Width = 105
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
    Width = 105
    Height = 74
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
      'optimization.'
      ''
      '(Maximize application'
      'on modern CPUs)')
    ParentFont = False
    ReadOnly = True
    TabOrder = 8
  end
  object TimerFrameRate: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = TimerFrameRateTimer
    Left = 304
    Top = 216
  end
end
