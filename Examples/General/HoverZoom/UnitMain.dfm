object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'HoverZoom example'
  ClientHeight = 416
  ClientWidth = 572
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    572
    416)
  TextHeight = 15
  object Image: TImage32
    Left = 16
    Top = 132
    Width = 537
    Height = 249
    Cursor = crCross
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    Background.CheckersStyle = bcsMedium
    Background.FillStyle = bfsCheckers
    TabOrder = 4
    TabStop = True
    OnMouseMove = ImageMouseMove
    OnMouseLeave = ImageMouseLeave
    OnResize = ImageResize
    OnScaleChange = ImageScaleChange
  end
  object CheckBoxLayer: TCheckBox
    Left = 16
    Top = 85
    Width = 301
    Height = 17
    Action = ActionViewLayer
    TabOrder = 3
  end
  object RadioButtonSmall: TRadioButton
    Left = 16
    Top = 8
    Width = 221
    Height = 17
    Action = ActionImageSmall
    TabOrder = 0
    TabStop = True
  end
  object RadioButtonLarge: TRadioButton
    Left = 16
    Top = 31
    Width = 221
    Height = 17
    Action = ActionImageLarge
    TabOrder = 1
  end
  object RadioButtonCustom: TRadioButton
    Left = 16
    Top = 56
    Width = 537
    Height = 17
    Action = ActionImageCustom
    TabOrder = 2
    OnDblClick = RadioButtonCustomDblClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 397
    Width = 572
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitLeft = 84
    ExplicitTop = 408
    ExplicitWidth = 0
  end
  object CheckBoxAnimate: TCheckBox
    Left = 16
    Top = 108
    Width = 301
    Height = 17
    Action = ActionAnimate
    State = cbChecked
    TabOrder = 6
  end
  object ActionList: TActionList
    Left = 360
    Top = 20
    object ActionViewLayer: TAction
      AutoCheck = True
      Caption = '&Display unscaled bitmap in a semi-transparent layer'
      OnExecute = ActionViewLayerExecute
      OnUpdate = ActionViewLayerUpdate
    end
    object ActionImageSmall: TAction
      AutoCheck = True
      Caption = '&Small image (initial scale = 1)'
      Checked = True
      GroupIndex = 1
      OnExecute = ActionImageSmallExecute
    end
    object ActionImageLarge: TAction
      AutoCheck = True
      Caption = '&Large image (initial scale < 1)'
      GroupIndex = 1
      OnExecute = ActionImageLargeExecute
    end
    object ActionImageCustom: TAction
      AutoCheck = True
      Caption = '&Custom...'
      GroupIndex = 1
      OnExecute = ActionImageCustomExecute
    end
    object ActionAnimate: TAction
      AutoCheck = True
      Caption = 'Animated zoom'
      Checked = True
      OnExecute = ActionAnimateExecute
    end
  end
  object TimerZoom: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TimerZoomTimer
    Left = 280
    Top = 212
  end
end
