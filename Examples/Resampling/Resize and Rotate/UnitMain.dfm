object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Resize and Rotate bitmap'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 15
  object ImageSource: TImgView32
    Left = 0
    Top = 0
    Width = 317
    Height = 343
    Align = alLeft
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    Background.CheckersStyle = bcsLight
    Background.FillStyle = bfsCheckers
    MousePan.Enabled = True
    MouseZoom.Enabled = True
    MouseZoom.Animate = True
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 17
    OverSize = 0
    TabOrder = 0
  end
  object ImageDest: TImgView32
    Left = 317
    Top = 0
    Width = 307
    Height = 343
    Align = alClient
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    Background.CheckersStyle = bcsLight
    Background.FillStyle = bfsCheckers
    MousePan.Enabled = True
    MouseZoom.Enabled = True
    MouseZoom.Animate = True
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 17
    OverSize = 0
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 343
    Width = 624
    Height = 79
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 2
    DesignSize = (
      624
      79)
    object Label1: TLabel
      Left = 162
      Top = 16
      Width = 34
      Height = 15
      Caption = '&Angle:'
      FocusControl = TrackBarAngle
      OnClick = SettingChanged
    end
    object Label2: TLabel
      Left = 8
      Top = 16
      Width = 35
      Height = 15
      Caption = '&Width:'
      FocusControl = SpinEditWidth
    end
    object Label3: TLabel
      Left = 8
      Top = 46
      Width = 39
      Height = 15
      Caption = '&Height:'
      FocusControl = SpinEditHeight
    end
    object Bevel1: TBevel
      Left = 511
      Top = 6
      Width = 5
      Height = 65
      Anchors = [akTop, akRight]
      Shape = bsLeftLine
    end
    object Bevel2: TBevel
      Left = 151
      Top = 6
      Width = 5
      Height = 65
      Shape = bsLeftLine
    end
    object Bevel3: TBevel
      Left = 162
      Top = 39
      Width = 340
      Height = 5
      Anchors = [akLeft, akTop, akRight]
      Shape = bsTopLine
    end
    object Label4: TLabel
      Left = 162
      Top = 50
      Width = 58
      Height = 15
      Caption = '&Resampler:'
    end
    object TrackBarAngle: TTrackBar
      Left = 202
      Top = 6
      Width = 303
      Height = 27
      Anchors = [akLeft, akTop, akRight]
      LineSize = 15
      Max = 360
      PageSize = 45
      Frequency = 15
      PositionToolTip = ptBottom
      ShowSelRange = False
      TabOrder = 2
      ThumbLength = 15
      TickMarks = tmTopLeft
      OnChange = SettingChanged
    end
    object SpinEditWidth: TSpinEdit
      Left = 64
      Top = 11
      Width = 77
      Height = 24
      Increment = 64
      MaxValue = 4096
      MinValue = 1
      TabOrder = 0
      Value = 1
      OnChange = SettingChanged
    end
    object SpinEditHeight: TSpinEdit
      Left = 64
      Top = 41
      Width = 77
      Height = 24
      Increment = 64
      MaxValue = 4096
      MinValue = 1
      TabOrder = 1
      Value = 1
      OnChange = SettingChanged
    end
    object CheckBoxUpdate: TCheckBox
      Left = 522
      Top = 48
      Width = 97
      Height = 17
      Anchors = [akTop, akRight]
      Caption = '&Live update'
      TabOrder = 4
      OnClick = CheckBoxUpdateClick
    end
    object Button1: TButton
      Left = 522
      Top = 11
      Width = 75
      Height = 25
      Action = ActionApply
      Anchors = [akTop, akRight]
      TabOrder = 5
    end
    object ComboBoxResampler: TComboBox
      Left = 232
      Top = 47
      Width = 145
      Height = 23
      Style = csDropDownList
      TabOrder = 3
      OnChange = SettingChanged
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 422
    Width = 624
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object TimerApply: TTimer
    Enabled = False
    Interval = 500
    OnTimer = TimerApplyTimer
    Left = 192
    Top = 152
  end
  object ActionList1: TActionList
    Left = 424
    Top = 158
    object ActionApply: TAction
      Caption = '&Apply'
      OnExecute = ActionApplyExecute
      OnUpdate = ActionApplyUpdate
    end
  end
end
