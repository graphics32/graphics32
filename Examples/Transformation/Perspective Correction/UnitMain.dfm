object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 
    'Perspective Correction example - Forward Projective Transformati' +
    'on'
  ClientHeight = 541
  ClientWidth = 1103
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 500
    Width = 1103
    Height = 41
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkFlat
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    object LabelStats: TLabel
      AlignWithMargins = True
      Left = 760
      Top = 0
      Width = 327
      Height = 39
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 16
      Margins.Bottom = 0
      Align = alRight
      Alignment = taRightJustify
      AutoSize = False
      Caption = '-'
      ShowAccelChar = False
      Layout = tlCenter
      ExplicitLeft = 776
    end
    object Label1: TLabel
      Left = 412
      Top = 12
      Width = 53
      Height = 15
      Caption = 'Rasterizer:'
    end
    object CheckBoxLive: TCheckBox
      Left = 188
      Top = 12
      Width = 97
      Height = 17
      AllowGrayed = True
      Caption = 'Live update'
      TabOrder = 0
      OnClick = CheckBoxLiveClick
    end
    object ButtonApply: TButton
      Left = 100
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Rasterize'
      TabOrder = 2
      OnClick = ButtonApplyClick
    end
    object CheckBoxExtrapolate: TCheckBox
      Left = 296
      Top = 12
      Width = 97
      Height = 17
      Caption = 'Extrapolate'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBoxExtrapolateClick
    end
    object ButtonReset: TButton
      Left = 12
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Reset'
      TabOrder = 1
      OnClick = ButtonResetClick
    end
    object ComboBoxRasterizer: TComboBox
      Left = 471
      Top = 9
      Width = 145
      Height = 23
      AutoDropDownWidth = True
      Style = csDropDownList
      TabOrder = 4
      OnChange = ComboBoxRasterizerChange
    end
    object CheckBoxLiveDraft: TCheckBox
      Left = 636
      Top = 12
      Width = 225
      Height = 17
      Caption = 'Use draft rasterizer during live update'
      TabOrder = 5
    end
  end
  object ImageSource: TImage32
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 277
    Height = 500
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 2
    Margins.Bottom = 0
    Align = alLeft
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smScale
    Background.CheckersStyle = bcsLight
    Background.FillStyle = bfsCheckers
    MousePan.Enabled = True
    MouseZoom.Enabled = True
    MouseZoom.Animate = True
    TabOrder = 1
    TabStop = True
  end
  object ImageDest: TImage32
    AlignWithMargins = True
    Left = 281
    Top = 0
    Width = 822
    Height = 500
    Margins.Left = 2
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smScale
    Background.InnerBorderWidth = 1
    Background.CheckersStyle = bcsLight
    Background.FillStyle = bfsCheckers
    MousePan.Enabled = True
    MouseZoom.Enabled = True
    MouseZoom.Animate = True
    TabOrder = 2
    TabStop = True
  end
  object TimerMarchingAnts: TTimer
    Interval = 50
    OnTimer = TimerMarchingAntsTimer
    Left = 544
    Top = 276
  end
  object TimerUpdate: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TimerUpdateTimer
    Left = 544
    Top = 336
  end
  object TimerDraft: TTimer
    Enabled = False
    Interval = 200
    OnTimer = TimerDraftTimer
    Left = 544
    Top = 388
  end
end
