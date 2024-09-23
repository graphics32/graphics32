object FrmBlurs: TFrmBlurs
  Left = 283
  Top = 189
  Caption = 'Blurs'
  ClientHeight = 405
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PnlControl: TPanel
    Left = 0
    Top = 0
    Width = 154
    Height = 386
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 0
    object RgpBlurType: TRadioGroup
      Left = 8
      Top = 8
      Width = 138
      Height = 116
      Align = alTop
      Caption = 'Blur Type'
      ItemIndex = 0
      Items.Strings = (
        '&None'
        '&Gaussian'
        '&Motion'
        '&Selective')
      TabOrder = 0
      OnClick = RgpBlurTypeClick
      ExplicitLeft = 0
      ExplicitTop = -2
      ExplicitWidth = 152
    end
    object PanelSelective: TPanel
      Left = 8
      Top = 203
      Width = 138
      Height = 54
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Padding.Top = 8
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitTop = 196
      ExplicitWidth = 152
      object LabelDelta: TLabel
        Left = 0
        Top = 8
        Width = 138
        Height = 15
        Align = alTop
        Caption = 'Delta (10)'
        FocusControl = TrackBarDelta
        ExplicitLeft = -1
        ExplicitTop = 6
        ExplicitWidth = 152
      end
      object TrackBarDelta: TTrackBar
        Left = 0
        Top = 23
        Width = 138
        Height = 31
        Align = alTop
        Max = 255
        PageSize = 10
        Position = 10
        TabOrder = 0
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarDeltaChange
        ExplicitLeft = -5
        ExplicitTop = 12
        ExplicitWidth = 152
      end
    end
    object PanelMotion: TPanel
      Left = 8
      Top = 257
      Width = 138
      Height = 71
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Padding.Top = 8
      TabOrder = 2
      ExplicitLeft = 1
      ExplicitTop = 250
      ExplicitWidth = 152
      object LblBlurAngle: TLabel
        Left = 0
        Top = 8
        Width = 138
        Height = 15
        Align = alTop
        Caption = 'Blur &Angle (45)'
        Enabled = False
        FocusControl = TbrBlurAngle
        ExplicitLeft = 8
        ExplicitTop = -4
        ExplicitWidth = 152
      end
      object TbrBlurAngle: TTrackBar
        Left = 0
        Top = 23
        Width = 138
        Height = 31
        Align = alTop
        Enabled = False
        Max = 180
        Min = -180
        Position = 45
        TabOrder = 0
        TickStyle = tsNone
        OnChange = TbrBlurAngleChange
        ExplicitLeft = 8
        ExplicitTop = 8
        ExplicitWidth = 137
      end
      object CbxBidirectional: TCheckBox
        Left = 0
        Top = 54
        Width = 138
        Height = 17
        Align = alTop
        Caption = 'Bi&directional motion'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 1
        OnClick = PageControlChange
        ExplicitLeft = 12
        ExplicitTop = 22
        ExplicitWidth = 136
      end
    end
    object PanelRadius: TPanel
      Left = 8
      Top = 124
      Width = 138
      Height = 79
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Padding.Top = 8
      TabOrder = 3
      object LblBlurRadius: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 16
        Width = 138
        Height = 15
        Margins.Left = 0
        Margins.Top = 8
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Blur &Radius (10)'
        FocusControl = TbrBlurRadius
        ExplicitLeft = 1
        ExplicitTop = 150
        ExplicitWidth = 90
      end
      object TbrBlurRadius: TTrackBar
        Left = 0
        Top = 31
        Width = 138
        Height = 31
        Align = alTop
        Max = 50
        Position = 10
        TabOrder = 0
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TbrBlurRadiusChange
        ExplicitLeft = 8
        ExplicitTop = 180
      end
      object CheckBoxCorrectGamma: TCheckBox
        Left = 0
        Top = 62
        Width = 138
        Height = 17
        Align = alTop
        Caption = '&Correct Gamma'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = PageControlChange
        ExplicitLeft = 8
        ExplicitTop = 132
      end
    end
  end
  object SbrMain: TStatusBar
    Left = 0
    Top = 386
    Width = 719
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object PageControl: TPageControl
    Left = 154
    Top = 0
    Width = 565
    Height = 386
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    OnChange = PageControlChange
    object TabSheet1: TTabSheet
      Caption = 'Page &1'
      object ImgViewPage1: TImgView32
        Left = 0
        Top = 0
        Width = 557
        Height = 356
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCustom
        Scale = 1.000000000000000000
        ScaleMode = smScale
        Background.CheckersStyle = bcsMedium
        Background.FillStyle = bfsCheckers
        MousePan.Enabled = True
        MouseZoom.Enabled = True
        MouseZoom.Animate = True
        ScrollBars.Increment = 0
        ScrollBars.Size = 16
        ScrollBars.Visibility = svAuto
        OverSize = 0
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Page &2'
      ImageIndex = 1
      object ImgViewPage2: TImgView32
        Left = 0
        Top = 0
        Width = 557
        Height = 356
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCustom
        Scale = 1.000000000000000000
        ScaleMode = smScale
        ScrollBars.Increment = 0
        ScrollBars.Size = 16
        ScrollBars.Visibility = svHidden
        OverSize = 0
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Page &3'
      ImageIndex = 2
      object ImgViewPage3: TImgView32
        Left = 0
        Top = 0
        Width = 557
        Height = 356
        Align = alClient
        Bitmap.DrawMode = dmBlend
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCustom
        Scale = 1.000000000000000000
        ScaleMode = smScale
        Background.CheckersStyle = bcsMedium
        Background.FillStyle = bfsCheckers
        ScrollBars.Increment = 0
        ScrollBars.Size = 16
        ScrollBars.Visibility = svHidden
        OverSize = 0
        TabOrder = 0
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 188
    Top = 116
    object MnuFile: TMenuItem
      Caption = '&File'
      object MnuOpen: TMenuItem
        Caption = '&Open ...'
        OnClick = MnuOpenClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MnuExit: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = MnuExitClick
      end
    end
    object MnuBlurType: TMenuItem
      Caption = 'Bl&ur Type'
      object MnuNone: TMenuItem
        Caption = '&None'
        OnClick = MnuGaussianTypeClick
      end
      object MnuGaussianType: TMenuItem
        Caption = '&Gaussian'
        Checked = True
        OnClick = MnuGaussianTypeClick
      end
      object MnuMotion: TMenuItem
        Caption = '&Motion'
        OnClick = MnuGaussianTypeClick
      end
      object MnuSelective: TMenuItem
        Caption = '&Selective'
        OnClick = MnuGaussianTypeClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'jpg'
    Filter = 'Image Files (*.jpg;*.bmp)|*.jpg;*.bmp;'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 188
    Top = 56
  end
  object TimerUpdate: TTimer
    Enabled = False
    Interval = 200
    OnTimer = TimerUpdateTimer
    Left = 188
    Top = 180
  end
end
