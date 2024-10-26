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
    Height = 382
    Align = alLeft
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object RgpBlurType: TRadioGroup
      Left = 0
      Top = 0
      Width = 154
      Height = 116
      Align = alTop
      Caption = 'Blur Type'
      ItemIndex = 0
      Items.Strings = (
        '&None'
        '&Gaussian'
        '&Motion'
        '&Selective')
      ParentBackground = False
      TabOrder = 0
      OnClick = RgpBlurTypeClick
    end
    object PanelSelective: TPanel
      Left = 0
      Top = 181
      Width = 154
      Height = 46
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
      object LabelDelta: TLabel
        Left = 0
        Top = 0
        Width = 54
        Height = 15
        Align = alTop
        Caption = 'Delta (10)'
        FocusControl = TrackBarDelta
      end
      object TrackBarDelta: TTrackBar
        Left = 0
        Top = 15
        Width = 154
        Height = 31
        Align = alTop
        Max = 255
        PageSize = 10
        Position = 10
        TabOrder = 0
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TrackBarDeltaChange
      end
    end
    object PanelMotion: TPanel
      Left = 0
      Top = 227
      Width = 154
      Height = 65
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 2
      object LblBlurAngle: TLabel
        Left = 0
        Top = 0
        Width = 80
        Height = 15
        Align = alTop
        Caption = 'Blur &Angle (45)'
        Enabled = False
        FocusControl = TbrBlurAngle
      end
      object TbrBlurAngle: TTrackBar
        Left = 0
        Top = 15
        Width = 154
        Height = 31
        Align = alTop
        Enabled = False
        Max = 180
        Min = -180
        Position = 45
        TabOrder = 0
        TickStyle = tsNone
        OnChange = TbrBlurAngleChange
      end
      object CbxBidirectional: TCheckBox
        Left = 0
        Top = 46
        Width = 154
        Height = 19
        Align = alTop
        Caption = 'Bi&directional motion'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 1
        OnClick = PageControlChange
      end
    end
    object PanelRadius: TPanel
      Left = 0
      Top = 116
      Width = 154
      Height = 65
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 3
      object LblBlurRadius: TLabel
        Left = 0
        Top = 0
        Width = 90
        Height = 15
        Align = alTop
        Caption = 'Blur &Radius (10)'
        FocusControl = TbrBlurRadius
      end
      object TbrBlurRadius: TTrackBar
        Left = 0
        Top = 15
        Width = 154
        Height = 31
        Align = alTop
        Max = 50
        Position = 10
        TabOrder = 0
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TbrBlurRadiusChange
      end
      object CheckBoxCorrectGamma: TCheckBox
        Left = 0
        Top = 46
        Width = 154
        Height = 19
        Align = alTop
        Caption = '&Correct Gamma'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = PageControlChange
      end
    end
  end
  object SbrMain: TStatusBar
    Left = 0
    Top = 382
    Width = 719
    Height = 23
    Panels = <>
    SimplePanel = True
  end
  object PageControl: TPageControl
    Left = 154
    Top = 0
    Width = 565
    Height = 382
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
        Height = 354
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
        Height = 354
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
        Height = 354
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
    DefaultExt = '.jpg'
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
