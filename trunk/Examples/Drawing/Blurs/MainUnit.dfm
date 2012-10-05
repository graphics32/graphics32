object FrmBlurs: TFrmBlurs
  Left = 283
  Top = 189
  Width = 727
  Height = 451
  Caption = 'Blurs'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 154
    Height = 386
    Align = alLeft
    TabOrder = 0
    object LblBlurRadius: TLabel
      Left = 12
      Top = 171
      Width = 90
      Height = 15
      Caption = 'Blur &Radius (10)'
      FocusControl = TbrBlurRadius
    end
    object LblBlurAngle: TLabel
      Left = 12
      Top = 251
      Width = 80
      Height = 15
      Caption = 'Blur &Angle (45)'
      Enabled = False
      FocusControl = TbrBlurAngle
    end
    object RgpBlurType: TRadioGroup
      Left = 10
      Top = 28
      Width = 130
      Height = 116
      Caption = 'Blur Type'
      ItemIndex = 1
      Items.Strings = (
        '&None'
        '&Gaussian'
        'Fa&stGaussian'
        '&Motion')
      TabOrder = 0
      OnClick = RgpBlurTypeClick
    end
    object TbrBlurRadius: TTrackBar
      Left = 8
      Top = 195
      Width = 137
      Height = 31
      Max = 50
      Position = 10
      TabOrder = 1
      TickStyle = tsNone
      OnChange = TbrBlurRadiusChange
    end
    object TbrBlurAngle: TTrackBar
      Left = 8
      Top = 275
      Width = 137
      Height = 31
      Enabled = False
      Max = 180
      Min = -180
      Position = 45
      TabOrder = 2
      TickStyle = tsNone
      OnChange = TbrBlurAngleChange
    end
    object CbxBidirectional: TCheckBox
      Left = 12
      Top = 307
      Width = 136
      Height = 17
      Caption = 'Bi&directional motion'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 3
      OnClick = PageControl1Change
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
  object PageControl1: TPageControl
    Left = 154
    Top = 0
    Width = 565
    Height = 386
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    OnChange = PageControl1Change
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
        ScrollBars.ShowHandleGrip = True
        ScrollBars.Style = rbsDefault
        ScrollBars.Size = 16
        ScrollBars.Visibility = svHidden
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
        ScrollBars.ShowHandleGrip = True
        ScrollBars.Style = rbsDefault
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
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCustom
        Scale = 1.000000000000000000
        ScaleMode = smScale
        ScrollBars.ShowHandleGrip = True
        ScrollBars.Style = rbsDefault
        ScrollBars.Size = 16
        ScrollBars.Visibility = svHidden
        OverSize = 0
        TabOrder = 0
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 116
    Top = 328
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open ...'
        OnClick = Open1Click
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
    object BlurType1: TMenuItem
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
      object MnuFastGaussian: TMenuItem
        Caption = 'F&astGaussian'
        OnClick = MnuGaussianTypeClick
      end
      object MnuMotion: TMenuItem
        Caption = '&Motion'
        OnClick = MnuGaussianTypeClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'png'
    Filter = 
      'All (*.png;*.jpg;*.bmp)|*.png;*.jpg;*.bmp;|PNG Files (*.png)|*.p' +
      'ng|JPG Files (*.jpg)|*.jpg|Bitmap Files (*.bmp)|*.bmp'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 120
    Top = 288
  end
end
