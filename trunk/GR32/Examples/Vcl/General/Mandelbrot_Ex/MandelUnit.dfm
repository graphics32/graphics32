object Form1: TForm1
  Left = 393
  Top = 538
  Width = 395
  Height = 399
  Caption = 'Mandelbrot Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Img: TSyntheticImage32
    Left = 0
    Top = 0
    Width = 387
    Height = 350
    Align = alClient
    RepaintMode = rmDirect
    TabOrder = 0
    OnMouseDown = ImgMouseDown
    AutoRasterize = True
    Rasterizer.UpdateRowCount = 0
    BitmapAlign = baTopLeft
    RenderMode = rnmFull
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 16
    object File1: TMenuItem
      Caption = 'File'
      object Save1: TMenuItem
        Caption = 'Save...'
        OnClick = Save1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
      end
    end
    object Rasterizer1: TMenuItem
      Caption = 'Rasterizer'
      object Regularsampling1: TMenuItem
        AutoCheck = True
        Caption = 'Regular Sampling'
        RadioItem = True
        OnClick = RasterizerMenuClick
      end
      object Progressive1: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = 'Progressive'
        Checked = True
        RadioItem = True
        OnClick = RasterizerMenuClick
      end
      object Swizzling1: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Swizzling'
        RadioItem = True
        OnClick = RasterizerMenuClick
      end
      object Tesseral1: TMenuItem
        Tag = 3
        AutoCheck = True
        Caption = 'Tesseral'
        RadioItem = True
        OnClick = RasterizerMenuClick
      end
      object Contour1: TMenuItem
        Tag = 4
        Caption = 'Contour'
        OnClick = RasterizerMenuClick
      end
    end
    object Sampler1: TMenuItem
      Caption = 'SuperSampler'
      object Default1: TMenuItem
        AutoCheck = True
        Caption = 'None'
        Checked = True
        RadioItem = True
        OnClick = Default1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object N2x2: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = 'SuperSampler 2x'
        RadioItem = True
        OnClick = Default1Click
      end
      object N3x2: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'SuperSampler 3x'
        RadioItem = True
        OnClick = Default1Click
      end
      object N4x2: TMenuItem
        Tag = 3
        AutoCheck = True
        Caption = 'SuperSampler 4x'
        RadioItem = True
        OnClick = Default1Click
      end
      object Adaptive: TMenuItem
        AutoCheck = True
        Caption = 'Adaptive'
        Enabled = False
        OnClick = AdaptiveClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object PatternSampler2: TMenuItem
        Tag = 4
        AutoCheck = True
        Caption = 'PatternSampler 2x'
        RadioItem = True
        OnClick = Default1Click
      end
      object PatternSampler3x1: TMenuItem
        Tag = 5
        AutoCheck = True
        Caption = 'PatternSampler 3x'
        RadioItem = True
        OnClick = Default1Click
      end
      object PatternSampler1: TMenuItem
        Tag = 6
        AutoCheck = True
        Caption = 'PatternSampler 4x'
        RadioItem = True
        OnClick = Default1Click
      end
    end
  end
  object SavePictureDialog1: TSavePictureDialog
    Left = 64
    Top = 16
  end
end
