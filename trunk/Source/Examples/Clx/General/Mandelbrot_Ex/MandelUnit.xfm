object Form1: TForm1
  Left = 388
  Top = 137
  Width = 468
  Height = 439
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
  PixelsPerInch = 96
  TextHeight = 13
  object Img: TSyntheticImage32
    Left = 0
    Top = 0
    Width = 460
    Height = 390
    Align = alClient
    RepaintMode = rmDirect
    TabOrder = 0
    OnMouseDown = ImgMouseDown
    AutoRasterize = True
    Rasterizer.UpdateRowCount = 0
    Color = clBlack
    ClearBuffer = True
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
        Caption = 'Regular Sampling'
        RadioItem = True
        OnClick = RasterizerMenuClick
      end
      object Progressive1: TMenuItem
        Tag = 1
        Caption = 'Progressive'
        Checked = True
        RadioItem = True
        OnClick = RasterizerMenuClick
      end
      object Swizzling1: TMenuItem
        Tag = 2
        Caption = 'Swizzling'
        RadioItem = True
        OnClick = RasterizerMenuClick
      end
      object Tesseral1: TMenuItem
        Tag = 3
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
        Caption = 'SuperSampler 2x'
        RadioItem = True
        OnClick = Default1Click
      end
      object N3x2: TMenuItem
        Tag = 2
        Caption = 'SuperSampler 3x'
        RadioItem = True
        OnClick = Default1Click
      end
      object N4x2: TMenuItem
        Tag = 3
        Caption = 'SuperSampler 4x'
        RadioItem = True
        OnClick = Default1Click
      end
      object Adaptive: TMenuItem
        Caption = 'Adaptive'
        Enabled = False
        OnClick = AdaptiveClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object PatternSampler2: TMenuItem
        Tag = 4
        Caption = 'PatternSampler 2x'
        RadioItem = True
        OnClick = Default1Click
      end
      object PatternSampler3x1: TMenuItem
        Tag = 5
        Caption = 'PatternSampler 3x'
        RadioItem = True
        OnClick = Default1Click
      end
      object PatternSampler1: TMenuItem
        Tag = 6
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
