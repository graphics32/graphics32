object MainForm: TMainForm
  Left = 388
  Top = 137
  Caption = 'Mandelbrot Example'
  ClientHeight = 412
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = ImgMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Img: TSyntheticImage32
    Left = 0
    Top = 0
    Width = 460
    Height = 412
    Align = alClient
    RepaintMode = rmDirect
    TabOrder = 0
    OnMouseDown = ImgMouseDown
    OnMouseWheel = ImgMouseWheel
    AutoRasterize = True
    Rasterizer.UpdateRowCount = 0
    Color = clBlack
    ClearBuffer = True
    RenderMode = rnmFull
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 16
    object miFile: TMenuItem
      Caption = '&File'
      object miSave: TMenuItem
        Caption = '&Save...'
        OnClick = miSaveClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Caption = 'E&xit'
        OnClick = miExitClick
      end
    end
    object miRasterizer: TMenuItem
      Caption = '&Rasterizer'
      object miRegularSampling: TMenuItem
        AutoCheck = True
        Caption = '&Regular'
        RadioItem = True
        OnClick = miRasterizerClick
      end
      object miProgressive: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = '&Progressive'
        Checked = True
        RadioItem = True
        OnClick = miRasterizerClick
      end
      object miSwizzling: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = '&Swizzling'
        RadioItem = True
        OnClick = miRasterizerClick
      end
      object miTesseral: TMenuItem
        Tag = 3
        AutoCheck = True
        Caption = '&Tesseral'
        RadioItem = True
        OnClick = miRasterizerClick
      end
      object miContour: TMenuItem
        Tag = 4
        AutoCheck = True
        Caption = '&Contour'
        RadioItem = True
        OnClick = miRasterizerClick
      end
      object miMultithreadedRegularRasterizer: TMenuItem
        Tag = 5
        AutoCheck = True
        Caption = '&Multithreaded Regular'
        RadioItem = True
        OnClick = miRasterizerClick
      end
    end
    object miSuperSampler: TMenuItem
      Caption = '&SuperSampler'
      object miDefault: TMenuItem
        AutoCheck = True
        Caption = '&None'
        Checked = True
        RadioItem = True
        OnClick = miDefaultClick
      end
      object N5: TMenuItem
        AutoCheck = True
        Caption = '-'
      end
      object miSuperSampler2x: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = 'SuperSampler &2x'
        RadioItem = True
        OnClick = miDefaultClick
      end
      object miSuperSampler3x: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'SuperSampler &3x'
        RadioItem = True
        OnClick = miDefaultClick
      end
      object miSuperSampler4x: TMenuItem
        Tag = 3
        AutoCheck = True
        Caption = 'SuperSampler &4x'
        RadioItem = True
        OnClick = miDefaultClick
      end
      object miAdaptive: TMenuItem
        AutoCheck = True
        Caption = 'Adaptive'
        Enabled = False
        OnClick = miAdaptiveClick
      end
      object N2: TMenuItem
        AutoCheck = True
        Caption = '-'
      end
      object miPatternSampler2x: TMenuItem
        Tag = 4
        AutoCheck = True
        Caption = 'PatternSampler 2x'
        RadioItem = True
        OnClick = miDefaultClick
      end
      object miPatternSampler3x: TMenuItem
        Tag = 5
        AutoCheck = True
        Caption = 'PatternSampler 3x'
        RadioItem = True
        OnClick = miDefaultClick
      end
      object miPatternSampler4x: TMenuItem
        Tag = 6
        AutoCheck = True
        Caption = 'PatternSampler 4x'
        RadioItem = True
        OnClick = miDefaultClick
      end
    end
    object miMaxIterations: TMenuItem
      Caption = 'Max. &Iterations'
      object miMaxIterations50: TMenuItem
        Tag = 50
        Caption = '5&0'
        RadioItem = True
        OnClick = miMaxIterationsClick
      end
      object miMaxIterations160: TMenuItem
        Tag = 160
        Caption = '&160'
        RadioItem = True
        OnClick = miMaxIterationsClick
      end
      object miMaxIterations256: TMenuItem
        Tag = 256
        Caption = '&256'
        RadioItem = True
        OnClick = miMaxIterationsClick
      end
      object miMaxIterations320: TMenuItem
        Tag = 320
        Caption = '&320'
        Checked = True
        RadioItem = True
        OnClick = miMaxIterationsClick
      end
      object miMaxIterations512: TMenuItem
        Tag = 512
        Caption = '&512'
        RadioItem = True
        OnClick = miMaxIterationsClick
      end
    end
    object miPalette: TMenuItem
      Caption = '&Palette'
      object miPaletteDefault: TMenuItem
        Caption = '&Default'
        Checked = True
        RadioItem = True
        OnClick = miPaletteClick
      end
      object miPaletteRainbow: TMenuItem
        Tag = 1
        Caption = '&Rainbow'
        RadioItem = True
        OnClick = miPaletteClick
      end
      object miPaletteMonochrome: TMenuItem
        Tag = 2
        Caption = '&Monochrome'
        RadioItem = True
        OnClick = miPaletteClick
      end
      object miPaletteSimple: TMenuItem
        Tag = 3
        Caption = '&Simple'
        RadioItem = True
        OnClick = miPaletteClick
      end
    end
  end
  object SavePictureDialog: TSavePictureDialog
    Left = 112
    Top = 16
  end
end
