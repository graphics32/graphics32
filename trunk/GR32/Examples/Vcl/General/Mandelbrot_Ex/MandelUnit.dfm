object Form1: TForm1
  Left = 381
  Top = 212
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Img: TSyntheticImage32
    Left = 0
    Top = 0
    Width = 387
    Height = 349
    Align = alClient
    RepaintMode = rmDirect
    TabOrder = 0
    OnMouseDown = ImgMouseDown
    AutoRasterize = True
    Rasterizer.UpdateRowCount = 0
    BitmapAlign = baTopLeft
    RenderMode = rmFull
  end
  object MainMenu1: TMainMenu
    Left = 48
    Top = 72
    object File1: TMenuItem
      Caption = 'File'
      object Save1: TMenuItem
        Caption = 'Save...'
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
        Checked = True
        RadioItem = True
        OnClick = Regularsampling1Click
      end
      object Progressive1: TMenuItem
        AutoCheck = True
        Caption = 'Progressive'
        RadioItem = True
        OnClick = Progressive1Click
      end
      object Tesseral1: TMenuItem
        AutoCheck = True
        Caption = 'Tesseral'
        RadioItem = True
        OnClick = Tesseral1Click
      end
      object Swizzling1: TMenuItem
        AutoCheck = True
        Caption = 'Swizzling'
        RadioItem = True
        OnClick = Swizzling1Click
      end
      object Contour1: TMenuItem
        Caption = 'Contour'
        OnClick = Contour1Click
      end
    end
    object Sampler1: TMenuItem
      Caption = 'Sampler'
      object Default1: TMenuItem
        Caption = 'Default'
        Checked = True
        RadioItem = True
        OnClick = Default1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Supersampler1: TMenuItem
        Caption = 'Supersampler'
        RadioItem = True
        object N2x2: TMenuItem
          AutoCheck = True
          Caption = '2x'
          Checked = True
          RadioItem = True
          OnClick = N2x2Click
        end
        object N3x2: TMenuItem
          AutoCheck = True
          Caption = '3x'
          RadioItem = True
          OnClick = N3x2Click
        end
        object N4x2: TMenuItem
          AutoCheck = True
          Caption = '4x'
          RadioItem = True
          OnClick = N4x2Click
        end
        object N6: TMenuItem
          Caption = '-'
        end
        object Adaptive2: TMenuItem
          AutoCheck = True
          Caption = 'Adaptive'
          OnClick = Adaptive2Click
        end
      end
      object PatternSampler1: TMenuItem
        Caption = 'PatternSampler'
      end
    end
  end
end
