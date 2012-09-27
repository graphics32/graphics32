object FrmGradientSampler: TFrmGradientSampler
  Left = 0
  Top = 0
  Caption = 'Gradient Sampler'
  ClientHeight = 400
  ClientWidth = 400
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
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox32: TPaintBox32
    Left = 0
    Top = 0
    Width = 400
    Height = 400
    Align = alClient
    RepaintMode = rmOptimizer
    TabOrder = 0
    OnDblClick = PaintBox32DblClick
    OnMouseDown = PaintBox32MouseDown
    OnMouseMove = PaintBox32MouseMove
    OnMouseUp = PaintBox32MouseUp
    OnPaintBuffer = PaintBox32PaintBuffer
  end
  object MainMenu: TMainMenu
    Left = 88
    Top = 96
    object MnuFile: TMenuItem
      Caption = '&File'
      object MnuClose: TMenuItem
        Caption = '&Close'
        OnClick = MnuCloseClick
      end
    end
    object MnuGradient: TMenuItem
      Caption = '&Gradient'
      object MnuGradientRadial: TMenuItem
        Caption = '&Circular'
        RadioItem = True
        OnClick = MnuGradientRadialClick
      end
      object MnuGradientConic: TMenuItem
        Caption = 'C&onic'
        RadioItem = True
        OnClick = MnuGradientConicClick
      end
      object MnuGradientDiamond: TMenuItem
        Caption = '&Diamond'
        Checked = True
        RadioItem = True
        OnClick = MnuGradientDiamondClick
      end
      object MnuGradientXY: TMenuItem
        Caption = '&XY'
        RadioItem = True
        OnClick = MnuGradientXYClick
      end
      object MnuGradientXYSqrt: TMenuItem
        Caption = 'XY &Sqrt'
        RadioItem = True
        OnClick = MnuGradientXYSqrtClick
      end
      object MnuGradientLinear: TMenuItem
        Caption = '&Linear'
        RadioItem = True
        OnClick = MnuGradientLinearClick
      end
      object MnuGradientCustom: TMenuItem
        Caption = 'C&ustom'
        RadioItem = True
        OnClick = MnuGradientCustomClick
      end
    end
    object MnuWrapMode: TMenuItem
      Caption = '&Wrap Mode'
      object MnuWrapModeClamp: TMenuItem
        Caption = '&Clamp'
        RadioItem = True
        OnClick = MnuWrapModeClampClick
      end
      object MnuWrapModeRepeat: TMenuItem
        Caption = '&Repeat'
        RadioItem = True
        OnClick = MnuWrapModeRepeatClick
      end
      object MnuWrapModeMirror: TMenuItem
        Caption = '&Mirror'
        Checked = True
        RadioItem = True
        OnClick = MnuWrapModeMirrorClick
      end
    end
    object MnuBackground: TMenuItem
      Caption = '&Background Gradient'
      object MnuBackgroundGradientTriangular: TMenuItem
        Caption = '&Triangular'
        Checked = True
        RadioItem = True
        OnClick = MnuBackgroundGradientTriangularClick
      end
      object MnuBackgroundGradientShepards: TMenuItem
        Caption = 'Shepards'
        RadioItem = True
        OnClick = MnuBackgroundGradientShepardsClick
      end
      object MnuBackgroundGradientCustomIDW: TMenuItem
        Caption = 'Custom Inverse Distance Weighting Gradient'
        RadioItem = True
        OnClick = MnuBackgroundGradientCustomIDWClick
      end
      object MnuBackgroundGradientVoronoi: TMenuItem
        Caption = '&Voronoi Gradient'
        RadioItem = True
        OnClick = MnuBackgroundGradientVoronoiClick
      end
    end
  end
  object Animation: TTimer
    Enabled = False
    Interval = 30
    OnTimer = AnimationTimer
    Left = 200
    Top = 96
  end
end
