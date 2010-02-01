object FormPolygons: TFormPolygons
  Left = 277
  Top = 106
  Caption = 'Polygons Example'
  ClientHeight = 527
  ClientWidth = 657
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage32
    Left = 0
    Top = 0
    Width = 512
    Height = 527
    Align = alClient
    Bitmap.ResamplerClassName = 'TKernelResampler'
    Bitmap.Resampler.KernelClassName = 'TCubicKernel'
    Bitmap.Resampler.Kernel.Coeff = -0.500000000000000000
    Bitmap.Resampler.KernelMode = kmDynamic
    Bitmap.Resampler.TableSize = 32
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smStretch
    TabOrder = 0
    OnMouseDown = ImageMouseDown
    OnResize = ImageResize
  end
  object Panel1: TPanel
    Left = 512
    Top = 0
    Width = 145
    Height = 527
    Align = alRight
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object lbLineOpacity: TLabel
      Left = 16
      Top = 210
      Width = 63
      Height = 13
      Caption = 'Line Opacity:'
    end
    object lbFillOpacity: TLabel
      Left = 16
      Top = 250
      Width = 52
      Height = 13
      Caption = 'Fill Opacity'
    end
    object lbOutlineThickness: TLabel
      Left = 16
      Top = 394
      Width = 87
      Height = 13
      Caption = 'Outline Thickness:'
    end
    object lbOutlineThicknessValue: TLabel
      Left = 113
      Top = 394
      Width = 14
      Height = 13
      Caption = '(1)'
    end
    object cbAntialiased: TCheckBox
      Left = 16
      Top = 40
      Width = 105
      Height = 17
      Caption = 'Antialiased'
      TabOrder = 0
      OnClick = ParamsChanged
    end
    object LineAlpha: TScrollBar
      Left = 16
      Top = 226
      Width = 121
      Height = 16
      Max = 255
      PageSize = 0
      Position = 255
      TabOrder = 1
      OnChange = ParamsChanged
    end
    object FillAlpha: TScrollBar
      Left = 16
      Top = 266
      Width = 121
      Height = 16
      Max = 255
      PageSize = 0
      Position = 127
      TabOrder = 2
      OnChange = ParamsChanged
    end
    object rgFillMode: TRadioGroup
      Left = 16
      Top = 290
      Width = 121
      Height = 65
      Caption = 'Fill Mode'
      ItemIndex = 0
      Items.Strings = (
        'pfAlternate'
        'pfWinding')
      TabOrder = 3
      OnClick = ParamsChanged
    end
    object btNewLine: TButton
      Left = 24
      Top = 8
      Width = 105
      Height = 25
      Caption = 'New Line'
      TabOrder = 4
      OnClick = btNewLineClick
    end
    object LineThickness: TScrollBar
      Left = 16
      Top = 434
      Width = 121
      Height = 16
      Max = 200
      Min = 1
      PageSize = 0
      Position = 10
      TabOrder = 5
      OnChange = ThicknessChanged
    end
    object ThickOutline: TCheckBox
      Left = 16
      Top = 410
      Width = 65
      Height = 17
      Caption = 'Enabled'
      TabOrder = 6
      OnClick = ThicknessChanged
    end
    object rgAntialiasMode: TRadioGroup
      Left = 16
      Top = 60
      Width = 121
      Height = 85
      Caption = 'Antialias Mode'
      Enabled = False
      ItemIndex = 0
      Items.Strings = (
        'am32times'
        'am16times'
        'am8times'
        'am4times'
        'am2times')
      TabOrder = 7
      OnClick = ParamsChanged
    end
    object Memo1: TMemo
      Left = 16
      Top = 466
      Width = 121
      Height = 47
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      Lines.Strings = (
        'LMB - Add vertex'
        'RMB - Clear')
      ParentFont = False
      ReadOnly = True
      TabOrder = 8
    end
    object Memo2: TMemo
      Left = 16
      Top = 150
      Width = 121
      Height = 57
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      Lines.Strings = (
        'Check Outline Thickness '
        'below to control '
        'antialiasing mode of the '
        'outline.')
      ParentFont = False
      ReadOnly = True
      TabOrder = 9
    end
    object Pattern: TCheckBox
      Left = 16
      Top = 360
      Width = 105
      Height = 17
      Caption = 'Pattern filling'
      TabOrder = 10
      OnClick = ParamsChanged
    end
  end
  object BitmapList: TBitmap32List
    Bitmaps = <
      item
        Bitmap.ResamplerClassName = 'TNearestResampler'
      end
      item
        Bitmap.ResamplerClassName = 'TNearestResampler'
      end>
    Left = 312
    Top = 328
  end
end
