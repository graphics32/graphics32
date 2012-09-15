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
  object PanelControl: TPanel
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
    object LblLineOpacity: TLabel
      Left = 16
      Top = 47
      Width = 59
      Height = 13
      Caption = 'Line Opacity'
    end
    object LblFillOpacity: TLabel
      Left = 16
      Top = 87
      Width = 52
      Height = 13
      Caption = 'Fill Opacity'
    end
    object LblOutlineThickness: TLabel
      Left = 16
      Top = 267
      Width = 50
      Height = 13
      Caption = 'Thickness:'
    end
    object LblOutlineThicknessValue: TLabel
      Left = 72
      Top = 267
      Width = 14
      Height = 13
      Caption = '(1)'
    end
    object LblMiterLimit: TLabel
      Left = 16
      Top = 396
      Width = 48
      Height = 13
      Caption = 'Miter Limit'
    end
    object LineAlpha: TScrollBar
      Left = 16
      Top = 63
      Width = 121
      Height = 16
      Max = 255
      PageSize = 0
      Position = 255
      TabOrder = 0
      OnChange = LineAlphaChange
    end
    object FillAlpha: TScrollBar
      Left = 16
      Top = 103
      Width = 121
      Height = 16
      Max = 255
      PageSize = 0
      Position = 127
      TabOrder = 1
      OnChange = FillAlphaChange
    end
    object RgpFillMode: TRadioGroup
      Left = 16
      Top = 126
      Width = 121
      Height = 65
      Caption = 'Fill Mode'
      ItemIndex = 0
      Items.Strings = (
        'pfAlternate'
        'pfWinding')
      TabOrder = 2
      OnClick = FillModeChange
    end
    object BtnNewLine: TButton
      Left = 24
      Top = 8
      Width = 105
      Height = 25
      Caption = 'New Line'
      TabOrder = 3
      OnClick = BtnNewLineClick
    end
    object LineThickness: TScrollBar
      Left = 16
      Top = 284
      Width = 121
      Height = 16
      Max = 200
      Min = 1
      PageSize = 0
      Position = 10
      TabOrder = 4
      OnChange = ThicknessChanged
    end
    object CbxThickOutline: TCheckBox
      Left = 16
      Top = 244
      Width = 97
      Height = 17
      Caption = 'Outline Enabled'
      TabOrder = 5
      OnClick = ThickOutlineChange
    end
    object MemoHint: TMemo
      Left = 16
      Top = 471
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
      TabOrder = 6
    end
    object CbxPattern: TCheckBox
      Left = 16
      Top = 197
      Width = 105
      Height = 17
      Caption = 'Pattern filling'
      TabOrder = 7
      OnClick = PatternFillingChange
    end
    object RgpJointMode: TRadioGroup
      Left = 16
      Top = 306
      Width = 121
      Height = 84
      Caption = 'Joint Mode'
      Enabled = False
      ItemIndex = 0
      Items.Strings = (
        'jmMiter'
        'jmBevel'
        'jmRound')
      TabOrder = 8
      OnClick = JointModeChange
    end
    object MiterLimit: TScrollBar
      Left = 16
      Top = 412
      Width = 121
      Height = 16
      Max = 255
      Min = 1
      PageSize = 0
      Position = 127
      TabOrder = 9
      OnChange = MiterLimitChange
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
