object MainForm: TMainForm
  Left = 378
  Top = 92
  Width = 542
  Height = 616
  Caption = 'Graphics32 Text Rendering'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PnlImage: TPanel
    Left = 169
    Top = 0
    Width = 365
    Height = 570
    Align = alClient
    BevelOuter = bvLowered
    BevelWidth = 2
    TabOrder = 0
    object Img: TImage32
      Left = 2
      Top = 2
      Width = 361
      Height = 566
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      Color = clWhite
      ParentColor = False
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
      OnClick = ImgClick
      OnMouseMove = ImgMouseMove
    end
  end
  object PnlControl: TPanel
    Left = 0
    Top = 0
    Width = 169
    Height = 570
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object LblGammaNote: TLabel
      Left = 16
      Top = 480
      Width = 144
      Height = 52
      Caption = 
        'nb: For the Gamma trackbar to work, the USEGR32GAMMA define in G' +
        'R32.inc must be enabled.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object BtnSelectFont: TButton
      Left = 8
      Top = 8
      Width = 155
      Height = 25
      Caption = 'Select Font...'
      TabOrder = 0
      OnClick = BtnSelectFontClick
    end
    object GbxSettings: TGroupBox
      Left = 10
      Top = 39
      Width = 153
      Height = 271
      Caption = 'Settings'
      TabOrder = 1
      object LblGamma: TLabel
        Left = 9
        Top = 41
        Width = 39
        Height = 13
        Caption = 'Gamma:'
      end
      object LblGammaValue: TLabel
        Left = 112
        Top = 41
        Width = 30
        Height = 13
        Alignment = taCenter
        Caption = '(1.00)'
      end
      object TbrGamma: TTrackBar
        Left = 2
        Top = 58
        Width = 148
        Height = 28
        Max = 240
        Frequency = 30
        Position = 100
        TabOrder = 1
        ThumbLength = 18
        TickStyle = tsNone
        OnChange = TbrGammaChange
      end
      object CbxHinted: TCheckBox
        Left = 9
        Top = 19
        Width = 89
        Height = 17
        Caption = 'Enable hinting'
        TabOrder = 0
        OnClick = CbxHintedClick
      end
      object RgpTextAlign: TRadioGroup
        Left = 8
        Top = 167
        Width = 133
        Height = 93
        Caption = 'Text Alignment'
        ItemIndex = 0
        Items.Strings = (
          'Left'
          'Center'
          'Right'
          'Justified')
        TabOrder = 3
        OnClick = RgpTextAlignClick
      end
      object RgxMethod: TRadioGroup
        Left = 7
        Top = 87
        Width = 133
        Height = 77
        Caption = 'Rendering Method'
        ItemIndex = 0
        Items.Strings = (
          'Default'
          'Cleartype'
          'Cleartype (smooth)')
        TabOrder = 2
        OnClick = RgxMethodClick
      end
    end
    object BtnExit: TButton
      Left = 6
      Top = 536
      Width = 155
      Height = 25
      Cancel = True
      Caption = 'E&xit'
      TabOrder = 2
      OnClick = BtnExitClick
    end
    object PnlZoom: TPanel
      Left = 9
      Top = 320
      Width = 153
      Height = 153
      BevelInner = bvLowered
      BorderWidth = 1
      Caption = 'PnlZoom'
      TabOrder = 3
      object PaintBox32: TPaintBox32
        Left = 3
        Top = 3
        Width = 147
        Height = 147
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 570
    Width = 534
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 32
    Top = 320
  end
end
