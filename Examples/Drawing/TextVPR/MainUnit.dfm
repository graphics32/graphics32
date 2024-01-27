object MainForm: TMainForm
  Left = 220
  Top = 169
  Caption = 'Graphics32 Text Rendering'
  ClientHeight = 484
  ClientWidth = 775
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
    Left = 341
    Top = 0
    Width = 434
    Height = 465
    Align = alClient
    BevelOuter = bvLowered
    BevelWidth = 2
    TabOrder = 0
    object Img: TImage32
      Left = 2
      Top = 2
      Width = 430
      Height = 461
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
    Width = 341
    Height = 465
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object GbxRendering: TGroupBox
      Left = 172
      Top = 8
      Width = 153
      Height = 446
      Caption = ' Rendering '
      TabOrder = 2
      object LblGamma: TLabel
        Left = 13
        Top = 141
        Width = 39
        Height = 13
        Caption = 'Gamma:'
      end
      object LblGammaValue: TLabel
        Left = 108
        Top = 141
        Width = 30
        Height = 13
        Alignment = taCenter
        Caption = '(1.00)'
      end
      object TbrGamma: TTrackBar
        Left = 5
        Top = 158
        Width = 141
        Height = 28
        Max = 3000
        Min = 30
        Frequency = 300
        Position = 1000
        TabOrder = 0
        ThumbLength = 18
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = TbrGammaChange
      end
      object RgxMethod: TRadioGroup
        Left = 12
        Top = 196
        Width = 128
        Height = 98
        Caption = ' Rendering Method '
        ItemIndex = 0
        Items.Strings = (
          'Default'
          'Cleartype'
          'Cleartype (smooth)')
        TabOrder = 1
        OnClick = RgxMethodClick
      end
      object PnlZoom: TPanel
        Left = 12
        Top = 306
        Width = 128
        Height = 128
        BevelInner = bvLowered
        BorderWidth = 1
        Caption = ' Rendering '
        TabOrder = 2
        object PaintBox32: TPaintBox32
          Left = 3
          Top = 3
          Width = 122
          Height = 122
          Align = alClient
          TabOrder = 0
        end
      end
      object RgpHinting: TRadioGroup
        Left = 11
        Top = 26
        Width = 131
        Height = 92
        Caption = ' Hinting '
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Hinting (No Horz)'
          'Hinting')
        TabOrder = 3
        OnClick = RgpHintingClick
      end
    end
    object BtnExit: TButton
      Left = 8
      Top = 429
      Width = 153
      Height = 25
      Cancel = True
      Caption = 'E&xit'
      TabOrder = 3
      OnClick = BtnExitClick
    end
    object GBxFont: TGroupBox
      Left = 8
      Top = 8
      Width = 153
      Height = 105
      Caption = ' Font '
      TabOrder = 0
      object LblFontInfo: TLabel
        Left = 2
        Top = 25
        Width = 148
        Height = 30
        Alignment = taCenter
        AutoSize = False
        Caption = 'FontInfo'
      end
      object BtnSelectFont: TButton
        Left = 11
        Top = 62
        Width = 133
        Height = 25
        Caption = 'Select Font...'
        TabOrder = 0
        OnClick = BtnSelectFontClick
      end
    end
    object GbxLayout: TGroupBox
      Left = 8
      Top = 121
      Width = 155
      Height = 297
      Caption = ' Layout '
      TabOrder = 1
      object RgpHorzAlign: TRadioGroup
        Left = 11
        Top = 21
        Width = 131
        Height = 117
        Caption = ' Horizontal '
        ItemIndex = 0
        Items.Strings = (
          'Left'
          'Center'
          'Right'
          'Justified')
        TabOrder = 0
        OnClick = RgpHorzAlignClick
      end
      object RgpVerticalAlign: TRadioGroup
        Left = 11
        Top = 144
        Width = 131
        Height = 92
        Caption = ' Vertical '
        ItemIndex = 0
        Items.Strings = (
          'Top'
          'Center'
          'Bottom')
        TabOrder = 1
        OnClick = RgpHorzAlignClick
      end
      object CbxSingleLine: TCheckBox
        Left = 11
        Top = 247
        Width = 97
        Height = 17
        Caption = 'Single line only'
        TabOrder = 2
        OnClick = RgpHorzAlignClick
      end
      object CbxWordbreak: TCheckBox
        Left = 11
        Top = 269
        Width = 97
        Height = 17
        Caption = 'Word break'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = RgpHorzAlignClick
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 465
    Width = 775
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = '  Note: The Gamma trackbar currently does nothing.'
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdTrueTypeOnly, fdEffects]
    Left = 384
    Top = 48
  end
end
