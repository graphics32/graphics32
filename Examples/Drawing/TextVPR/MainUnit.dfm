object MainForm: TMainForm
  Left = 350
  Top = 156
  Caption = 'Graphics32 Text Rendering'
  ClientHeight = 484
  ClientWidth = 775
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  TextHeight = 13
  object PnlImage: TPanel
    Left = 341
    Top = 0
    Width = 434
    Height = 461
    Align = alClient
    BevelOuter = bvLowered
    BevelWidth = 2
    ParentBackground = False
    TabOrder = 0
    object Img: TImage32
      Left = 2
      Top = 2
      Width = 430
      Height = 457
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
    Height = 461
    Align = alLeft
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    object GroupBoxRendering: TGroupBox
      Left = 157
      Top = 0
      Width = 184
      Height = 461
      Align = alClient
      Caption = ' Rendering '
      ParentBackground = False
      TabOrder = 0
      object RadioGroupMethod: TRadioGroup
        Left = 0
        Top = 177
        Width = 180
        Height = 98
        Align = alTop
        Caption = ' Rendering Method '
        ItemIndex = 0
        Items.Strings = (
          'Default'
          'Cleartype'
          'Cleartype (smooth)')
        ParentBackground = False
        TabOrder = 0
        OnClick = RadioGroupMethodClick
      end
      object PnlZoom: TPanel
        Left = 0
        Top = 275
        Width = 180
        Height = 168
        Align = alClient
        BevelInner = bvLowered
        BorderWidth = 1
        Caption = ' Rendering '
        ParentBackground = False
        TabOrder = 1
        object PaintBox32: TPaintBox32
          Left = 3
          Top = 3
          Width = 174
          Height = 162
          Align = alClient
          TabOrder = 0
        end
      end
      object RadioGroupHinting: TRadioGroup
        Left = 0
        Top = 0
        Width = 180
        Height = 92
        Align = alTop
        Caption = ' Hinting '
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Hinting (No Horz)'
          'Hinting')
        ParentBackground = False
        TabOrder = 2
        OnClick = RadioGroupHintingClick
      end
      object GroupBoxGamma: TGroupBox
        Left = 0
        Top = 92
        Width = 180
        Height = 85
        Align = alTop
        Caption = 'Gamma'
        ParentBackground = False
        TabOrder = 3
        DesignSize = (
          180
          85)
        object LblGamma: TLabel
          Left = 7
          Top = 20
          Width = 39
          Height = 13
          Caption = 'Gamma:'
        end
        object LblGammaValue: TLabel
          Left = 52
          Top = 20
          Width = 30
          Height = 13
          Caption = '(1.00)'
        end
        object TbrGamma: TTrackBar
          Left = 1
          Top = 39
          Width = 173
          Height = 28
          Anchors = [akLeft, akTop, akRight]
          Max = 3000
          Min = 30
          Frequency = 300
          Position = 1000
          TabOrder = 0
          TickMarks = tmBoth
          TickStyle = tsNone
          OnChange = TbrGammaChange
        end
      end
    end
    object PanelLeft: TPanel
      Left = 0
      Top = 0
      Width = 157
      Height = 461
      Align = alLeft
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
      object GroupBoxFont: TGroupBox
        Left = 0
        Top = 0
        Width = 157
        Height = 85
        Align = alTop
        Caption = ' Font '
        ParentBackground = False
        TabOrder = 0
        object LblFontInfo: TLabel
          Left = 0
          Top = 0
          Width = 153
          Height = 30
          Align = alTop
          Alignment = taCenter
          AutoSize = False
          Caption = 'FontInfo'
          ShowAccelChar = False
        end
        object ButtonSelectFont: TButton
          Left = 0
          Top = 30
          Width = 153
          Height = 25
          Align = alTop
          Caption = 'Select Font...'
          TabOrder = 0
          OnClick = ButtonSelectFontClick
        end
      end
      object GroupBoxLayout: TGroupBox
        Left = 0
        Top = 85
        Width = 157
        Height = 351
        Align = alClient
        Caption = ' Layout '
        ParentBackground = False
        TabOrder = 1
        object RadioGroupHorizontalAlign: TRadioGroup
          Left = 0
          Top = 0
          Width = 153
          Height = 117
          Align = alTop
          Caption = ' Horizontal '
          ItemIndex = 0
          Items.Strings = (
            'Left'
            'Center'
            'Right'
            'Justified')
          ParentBackground = False
          TabOrder = 0
          OnClick = RadioGroupHorizontalAlignClick
        end
        object RadioGroupVerticalAlign: TRadioGroup
          Left = 0
          Top = 117
          Width = 153
          Height = 92
          Align = alTop
          Caption = ' Vertical '
          ItemIndex = 0
          Items.Strings = (
            'Top'
            'Center'
            'Bottom')
          ParentBackground = False
          TabOrder = 1
          OnClick = RadioGroupHorizontalAlignClick
        end
        object CheckBoxSingleLine: TCheckBox
          Left = 0
          Top = 209
          Width = 153
          Height = 17
          Align = alTop
          Caption = 'Single line only'
          TabOrder = 2
          OnClick = RadioGroupHorizontalAlignClick
        end
        object CheckBoxWordbreak: TCheckBox
          Left = 0
          Top = 226
          Width = 153
          Height = 17
          Align = alTop
          Caption = 'Word break'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = RadioGroupHorizontalAlignClick
        end
      end
      object ButtonExit: TButton
        Left = 0
        Top = 436
        Width = 157
        Height = 25
        Align = alBottom
        Cancel = True
        Caption = 'E&xit'
        TabOrder = 2
        OnClick = ButtonExitClick
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 461
    Width = 775
    Height = 23
    Panels = <>
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
