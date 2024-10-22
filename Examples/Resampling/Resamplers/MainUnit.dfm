object FrmResamplersExample: TFrmResamplersExample
  Left = 270
  Top = 228
  Caption = 'Resamplers Example'
  ClientHeight = 395
  ClientWidth = 475
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 200
  ParentFont = True
  TextHeight = 15
  object SidePanel: TPanel
    Left = 329
    Top = 0
    Width = 146
    Height = 376
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object PnlResampler: TPanel
      Left = 0
      Top = 0
      Width = 146
      Height = 160
      Align = alTop
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        142
        156)
      object LblResamplersClass: TLabel
        Left = 8
        Top = 24
        Width = 88
        Height = 15
        Anchors = [akTop, akRight]
        Caption = 'Resampler Class:'
        ExplicitLeft = 12
      end
      object LblPixelAccessMode: TLabel
        Left = 8
        Top = 67
        Width = 101
        Height = 15
        Anchors = [akTop, akRight]
        Caption = 'Pixel Access Mode:'
        ExplicitLeft = 12
      end
      object LblWrapMode: TLabel
        Left = 8
        Top = 110
        Width = 65
        Height = 15
        Anchors = [akTop, akRight]
        Caption = 'Wrap Mode:'
        ExplicitLeft = 12
      end
      object PnlResamplerProperties: TPanel
        Left = 0
        Top = 0
        Width = 142
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Resampler Properties'
        Color = clHighlight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHighlightText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
      end
      object ComboBoxResamplerClassName: TComboBox
        Left = 12
        Top = 40
        Width = 119
        Height = 23
        Style = csDropDownList
        Anchors = [akTop, akRight]
        TabOrder = 1
        OnChange = ComboBoxResamplerClassNameChange
      end
      object ComboBoxPixelAccessMode: TComboBox
        Left = 12
        Top = 83
        Width = 119
        Height = 23
        Style = csDropDownList
        Anchors = [akTop, akRight]
        TabOrder = 2
        OnChange = ComboBoxPixelAccessModeChange
        Items.Strings = (
          'Unsafe'
          'Safe'
          'Wrap')
      end
      object ComboBoxWrapMode: TComboBox
        Left = 12
        Top = 126
        Width = 119
        Height = 23
        Style = csDropDownList
        Anchors = [akTop, akRight]
        TabOrder = 3
        OnChange = ComboBoxPixelAccessModeChange
        Items.Strings = (
          'Clamp'
          'Repeat'
          'Mirror')
      end
    end
    object PanelKernel: TPanel
      Left = 0
      Top = 160
      Width = 146
      Height = 201
      Align = alTop
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      DesignSize = (
        142
        197)
      object LblKernelClass: TLabel
        Left = 8
        Top = 24
        Width = 66
        Height = 15
        Anchors = [akTop, akRight]
        Caption = 'Kernel Class:'
        ExplicitLeft = 12
      end
      object LblKernelMode: TLabel
        Left = 8
        Top = 67
        Width = 70
        Height = 15
        Anchors = [akTop, akRight]
        Caption = 'Kernel Mode:'
        ExplicitLeft = 12
      end
      object LblTableSize: TLabel
        Left = 8
        Top = 116
        Width = 99
        Height = 15
        Caption = 'Table Size (32/100):'
      end
      object LblParameter: TLabel
        Left = 8
        Top = 155
        Width = 57
        Height = 15
        Caption = 'Parameter:'
        Visible = False
      end
      object PnlKernelProperties: TPanel
        Left = 0
        Top = 0
        Width = 142
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Kernel Properties'
        Color = clHighlight
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHighlightText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
      end
      object ComboBoxKernelClassName: TComboBox
        Left = 12
        Top = 40
        Width = 119
        Height = 23
        Style = csDropDownList
        Anchors = [akTop, akRight]
        TabOrder = 1
        OnChange = ComboBoxKernelClassNameChange
      end
      object ComboBoxKernelMode: TComboBox
        Left = 12
        Top = 83
        Width = 119
        Height = 23
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemIndex = 0
        TabOrder = 2
        Text = 'Default (precise, slow)'
        OnChange = ComboBoxKernelModeChange
        Items.Strings = (
          'Default (precise, slow)'
          'Table Nearest (truncated, fastest)'
          'Table Linear (interpolated, fast)')
      end
      object GaugeBarTableSize: TGaugeBar
        Left = 16
        Top = 136
        Width = 113
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Min = 1
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 32
        OnChange = GaugeBarTableSizeChange
      end
      object GaugeBarParameter: TGaugeBar
        Left = 16
        Top = 175
        Width = 113
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Min = 1
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Visible = False
        Position = 50
        OnChange = GaugeBarParameterChange
      end
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 329
    Height = 376
    Hint = 'Downsampling using StretchTransfer resampling'
    ActivePage = TabManual
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnChange = ComboBoxResamplerClassNameChange
    object TabManual: TTabSheet
      Caption = 'Manual'
      object ImagePattern: TImage32
        Left = 0
        Top = 0
        Width = 321
        Height = 346
        Hint = 'Upsampling using manual resampling'
        Align = alClient
        Bitmap.ResamplerClassName = 'TKernelResampler'
        Bitmap.Resampler.KernelClassName = 'TCosineKernel'
        Bitmap.Resampler.KernelMode = kmTableLinear
        Bitmap.Resampler.TableSize = 32
        BitmapAlign = baTopLeft
        RepaintMode = rmOptimizer
        Scale = 1.000000000000000000
        ScaleMode = smStretch
        TabOrder = 0
        OnResize = ImagePatternResize
      end
    end
    object TabStretchTransfer: TTabSheet
      Caption = 'StretchTransfer'
      ImageIndex = 3
      object PaintBoxStretchTransfer: TPaintBox32
        Left = 0
        Top = 0
        Width = 321
        Height = 346
        Hint = 'Upsampling using StretchTransfer resampling'
        Align = alClient
        TabOrder = 0
        OnPaintBuffer = PaintBoxStretchTransferPaintBuffer
        ExplicitHeight = 348
      end
    end
    object TabResampling: TTabSheet
      Caption = 'Downsampling'
      ImageIndex = 1
      object PaintBoxResampling: TPaintBox32
        Left = 0
        Top = 0
        Width = 321
        Height = 346
        Align = alClient
        RepaintMode = rmOptimizer
        TabOrder = 0
        OnPaintBuffer = PaintBoxResamplingPaintBuffer
        ExplicitHeight = 348
      end
    end
    object TabKernel: TTabSheet
      Hint = 'Kernel curve'
      Caption = 'Curve'
      ImageIndex = 2
      TabVisible = False
      object PaintBoxCurve: TPaintBox32
        Left = 0
        Top = 0
        Width = 321
        Height = 346
        Align = alClient
        TabOrder = 0
        OnPaintBuffer = PaintBoxCurvePaintBuffer
        ExplicitHeight = 348
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 376
    Width = 475
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object TimerTableSize: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTableSizeTimer
    Left = 144
    Top = 144
  end
  object TimerParameter: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerParameterTimer
    Left = 144
    Top = 200
  end
end
