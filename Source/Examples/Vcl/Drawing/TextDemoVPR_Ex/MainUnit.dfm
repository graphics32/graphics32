object MainForm: TMainForm
  Left = 484
  Top = 101
  Caption = 'GR32 VPR Text Rendering'
  ClientHeight = 569
  ClientWidth = 597
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
  PixelsPerInch = 96
  TextHeight = 13
  object PnlImage: TPanel
    Left = 169
    Top = 0
    Width = 428
    Height = 569
    Align = alClient
    BevelOuter = bvLowered
    BevelWidth = 2
    TabOrder = 0
    object Img: TImage32
      Left = 2
      Top = 2
      Width = 424
      Height = 565
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
    Height = 569
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
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
      Height = 178
      Caption = 'Settings'
      TabOrder = 1
      object LblGamma: TLabel
        Left = 9
        Top = 41
        Width = 36
        Height = 11
        Caption = 'Gamma:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LblGammaValue: TLabel
        Left = 118
        Top = 41
        Width = 24
        Height = 11
        Alignment = taCenter
        Caption = '(1.00)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object TbrGamma: TTrackBar
        Left = 2
        Top = 58
        Width = 148
        Height = 28
        Max = 240
        Frequency = 30
        Position = 100
        TabOrder = 0
        ThumbLength = 18
        OnChange = TbrGammaChange
      end
      object CbxHinted: TCheckBox
        Left = 9
        Top = 19
        Width = 89
        Height = 17
        Caption = 'Enable hinting'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = CbxHintedClick
      end
    end
    object PaintBox32: TPaintBox32
      Left = 9
      Top = 223
      Width = 154
      Height = 154
      TabOrder = 2
    end
    object RgxMethod: TRadioGroup
      Left = 19
      Top = 131
      Width = 133
      Height = 78
      Caption = 'Rendering Method'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        'Default'
        'Cleartype'
        'Cleartype (smooth)')
      ParentFont = False
      TabOrder = 3
      OnClick = RgxMethodClick
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 40
    Top = 392
  end
end
