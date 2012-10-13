object MainForm: TMainForm
  Left = 484
  Top = 101
  Width = 605
  Height = 596
  Caption = 'GR32 VPR Text Rendering'
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
    object LblGammaNote: TLabel
      Left = 16
      Top = 392
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
      Height = 178
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
        Left = 118
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
      ItemIndex = 0
      Items.Strings = (
        'Default'
        'Cleartype'
        'Cleartype (smooth)')
      TabOrder = 3
      OnClick = RgxMethodClick
    end
    object BtnExit: TButton
      Left = 6
      Top = 536
      Width = 155
      Height = 25
      Cancel = True
      Caption = 'E&xit'
      TabOrder = 4
      OnClick = BtnExitClick
    end
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
