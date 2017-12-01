object FormImage32Example: TFormImage32Example
  Left = 224
  Top = 159
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'TImage32 Example'
  ClientHeight = 418
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    542
    418)
  PixelsPerInch = 96
  TextHeight = 13
  object PnlImage: TPanel
    Left = 8
    Top = 8
    Width = 385
    Height = 402
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvLowered
    BorderWidth = 1
    TabOrder = 0
    object Image: TImage32
      Left = 2
      Top = 2
      Width = 381
      Height = 398
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
    end
  end
  object PnlControl: TPanel
    Left = 400
    Top = 0
    Width = 142
    Height = 418
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object RgpScaleMode: TRadioGroup
      Left = 8
      Top = 104
      Width = 129
      Height = 129
      Caption = 'f'
      ItemIndex = 0
      Items.Strings = (
        'smNormal'
        'smStretch'
        'smScale'
        'smResize'
        'smOptimal'
        'smOptimalScaled')
      TabOrder = 0
      OnClick = RgpScaleModeClick
    end
    object RgpKernel: TRadioGroup
      Left = 8
      Top = 288
      Width = 129
      Height = 121
      Caption = 'Kernels'
      ItemIndex = 0
      Items.Strings = (
        'TBoxKernel'
        'TLinearKernel'
        'TSplineKernel'
        'TLanczosKernel'
        'TMitchellKernel')
      TabOrder = 1
      OnClick = RgpKernelClick
    end
    object RgpBitmapAlign: TRadioGroup
      Left = 8
      Top = 8
      Width = 129
      Height = 89
      Caption = 'BitmapAlign'
      ItemIndex = 0
      Items.Strings = (
        'baTopLeft'
        'baCenter'
        'baTile')
      TabOrder = 2
      OnClick = RgpBitmapAlignClick
    end
    object LblScale: TStaticText
      Left = 8
      Top = 242
      Width = 33
      Height = 17
      Caption = 'Scale:'
      Enabled = False
      TabOrder = 3
    end
    object SbrScale: TGaugeBar
      Left = 8
      Top = 260
      Width = 129
      Height = 16
      Backgnd = bgPattern
      BorderStyle = bsNone
      Enabled = False
      Max = 1000
      Min = 25
      ShowHandleGrip = True
      Position = 100
      OnChange = SbrScaleChange
    end
  end
end
