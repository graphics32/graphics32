object FrmBuildLineBenchmark: TFrmBuildLineBenchmark
  Left = 0
  Top = 0
  Caption = 'BuildLine Benchmark'
  ClientHeight = 379
  ClientWidth = 808
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    808
    379)
  PixelsPerInch = 96
  TextHeight = 13
  object LblLineWidth: TLabel
    Left = 718
    Top = 20
    Width = 32
    Height = 13
    Caption = 'Width:'
  end
  object Image32: TImage32
    Left = 8
    Top = 8
    Width = 545
    Height = 363
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnClick = Image32Click
    OnResize = Image32Resize
  end
  object MemoOutput: TMemo
    Left = 559
    Top = 39
    Width = 241
    Height = 330
    Anchors = [akTop, akRight]
    TabOrder = 1
  end
  object BtnBenchmark: TButton
    Left = 559
    Top = 8
    Width = 65
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Benchmark'
    TabOrder = 2
    OnClick = BtnBenchmarkClick
  end
  object BtnClear: TButton
    Left = 630
    Top = 8
    Width = 40
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Clear'
    TabOrder = 3
    OnClick = BtnClearClick
  end
  object BtnDraw: TButton
    Left = 676
    Top = 8
    Width = 36
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Draw'
    TabOrder = 4
    OnClick = BtnDrawClick
  end
  object GaugeBar32: TGaugeBar32
    Left = 718
    Top = 8
    Width = 82
    Height = 12
    Backgnd = bgPattern
    Max = 200
    Min = 10
    ShowHandleGrip = True
    Position = 10
    OnChange = GaugeBar32Change
  end
end
