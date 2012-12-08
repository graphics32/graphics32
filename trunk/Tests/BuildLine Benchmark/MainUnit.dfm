object FrmBuildLineBenchmark: TFrmBuildLineBenchmark
  Left = 0
  Top = 0
  Caption = 'BuildLine Benchmark'
  ClientHeight = 636
  ClientWidth = 934
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
    934
    636)
  PixelsPerInch = 96
  TextHeight = 13
  object LblLineWidth: TLabel
    Left = 844
    Top = 20
    Width = 32
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Width:'
  end
  object Image32: TImage32
    Left = 8
    Top = 8
    Width = 671
    Height = 620
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
    Left = 685
    Top = 39
    Width = 241
    Height = 587
    Anchors = [akTop, akRight, akBottom]
    TabOrder = 1
  end
  object BtnBenchmark: TButton
    Left = 685
    Top = 8
    Width = 65
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Benchmark'
    TabOrder = 2
    OnClick = BtnBenchmarkClick
  end
  object BtnClear: TButton
    Left = 756
    Top = 8
    Width = 40
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Clear'
    TabOrder = 3
    OnClick = BtnClearClick
  end
  object BtnDraw: TButton
    Left = 802
    Top = 8
    Width = 36
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Draw'
    TabOrder = 4
    OnClick = BtnDrawClick
  end
  object GaugeBar32: TGaugeBar32
    Left = 844
    Top = 8
    Width = 82
    Height = 12
    Anchors = [akTop, akRight]
    Backgnd = bgPattern
    Max = 200
    Min = 10
    ShowHandleGrip = True
    Position = 10
    OnChange = GaugeBar32Change
  end
end
