object MainForm: TMainForm
  Left = 340
  Top = 106
  Caption = 'Polygon Renderer Benchmark'
  ClientHeight = 568
  ClientWidth = 714
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PnlTop: TPanel
    Left = 0
    Top = 0
    Width = 714
    Height = 334
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 0
    object Img: TImage32
      Left = 10
      Top = 10
      Width = 694
      Height = 314
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
      OnResize = ImgResize
    end
  end
  object PnlBottom: TPanel
    Left = 0
    Top = 334
    Width = 714
    Height = 234
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 1
    object GbxSettings: TGroupBox
      Left = 10
      Top = 10
      Width = 328
      Height = 214
      Align = alLeft
      Caption = 'Benchmark Settings'
      TabOrder = 0
      object LblTest: TLabel
        Left = 14
        Top = 34
        Width = 25
        Height = 13
        Caption = '&Test:'
        Color = clBtnFace
        FocusControl = CmbTest
        ParentColor = False
      end
      object LblRenderer: TLabel
        Left = 14
        Top = 61
        Width = 49
        Height = 13
        Caption = '&Renderer:'
        Color = clBtnFace
        FocusControl = CmbRenderer
        ParentColor = False
      end
      object BtnBenchmark: TButton
        Left = 14
        Top = 158
        Width = 139
        Height = 25
        Caption = 'Do Benchmark'
        TabOrder = 0
        OnClick = BtnBenchmarkClick
      end
      object CmbTest: TComboBox
        Left = 78
        Top = 31
        Width = 225
        Height = 21
        Style = csDropDownList
        TabOrder = 1
      end
      object CmbRenderer: TComboBox
        Left = 78
        Top = 58
        Width = 225
        Height = 21
        Style = csDropDownList
        TabOrder = 2
      end
      object CbxAllTests: TCheckBox
        Left = 14
        Top = 98
        Width = 115
        Height = 22
        Caption = 'Benchmark all tests'
        TabOrder = 3
      end
      object CbxAllRenderers: TCheckBox
        Left = 14
        Top = 122
        Width = 139
        Height = 22
        Caption = 'Benchmark all renderers'
        TabOrder = 4
      end
      object BtnExit: TButton
        Left = 176
        Top = 158
        Width = 127
        Height = 25
        Cancel = True
        Caption = 'E&xit'
        TabOrder = 5
        OnClick = BtnExitClick
      end
    end
    object GbxResults: TGroupBox
      Left = 348
      Top = 10
      Width = 356
      Height = 214
      Align = alClient
      Caption = 'Benchmark Results'
      TabOrder = 1
      object PnlBenchmark: TPanel
        Left = 2
        Top = 15
        Width = 352
        Height = 197
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        TabOrder = 0
        object MemoLog: TMemo
          Left = 10
          Top = 10
          Width = 332
          Height = 177
          Align = alClient
          TabOrder = 0
        end
      end
    end
    object PnlSpacer: TPanel
      Left = 338
      Top = 10
      Width = 10
      Height = 214
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
end
