object MainForm: TMainForm
  Left = 242
  Top = 750
  Caption = 'Polygon Renderer Benchmark'
  ClientHeight = 746
  ClientWidth = 714
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  DesignSize = (
    714
    746)
  PixelsPerInch = 96
  TextHeight = 13
  object Img: TImage32
    Left = 8
    Top = 8
    Width = 698
    Height = 514
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
  object GbxSettings: TGroupBox
    Left = 8
    Top = 528
    Width = 328
    Height = 210
    Anchors = [akLeft, akBottom]
    Caption = 'Benchmark Settings'
    TabOrder = 1
    object LblTest: TLabel
      Left = 14
      Top = 34
      Width = 25
      Height = 13
      Caption = 'Test:'
      Color = clBtnFace
      ParentColor = False
    end
    object LblRenderer: TLabel
      Left = 14
      Top = 61
      Width = 49
      Height = 13
      Caption = 'Renderer:'
      Color = clBtnFace
      ParentColor = False
    end
    object BtnBenchmark: TButton
      Left = 126
      Top = 166
      Width = 75
      Height = 25
      Caption = 'Benchmark'
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
      Left = 22
      Top = 98
      Width = 115
      Height = 22
      Caption = 'Benchmark all tests'
      TabOrder = 3
    end
    object CbxAllRenderers: TCheckBox
      Left = 22
      Top = 122
      Width = 139
      Height = 22
      Caption = 'Benchmark all renderers'
      TabOrder = 4
    end
  end
  object GbxResults: TGroupBox
    Left = 342
    Top = 528
    Width = 364
    Height = 210
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Benchmark Results'
    TabOrder = 2
    object MemoLog: TMemo
      Left = 2
      Top = 15
      Width = 360
      Height = 193
      Align = alClient
      TabOrder = 0
    end
  end
end
