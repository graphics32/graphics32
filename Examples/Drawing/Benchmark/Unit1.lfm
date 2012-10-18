object MainForm: TMainForm
  Left = 242
  Height = 828
  Top = 750
  Width = 714
  Caption = 'Polygon Renderer Benchmark'
  ClientHeight = 828
  ClientWidth = 714
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  LCLVersion = '0.9.31'
  object Img: TImage32
    Left = 8
    Height = 505
    Top = 8
    Width = 697
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1
    ScaleMode = smNormal
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 8
    Height = 296
    Top = 528
    Width = 328
    Caption = 'Benchmark Settings'
    ClientHeight = 280
    ClientWidth = 324
    TabOrder = 1
    object Label1: TLabel
      Left = 14
      Height = 15
      Top = 10
      Width = 27
      Caption = 'Test:'
      ParentColor = False
    end
    object Label2: TLabel
      Left = 14
      Height = 15
      Top = 42
      Width = 58
      Caption = 'Renderer:'
      ParentColor = False
    end
    object Button1: TButton
      Left = 110
      Height = 25
      Top = 130
      Width = 75
      Caption = 'Benchmark'
      OnClick = Button1Click
      TabOrder = 0
    end
    object cmbTest: TComboBox
      Left = 78
      Height = 29
      Top = 2
      Width = 225
      ItemHeight = 0
      Style = csDropDownList
      TabOrder = 1
    end
    object cmbRenderer: TComboBox
      Left = 78
      Height = 29
      Top = 34
      Width = 225
      ItemHeight = 0
      Style = csDropDownList
      TabOrder = 2
    end
    object cbAllTests: TCheckBox
      Left = 22
      Height = 22
      Top = 74
      Width = 143
      Caption = 'Benchmark all tests'
      TabOrder = 3
    end
    object cbAllRenderers: TCheckBox
      Left = 22
      Height = 22
      Top = 98
      Width = 172
      Caption = 'Benchmark all renderers'
      TabOrder = 4
    end
  end
  object GroupBox2: TGroupBox
    Left = 352
    Height = 296
    Top = 528
    Width = 344
    Caption = 'Benchmark Results'
    ClientHeight = 280
    ClientWidth = 340
    TabOrder = 2
    object Memo1: TMemo
      Left = 0
      Height = 280
      Top = 0
      Width = 340
      Align = alClient
      TabOrder = 0
    end
  end
end