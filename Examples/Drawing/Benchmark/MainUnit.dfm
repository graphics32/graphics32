object MainForm: TMainForm
  Left = 242
  Top = 750
  Caption = 'Polygon Renderer Benchmark'
  ClientHeight = 828
  ClientWidth = 714
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Img: TImage32
    Left = 8
    Top = 8
    Width = 697
    Height = 505
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 528
    Width = 328
    Height = 296
    Caption = 'Benchmark Settings'
    TabOrder = 1
    object Label1: TLabel
      Left = 14
      Top = 10
      Width = 25
      Height = 13
      Caption = 'Test:'
      Color = clBtnFace
      ParentColor = False
    end
    object Label2: TLabel
      Left = 14
      Top = 42
      Width = 49
      Height = 13
      Caption = 'Renderer:'
      Color = clBtnFace
      ParentColor = False
    end
    object Button1: TButton
      Left = 110
      Top = 130
      Width = 75
      Height = 25
      Caption = 'Benchmark'
      TabOrder = 0
      OnClick = Button1Click
    end
    object cmbTest: TComboBox
      Left = 78
      Top = 2
      Width = 225
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object cmbRenderer: TComboBox
      Left = 78
      Top = 34
      Width = 225
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
    object cbAllTests: TCheckBox
      Left = 22
      Top = 74
      Width = 143
      Height = 22
      Caption = 'Benchmark all tests'
      TabOrder = 3
    end
    object cbAllRenderers: TCheckBox
      Left = 22
      Top = 98
      Width = 172
      Height = 22
      Caption = 'Benchmark all renderers'
      TabOrder = 4
    end
  end
  object GroupBox2: TGroupBox
    Left = 352
    Top = 528
    Width = 344
    Height = 296
    Caption = 'Benchmark Results'
    TabOrder = 2
    object Memo1: TMemo
      Left = 2
      Top = 15
      Width = 340
      Height = 279
      Align = alClient
      TabOrder = 0
    end
  end
end
