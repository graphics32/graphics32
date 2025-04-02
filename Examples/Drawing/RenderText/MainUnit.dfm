object FormRenderText: TFormRenderText
  Left = 381
  Top = 128
  Caption = 'RenderText example'
  ClientHeight = 207
  ClientWidth = 566
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 580
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  TextHeight = 15
  object Image: TImage32
    Left = 0
    Top = 61
    Width = 566
    Height = 146
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 1
    OnClick = ImageClick
    OnResize = ImageResize
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 566
    Height = 61
    Align = alTop
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    DesignSize = (
      566
      61)
    object LabelText: TLabel
      Left = 4
      Top = 7
      Width = 78
      Height = 13
      Caption = 'Enter text here:'
    end
    object LabelFont: TLabel
      Left = 386
      Top = 7
      Width = 26
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Font:'
    end
    object Bevel1: TBevel
      Left = 378
      Top = 4
      Width = 5
      Height = 51
      Shape = bsLeftLine
    end
    object EditText: TEdit
      Left = 92
      Top = 4
      Width = 277
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      TabOrder = 0
      Text = 'Excellence endures 0123456789'
      OnChange = Changed
    end
    object ButtonBenchmark: TButton
      Left = 252
      Top = 32
      Width = 117
      Height = 21
      Caption = 'Run Benchmark'
      TabOrder = 4
      OnClick = ButtonBenchmarkClick
    end
    object CheckBoxAntiAlias: TCheckBox
      Left = 4
      Top = 34
      Width = 69
      Height = 17
      Hint = 'Enable anti-aliasing.'#13#10'TCanvas32 is always anti-aliased'
      Caption = 'Anti-alias'
      TabOrder = 2
      OnClick = Changed
    end
    object CheckBoxCanvas32: TCheckBox
      Left = 79
      Top = 34
      Width = 74
      Height = 17
      Hint = 
        'Displays the text using TCanvas32.RenderText.'#13#10'Otherwise uses TB' +
        'itmap32.RenderText'
      Caption = 'TCanvas32'
      TabOrder = 3
      OnClick = CheckBoxCanvas32Click
    end
    object ComboBoxFont: TComboBox
      Left = 418
      Top = 4
      Width = 143
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnChange = Changed
    end
    object CheckBoxBold: TCheckBox
      Left = 418
      Top = 34
      Width = 62
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Bold'
      TabOrder = 5
      OnClick = Changed
    end
    object CheckBoxItalic: TCheckBox
      Left = 491
      Top = 34
      Width = 62
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Italic'
      TabOrder = 6
      OnClick = Changed
    end
    object CheckBoxShowOrigin: TCheckBox
      Left = 163
      Top = 34
      Width = 83
      Height = 17
      Hint = 
        'Draws lines showing the Y-coordinate of the text lines.'#13#10'Note th' +
        'at the line is above the corresponding text.'#13#10'Do not confuse thi' +
        's with the baseline of the font which would be below the text.'
      Caption = 'Show origin'
      TabOrder = 7
      OnClick = Changed
    end
  end
end
