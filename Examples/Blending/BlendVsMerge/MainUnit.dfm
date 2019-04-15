object MainForm: TMainForm
  Left = 296
  Top = 97
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Blend vs. Merge Example'
  ClientHeight = 456
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    272
    456)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelOverlay: TLabel
    Left = 8
    Top = 169
    Width = 256
    Height = 17
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = ' Overlay'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
    ExplicitTop = 187
  end
  object LabelBlendSettings: TLabel
    Left = 8
    Top = 88
    Width = 256
    Height = 17
    AutoSize = False
    Caption = ' Blend Settings'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object LabelVisible: TLabel
    Left = 8
    Top = 8
    Width = 256
    Height = 17
    AutoSize = False
    Caption = ' Visible Layer'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object LabelMergeHint: TLabel
    Left = 71
    Top = 112
    Width = 174
    Height = 13
    Caption = 'Alpha values will be merged correctly'
  end
  object LabelBlendHint: TLabel
    Left = 71
    Top = 134
    Width = 172
    Height = 26
    Caption = 
      'Alpha values of the background are assumed to be opaque -> faste' +
      'r!'
    WordWrap = True
  end
  object DstImg: TImage32
    Left = 8
    Top = 192
    Width = 256
    Height = 256
    Anchors = [akLeft, akBottom]
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Color = clBlack
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnPaintStage = DstImgPaintStage
  end
  object RadioButtonBlend: TRadioButton
    Left = 8
    Top = 134
    Width = 57
    Height = 17
    Caption = '&Blend'
    TabOrder = 1
    OnClick = RadioButtonBlendClick
  end
  object RadioButtonMerge: TRadioButton
    Left = 8
    Top = 111
    Width = 57
    Height = 17
    Caption = '&Merge'
    Checked = True
    TabOrder = 2
    TabStop = True
    OnClick = RadioButtonMergeClick
  end
  object CheckBoxForeground: TCheckBox
    Left = 8
    Top = 54
    Width = 81
    Height = 17
    Caption = '&Foreground'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBoxImageClick
  end
  object CheckBoxBackground: TCheckBox
    Left = 8
    Top = 31
    Width = 105
    Height = 17
    Caption = 'Back&ground  --->'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CheckBoxImageClick
  end
  object CheckBoxTransparent: TCheckBox
    Left = 112
    Top = 31
    Width = 113
    Height = 17
    Caption = 'with transparency'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBoxImageClick
  end
end
