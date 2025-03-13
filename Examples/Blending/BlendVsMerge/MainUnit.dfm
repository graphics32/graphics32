object MainForm: TMainForm
  Left = 296
  Top = 97
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Blend vs. Merge Example'
  ClientHeight = 456
  ClientWidth = 272
  Color = clBtnFace
  ParentFont = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    272
    456)
  TextHeight = 15
  object LabelOverlay: TLabel
    Left = 8
    Top = 171
    Width = 40
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Overlay'
    Layout = tlCenter
  end
  object LabelBlendSettings: TLabel
    Left = 8
    Top = 84
    Width = 75
    Height = 15
    Caption = 'Blend Settings'
    Layout = tlCenter
  end
  object LabelVisible: TLabel
    Left = 8
    Top = 8
    Width = 65
    Height = 15
    Caption = 'Visible Layer'
    Layout = tlCenter
  end
  object LabelMergeHint: TLabel
    Left = 71
    Top = 106
    Width = 190
    Height = 31
    AutoSize = False
    Caption = 'Alpha values will be merged correctly.'
    WordWrap = True
  end
  object LabelBlendHint: TLabel
    Left = 71
    Top = 138
    Width = 190
    Height = 29
    AutoSize = False
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
    Top = 138
    Width = 57
    Height = 17
    Caption = '&Blend'
    TabOrder = 1
    OnClick = RadioButtonBlendClick
  end
  object RadioButtonMerge: TRadioButton
    Left = 8
    Top = 105
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
    Top = 52
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
    Top = 29
    Width = 116
    Height = 17
    Caption = 'Back&ground  --->'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CheckBoxImageClick
  end
  object CheckBoxTransparent: TCheckBox
    Left = 130
    Top = 29
    Width = 113
    Height = 17
    Caption = 'with transparency'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBoxImageClick
  end
end
