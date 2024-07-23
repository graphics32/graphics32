object FormColorPicker: TFormColorPicker
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Color Picker'
  ClientHeight = 307
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  DesignSize = (
    563
    307)
  TextHeight = 13
  object LabelWebColor: TLabel
    Left = 432
    Top = 178
    Width = 23
    Height = 13
    Caption = 'Hex:'
  end
  object LabelRed: TLabel
    Left = 270
    Top = 12
    Width = 43
    Height = 18
    AutoSize = False
    Caption = 'Red:'
  end
  object LabelGreen: TLabel
    Left = 270
    Top = 52
    Width = 43
    Height = 18
    AutoSize = False
    Caption = 'Green:'
  end
  object LabelBlue: TLabel
    Left = 270
    Top = 92
    Width = 43
    Height = 18
    AutoSize = False
    Caption = 'Blue:'
  end
  object LabelAlpha: TLabel
    Left = 270
    Top = 132
    Width = 43
    Height = 18
    AutoSize = False
    Caption = 'Alpha:'
  end
  object LabelPreview: TLabel
    Left = 270
    Top = 178
    Width = 42
    Height = 13
    Caption = 'Preview:'
  end
  object PanelControl: TPanel
    Left = 0
    Top = 274
    Width = 563
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      563
      33)
    object ButtonOK: TButton
      Left = 411
      Top = 6
      Width = 64
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object ButtonCancel: TButton
      Left = 486
      Top = 6
      Width = 65
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object ButtonPickFromScreen: TButton
      Left = 8
      Top = 5
      Width = 105
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'Pick from screen'
      TabOrder = 2
      OnClick = ButtonPickFromScreenClick
    end
    object CheckBoxWebSafe: TCheckBox
      Left = 228
      Top = 8
      Width = 73
      Height = 17
      Caption = 'WebSafe'
      TabOrder = 3
      OnClick = CheckBoxWebSafeClick
    end
  end
  object EditColor: TEdit
    Left = 474
    Top = 175
    Width = 81
    Height = 21
    Alignment = taCenter
    Anchors = [akTop, akRight]
    TabOrder = 1
    Text = '$00000000'
    OnChange = EditColorChange
  end
  object PanelColorPickerMain: TPanel
    Left = 8
    Top = 8
    Width = 256
    Height = 256
    BevelOuter = bvNone
    ParentColor = True
    ShowCaption = False
    TabOrder = 2
  end
  object SpinEditRed: TSpinEdit
    Left = 509
    Top = 8
    Width = 46
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 255
    MinValue = 0
    TabOrder = 3
    Value = 0
    OnChange = SpinEditColorChange
  end
  object SpinEditGreen: TSpinEdit
    Left = 509
    Top = 48
    Width = 46
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 255
    MinValue = 0
    TabOrder = 4
    Value = 0
    OnChange = SpinEditColorChange
  end
  object SpinEditBlue: TSpinEdit
    Left = 509
    Top = 88
    Width = 46
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 255
    MinValue = 0
    TabOrder = 5
    Value = 0
    OnChange = SpinEditColorChange
  end
  object SpinEditAlpha: TSpinEdit
    Left = 509
    Top = 128
    Width = 46
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 255
    MinValue = 0
    TabOrder = 6
    Value = 0
    OnChange = SpinEditColorChange
  end
  object PanelPreview: TPanel
    Left = 328
    Top = 169
    Width = 60
    Height = 32
    BevelKind = bkFlat
    BevelOuter = bvNone
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    ShowCaption = False
    TabOrder = 7
  end
  object PanelSwatches: TPanel
    Left = 270
    Top = 240
    Width = 285
    Height = 24
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 8
  end
end
