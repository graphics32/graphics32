object FormColorPicker: TFormColorPicker
  Left = 0
  Top = 0
  Caption = 'Color Picker'
  ClientHeight = 303
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    563
    303)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelWebColor: TLabel
    Left = 432
    Top = 178
    Width = 23
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Hex:'
  end
  object LabelRed: TLabel
    Left = 270
    Top = 11
    Width = 23
    Height = 14
    AutoSize = False
    Caption = 'Red:'
  end
  object LabelGreen: TLabel
    Left = 270
    Top = 51
    Width = 33
    Height = 13
    Caption = 'Green:'
  end
  object LabelBlue: TLabel
    Left = 270
    Top = 91
    Width = 19
    Height = 14
    AutoSize = False
    Caption = 'Blue:'
  end
  object LabelAlpha: TLabel
    Left = 270
    Top = 131
    Width = 31
    Height = 13
    Caption = 'Alpha:'
  end
  object LabelPreview: TLabel
    Left = 311
    Top = 178
    Width = 42
    Height = 13
    Caption = 'Preview:'
  end
  object LabelPalette: TLabel
    Left = 270
    Top = 244
    Width = 38
    Height = 13
    Caption = 'Palette:'
    Visible = False
  end
  object PanelControl: TPanel
    Left = 0
    Top = 270
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
      ModalResult = 1
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
  object ColorPickerGTK: TColorPickerGTK
    Left = 8
    Top = 8
    Width = 256
    Height = 256
    ParentBackground = False
    SelectedColor = -16777216
    TabOrder = 2
    OnChanged = ColorPickerChanged
  end
  object ColorPickerRed: TColorPickerComponent
    Left = 311
    Top = 8
    Width = 192
    Height = 22
    Anchors = [akTop, akRight]
    Border = True
    ColorComponent = ccRed
    ParentBackground = False
    SelectedColor = -16777216
    TabOrder = 3
    OnChanged = ColorPickerChanged
  end
  object ColorPickerGreen: TColorPickerComponent
    Left = 311
    Top = 48
    Width = 192
    Height = 22
    Anchors = [akTop, akRight]
    Border = True
    ColorComponent = ccGreen
    ParentBackground = False
    SelectedColor = -16777216
    TabOrder = 4
    OnChanged = ColorPickerChanged
  end
  object ColorPickerBlue: TColorPickerComponent
    Left = 311
    Top = 88
    Width = 192
    Height = 22
    Anchors = [akTop, akRight]
    Border = True
    ColorComponent = ccBlue
    ParentBackground = False
    SelectedColor = -16777216
    TabOrder = 5
    OnChanged = ColorPickerChanged
  end
  object ColorPickerAlpha: TColorPickerComponent
    Left = 311
    Top = 128
    Width = 192
    Height = 22
    Anchors = [akTop, akRight]
    Border = True
    ColorComponent = ccAlpha
    ParentBackground = False
    SelectedColor = -16777216
    TabOrder = 6
    OnChanged = ColorPickerChanged
  end
  object SpinEditRed: TSpinEdit
    Left = 509
    Top = 8
    Width = 46
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 255
    MinValue = 0
    TabOrder = 7
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
    TabOrder = 8
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
    TabOrder = 9
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
    TabOrder = 10
    Value = 0
    OnChange = SpinEditColorChange
  end
  object ColorSwatch: TColorSwatch
    Left = 359
    Top = 164
    Width = 33
    Height = 32
    Border = True
    Color = -360334
    ParentBackground = False
    TabOrder = 11
  end
  object ColorSwatchBlack: TColorSwatch
    Left = 321
    Top = 238
    Width = 24
    Height = 24
    Border = True
    Color = -16777216
    ParentBackground = False
    TabOrder = 12
    OnClick = ColorSwatchClick
  end
  object ColorSwatchWhite: TColorSwatch
    Left = 351
    Top = 238
    Width = 24
    Height = 24
    Border = True
    Color = -1
    ParentBackground = False
    TabOrder = 13
    OnClick = ColorSwatchClick
  end
  object ColorSwatchGreen: TColorSwatch
    Left = 411
    Top = 238
    Width = 24
    Height = 24
    Border = True
    Color = -16711936
    ParentBackground = False
    TabOrder = 14
    OnClick = ColorSwatchClick
  end
  object ColorSwatchRed: TColorSwatch
    Left = 381
    Top = 238
    Width = 24
    Height = 24
    Border = True
    Color = -65536
    ParentBackground = False
    TabOrder = 15
    OnClick = ColorSwatchClick
  end
  object ColorSwatchAqua: TColorSwatch
    Left = 531
    Top = 238
    Width = 24
    Height = 24
    Border = True
    Color = -16711681
    ParentBackground = False
    TabOrder = 16
    OnClick = ColorSwatchClick
  end
  object ColorSwatchFuchsia: TColorSwatch
    Left = 501
    Top = 238
    Width = 24
    Height = 24
    Border = True
    Color = -65281
    ParentBackground = False
    TabOrder = 17
    OnClick = ColorSwatchClick
  end
  object ColorSwatchYellow: TColorSwatch
    Left = 471
    Top = 238
    Width = 24
    Height = 24
    Border = True
    Color = -256
    ParentBackground = False
    TabOrder = 18
    OnClick = ColorSwatchClick
  end
  object ColorSwatchBlue: TColorSwatch
    Left = 441
    Top = 238
    Width = 24
    Height = 24
    Border = True
    Color = -16776961
    ParentBackground = False
    TabOrder = 19
    OnClick = ColorSwatchClick
  end
end
