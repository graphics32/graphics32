object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Color Picker'
  ClientHeight = 293
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    358
    293)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelRed: TLabel
    Left = 279
    Top = 35
    Width = 11
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'R:'
  end
  object LabelColor: TLabel
    Left = 280
    Top = 11
    Width = 69
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Current Color:'
  end
  object LabelGreen: TLabel
    Left = 279
    Top = 63
    Width = 11
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'G:'
  end
  object LabelBlue: TLabel
    Left = 280
    Top = 91
    Width = 10
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'B:'
  end
  object LabelWebColor: TLabel
    Left = 271
    Top = 156
    Width = 69
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Current Color:'
  end
  object LabelAlpha: TLabel
    Left = 280
    Top = 119
    Width = 11
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'A:'
  end
  object PageControlColorPicker: TPageControl
    Left = 8
    Top = 8
    Width = 257
    Height = 277
    ActivePage = TabSheetComponents
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    object TabColorPickerGTK: TTabSheet
      Caption = 'GTK like'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ColorPickerGTK: TColorPickerGTK
        Left = 3
        Top = 3
        Width = 238
        Height = 238
        Border = True
        Hue = 0.017156863585114480
        ParentBackground = False
        Saturation = 0.544000029563903800
        SelectedColor = -360334
        TabOrder = 0
        Value = 0.980392158031463700
        OnChanged = ColorPickerGTKChanged
      end
    end
    object TabColorPickerHSV: TTabSheet
      Caption = 'HSV'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ColorPickerHSV: TColorPickerHSV
        Left = 0
        Top = 0
        Width = 249
        Height = 248
        Border = True
        Hue = 0.017156863585114480
        ParentBackground = False
        Saturation = 0.544000029563903800
        SelectedColor = -360334
        TabOrder = 0
        Value = 0.980392158031463700
        OnChanged = ColorPickerHSVChanged
      end
    end
    object TabColorPickerRGBA: TTabSheet
      Caption = 'RGBA'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ColorPickerRGBA: TColorPickerRGBA
        Left = 0
        Top = 0
        Width = 249
        Height = 249
        Align = alClient
        Border = True
        ParentBackground = False
        SelectedColor = -360334
        TabOrder = 0
        OnChanged = ColorPickerRGBAChanged
      end
    end
    object TabSheetComponents: TTabSheet
      Caption = 'Components'
      ImageIndex = 3
      object LabelComponentRed: TLabel
        Left = 3
        Top = 11
        Width = 23
        Height = 13
        Caption = 'Red:'
      end
      object LabelComponentAlpha: TLabel
        Left = 3
        Top = 119
        Width = 31
        Height = 13
        Caption = 'Alpha:'
      end
      object LabelComponentBlue: TLabel
        Left = 3
        Top = 83
        Width = 24
        Height = 13
        Caption = 'Blue:'
      end
      object LabelComponentGreen: TLabel
        Left = 3
        Top = 47
        Width = 33
        Height = 13
        Caption = 'Green:'
      end
      object ColorPickerRed: TColorPickerComponent
        Left = 37
        Top = 3
        Width = 209
        Height = 30
        Border = True
        ColorComponent = ccRed
        ParentBackground = False
        SelectedColor = -360334
        TabOrder = 0
        OnChanged = ColorPickerRedChanged
      end
      object ColorPickerGreen: TColorPickerComponent
        Left = 37
        Top = 39
        Width = 209
        Height = 30
        Border = True
        ColorComponent = ccGreen
        ParentBackground = False
        SelectedColor = -360334
        TabOrder = 1
        OnChanged = ColorPickerGreenChanged
      end
      object ColorPickerAlpha: TColorPickerComponent
        Left = 37
        Top = 111
        Width = 209
        Height = 30
        Border = True
        ColorComponent = ccAlpha
        ParentBackground = False
        SelectedColor = -360334
        TabOrder = 2
        OnChanged = ColorPickerAlphaChanged
      end
      object ColorPickerBlue: TColorPickerComponent
        Left = 37
        Top = 75
        Width = 209
        Height = 30
        Border = True
        ColorComponent = ccBlue
        ParentBackground = False
        SelectedColor = -360334
        TabOrder = 3
        OnChanged = ColorPickerBlueChanged
      end
    end
  end
  object SpinEditRed: TSpinEdit
    Left = 296
    Top = 32
    Width = 54
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 255
    MinValue = 0
    TabOrder = 1
    Value = 0
    OnChange = SpinEditColorChange
  end
  object SpinEditGreen: TSpinEdit
    Left = 296
    Top = 60
    Width = 54
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 255
    MinValue = 0
    TabOrder = 2
    Value = 0
    OnChange = SpinEditColorChange
  end
  object SpinEditBlue: TSpinEdit
    Left = 296
    Top = 88
    Width = 54
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 255
    MinValue = 0
    TabOrder = 3
    Value = 0
    OnChange = SpinEditColorChange
  end
  object EditColor: TEdit
    Left = 271
    Top = 172
    Width = 79
    Height = 21
    Alignment = taCenter
    Anchors = [akTop, akRight]
    TabOrder = 4
    Text = '$00000000'
    OnChange = EditColorChange
    OnKeyPress = EditColorKeyPress
  end
  object ButtonFromScreen: TButton
    Left = 271
    Top = 260
    Width = 79
    Height = 25
    Caption = 'from Screen'
    TabOrder = 5
    OnClick = ButtonFromScreenClick
  end
  object SpinEditAlpha: TSpinEdit
    Left = 296
    Top = 116
    Width = 54
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 255
    MinValue = 0
    TabOrder = 6
    Value = 0
    OnChange = SpinEditColorChange
  end
  object ColorSwatch: TColorSwatch
    Left = 271
    Top = 207
    Width = 79
    Height = 34
    Border = True
    Color = -360334
    ParentBackground = False
    TabOrder = 7
  end
end
