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
    Top = 124
    Width = 69
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Current Color:'
  end
  object PageControlColorPicker: TPageControl
    Left = 8
    Top = 8
    Width = 257
    Height = 277
    ActivePage = TabColorPickerGTK
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    object TabColorPickerGTK: TTabSheet
      Caption = 'GTK like'
      object ColorPickerGTK: TColorPickerGTK
        Left = 3
        Top = 3
        Width = 238
        Height = 238
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
      object ColorPickerHSV: TColorPickerHSV
        Left = 0
        Top = 0
        Width = 249
        Height = 248
        Hue = 0.017156863585114480
        ParentBackground = False
        Saturation = 0.544000029563903800
        SelectedColor = -360334
        TabOrder = 0
        Value = 0.980392158031463700
        OnChanged = ColorPickerHSVChanged
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
    Top = 140
    Width = 79
    Height = 21
    Alignment = taCenter
    Anchors = [akTop, akRight]
    TabOrder = 4
    Text = '$00000000'
    OnChange = EditColorChange
    OnKeyPress = EditColorKeyPress
  end
  object Button1: TButton
    Left = 271
    Top = 260
    Width = 79
    Height = 25
    Caption = 'from Screen'
    TabOrder = 5
    OnClick = Button1Click
  end
end
