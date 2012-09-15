object FrmNewImage: TFrmNewImage
  Left = 282
  Top = 194
  BorderStyle = bsDialog
  Caption = 'New Image'
  ClientHeight = 194
  ClientWidth = 250
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LblWidth: TLabel
    Left = 24
    Top = 27
    Width = 32
    Height = 13
    Caption = 'Width:'
    FocusControl = EdtImageWidth
  end
  object LblHeight: TLabel
    Left = 24
    Top = 67
    Width = 35
    Height = 13
    Caption = 'Height:'
    FocusControl = EdtImageHeight
  end
  object LblWidthUnit: TLabel
    Left = 200
    Top = 27
    Width = 27
    Height = 13
    Caption = 'pixels'
  end
  object LblHeightUnit: TLabel
    Left = 200
    Top = 67
    Width = 27
    Height = 13
    Caption = 'pixels'
  end
  object LblBackgroundColor: TLabel
    Left = 24
    Top = 116
    Width = 88
    Height = 13
    Caption = 'Background Color:'
  end
  object EdtImageWidth: TEdit
    Left = 72
    Top = 24
    Width = 97
    Height = 21
    TabOrder = 0
    Text = '640'
  end
  object BtnUpDownWidth: TUpDown
    Left = 169
    Top = 24
    Width = 18
    Height = 21
    Associate = EdtImageWidth
    Min = 1
    Max = 2000
    Position = 640
    TabOrder = 1
  end
  object EdtImageHeight: TEdit
    Left = 72
    Top = 64
    Width = 97
    Height = 21
    TabOrder = 2
    Text = '480'
  end
  object BtnUpDownHeight: TUpDown
    Left = 169
    Top = 64
    Width = 18
    Height = 21
    Associate = EdtImageHeight
    Min = 1
    Max = 2000
    Position = 480
    TabOrder = 3
  end
  object PnlColor: TPanel
    Left = 120
    Top = 112
    Width = 67
    Height = 21
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clWhite
    TabOrder = 4
  end
  object BtnSelect: TButton
    Left = 192
    Top = 112
    Width = 35
    Height = 20
    Caption = 'Select'
    TabOrder = 5
    TabStop = False
    OnClick = BtnSelectClick
  end
  object BtnOK: TButton
    Left = 104
    Top = 164
    Width = 65
    Height = 22
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object BtnCancel: TButton
    Left = 176
    Top = 164
    Width = 65
    Height = 22
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object ColorDialog: TColorDialog
    Color = clWhite
    Options = [cdFullOpen]
    Left = 196
    Top = 132
  end
end
