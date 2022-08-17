object FmDisplay: TFmDisplay
  Left = 299
  Top = 55
  BorderStyle = bsDialog
  Caption = 'Display'
  ClientHeight = 273
  ClientWidth = 355
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    355
    273)
  PixelsPerInch = 96
  TextHeight = 13
  object LbQuestion: TLabel
    Left = 82
    Top = 245
    Width = 160
    Height = 13
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    Caption = 'Does the image display correctly?'
  end
  object LbRenderer: TLabel
    Left = 145
    Top = 9
    Width = 58
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Interpreter:'
  end
  object BtNo: TButton
    Left = 300
    Top = 240
    Width = 47
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&No'
    ModalResult = 7
    TabOrder = 3
  end
  object BtYes: TButton
    Left = 248
    Top = 240
    Width = 46
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Yes'
    Default = True
    ModalResult = 6
    TabOrder = 2
  end
  object RbPngImage: TRadioButton
    Left = 274
    Top = 8
    Width = 73
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'PNG Image'
    TabOrder = 1
    OnClick = RbPngImageClick
  end
  object RbInternal: TRadioButton
    Left = 209
    Top = 8
    Width = 59
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Internal'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = RbInternalClick
  end
  object Image32: TImage32
    Left = 8
    Top = 31
    Width = 339
    Height = 203
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 4
  end
end
