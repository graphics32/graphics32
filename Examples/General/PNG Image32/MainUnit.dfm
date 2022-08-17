object FmPngDemo: TFmPngDemo
  Left = 299
  Top = 55
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'PNG Demonstration'
  ClientHeight = 279
  ClientWidth = 475
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ImageDisplay: TImage32
    Left = 0
    Top = 38
    Width = 475
    Height = 241
    Align = alClient
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    ExplicitTop = 8
    ExplicitHeight = 262
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 475
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object CheckBoxFit: TCheckBox
      Left = 184
      Top = 11
      Width = 97
      Height = 19
      Caption = 'Scale to fit'
      TabOrder = 0
      OnClick = CheckBoxFitClick
    end
  end
  object ButtonLoad: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 2
    OnClick = ButtonLoadClick
  end
  object ButtonSave: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Save...'
    TabOrder = 3
    OnClick = ButtonSaveClick
  end
end
