object FormMain: TFormMain
  Left = 0
  Top = 0
  ClientHeight = 514
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object ImgView: TImgView32
    Left = 0
    Top = 34
    Width = 570
    Height = 480
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 17
    OverSize = 0
    TabOrder = 0
    TabStop = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 570
    Height = 34
    Align = alTop
    TabOrder = 1
    object BSave: TButton
      Left = 32
      Top = 8
      Width = 75
      Height = 20
      Caption = 'Save'
      TabOrder = 0
      OnClick = BSaveClick
    end
    object CExportLayers: TCheckBox
      Left = 120
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Export layers'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object BRandom: TButton
      Left = 240
      Top = 8
      Width = 137
      Height = 20
      Caption = 'Random shapes'
      TabOrder = 2
      OnClick = BRandomClick
    end
  end
end
