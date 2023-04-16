object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Export TImage32 layers to PSD'
  ClientHeight = 514
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object ImgView: TImgView32
    Left = 0
    Top = 41
    Width = 570
    Height = 473
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
    ExplicitTop = 34
    ExplicitHeight = 480
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 570
    Height = 41
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkFlat
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    DesignSize = (
      570
      39)
    object ButtonSave: TButton
      Left = 8
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 0
      OnClick = ButtonSaveClick
    end
    object CheckBoxExportLayers: TCheckBox
      Left = 100
      Top = 11
      Width = 97
      Height = 17
      Caption = 'Export layers'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object ButtonRandom: TButton
      Left = 424
      Top = 7
      Width = 137
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Random shapes'
      TabOrder = 2
      OnClick = ButtonRandomClick
    end
  end
end
