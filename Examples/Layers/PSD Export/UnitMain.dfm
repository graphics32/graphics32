object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Export TImage32 layers to PSD'
  ClientHeight = 514
  ClientWidth = 647
  Color = clBtnFace
  ParentFont = True
  OnCreate = FormCreate
  TextHeight = 15
  object ImgView: TImgView32
    Left = 0
    Top = 41
    Width = 647
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
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 647
    Height = 41
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkFlat
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    DesignSize = (
      647
      39)
    object LabelCompression: TLabel
      Left = 216
      Top = 11
      Width = 73
      Height = 15
      Caption = '&Compression:'
      FocusControl = ComboBoxCompression
    end
    object ButtonSave: TButton
      Left = 8
      Top = 7
      Width = 75
      Height = 25
      Caption = '&Save'
      TabOrder = 0
      OnClick = ButtonSaveClick
    end
    object CheckBoxExportLayers: TCheckBox
      Left = 100
      Top = 11
      Width = 97
      Height = 17
      Caption = '&Export layers'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object ButtonRandom: TButton
      Left = 501
      Top = 7
      Width = 137
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Random shapes'
      TabOrder = 2
      OnClick = ButtonRandomClick
    end
    object ComboBoxCompression: TComboBox
      Left = 300
      Top = 8
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'Raw (no compression)'
      Items.Strings = (
        'Raw (no compression)'
        'RLE'
        'ZIP')
    end
  end
end
