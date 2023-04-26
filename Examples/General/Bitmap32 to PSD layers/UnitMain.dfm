object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Export TBitmap32 to PSD with layers'
  ClientHeight = 562
  ClientWidth = 692
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 692
    Height = 33
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkFlat
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    object ButtonSave: TButton
      Left = 89
      Top = 4
      Width = 75
      Height = 24
      Caption = '&Save'
      TabOrder = 0
      OnClick = ButtonSaveClick
    end
    object ButtonOpen: TButton
      Left = 8
      Top = 4
      Width = 75
      Height = 24
      Caption = '&Open'
      TabOrder = 1
      OnClick = ButtonOpenClick
    end
    object CheckBoxViewTiles: TCheckBox
      Left = 184
      Top = 7
      Width = 137
      Height = 17
      Caption = 'View tile rectangles'
      TabOrder = 2
      OnClick = CheckBoxViewTilesClick
    end
  end
  object ImgView: TImgView32
    Left = 0
    Top = 33
    Width = 692
    Height = 529
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smOptimal
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 17
    OverSize = 0
    TabOrder = 1
  end
end
