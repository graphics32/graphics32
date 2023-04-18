object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Export TImage32 layers to PSD'
  ClientHeight = 562
  ClientWidth = 692
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
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
    object BSave: TButton
      Left = 128
      Top = 2
      Width = 75
      Height = 24
      Caption = '&Save'
      TabOrder = 0
      OnClick = BSaveClick
    end
    object BOpen: TButton
      Left = 16
      Top = 2
      Width = 75
      Height = 24
      Caption = 'Open'
      TabOrder = 1
      OnClick = BOpenClick
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
