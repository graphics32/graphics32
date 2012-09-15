object FrmClipper: TFrmClipper
  Left = 425
  Top = 191
  Caption = 'Clipper Demo'
  ClientHeight = 414
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PnlControl: TPanel
    Left = 0
    Top = 0
    Width = 401
    Height = 41
    Align = alTop
    TabOrder = 0
    object BtnClose: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Close'
      TabOrder = 0
      OnClick = BtnCloseClick
    end
  end
  object ImgView32: TImgView32
    Left = 0
    Top = 41
    Width = 401
    Height = 373
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 16
    ScrollBars.Visibility = svAuto
    OverSize = 0
    TabOrder = 1
  end
end
