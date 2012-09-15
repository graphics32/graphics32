object RGBALoaderForm: TRGBALoaderForm
  Left = 200
  Top = 138
  BorderStyle = bsDialog
  Caption = 'New Bitmap Layer with Alpha Channel'
  ClientHeight = 367
  ClientWidth = 412
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
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 412
    Height = 2
    Align = alTop
    Shape = bsTopLine
  end
  object LblRGBImage: TLabel
    Left = 24
    Top = 83
    Width = 57
    Height = 13
    Caption = 'RGB Image:'
  end
  object LblAlphaImage: TLabel
    Left = 216
    Top = 83
    Width = 64
    Height = 13
    Caption = 'Alpha Image:'
  end
  object LblNote: TLabel
    Left = 24
    Top = 294
    Width = 323
    Height = 13
    Caption = 
      'If the images have different sizes, the alpha image will be resc' +
      'aled.'
  end
  object BtnZoomInImage: TSpeedButton
    Left = 148
    Top = 80
    Width = 21
    Height = 21
    Caption = '+'
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Layout = blGlyphBottom
    ParentFont = False
    OnClick = BtnZoomInImageClick
  end
  object BtnZoomOutImage: TSpeedButton
    Left = 170
    Top = 80
    Width = 21
    Height = 21
    Caption = '-'
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Layout = blGlyphBottom
    ParentFont = False
    OnClick = BtnZoomOutImageClick
  end
  object BtnZoomInAlpha: TSpeedButton
    Left = 340
    Top = 80
    Width = 21
    Height = 21
    Caption = '+'
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Layout = blGlyphBottom
    ParentFont = False
    OnClick = BtnZoomInAlphaClick
  end
  object BtnZoomOutAlpha: TSpeedButton
    Left = 362
    Top = 80
    Width = 21
    Height = 21
    Caption = '-'
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Layout = blGlyphBottom
    ParentFont = False
    OnClick = BtnZoomOutAlphaClick
  end
  object PnlInfo: TPanel
    Left = 0
    Top = 2
    Width = 412
    Height = 55
    Align = alTop
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
    object LblInfo: TLabel
      Left = 16
      Top = 6
      Width = 382
      Height = 39
      Caption = 
        'Load two images, one of them will be used to fill RGB components' +
        ' '#13#10'of the layer, another will be converted to a grayscale image ' +
        'and '#13#10'used as alpha channel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
    end
  end
  object ImgRGB: TImgView32
    Left = 24
    Top = 112
    Width = 169
    Height = 169
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Color = clAppWorkSpace
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.Color = clBtnShadow
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 16
    OverSize = 0
    TabOrder = 1
  end
  object BtnLoadImage: TButton
    Left = 96
    Top = 80
    Width = 49
    Height = 21
    Caption = 'Load...'
    TabOrder = 2
    OnClick = BtnLoadImageClick
  end
  object ImgAlpha: TImgView32
    Left = 216
    Top = 112
    Width = 169
    Height = 169
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Color = clAppWorkSpace
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.Color = clBtnShadow
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 16
    OverSize = 0
    TabOrder = 3
  end
  object BtnLoadAlpha: TButton
    Left = 288
    Top = 80
    Width = 49
    Height = 21
    Caption = 'Load...'
    TabOrder = 4
    OnClick = BtnLoadAlphaClick
  end
  object BtnOK: TButton
    Left = 232
    Top = 328
    Width = 75
    Height = 21
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object BtnCancel: TButton
    Left = 312
    Top = 328
    Width = 75
    Height = 21
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object BtnResetScales: TButton
    Left = 24
    Top = 328
    Width = 75
    Height = 21
    Caption = 'Reset Scales'
    TabOrder = 7
    OnClick = BtnResetScalesClick
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 192
    Top = 56
  end
end
