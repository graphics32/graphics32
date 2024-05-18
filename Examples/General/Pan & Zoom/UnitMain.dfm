object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Pan & Zoom example'
  ClientHeight = 514
  ClientWidth = 701
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 353
    Height = 514
    Align = alLeft
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 349
      Height = 15
      Align = alTop
      Alignment = taCenter
      Caption = 'TImage32'
      ExplicitWidth = 52
    end
    object LabelImageZoom: TLabel
      Left = 0
      Top = 495
      Width = 349
      Height = 15
      Align = alBottom
      ShowAccelChar = False
      ExplicitWidth = 3
    end
    object Image: TImage32
      Left = 0
      Top = 15
      Width = 349
      Height = 480
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      Scale = 1.000000000000000000
      ScaleMode = smScale
      TabOrder = 0
      TabStop = True
      OnChange = ImageChange
      OnMouseDown = ImageMouseDown
    end
  end
  object PanelRight: TPanel
    Left = 353
    Top = 0
    Width = 348
    Height = 514
    Align = alClient
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 1
    object Label2: TLabel
      Left = 0
      Top = 0
      Width = 344
      Height = 15
      Align = alTop
      Alignment = taCenter
      Caption = 'TImgView32'
      ExplicitWidth = 65
    end
    object LabelImgViewZoom: TLabel
      Left = 0
      Top = 495
      Width = 344
      Height = 15
      Align = alBottom
      ShowAccelChar = False
      ExplicitWidth = 3
    end
    object ImgView: TImgView32
      Left = 0
      Top = 15
      Width = 344
      Height = 480
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      Scale = 1.000000000000000000
      ScaleMode = smScale
      ScrollBars.ShowHandleGrip = True
      ScrollBars.Style = rbsDefault
      ScrollBars.Size = 17
      ScrollBars.Visibility = svAuto
      SizeGrip = sgNone
      OverSize = 0
      TabOrder = 0
      TabStop = True
      OnChange = ImgViewChange
      OnMouseDown = ImgViewMouseDown
    end
  end
end
