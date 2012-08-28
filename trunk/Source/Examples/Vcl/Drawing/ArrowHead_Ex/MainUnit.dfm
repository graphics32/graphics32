object FmArrowHeadDemo: TFmArrowHeadDemo
  Left = 375
  Top = 138
  Caption = 'ArrowHead Demo'
  ClientHeight = 470
  ClientWidth = 567
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object ImgView32: TImgView32
    Left = 177
    Top = 0
    Width = 390
    Height = 470
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 16
    ScrollBars.Visibility = svHidden
    OverSize = 0
    TabOrder = 0
    OnMouseDown = ImgView32MouseDown
    OnMouseMove = ImgView32MouseMove
    OnMouseUp = ImgView32MouseUp
    OnResize = ImgView32Resize
  end
  object pnlControl: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 470
    Align = alLeft
    TabOrder = 1
    object lblArrowSize: TLabel
      Left = 15
      Top = 24
      Width = 57
      Height = 15
      Caption = '&Arrow Size'
      FocusControl = Edit1
    end
    object btnClose: TButton
      Left = 15
      Top = 411
      Width = 140
      Height = 25
      Cancel = True
      Caption = '&Close'
      TabOrder = 4
      OnClick = btnCloseClick
    end
    object rgArrowStyle: TRadioGroup
      Left = 15
      Top = 74
      Width = 140
      Height = 143
      Caption = 'Arrow &Style'
      ItemIndex = 1
      Items.Strings = (
        'None'
        '3 point'
        '4 point'
        'Diamond'
        'Ellipse')
      TabOrder = 1
      OnClick = rgArrowStyleClick
    end
    object Edit1: TEdit
      Left = 15
      Top = 41
      Width = 140
      Height = 23
      TabOrder = 0
      Text = '20'
      OnChange = Edit1Change
    end
    object rgPosition: TRadioGroup
      Left = 15
      Top = 228
      Width = 140
      Height = 97
      Caption = 'Arrow &Locations'
      ItemIndex = 2
      Items.Strings = (
        'Arrow at start'
        'Arrow at end'
        'Arrow at both ends')
      TabOrder = 2
      OnClick = rgArrowStyleClick
    end
    object CbAnimate: TCheckBox
      Left = 49
      Top = 359
      Width = 65
      Height = 17
      Caption = 'Ani&mate'
      TabOrder = 3
      OnClick = CbAnimateClick
    end
  end
  object Animation: TTimer
    Enabled = False
    Interval = 30
    OnTimer = AnimationTimer
    Left = 272
    Top = 232
  end
end
