object FmArrowHead: TFmArrowHead
  Left = 375
  Top = 138
  Caption = 'ArrowHead'
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
    Left = 173
    Top = 0
    Width = 394
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
  object PnlControl: TPanel
    Left = 0
    Top = 0
    Width = 173
    Height = 470
    Align = alLeft
    TabOrder = 1
    object LblArrowSize: TLabel
      Left = 15
      Top = 19
      Width = 57
      Height = 15
      Caption = '&Arrow Size'
      FocusControl = EdtArrowSize
    end
    object LblLineWidth: TLabel
      Left = 16
      Top = 315
      Width = 58
      Height = 15
      Caption = 'Line &Width'
      FocusControl = TbrLineWidth
    end
    object BtnClose: TButton
      Left = 15
      Top = 424
      Width = 140
      Height = 25
      Cancel = True
      Caption = '&Close'
      TabOrder = 6
      OnClick = BtnCloseClick
    end
    object RgpArrowStyle: TRadioGroup
      Left = 15
      Top = 69
      Width = 140
      Height = 131
      Caption = 'Arrow &Style'
      ItemIndex = 2
      Items.Strings = (
        'None'
        '3 point'
        '4 point'
        'Diamond'
        'Ellipse')
      TabOrder = 1
      OnClick = RgpArrowStyleClick
    end
    object EdtArrowSize: TEdit
      Left = 15
      Top = 36
      Width = 140
      Height = 23
      TabOrder = 0
      Text = '20'
      OnChange = EdtArrowSizeChange
    end
    object RgpPosition: TRadioGroup
      Left = 15
      Top = 210
      Width = 140
      Height = 97
      Caption = 'Arrow &Locations'
      ItemIndex = 2
      Items.Strings = (
        'Arrow at start'
        'Arrow at end'
        'Arrow at both ends')
      TabOrder = 2
      OnClick = RgpArrowStyleClick
    end
    object TbrLineWidth: TTrackBar
      Left = 9
      Top = 335
      Width = 153
      Height = 31
      Max = 8
      Min = 1
      Position = 3
      TabOrder = 3
      OnChange = TbrLineWidthChange
    end
    object TbrAnimationSpeed: TTrackBar
      Left = 91
      Top = 381
      Width = 70
      Height = 31
      Max = 8
      Min = 1
      Position = 3
      TabOrder = 5
      TickStyle = tsNone
      OnChange = TbrAnimationSpeedChange
    end
    object CbxAnimate: TCheckBox
      Left = 15
      Top = 384
      Width = 69
      Height = 17
      Caption = 'Ani&mate'
      TabOrder = 4
      OnClick = CbxAnimateClick
    end
  end
  object Animation: TTimer
    Enabled = False
    Interval = 30
    OnTimer = AnimationTimer
    Left = 184
    Top = 16
  end
end
