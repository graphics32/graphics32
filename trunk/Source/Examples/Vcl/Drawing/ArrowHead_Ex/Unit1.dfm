object Form1: TForm1
  Left = 253
  Top = 146
  Width = 575
  Height = 497
  Caption = 'ArrowHead Demo'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object ImgView321: TImgView32
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
    OverSize = 0
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 470
    Align = alLeft
    TabOrder = 1
    object Label1: TLabel
      Left = 15
      Top = 24
      Width = 57
      Height = 15
      Caption = '&Arrow Size'
      FocusControl = Edit1
    end
    object Button1: TButton
      Left = 15
      Top = 411
      Width = 140
      Height = 25
      Cancel = True
      Caption = 'Close'
      TabOrder = 2
      OnClick = Button1Click
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
      OnClick = Edit1Change
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
      ItemIndex = 2
      Items.Strings = (
        'Arrow at start'
        'Arrow at end'
        'Arrow at both ends')
      TabOrder = 3
      OnClick = Edit1Change
    end
  end
end
