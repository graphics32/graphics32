object MainForm: TMainForm
  Left = 296
  Top = 97
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Texture Blend Example'
  ClientHeight = 665
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelMasterAlpha: TLabel
    Left = 272
    Top = 32
    Width = 59
    Height = 13
    Caption = 'MasterAlpha'
  end
  object LabelCombinedTexture: TLabel
    Left = 272
    Top = 88
    Width = 258
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Combined Texture'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object LabelWeightmap: TLabel
    Left = 8
    Top = 88
    Width = 258
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Weightmap'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object LabelTextureA: TLabel
    Left = 8
    Top = 376
    Width = 258
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Texture A'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object LabelTextureB: TLabel
    Left = 272
    Top = 376
    Width = 258
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Texture B'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object LabelBlendSettings: TLabel
    Left = 272
    Top = 8
    Width = 258
    Height = 17
    AutoSize = False
    Caption = ' Blend Settings'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object LabelBlendmode: TLabel
    Left = 272
    Top = 56
    Width = 53
    Height = 13
    Caption = 'Blendmode'
  end
  object LabelWeightmapSettings: TLabel
    Left = 8
    Top = 8
    Width = 258
    Height = 17
    AutoSize = False
    Caption = ' Weightmap Settings'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object MasterAlphaBar: TGaugeBar
    Left = 336
    Top = 32
    Width = 193
    Height = 16
    Backgnd = bgPattern
    Max = 255
    ShowArrows = False
    ShowHandleGrip = True
    Style = rbsMac
    Position = 200
    OnChange = MasterAlphaBarChange
  end
  object BlendBox: TComboBox
    Left = 336
    Top = 56
    Width = 193
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = MasterAlphaBarChange
    Items.Strings = (
      'Normal'
      'Soft Masked'
      'Color Add'
      'Color Sub'
      'Color Div'
      'Color Modulate'
      'Color Max'
      'Color Min'
      'Color Difference'
      'Color Average'
      'Color Exclusion')
  end
  object CombImg: TImage32
    Left = 272
    Top = 112
    Width = 258
    Height = 258
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Color = clBlack
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 2
  end
  object WeightmapImg: TImage32
    Left = 8
    Top = 112
    Width = 258
    Height = 258
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Color = clBlack
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 3
  end
  object TexAImg: TImage32
    Left = 8
    Top = 400
    Width = 258
    Height = 258
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Color = clBlack
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 4
  end
  object TexBImg: TImage32
    Left = 272
    Top = 400
    Width = 258
    Height = 258
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Color = clBlack
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 5
  end
  object GenerateButton: TButton
    Left = 8
    Top = 32
    Width = 257
    Height = 25
    Caption = 'Generate Random Weightmap'
    TabOrder = 6
  end
end
