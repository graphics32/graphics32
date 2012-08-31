object FormRenderText: TFormRenderText
  Left = 381
  Top = 128
  Caption = 'RenderText Example'
  ClientHeight = 201
  ClientWidth = 329
  Color = clBtnFace
  Constraints.MinWidth = 256
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage32
    Left = 0
    Top = 61
    Width = 329
    Height = 140
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnResize = ImageResize
  end
  object PnlControl: TPanel
    Left = 0
    Top = 0
    Width = 329
    Height = 61
    Align = alTop
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    DesignSize = (
      329
      61)
    object LblAALevel: TLabel
      Left = 4
      Top = 36
      Width = 46
      Height = 13
      Caption = 'AA Level:'
    end
    object LblEnterText: TLabel
      Left = 4
      Top = 6
      Width = 78
      Height = 13
      Caption = 'Enter text here:'
    end
    object BtnTextOut: TSpeedButton
      Left = 116
      Top = 32
      Width = 22
      Height = 22
      GroupIndex = 1
      Down = True
      Caption = '0'
      Layout = blGlyphBottom
      OnClick = BtnTextOutClick
    end
    object BtnAntialias1: TSpeedButton
      Tag = 1
      Left = 140
      Top = 32
      Width = 22
      Height = 22
      GroupIndex = 1
      Caption = '1'
      Layout = blGlyphBottom
      OnClick = BtnTextOutClick
    end
    object BtnAntialias2: TSpeedButton
      Tag = 2
      Left = 164
      Top = 32
      Width = 22
      Height = 22
      GroupIndex = 1
      Caption = '2'
      Layout = blGlyphBottom
      OnClick = BtnTextOutClick
    end
    object BtnAntialias3: TSpeedButton
      Tag = 3
      Left = 188
      Top = 32
      Width = 22
      Height = 22
      GroupIndex = 1
      Caption = '3'
      Layout = blGlyphBottom
      OnClick = BtnTextOutClick
    end
    object BtnAntialias4: TSpeedButton
      Tag = 4
      Left = 212
      Top = 32
      Width = 22
      Height = 22
      GroupIndex = 1
      Caption = '4'
      Layout = blGlyphBottom
      OnClick = BtnTextOutClick
    end
    object BtnClearType: TSpeedButton
      Tag = -1
      Left = 92
      Top = 32
      Width = 22
      Height = 22
      GroupIndex = 1
      Caption = '-1'
      Layout = blGlyphBottom
      OnClick = BtnTextOutClick
    end
    object EditText: TEdit
      Left = 92
      Top = 4
      Width = 234
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      TabOrder = 0
      Text = 'EditText'
      OnChange = EditTextChange
    end
    object BtnClickMe: TButton
      Left = 253
      Top = 32
      Width = 73
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Click me!'
      TabOrder = 1
      OnClick = BtnClickMeClick
    end
  end
end
