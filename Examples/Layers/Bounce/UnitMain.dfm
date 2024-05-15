object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Easing example'
  ClientHeight = 467
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnResize = FormResize
  TextHeight = 15
  object Image32: TImage32
    Left = 0
    Top = 0
    Width = 409
    Height = 406
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Color = clBlack
    ParentColor = False
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
  object PanelOptions: TPanel
    Left = 0
    Top = 406
    Width = 409
    Height = 61
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    DesignSize = (
      409
      61)
    object CheckBoxAnimateHorizontal: TCheckBox
      Left = 12
      Top = 12
      Width = 145
      Height = 17
      Caption = 'Animate horizontal'
      TabOrder = 0
      OnClick = CheckBoxAnimateHorizontalClick
    end
    object CheckBoxAnimateVertical: TCheckBox
      Left = 12
      Top = 35
      Width = 145
      Height = 17
      Caption = 'Animate vertical'
      TabOrder = 1
      OnClick = CheckBoxAnimateVerticalClick
    end
    object ButtonBounce: TButton
      Left = 317
      Top = 20
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Jump!'
      TabOrder = 2
      OnClick = ButtonBounceClick
    end
  end
end
