object FormRotLayer: TFormRotLayer
  Left = 208
  Top = 110
  Caption = 'Rotation Layer Example'
  ClientHeight = 477
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    365
    477)
  PixelsPerInch = 96
  TextHeight = 13
  object lbAngle: TLabel
    Left = 12
    Top = 313
    Width = 76
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'RotLayer.Angle:'
  end
  object lbPositionX: TLabel
    Left = 12
    Top = 345
    Width = 96
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'RotLayer.Position.X:'
  end
  object lbPositionY: TLabel
    Left = 12
    Top = 373
    Width = 96
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'RotLayer.Position.Y:'
  end
  object lbScale: TLabel
    Left = 12
    Top = 441
    Width = 85
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'ImgView32.Scale:'
  end
  object ImgView: TImgView32
    Left = 4
    Top = 8
    Width = 354
    Height = 294
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Color = clAppWorkSpace
    ParentColor = False
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsMac
    ScrollBars.Size = 16
    OverSize = 0
    TabOrder = 0
  end
  object gbAngle: TGaugeBar
    Left = 120
    Top = 313
    Width = 153
    Height = 16
    Anchors = [akLeft, akBottom]
    Backgnd = bgPattern
    Max = 180
    Min = -180
    ShowHandleGrip = True
    Style = rbsMac
    Position = 0
    OnChange = gbAngleChange
  end
  object gbPositionX: TGaugeBar
    Left = 120
    Top = 345
    Width = 153
    Height = 16
    Anchors = [akLeft, akBottom]
    Backgnd = bgPattern
    Max = 200
    ShowHandleGrip = True
    Style = rbsMac
    Position = 100
    OnChange = gbPositionChange
  end
  object gbPositionY: TGaugeBar
    Left = 120
    Top = 373
    Width = 153
    Height = 16
    Anchors = [akLeft, akBottom]
    Backgnd = bgPattern
    Max = 200
    ShowHandleGrip = True
    Style = rbsMac
    Position = 100
    OnChange = gbPositionChange
  end
  object gbScale: TGaugeBar
    Left = 120
    Top = 441
    Width = 153
    Height = 16
    Anchors = [akLeft, akBottom]
    Backgnd = bgPattern
    Min = -100
    ShowHandleGrip = True
    Style = rbsMac
    Position = 0
    OnChange = gbScaleChange
  end
  object cbScaled: TCheckBox
    Left = 8
    Top = 405
    Width = 125
    Height = 17
    Alignment = taLeftJustify
    Anchors = [akLeft, akBottom]
    Caption = 'RotLayer.Scaled:'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = cbScaledClick
  end
end
