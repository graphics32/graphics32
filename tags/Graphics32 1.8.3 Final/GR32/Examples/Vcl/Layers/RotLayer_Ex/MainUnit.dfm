object Form1: TForm1
  Left = 208
  Top = 110
  Width = 373
  Height = 504
  Caption = 'Rotation Layer Example'
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
  object Label1: TLabel
    Left = 12
    Top = 313
    Width = 76
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'RotLayer.Angle:'
  end
  object Label2: TLabel
    Left = 12
    Top = 345
    Width = 96
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'RotLayer.Position.X:'
  end
  object Label3: TLabel
    Left = 12
    Top = 373
    Width = 96
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'RotLayer.Position.Y:'
  end
  object Label4: TLabel
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
    Color = clAppWorkSpace
    ParentColor = False
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsMac
    OverSize = 0
    TabOrder = 0
  end
  object GaugeBar1: TGaugeBar
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
    OnChange = GaugeBar1Change
  end
  object GaugeBar2: TGaugeBar
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
    OnChange = GaugeBar2Change
  end
  object GaugeBar3: TGaugeBar
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
    OnChange = GaugeBar2Change
  end
  object GaugeBar4: TGaugeBar
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
    OnChange = GaugeBar4Change
  end
  object CheckBox1: TCheckBox
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
    OnClick = CheckBox1Click
  end
end
