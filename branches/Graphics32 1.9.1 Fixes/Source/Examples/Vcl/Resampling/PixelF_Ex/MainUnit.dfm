object MainForm: TMainForm
  Left = 272
  Top = 282
  BorderStyle = bsToolWindow
  Caption = 'PixelF Example'
  ClientHeight = 258
  ClientWidth = 459
  Color = clBtnFace
  Constraints.MaxHeight = 287
  Constraints.MaxWidth = 467
  Constraints.MinHeight = 280
  Constraints.MinWidth = 465
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    459
    258)
  PixelsPerInch = 96
  TextHeight = 13
  object Image32: TImage32
    Left = 8
    Top = 8
    Width = 300
    Height = 244
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = True
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnPaintStage = Image32PaintStage
  end
  object pnlSettings: TPanel
    Left = 316
    Top = 0
    Width = 143
    Height = 258
    Align = alRight
    TabOrder = 1
    object lbTwirlPower: TLabel
      Left = 8
      Top = 32
      Width = 58
      Height = 13
      Caption = 'Twirl Power:'
    end
    object pnlTwirlDistortion: TPanel
      Left = 1
      Top = 1
      Width = 141
      Height = 16
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Twirl Distortion'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object gbTwist: TGaugeBar
      Left = 8
      Top = 48
      Width = 129
      Height = 12
      Backgnd = bgPattern
      HandleSize = 16
      Max = 50
      Min = -50
      ShowArrows = False
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = gbTwistChange
    end
    object rbGetPixelFS: TRadioButton
      Left = 8
      Top = 96
      Width = 129
      Height = 17
      Caption = 'TBitmap32.PixelFS'
      TabOrder = 2
      OnClick = gbTwistChange
    end
    object rbPixelS: TRadioButton
      Left = 8
      Top = 72
      Width = 105
      Height = 17
      Caption = 'TBitmap32.PixelS'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = gbTwistChange
    end
  end
end
