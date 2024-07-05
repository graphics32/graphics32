object MainForm: TMainForm
  Left = 272
  Top = 282
  BorderStyle = bsToolWindow
  Caption = 'PixelF Example'
  ClientHeight = 265
  ClientWidth = 451
  Color = clBtnFace
  ParentFont = True
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object Image32: TImage32
    Left = 0
    Top = 0
    Width = 308
    Height = 265
    Align = alClient
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCenter
    Scale = 1.000000000000000000
    ScaleMode = smOptimalScaled
    TabOrder = 0
    OnPaintStage = Image32PaintStage
    ExplicitLeft = 2
    ExplicitWidth = 300
    ExplicitHeight = 363
  end
  object PnlSettings: TPanel
    Left = 308
    Top = 0
    Width = 143
    Height = 265
    Align = alRight
    BevelEdges = [beLeft]
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitHeight = 363
    object LblTwirlPower: TLabel
      Left = 8
      Top = 32
      Width = 63
      Height = 15
      Caption = 'Twirl Power:'
    end
    object PnlTwirlDistortion: TPanel
      Left = 0
      Top = 0
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
      ExplicitLeft = 1
      ExplicitTop = 1
    end
    object GbrTwist: TGaugeBar
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
      OnChange = GbrTwistChange
    end
    object RbxGetPixelFS: TRadioButton
      Left = 8
      Top = 96
      Width = 129
      Height = 17
      Caption = 'TBitmap32.PixelFS'
      TabOrder = 2
      OnClick = GbrTwistChange
    end
    object RbxPixelS: TRadioButton
      Left = 8
      Top = 72
      Width = 105
      Height = 17
      Caption = 'TBitmap32.PixelS'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = GbrTwistChange
    end
  end
end
