object FrmLion: TFrmLion
  Left = 0
  Top = 0
  Caption = 'Lion'
  ClientHeight = 512
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox32: TPaintBox32
    Left = 0
    Top = 0
    Width = 400
    Height = 512
    Align = alClient
    TabOrder = 0
    OnMouseDown = PaintBox32MouseDown
    OnMouseUp = PaintBox32MouseUp
    OnPaintBuffer = PaintBox32PaintBuffer
    OnResize = PaintBox32Resize
  end
  object PnlSettings: TPanel
    Left = 400
    Top = 0
    Width = 177
    Height = 512
    Align = alRight
    TabOrder = 1
    DesignSize = (
      177
      512)
    object LblAlpha: TLabel
      Left = 6
      Top = 120
      Width = 31
      Height = 13
      Caption = 'Alpha:'
    end
    object LblStrokeWidth: TLabel
      Left = 6
      Top = 143
      Width = 32
      Height = 13
      Caption = 'Width:'
      Visible = False
    end
    object PnlSampler: TPanel
      Left = 1
      Top = 1
      Width = 175
      Height = 16
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Drawing'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object GbrAlpha: TGaugeBar
      Left = 46
      Top = 118
      Width = 125
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Backgnd = bgPattern
      Max = 255
      ShowHandleGrip = True
      Style = rbsMac
      Position = 255
      OnChange = GbrAlphaChange
    end
    object RgpBrush: TRadioGroup
      Left = 6
      Top = 23
      Width = 165
      Height = 66
      Caption = 'Brush'
      ItemIndex = 0
      Items.Strings = (
        'Solid'
        'Outline')
      TabOrder = 2
      OnClick = RgpBrushClick
    end
    object CbxClearBackground: TCheckBox
      Left = 6
      Top = 95
      Width = 115
      Height = 17
      Caption = 'Clear Background'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CbxClearBackgroundClick
    end
    object PnlInteraction: TPanel
      Left = 1
      Top = 177
      Width = 175
      Height = 16
      BevelOuter = bvNone
      Caption = 'Interaction'
      Color = clBtnText
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 4
    end
    object GbrWidth: TGaugeBar
      Left = 46
      Top = 141
      Width = 125
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Visible = False
      Position = 20
      OnChange = GbrWidthChange
    end
    object RgpMouse: TRadioGroup
      Left = 6
      Top = 199
      Width = 165
      Height = 66
      Caption = 'Mouse'
      Enabled = False
      ItemIndex = 0
      Items.Strings = (
        'Zoom / Rotate / Move'
        'Lens')
      TabOrder = 6
      OnClick = RgpBrushClick
    end
  end
end
