object FrmMeshGradients: TFrmMeshGradients
  Left = 0
  Top = 0
  Caption = 'Mesh Gradient Demo'
  ClientHeight = 481
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    688
    481)
  PixelsPerInch = 96
  TextHeight = 13
  object PnlSettings: TPanel
    Left = 511
    Top = 0
    Width = 177
    Height = 481
    Align = alRight
    TabOrder = 0
    DesignSize = (
      177
      481)
    object LblBackgroundSampler: TLabel
      Left = 8
      Top = 23
      Width = 101
      Height = 13
      Caption = 'Background Sampler:'
    end
    object LblVertexColor: TLabel
      Left = 8
      Top = 236
      Width = 29
      Height = 13
      Caption = 'Color:'
      Visible = False
    end
    object VertexColorShape: TShape
      Left = 43
      Top = 234
      Width = 16
      Height = 16
      Visible = False
      OnMouseDown = VertexColorShapeMouseDown
    end
    object LblPower: TLabel
      Left = 8
      Top = 68
      Width = 34
      Height = 13
      Caption = 'Power:'
    end
    object PnlSampler: TPanel
      Left = 1
      Top = 1
      Width = 175
      Height = 16
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Sampler'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object CmbBackgroundSampler: TComboBox
      Left = 8
      Top = 39
      Width = 161
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 0
      TabOrder = 1
      Text = 'None'
      OnChange = CmbBackgroundSamplerChange
      Items.Strings = (
        'None'
        'Voronoi'
        'Voronoi (HQ)'
        'Shepards'
        'Custom IDW')
    end
    object PnlVertex: TPanel
      Left = 1
      Top = 206
      Width = 175
      Height = 16
      BevelOuter = bvNone
      Caption = 'Vertex'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object GbrPower: TGaugeBar
      Left = 48
      Top = 66
      Width = 121
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Backgnd = bgPattern
      Max = 10000
      Min = 1
      ShowHandleGrip = True
      Style = rbsMac
      Position = 5000
      OnChange = GbrPowerChange
    end
    object BtnStore: TButton
      Left = 14
      Top = 417
      Width = 147
      Height = 25
      Caption = '&Store Vertices'
      TabOrder = 4
      OnClick = BtnStoreClick
    end
    object BtnRecall: TButton
      Left = 14
      Top = 448
      Width = 147
      Height = 25
      Caption = '&Recall Vertices'
      Enabled = False
      TabOrder = 5
      OnClick = BtnRecallClick
    end
    object PnlDelaunayTriangulation: TPanel
      Left = 1
      Top = 107
      Width = 175
      Height = 16
      BevelOuter = bvNone
      Caption = 'Delaunay Triangulation'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
    end
    object CbxColoredPolygons: TCheckBox
      Left = 16
      Top = 129
      Width = 129
      Height = 17
      Caption = 'Show Colored Polygon'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = CbxColoredPolygonsClick
    end
  end
  object PaintBox32: TPaintBox32
    Left = 8
    Top = 8
    Width = 497
    Height = 465
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    OnDblClick = SelectVertexColorClick
    OnMouseDown = PaintBox32MouseDown
    OnMouseMove = PaintBox32MouseMove
    OnMouseUp = PaintBox32MouseUp
    OnPaintBuffer = PaintBox32PaintBuffer
  end
  object ColorDialog: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 336
    Top = 240
  end
end
