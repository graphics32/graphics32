object MainForm: TMainForm
  Left = 255
  Top = 121
  Caption = 'Image View Layers Example'
  ClientHeight = 575
  ClientWidth = 787
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ImgView: TImgView32
    Left = 0
    Top = 0
    Width = 656
    Height = 575
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 16
    SizeGrip = sgNone
    OverSize = 0
    TabOrder = 0
    TabStop = True
    OnMouseDown = ImgViewMouseDown
    OnMouseWheelDown = ImgViewMouseWheelDown
    OnMouseWheelUp = ImgViewMouseWheelUp
    OnPaintStage = ImgViewPaintStage
  end
  object SidePanel: TPanel
    Left = 656
    Top = 0
    Width = 131
    Height = 575
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object pnlImage: TPanel
      Left = 0
      Top = 0
      Width = 131
      Height = 130
      Align = alTop
      TabOrder = 0
      Visible = False
      object lbScale: TLabel
        Left = 8
        Top = 24
        Width = 29
        Height = 13
        Caption = 'Scale:'
      end
      object ScaleCombo: TComboBox
        Left = 16
        Top = 40
        Width = 105
        Height = 21
        DropDownCount = 9
        TabOrder = 0
        Text = '100%'
        OnChange = ScaleComboChange
        Items.Strings = (
          '    25%'
          '    50%'
          '    75%'
          '  100%'
          '  200%'
          '  300%'
          '  400%'
          '  800%'
          '1600%')
      end
      object Panel2: TPanel
        Left = 1
        Top = 1
        Width = 129
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Image Properties'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object ImageInterpolate: TCheckBox
        Left = 16
        Top = 72
        Width = 97
        Height = 17
        Caption = 'Interpolated'
        TabOrder = 2
        OnClick = ImageInterpolateClick
      end
      object cbOptRedraw: TCheckBox
        Left = 16
        Top = 96
        Width = 105
        Height = 17
        Caption = 'Optimize Repaints'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = cbOptRedrawClick
      end
    end
    object pnlBitmapLayer: TPanel
      Left = 0
      Top = 130
      Width = 131
      Height = 168
      Align = alTop
      TabOrder = 1
      Visible = False
      object lbOpacity: TLabel
        Left = 8
        Top = 24
        Width = 41
        Height = 13
        Caption = 'Opacity:'
      end
      object Panel3: TPanel
        Left = 1
        Top = 1
        Width = 129
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Bitmap Layer Properties'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object LayerOpacity: TGaugeBar
        Left = 16
        Top = 40
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 255
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 255
        OnChange = LayerOpacityChanged
      end
      object LayerInterpolate: TCheckBox
        Left = 16
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Interpolated'
        TabOrder = 2
        OnClick = LayerInterpolateClick
      end
      object LayerRescale: TButton
        Left = 16
        Top = 112
        Width = 105
        Height = 17
        Caption = 'Rescale'
        TabOrder = 3
        OnClick = LayerRescaleClick
      end
      object LayerResetScale: TButton
        Left = 16
        Top = 136
        Width = 105
        Height = 17
        Caption = 'Scale to 100%'
        TabOrder = 4
        OnClick = LayerResetScaleClick
      end
      object Cropped: TCheckBox
        Left = 16
        Top = 88
        Width = 97
        Height = 17
        Caption = 'Cropped'
        TabOrder = 5
        OnClick = CroppedClick
      end
    end
    object pnlMagnification: TPanel
      Left = 0
      Top = 298
      Width = 131
      Height = 168
      Align = alTop
      TabOrder = 2
      Visible = False
      object lbMagifierOpacity: TLabel
        Left = 8
        Top = 24
        Width = 41
        Height = 13
        Caption = 'Opacity:'
      end
      object lbMagnification: TLabel
        Left = 8
        Top = 64
        Width = 67
        Height = 13
        Caption = 'Magnification:'
      end
      object lbRotation: TLabel
        Left = 8
        Top = 104
        Width = 45
        Height = 13
        Caption = 'Rotation:'
      end
      object Panel4: TPanel
        Left = 1
        Top = 1
        Width = 129
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Magnifier (All) Properties'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object MagnOpacity: TGaugeBar
        Left = 16
        Top = 40
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 255
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 255
        OnChange = MagnChange
      end
      object MagnMagnification: TGaugeBar
        Left = 16
        Top = 80
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 50
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 10
        OnChange = MagnChange
      end
      object MagnRotation: TGaugeBar
        Left = 16
        Top = 120
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 180
        Min = -180
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 0
        OnChange = MagnChange
      end
      object MagnInterpolate: TCheckBox
        Left = 16
        Top = 144
        Width = 97
        Height = 17
        Caption = 'Interpolated'
        TabOrder = 4
        OnClick = MagnChange
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 216
    object mnFile: TMenuItem
      Caption = 'File'
      OnClick = mnFileClick
      object mnFileNew: TMenuItem
        Caption = 'New...'
        OnClick = mnFileNewClick
      end
      object mnFileOpen: TMenuItem
        Caption = 'Open...'
        OnClick = mnFileOpenClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mnPrint: TMenuItem
        Caption = 'Print'
        OnClick = mnPrintClick
      end
    end
    object mnLayers: TMenuItem
      Caption = 'Layers'
      OnClick = mnLayersClick
      object mnNewBitmapLayer: TMenuItem
        Caption = 'New Bitmap Layer'
        OnClick = mnNewBitmapLayerClick
      end
      object mnNewBitmapRGBA: TMenuItem
        Caption = 'New Bitmap Layer with Alpha Channel'
        OnClick = mnNewBitmapRGBAClick
      end
      object mnNewCustomLayer: TMenuItem
        Caption = 'New Custom Layer'
        object mnSimpleDrawing: TMenuItem
          Caption = 'Simple Drawing Layer'
          OnClick = mnSimpleDrawingClick
        end
        object mnMagnifier: TMenuItem
          Caption = 'Magnifier'
          OnClick = mnMagnifierClick
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnFlatten: TMenuItem
        Caption = 'Flatten Layers'
        OnClick = mnFlattenClick
      end
    end
    object mnArrange: TMenuItem
      Caption = 'Selection'
      OnClick = mnArrangeClick
      object mnBringFront: TMenuItem
        Tag = 1
        Caption = 'Bring to Front'
        OnClick = mnReorder
      end
      object mnSendBack: TMenuItem
        Tag = 2
        Caption = 'Send to Back'
        OnClick = mnReorder
      end
      object N1: TMenuItem
        Caption = '-'
        OnClick = mnReorder
      end
      object mnLevelUp: TMenuItem
        Tag = 3
        Caption = 'Up One Level'
        OnClick = mnReorder
      end
      object mnLevelDown: TMenuItem
        Tag = 4
        Caption = 'Down one Level'
        OnClick = mnReorder
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnScaled: TMenuItem
        Caption = 'Scaled'
        Checked = True
        OnClick = mnScaledClick
      end
      object mnDelete: TMenuItem
        Caption = 'Delete'
        OnClick = mnDeleteClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnFlipHorz: TMenuItem
        Caption = 'Flip Horizontally'
        OnClick = mnFlipHorzClick
      end
      object mnFlipVert: TMenuItem
        Caption = 'Flip Vertically'
        OnClick = mnFlipVertClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mnRotate90: TMenuItem
        Caption = 'Rotate 90'
        OnClick = mnRotate90Click
      end
      object mnRotate180: TMenuItem
        Caption = 'Rotate 180'
        OnClick = mnRotate180Click
      end
      object mnRotate270: TMenuItem
        Caption = 'Rotate 270'
        OnClick = mnRotate270Click
      end
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 248
  end
  object SaveDialog: TSaveDialog
    Left = 280
  end
end
