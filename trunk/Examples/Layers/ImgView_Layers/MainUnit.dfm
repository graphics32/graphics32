object MainForm: TMainForm
  Left = 255
  Top = 121
  Caption = 'Image View Layers Example'
  ClientHeight = 590
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
    Height = 590
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
  object PnlControl: TPanel
    Left = 656
    Top = 0
    Width = 131
    Height = 590
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object PnlImage: TPanel
      Left = 0
      Top = 0
      Width = 131
      Height = 130
      Align = alTop
      TabOrder = 0
      Visible = False
      object LblScale: TLabel
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
      object PnlImageHeader: TPanel
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
      object CbxImageInterpolate: TCheckBox
        Left = 16
        Top = 72
        Width = 97
        Height = 17
        Caption = 'Interpolated'
        TabOrder = 2
        OnClick = CbxImageInterpolateClick
      end
      object CbxOptRedraw: TCheckBox
        Left = 16
        Top = 96
        Width = 105
        Height = 17
        Caption = 'Optimize Repaints'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = CbxOptRedrawClick
      end
    end
    object PnlBitmapLayer: TPanel
      Left = 0
      Top = 130
      Width = 131
      Height = 168
      Align = alTop
      TabOrder = 1
      Visible = False
      object LblOpacity: TLabel
        Left = 8
        Top = 24
        Width = 41
        Height = 13
        Caption = 'Opacity:'
      end
      object PnlBitmapLayerHeader: TPanel
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
      object GbrLayerOpacity: TGaugeBar
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
      object CbxLayerInterpolate: TCheckBox
        Left = 16
        Top = 64
        Width = 97
        Height = 17
        Caption = '&Interpolated'
        TabOrder = 2
        OnClick = CbxLayerInterpolateClick
      end
      object BtnLayerRescale: TButton
        Left = 16
        Top = 112
        Width = 105
        Height = 17
        Caption = 'Rescale'
        TabOrder = 3
        OnClick = BtnLayerRescaleClick
      end
      object BtnLayerResetScale: TButton
        Left = 16
        Top = 136
        Width = 105
        Height = 17
        Caption = 'Scale to 100%'
        TabOrder = 4
        OnClick = BtnLayerResetScaleClick
      end
      object CbxCropped: TCheckBox
        Left = 16
        Top = 88
        Width = 97
        Height = 17
        Caption = '&Cropped'
        TabOrder = 5
        OnClick = CbxCroppedClick
      end
    end
    object PnlMagnification: TPanel
      Left = 0
      Top = 408
      Width = 131
      Height = 168
      Align = alTop
      TabOrder = 2
      Visible = False
      object LblMagifierOpacity: TLabel
        Left = 8
        Top = 24
        Width = 41
        Height = 13
        Caption = 'Opacity:'
      end
      object LblMagnification: TLabel
        Left = 8
        Top = 64
        Width = 67
        Height = 13
        Caption = 'Magnification:'
      end
      object LblRotation: TLabel
        Left = 8
        Top = 104
        Width = 45
        Height = 13
        Caption = 'Rotation:'
      end
      object PnlMagnificationHeader: TPanel
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
      object GbrMagnOpacity: TGaugeBar
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
        OnChange = PropertyChange
      end
      object GbrMagnMagnification: TGaugeBar
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
        OnChange = PropertyChange
      end
      object GbrMagnRotation: TGaugeBar
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
        OnChange = PropertyChange
      end
      object CbxMagnInterpolate: TCheckBox
        Left = 16
        Top = 144
        Width = 97
        Height = 17
        Caption = 'Interpolated'
        TabOrder = 4
        OnClick = PropertyChange
      end
    end
    object PnlButtonMockup: TPanel
      Left = 0
      Top = 298
      Width = 131
      Height = 110
      Align = alTop
      TabOrder = 3
      Visible = False
      object LblBorderRadius: TLabel
        Left = 8
        Top = 24
        Width = 71
        Height = 13
        Caption = 'Border Radius:'
      end
      object LblBorderWidth: TLabel
        Left = 8
        Top = 64
        Width = 67
        Height = 13
        Caption = 'Border Width:'
      end
      object PnlButtonMockupHeader: TPanel
        Left = 1
        Top = 1
        Width = 129
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Button (All) Properties'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object GbrBorderRadius: TGaugeBar
        Left = 16
        Top = 40
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 20
        Min = 1
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 5
        OnChange = PropertyChange
      end
      object GbrBorderWidth: TGaugeBar
        Left = 16
        Top = 80
        Width = 105
        Height = 12
        Backgnd = bgPattern
        HandleSize = 16
        Max = 30
        Min = 10
        ShowArrows = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 20
        OnChange = PropertyChange
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 64
    Top = 8
    object MnuFile: TMenuItem
      Caption = 'File'
      OnClick = MnuFileClick
      object MnuFileNew: TMenuItem
        Caption = 'New...'
        OnClick = MnuFileNewClick
      end
      object MnuFileOpen: TMenuItem
        Caption = 'Open...'
        OnClick = MnuFileOpenClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MnuPrint: TMenuItem
        Caption = 'Print'
        OnClick = MnuPrintClick
      end
    end
    object MnuLayers: TMenuItem
      Caption = 'Layers'
      OnClick = MnuLayersClick
      object MnuNewBitmapLayer: TMenuItem
        Caption = 'New Bitmap Layer'
        OnClick = MnuNewBitmapLayerClick
      end
      object MnuNewBitmapRGBA: TMenuItem
        Caption = 'New Bitmap Layer with Alpha Channel'
        OnClick = MnuNewBitmapRGBAClick
      end
      object MnuNewCustomLayer: TMenuItem
        Caption = 'New Custom Layer'
        object MnuSimpleDrawing: TMenuItem
          Caption = 'Simple Drawing Layer'
          OnClick = MnuSimpleDrawingClick
        end
        object MnuButtonMockup: TMenuItem
          Caption = 'Button Mockup'
          OnClick = MnuButtonMockupClick
        end
        object MnuMagnifier: TMenuItem
          Caption = 'Magnifier'
          OnClick = MnuMagnifierClick
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MnuFlatten: TMenuItem
        Caption = 'Flatten Layers'
        OnClick = MnuFlattenClick
      end
    end
    object MimArrange: TMenuItem
      Caption = 'Selection'
      OnClick = MimArrangeClick
      object MnuBringFront: TMenuItem
        Tag = 1
        Caption = 'Bring to Front'
        OnClick = MnuReorderClick
      end
      object MnuSendBack: TMenuItem
        Tag = 2
        Caption = 'Send to Back'
        OnClick = MnuReorderClick
      end
      object N1: TMenuItem
        Caption = '-'
        OnClick = MnuReorderClick
      end
      object MnuLevelUp: TMenuItem
        Tag = 3
        Caption = 'Up One Level'
        OnClick = MnuReorderClick
      end
      object MnuLevelDown: TMenuItem
        Tag = 4
        Caption = 'Down one Level'
        OnClick = MnuReorderClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MnuScaled: TMenuItem
        Caption = 'Scaled'
        Checked = True
        OnClick = MnuScaledClick
      end
      object MnuDelete: TMenuItem
        Caption = 'Delete'
        OnClick = MnuDeleteClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MnuFlipHorz: TMenuItem
        Caption = 'Flip Horizontally'
        OnClick = MnuFlipHorzClick
      end
      object MnuFlipVert: TMenuItem
        Caption = 'Flip Vertically'
        OnClick = MnuFlipVertClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MnuRotate90: TMenuItem
        Caption = 'Rotate 90'
        OnClick = MnuRotate90Click
      end
      object MnuRotate180: TMenuItem
        Caption = 'Rotate 180'
        OnClick = MnuRotate180Click
      end
      object MnuRotate270: TMenuItem
        Caption = 'Rotate 270'
        OnClick = MnuRotate270Click
      end
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 64
    Top = 56
  end
  object SaveDialog: TSaveDialog
    Left = 64
    Top = 104
  end
end
