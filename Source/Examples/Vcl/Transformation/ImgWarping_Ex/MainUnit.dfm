object MainForm: TMainForm
  Left = 254
  Top = 244
  Caption = 'Image Warping Example'
  ClientHeight = 623
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 105
    Height = 137
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
  end
  object MainPanel: TPanel
    Left = 482
    Top = 0
    Width = 141
    Height = 623
    Align = alRight
    BevelOuter = bvNone
    BevelWidth = 2
    Caption = 'MainPanel'
    TabOrder = 1
    object GeneralPanel: TPanel
      Left = 0
      Top = 0
      Width = 141
      Height = 73
      Align = alTop
      TabOrder = 0
      object Label4: TLabel
        Left = 1
        Top = 1
        Width = 139
        Height = 16
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'General'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object Label5: TLabel
        Left = 8
        Top = 24
        Width = 65
        Height = 13
        Caption = 'Remap Scale:'
      end
      object ScaleBar: TGaugeBar
        Left = 8
        Top = 40
        Width = 121
        Height = 15
        Backgnd = bgPattern
        LargeChange = 10
        Max = 300
        Min = -300
        ShowHandleGrip = False
        Style = rbsMac
        Position = 100
        OnMouseUp = ScaleBarMouseUp
      end
    end
    object BrushPanel: TPanel
      Left = 0
      Top = 233
      Width = 141
      Height = 304
      Align = alTop
      TabOrder = 1
      object Label6: TLabel
        Left = 1
        Top = 1
        Width = 139
        Height = 16
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'Brush'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object FeatherLabel: TLabel
        Left = 8
        Top = 88
        Width = 42
        Height = 13
        Caption = 'Feather:'
      end
      object Label2: TLabel
        Left = 8
        Top = 56
        Width = 46
        Height = 13
        Caption = 'Pressure:'
      end
      object Label3: TLabel
        Left = 8
        Top = 120
        Width = 29
        Height = 13
        Caption = 'Pinch:'
      end
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 23
        Height = 13
        Caption = 'Size:'
        Transparent = True
      end
      object Bevel2: TBevel
        Left = 68
        Top = 131
        Width = 25
        Height = 25
        Shape = bsLeftLine
      end
      object Label9: TLabel
        Left = 8
        Top = 160
        Width = 59
        Height = 13
        Caption = 'Brush Mesh:'
        Transparent = True
      end
      object FeatherBar: TGaugeBar
        Left = 8
        Top = 104
        Width = 121
        Height = 15
        Backgnd = bgPattern
        LargeChange = 10
        ShowHandleGrip = False
        Style = rbsMac
        Position = 12
        OnChange = PressureBarChange
      end
      object PressureBar: TGaugeBar
        Left = 8
        Top = 72
        Width = 121
        Height = 15
        Backgnd = bgPattern
        LargeChange = 10
        ShowHandleGrip = False
        Style = rbsMac
        Position = 50
        OnChange = PressureBarChange
      end
      object PinchBar: TGaugeBar
        Left = 8
        Top = 136
        Width = 121
        Height = 15
        Backgnd = bgPattern
        LargeChange = 10
        Min = -100
        ShowHandleGrip = False
        Style = rbsMac
        Position = 0
        OnChange = PressureBarChange
      end
      object SizeBar: TGaugeBar
        Left = 8
        Top = 40
        Width = 121
        Height = 15
        Backgnd = bgPattern
        LargeChange = 10
        Max = 500
        Min = 5
        ShowHandleGrip = False
        Style = rbsMac
        Position = 100
        OnChange = SizeBarChange
        OnMouseUp = GaugeMouseUp
      end
      object BrushMeshPreview: TPaintBox32
        Left = 8
        Top = 176
        Width = 121
        Height = 121
        TabOrder = 4
        OnResize = BrushMeshPreviewResize
      end
    end
    object ToolPanel: TPanel
      Left = 0
      Top = 73
      Width = 141
      Height = 160
      Align = alTop
      TabOrder = 2
      object Label7: TLabel
        Left = 1
        Top = 1
        Width = 139
        Height = 16
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'Warp Tool'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object ParamLabel: TLabel
        Left = 8
        Top = 88
        Width = 30
        Height = 13
        Caption = 'Param'
        Enabled = False
      end
      object RateLabel: TLabel
        Left = 8
        Top = 120
        Width = 27
        Height = 13
        Caption = 'Rate:'
        Enabled = False
      end
      object ToolGroup: TRadioGroup
        Left = 8
        Top = 24
        Width = 121
        Height = 57
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Warp'
          'Zoom'
          'Twirl'
          'Flower')
        TabOrder = 0
        OnClick = ToolGroupClick
      end
      object ParamBar: TGaugeBar
        Left = 8
        Top = 104
        Width = 121
        Height = 15
        Backgnd = bgPattern
        Enabled = False
        LargeChange = 10
        ShowHandleGrip = False
        Style = rbsMac
        Position = 20
        OnChange = PressureBarChange
        OnMouseUp = GaugeMouseUp
      end
      object RateBar: TGaugeBar
        Left = 8
        Top = 136
        Width = 121
        Height = 15
        Backgnd = bgPattern
        Enabled = False
        LargeChange = 10
        Max = 399
        ShowHandleGrip = False
        Style = rbsMac
        Position = 350
        OnChange = RateBarChange
        OnMouseUp = GaugeMouseUp
      end
    end
  end
  object DstImg: TImgView32
    Left = 0
    Top = 0
    Width = 482
    Height = 623
    Align = alClient
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 16
    ScrollBars.Visibility = svAuto
    OverSize = 0
    TabOrder = 2
    OnMouseDown = DstImgMouseDown
    OnMouseMove = DstImgMouseMove
    OnMouseUp = DstImgMouseUp
    OnPaintStage = DstImgPaintStage
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 136
    Top = 8
  end
  object UpdateTimer: TTimer
    Interval = 50
    OnTimer = UpdateTimerTimer
    Left = 344
    Top = 8
  end
  object OpenMeshDialog: TOpenDialog
    Filter = 'Photoshop Liquify Mesh (*.msh)|*.msh'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 240
    Top = 8
  end
  object SaveMeshDialog: TSaveDialog
    Filter = 'Photoshop Liquify Mesh (*.msh)|*.msh'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 240
    Top = 64
  end
  object SavePictureDialog: TSavePictureDialog
    DefaultExt = '*.bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    FilterIndex = 0
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 136
    Top = 64
  end
  object MainMenu: TMainMenu
    Left = 344
    Top = 64
    object File1: TMenuItem
      Caption = 'File'
      object mOpenImage: TMenuItem
        Caption = '&Open Image...'
        ShortCut = 16463
        OnClick = mOpenImageClick
      end
      object mSaveImage: TMenuItem
        Caption = '&Save Image...'
        ShortCut = 16467
        OnClick = mSaveImageClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mOpenMesh: TMenuItem
        Caption = 'Open Mesh...'
        OnClick = mOpenMeshClick
      end
      object mSaveMesh: TMenuItem
        Caption = 'Save Mesh...'
        OnClick = mSaveMeshClick
      end
      object mResetMesh: TMenuItem
        Caption = 'Reset Mesh'
        ShortCut = 16466
        OnClick = mResetMeshClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mExit: TMenuItem
        Caption = 'Exit'
        OnClick = mExitClick
      end
    end
    object Sampling1: TMenuItem
      Caption = 'Sampling'
      object mSupersampleNow: TMenuItem
        Caption = 'Supersample Now!'
        OnClick = mSupersampleNowClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mSamplingKernel: TMenuItem
        Caption = 'Sampling Kernel'
      end
      object mKernelMode: TMenuItem
        Caption = 'KernelMode'
        object mKmDefault: TMenuItem
          Caption = 'kmDefault (slow, but exact)'
          RadioItem = True
          OnClick = mkmDefaultClick
        end
        object mKmTableNearest: TMenuItem
          Tag = 1
          Caption = 'kmTableNearest (fastest, "curve" sampling)'
          RadioItem = True
          OnClick = mkmDefaultClick
        end
        object mKmTableLinear: TMenuItem
          Tag = 2
          Caption = 'kmTableLinear (fast, "curve" approximation)'
          Checked = True
          RadioItem = True
          OnClick = mkmDefaultClick
        end
      end
      object mSamplingGrid: TMenuItem
        Caption = 'Sampling Grid'
        object m2x2: TMenuItem
          Tag = 2
          Caption = '2x2'
          OnClick = m3x3Click
        end
        object m3x3: TMenuItem
          Tag = 3
          Caption = '3x3'
          Checked = True
          RadioItem = True
          OnClick = m3x3Click
        end
        object m5x5: TMenuItem
          Tag = 5
          Caption = '5x5'
          RadioItem = True
          OnClick = m3x3Click
        end
        object m7x7: TMenuItem
          Tag = 7
          Caption = '7x7'
          RadioItem = True
          OnClick = m3x3Click
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mBilinearWarp: TMenuItem
        Caption = 'Bilinear Warp'
        Checked = True
        OnClick = Bi1Click
      end
    end
  end
end
