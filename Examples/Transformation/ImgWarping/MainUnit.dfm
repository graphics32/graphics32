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
  object PnlMain: TPanel
    Left = 482
    Top = 0
    Width = 141
    Height = 623
    Align = alRight
    BevelOuter = bvNone
    BevelWidth = 2
    Caption = 'PnlMain'
    TabOrder = 0
    object PnlGeneral: TPanel
      Left = 0
      Top = 0
      Width = 141
      Height = 73
      Align = alTop
      TabOrder = 0
      object LblGeneral: TLabel
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
      object LblRemapScale: TLabel
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
    object PnlBrush: TPanel
      Left = 0
      Top = 233
      Width = 141
      Height = 304
      Align = alTop
      TabOrder = 1
      object LblBrush: TLabel
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
      object LblBrushFeather: TLabel
        Left = 8
        Top = 88
        Width = 42
        Height = 13
        Caption = 'Feather:'
      end
      object LblBrushPressure: TLabel
        Left = 8
        Top = 56
        Width = 46
        Height = 13
        Caption = 'Pressure:'
      end
      object LblBrushPinch: TLabel
        Left = 8
        Top = 120
        Width = 29
        Height = 13
        Caption = 'Pinch:'
      end
      object LblBrushSize: TLabel
        Left = 8
        Top = 24
        Width = 23
        Height = 13
        Caption = 'Size:'
        Transparent = True
      end
      object LblBrushMesh: TLabel
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
      object LblWaroTool: TLabel
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
      object LblParam: TLabel
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
    TabOrder = 1
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
    object MnuFile: TMenuItem
      Caption = 'File'
      object MnuFileOpenImage: TMenuItem
        Caption = '&Open Image...'
        ShortCut = 16463
        OnClick = MnuFileOpenImageClick
      end
      object MnuFileSaveImage: TMenuItem
        Caption = '&Save Image...'
        ShortCut = 16467
        OnClick = MnuFileSaveImageClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MnuFileOpenMesh: TMenuItem
        Caption = 'Open Mesh...'
        OnClick = MnuFileOpenMeshClick
      end
      object MnuFileSaveMesh: TMenuItem
        Caption = 'Save Mesh...'
        OnClick = MnuFileSaveMeshClick
      end
      object MnuResetMesh: TMenuItem
        Caption = 'Reset Mesh'
        ShortCut = 16466
        OnClick = MnuResetMeshClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MnuFileExit: TMenuItem
        Caption = 'Exit'
        OnClick = MnuFileExitClick
      end
    end
    object MnuSampling: TMenuItem
      Caption = 'Sampling'
      object MnuSupersampleNow: TMenuItem
        Caption = 'Supersample Now!'
        OnClick = MnuSupersampleNowClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MnuSamplingKernel: TMenuItem
        Caption = 'Sampling Kernel'
      end
      object MnuKernelMode: TMenuItem
        Caption = 'KernelMode'
        object MnuKernelModeDefault: TMenuItem
          AutoCheck = True
          Caption = 'kmDefault (slow, but exact)'
          RadioItem = True
          OnClick = MnuKernelModeClick
        end
        object MnuKernelModeTableNearest: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'kmTableNearest (fastest, "curve" sampling)'
          RadioItem = True
          OnClick = MnuKernelModeClick
        end
        object MnuKernelModeTableLinear: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = 'kmTableLinear (fast, "curve" approximation)'
          Checked = True
          RadioItem = True
          OnClick = MnuKernelModeClick
        end
      end
      object MnuSamplingGrid: TMenuItem
        Caption = 'Sampling Grid'
        object MnuSamplingGrid2x2: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = '2x2'
          RadioItem = True
          OnClick = MnuSamplingGridClick
        end
        object MnuSamplingGrid3x3: TMenuItem
          Tag = 3
          AutoCheck = True
          Caption = '3x3'
          Checked = True
          RadioItem = True
          OnClick = MnuSamplingGridClick
        end
        object MnuSamplingGrid5x5: TMenuItem
          Tag = 5
          AutoCheck = True
          Caption = '5x5'
          RadioItem = True
          OnClick = MnuSamplingGridClick
        end
        object MnuSamplingGrid7x7: TMenuItem
          Tag = 7
          AutoCheck = True
          Caption = '7x7'
          RadioItem = True
          OnClick = MnuSamplingGridClick
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MnuBilinearWarp: TMenuItem
        AutoCheck = True
        Caption = 'Bilinear Warp'
        Checked = True
        OnClick = MnuBilinearWarpClick
      end
    end
  end
end
