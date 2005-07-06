object MainForm: TMainForm
  Left = 410
  Top = 241
  Width = 624
  Height = 635
  Caption = 'Image Warping Ex'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
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
    Left = 0
    Top = 0
    Width = 141
    Height = 608
    Align = alLeft
    BevelOuter = bvNone
    BevelWidth = 2
    BorderStyle = bsSingle
    Caption = 'MainPanel'
    TabOrder = 1
    object GeneralPanel: TPanel
      Left = 0
      Top = 0
      Width = 137
      Height = 137
      Align = alTop
      TabOrder = 0
      object Label4: TLabel
        Left = 1
        Top = 1
        Width = 135
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
      object ToolBar: TToolBar
        Left = 8
        Top = 65
        Width = 121
        Height = 59
        Align = alNone
        ButtonHeight = 19
        ButtonWidth = 92
        Caption = 'ToolBar'
        Flat = True
        Indent = 8
        List = True
        ShowCaptions = True
        TabOrder = 0
        object ImgButton: TToolButton
          Left = 8
          Top = 0
          Hint = 'Image ...'
          Caption = 'Image options'
          DropdownMenu = ImgBtnPopup
          ImageIndex = 0
          PopupMenu = ImgBtnPopup
          Wrap = True
          Style = tbsDropDown
          OnClick = ImgButtonClick
        end
        object MshButton: TToolButton
          Left = 8
          Top = 19
          Hint = 'Mesh ...'
          Caption = 'Mesh options'
          DropdownMenu = MshBtnPopup
          ImageIndex = 1
          PopupMenu = MshBtnPopup
          Wrap = True
          Style = tbsDropDown
          OnClick = ImgButtonClick
        end
        object SampleButton: TToolButton
          Left = 8
          Top = 38
          Hint = 'Sampling options...'
          Caption = 'Sampling options'
          DropdownMenu = SmplBtnPopup
          ImageIndex = 2
          PopupMenu = SmplBtnPopup
          Style = tbsDropDown
          OnClick = ImgButtonClick
        end
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
      Top = 297
      Width = 137
      Height = 304
      Align = alTop
      TabOrder = 1
      object Label6: TLabel
        Left = 1
        Top = 1
        Width = 135
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
        OnMouseUp = GaugeMouseUp
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
        OnMouseUp = GaugeMouseUp
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
        OnMouseUp = GaugeMouseUp
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
        RepaintMode = rmFull
        TabOrder = 4
        OnResize = BrushMeshPreviewResize
      end
    end
    object ToolPanel: TPanel
      Left = 0
      Top = 137
      Width = 137
      Height = 160
      Align = alTop
      TabOrder = 2
      object Label7: TLabel
        Left = 1
        Top = 1
        Width = 135
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
    Left = 141
    Top = 0
    Width = 475
    Height = 608
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    SizeGrip = sgAuto
    OverSize = 0
    TabOrder = 2
    OnMouseDown = DstImgMouseDown
    OnMouseMove = DstImgMouseMove
    OnMouseUp = DstImgMouseUp
    OnPaintStage = DstImgPaintStage
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 176
    Top = 8
  end
  object UpdateTimer: TTimer
    Interval = 50
    OnTimer = UpdateTimerTimer
    Left = 240
    Top = 8
  end
  object OpenMeshDialog: TOpenDialog
    Filter = 'Photoshop Liquify Mesh (*.msh)|*.msh'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 208
    Top = 8
  end
  object SaveMeshDialog: TSaveDialog
    Filter = 'Photoshop Liquify Mesh (*.msh)|*.msh'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 208
    Top = 40
  end
  object ImgBtnPopup: TPopupMenu
    Left = 144
    Top = 8
    object SaveImage1: TMenuItem
      Caption = 'Save Image ...'
      OnClick = SaveImage1Click
    end
    object OpenImage1: TMenuItem
      Caption = 'Open Image ...'
      OnClick = OpenImage1Click
    end
  end
  object MshBtnPopup: TPopupMenu
    Left = 144
    Top = 40
    object ResetMesh1: TMenuItem
      Caption = 'Reset Mesh'
      OnClick = ResetMesh1Click
    end
    object SaveMesh1: TMenuItem
      Caption = 'Save Mesh ...'
      OnClick = SaveMesh1Click
    end
    object OpenMesh1: TMenuItem
      Caption = 'Open Mesh ...'
      OnClick = OpenMesh1Click
    end
  end
  object SmplBtnPopup: TPopupMenu
    Left = 144
    Top = 72
    object SupersampleNow: TMenuItem
      Caption = 'Supersample Now!'
      OnClick = SupersampleNowClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SamplingKernel1: TMenuItem
      Caption = 'Sampling Kernel'
    end
    object KernelMode1: TMenuItem
      Caption = 'KernelMode'
      object kmDefaultrealtime1: TMenuItem
        AutoCheck = True
        Caption = 'kmDefault (slow, but exact)'
        RadioItem = True
        OnClick = kmDefaultrealtime1Click
      end
      object kmNearestfastbutfair1: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = 'kmTableNearest (fastest, "curve" sampling)'
        RadioItem = True
        OnClick = kmDefaultrealtime1Click
      end
      object kmTableLinear1: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'kmTableLinear (fast, "curve" approximation)'
        Checked = True
        RadioItem = True
        OnClick = kmDefaultrealtime1Click
      end
    end
    object SamplingGrid1: TMenuItem
      Caption = 'Sampling Grid'
      object N2x21: TMenuItem
        Tag = 2
        Caption = '2x2'
        OnClick = N3x31Click
      end
      object N3x31: TMenuItem
        Tag = 3
        AutoCheck = True
        Caption = '3x3'
        Checked = True
        RadioItem = True
        OnClick = N3x31Click
      end
      object N5x51: TMenuItem
        Tag = 5
        AutoCheck = True
        Caption = '5x5'
        RadioItem = True
        OnClick = N3x31Click
      end
      object N7x71: TMenuItem
        Tag = 7
        AutoCheck = True
        Caption = '7x7'
        RadioItem = True
        OnClick = N3x31Click
      end
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Bi1: TMenuItem
      AutoCheck = True
      Caption = 'Bilinear Warp'
      Checked = True
      OnClick = Bi1Click
    end
  end
  object SavePictureDialog: TSavePictureDialog
    DefaultExt = '*.bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    FilterIndex = 0
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 176
    Top = 40
  end
end
