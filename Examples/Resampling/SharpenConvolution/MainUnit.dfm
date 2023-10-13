object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Sharpen convolution using a kernel sampler'
  ClientHeight = 451
  ClientWidth = 666
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 17
  object PanelOptions: TPanel
    Left = 0
    Top = 392
    Width = 666
    Height = 59
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkFlat
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      666
      57)
    object Label1: TLabel
      Left = 13
      Top = 17
      Width = 48
      Height = 17
      Caption = 'Amount:'
      FocusControl = SpinEditValue
    end
    object SpinEditValue: TSpinEdit
      Left = 72
      Top = 14
      Width = 61
      Height = 27
      MaxValue = 5
      MinValue = -5
      TabOrder = 0
      Value = 0
    end
    object ButtonApply: TButton
      Left = 576
      Top = 14
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Sharpen'
      TabOrder = 1
      OnClick = ButtonApplyClick
    end
  end
  object PanelImages: TPanel
    Left = 0
    Top = 0
    Width = 666
    Height = 392
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    ExplicitLeft = 324
    ExplicitWidth = 300
    ExplicitHeight = 365
    object Splitter1: TSplitter
      Left = 325
      Top = 0
      Height = 392
      ExplicitLeft = 336
      ExplicitTop = 184
      ExplicitHeight = 100
    end
    object PanelSource: TPanel
      Left = 0
      Top = 0
      Width = 325
      Height = 392
      Align = alLeft
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 0
      object ImgViewSource: TImgView32
        Left = 0
        Top = 0
        Width = 325
        Height = 392
        Align = alClient
        Bitmap.DrawMode = dmBlend
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCustom
        Scale = 1.000000000000000000
        ScaleMode = smScale
        Background.CheckersStyle = bcsMedium
        Background.FillStyle = bfsColor
        MousePan.Enabled = True
        MouseZoom.Enabled = True
        ScrollBars.ShowHandleGrip = True
        ScrollBars.Style = rbsDefault
        ScrollBars.Size = 17
        OverSize = 0
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 300
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 325
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Source'
          Color = clHighlight
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clHighlightText
          Font.Height = -13
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 2
          ExplicitWidth = 337
        end
      end
    end
    object PanelResult: TPanel
      Left = 328
      Top = 0
      Width = 338
      Height = 392
      Align = alClient
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 1
      ExplicitLeft = 0
      ExplicitWidth = 300
      ExplicitHeight = 365
      object ImgViewResult: TImgView32
        Left = 0
        Top = 0
        Width = 338
        Height = 392
        Align = alClient
        Bitmap.DrawMode = dmBlend
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCustom
        Scale = 1.000000000000000000
        ScaleMode = smScale
        Background.CheckersStyle = bcsMedium
        Background.FillStyle = bfsColor
        MousePan.Enabled = True
        MouseZoom.Enabled = True
        ScrollBars.ShowHandleGrip = True
        ScrollBars.Style = rbsDefault
        ScrollBars.Size = 17
        OverSize = 0
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 337
        ExplicitHeight = 281
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 338
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Result'
          Color = clHighlight
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clHighlightText
          Font.Height = -13
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 2
          ExplicitWidth = 337
        end
      end
    end
  end
end
