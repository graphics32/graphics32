object FmPngExplorer: TFmPngExplorer
  Left = 309
  Top = 95
  Caption = 'PNG Explorer'
  ClientHeight = 502
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 13
  object SplitterHorizontal: TSplitter
    Left = 204
    Top = 41
    Height = 461
    ExplicitTop = 24
    ExplicitHeight = 440
  end
  object TreeView: TTreeView
    Left = 0
    Top = 41
    Width = 204
    Height = 461
    Align = alLeft
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = TreeViewChange
    ExplicitTop = 24
    ExplicitHeight = 422
  end
  object PnMain: TPanel
    Left = 207
    Top = 41
    Width = 478
    Height = 461
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 24
    ExplicitWidth = 472
    ExplicitHeight = 422
    object SplitterVertical: TSplitter
      Left = 0
      Top = 194
      Width = 478
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 173
      ExplicitWidth = 484
    end
    object ListView: TListView
      Left = 0
      Top = 0
      Width = 478
      Height = 194
      Align = alClient
      Columns = <>
      ReadOnly = True
      TabOrder = 0
      ViewStyle = vsReport
      ExplicitWidth = 472
      ExplicitHeight = 155
    end
    object PanelPreview: TPanel
      Left = 0
      Top = 197
      Width = 478
      Height = 264
      Align = alBottom
      BevelInner = bvLowered
      BevelOuter = bvNone
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      ExplicitTop = 158
      ExplicitWidth = 472
      object ImgView32: TImgView32
        Left = 1
        Top = 1
        Width = 476
        Height = 262
        Align = alClient
        Bitmap.DrawMode = dmBlend
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCustom
        Scale = 1.000000000000000000
        ScaleMode = smScale
        Background.CheckersStyle = bcsMedium
        Background.FillStyle = bfsCheckers
        MousePan.Enabled = True
        MouseZoom.Enabled = True
        ScrollBars.ShowHandleGrip = True
        ScrollBars.Style = rbsDefault
        ScrollBars.Size = 16
        ScrollBars.Visibility = svAuto
        OverSize = 0
        TabOrder = 0
        ExplicitWidth = 470
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 685
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 2
    object ButtonLoad: TButton
      Left = 8
      Top = 8
      Width = 97
      Height = 25
      Caption = 'Load PNG...'
      TabOrder = 0
      OnClick = ButtonLoadClick
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'PNG (*.png)|*.png'
    Left = 72
    Top = 80
  end
end
