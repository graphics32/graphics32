object FormPixelCombine: TFormPixelCombine
  Left = 295
  Top = 110
  Caption = 'PixelCombine Example'
  ClientHeight = 361
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 575
    Top = 0
    Height = 361
    Align = alRight
    ResizeStyle = rsUpdate
    ExplicitLeft = 636
    ExplicitTop = 176
    ExplicitHeight = 100
  end
  object ImgView: TImgView32
    Left = 0
    Top = 0
    Width = 575
    Height = 361
    Align = alClient
    Bitmap.DrawMode = dmBlend
    Bitmap.CombineMode = cmMerge
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Color = clBtnShadow
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smScale
    Background.FillStyle = bfsCheckers
    Background.CheckersStyle = bcsMedium
    ScrollBars.Increment = 0
    ScrollBars.Size = 16
    OverSize = 0
    TabOrder = 0
    OnMouseDown = ImgViewMouseDown
  end
  object PanelOptions: TPanel
    Left = 578
    Top = 0
    Width = 185
    Height = 361
    Align = alRight
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    object ListViewOperation: TListView
      Left = 0
      Top = 49
      Width = 185
      Height = 312
      Align = alClient
      Checkboxes = True
      Columns = <
        item
          AutoSize = True
        end>
      GroupView = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = ListViewOperationSelectItem
      OnItemChecked = ListViewOperationItemChecked
    end
    object PanelLayer: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 29
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        185
        29)
      object Label1: TLabel
        Left = 6
        Top = 7
        Width = 29
        Height = 13
        Caption = 'Layer:'
        FocusControl = ComboBoxLayer
      end
      object ComboBoxLayer: TComboBox
        Left = 48
        Top = 4
        Width = 109
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnSelect = ComboBoxLayerSelect
      end
      object ButtonLayerAdd: TButton
        Left = 160
        Top = 4
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '+'
        TabOrder = 1
        OnClick = ButtonLayerAddClick
      end
    end
    object TrackBarAlpha: TTrackBar
      Left = 0
      Top = 29
      Width = 185
      Height = 20
      Align = alTop
      Max = 255
      TabOrder = 2
      ThumbLength = 15
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = TrackBarAlphaChange
    end
  end
end
