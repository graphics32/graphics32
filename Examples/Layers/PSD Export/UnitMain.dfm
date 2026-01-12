object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Export TImage32 layers to PSD'
  ClientHeight = 514
  ClientWidth = 647
  Color = clBtnFace
  ParentFont = True
  ShowHint = True
  OnCreate = FormCreate
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 525
    Top = 41
    Height = 473
    Align = alRight
    ResizeStyle = rsUpdate
    ExplicitLeft = 488
    ExplicitTop = 256
    ExplicitHeight = 100
  end
  object ImgView: TImgView32
    Left = 0
    Top = 41
    Width = 525
    Height = 473
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    MousePan.Enabled = True
    MouseZoom.Enabled = True
    ScrollBars.Increment = 0
    ScrollBars.Size = 17
    OverSize = 0
    TabOrder = 0
    TabStop = True
    OnMouseDown = ImgViewMouseDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 647
    Height = 41
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkFlat
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    object LabelCompression: TLabel
      Left = 233
      Top = 11
      Width = 73
      Height = 15
      Caption = '&Compression:'
      FocusControl = ComboBoxCompression
    end
    object ButtonCompressionWarning: TSpeedButton
      Left = 468
      Top = 8
      Width = 23
      Height = 23
      Hint = 'Warning'
      Flat = True
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003CBEEC033BBBEBD439B9EAC338B7
        E803FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003BBBEB9584D4F1FF81D1F0FF36B4
        E769FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF003BBBEB1239B9E9F2F1FAFDFFD5EFFAFF34B1
        E6ED32AEE412FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF0039B8E9B493D8F2FFF3FCFEFFE7FAFEFF8ED3
        F0FF30ABE396FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF0039B8E93037B6E8FDF9FDFFFF93E9F9FF9DEBFAFFECFA
        FEFF2EA8E1FC2CA5E030FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF0037B6E8D4A8DFF4FFEDF9FDFF3DA2D6FF3DA2D6FFD4F5
        FCFFA1D7F1FF2AA2DEC3FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF0037B5E85A70C8EDFFF9FEFFFF5DDCF4FF3DA1D5FF3DA1D5FF5BD9
        F4FFEDFBFEFF67BAE5FF259BDB5AFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF0037B5E80C35B3E6ECC8EAF7FFE6FAFDFF5CDAF4FF3CA0D5FF3CA0D5FF56D7
        F2FFC7F3FCFFBFE3F4FF2398D9E42195D80CFFFFFF00FFFFFF00FFFFFF00FFFF
        FF0035B2E6AA8BD2F0FFEAFBFEFF93E6F8FF5BDAF4FF46B0DDFF3CA0D5FF55D7
        F2FF5BDBF5FFDEF8FDFF7CBFE7FF1C8DD487FFFFFF00FFFFFF00FFFFFF0035B2
        E62433B0E5FBF3FBFEFFC2F2FBFF5BDCF6FF5BDAF4FF63DFF6FF56CBEBFF54D6
        F2FF53D9F5FF93E7F8FFE3F4FBFF1686D0F9137FCD24FFFFFF00FFFFFF0033AF
        E5CA9CD7F1FFE7F9FDFF8AE5F8FF59DBF6FF5ADAF4FF3CA0D5FF3CA0D5FF53D6
        F2FF51D8F5FF4FD6F4FFD8F6FCFF87BEE5FF0E78C9B4FFFFFF0033AFE54B61BE
        E8FFF4FCFEFFB4EFFAFF57DAF5FF57DAF5FF56D8F3FF57D7F2FF57D6F2FF56D9
        F4FF50D8F5FF4DD7F4FF61DAF6FFEAFBFEFF4392D2FF066CC24B30ACE3E3BDE3
        F5FFF4FCFEFFEFFBFEFFEEFBFEFFEEFBFEFFEFFCFEFFEFFCFEFFEFFBFEFFEEFB
        FEFFEDFBFEFFEDFBFEFFECFBFEFFF2FCFEFFAACEEBFF0367C0D82EA9E2A22CA6
        E0FF2AA3DFFF28A0DDFF269DDCFF249ADAFF2297D9FF1F92D6FF1A8BD3FF1584
        CFFF117DCCFF0D76C9FF0970C5FF066BC2FF0367C0FF0063BEA2FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
      Visible = False
      OnClick = ButtonCompressionWarningClick
    end
    object ButtonSave: TButton
      Left = 8
      Top = 7
      Width = 75
      Height = 25
      Caption = '&Save...'
      TabOrder = 0
      OnClick = ButtonSaveClick
    end
    object ComboBoxCompression: TComboBox
      Left = 317
      Top = 8
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Raw (no compression)'
      OnChange = ComboBoxCompressionChange
      Items.Strings = (
        'Raw (no compression)'
        'RLE'
        'ZIP')
    end
    object ButtonLoad: TButton
      Left = 89
      Top = 7
      Width = 75
      Height = 25
      Caption = '&Load...'
      TabOrder = 2
      OnClick = ButtonLoadClick
    end
  end
  object PanelLayers: TPanel
    Left = 528
    Top = 41
    Width = 119
    Height = 473
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object ButtonAddLayer: TButton
      Left = 0
      Top = 0
      Width = 119
      Height = 25
      Align = alTop
      Caption = '&Add layer'
      TabOrder = 0
      OnClick = ButtonAddLayerClick
    end
    object ButtonClear: TButton
      Left = 0
      Top = 25
      Width = 119
      Height = 25
      Align = alTop
      Caption = '&Clear'
      TabOrder = 1
      OnClick = ButtonClearClick
    end
    object ListViewLayers: TListView
      Left = 0
      Top = 50
      Width = 119
      Height = 398
      Align = alClient
      BorderStyle = bsNone
      Checkboxes = True
      Columns = <
        item
          Width = -1
          WidthType = (
            -1)
        end>
      Items.ItemData = {
        05200000000100000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
        0003780078007800}
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 2
      ViewStyle = vsReport
      OnSelectItem = ListViewLayersSelectItem
      OnItemChecked = ListViewLayersItemChecked
    end
    object TrackBarAlpha: TTrackBar
      Left = 0
      Top = 448
      Width = 119
      Height = 25
      Align = alBottom
      Enabled = False
      Max = 255
      PageSize = 32
      PositionToolTip = ptTop
      TabOrder = 3
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = TrackBarAlphaChange
    end
  end
end
