object FormSVGviewer: TFormSVGviewer
  Left = 0
  Top = 0
  Caption = 'SVG Viewer'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object SplitterMain: TSplitter
    Left = 250
    Top = 0
    Width = 5
    Height = 581
    ExplicitHeight = 600
  end
  object PnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 250
    Height = 581
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 600
    object DriveComboBox: TDriveComboBox
      Left = 0
      Top = 0
      Width = 250
      Height = 19
      Align = alTop
      DirList = DirectoryListBox
      TabOrder = 0
    end
    object DirectoryListBox: TDirectoryListBox
      Left = 0
      Top = 19
      Width = 250
      Height = 250
      Align = alTop
      FileList = FileListBox
      TabOrder = 1
    end
    object FileListBox: TFileListBox
      Left = 0
      Top = 269
      Width = 250
      Height = 312
      Align = alClient
      ItemHeight = 13
      Mask = '*.svg'
      TabOrder = 2
      OnChange = FileListBoxChange
      ExplicitHeight = 331
    end
  end
  object PnlRight: TPanel
    Left = 255
    Top = 0
    Width = 645
    Height = 581
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitHeight = 600
    object PageControlSVG: TPageControl
      Left = 0
      Top = 0
      Width = 645
      Height = 581
      ActivePage = TabSheetPreview
      Align = alClient
      TabOrder = 0
      ExplicitHeight = 600
      object TabSheetPreview: TTabSheet
        Caption = 'Preview'
        DesignSize = (
          637
          553)
        object Image32: TImage32
          Left = 0
          Top = 0
          Width = 637
          Height = 553
          Align = alClient
          Bitmap.DrawMode = dmBlend
          Bitmap.ResamplerClassName = 'TNearestResampler'
          BitmapAlign = baCustom
          Scale = 1.000000000000000000
          ScaleMode = smScale
          Background.FillStyle = bfsCheckers
          Background.CheckersStyle = bcsMedium
          MousePan.Enabled = True
          MouseZoom.Enabled = True
          MouseZoom.Animate = True
          TabOrder = 0
          ExplicitHeight = 572
        end
        object Button1: TButton
          Left = 559
          Top = 525
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Save as PNG'
          TabOrder = 1
          OnClick = Button1Click
          ExplicitTop = 544
        end
      end
      object TabSheetSource: TTabSheet
        Caption = 'Source'
        ImageIndex = 1
        object MemoSource: TMemo
          Left = 0
          Top = 0
          Width = 637
          Height = 553
          Align = alClient
          BorderStyle = bsNone
          ScrollBars = ssBoth
          TabOrder = 0
          OnChange = MemoSourceChange
          ExplicitHeight = 572
        end
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 581
    Width = 900
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
    ExplicitLeft = 96
    ExplicitTop = 492
    ExplicitWidth = 0
  end
end
