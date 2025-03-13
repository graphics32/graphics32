object MainForm: TMainForm
  Left = 230
  Top = 122
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'ByteMaps Example'
  ClientHeight = 333
  ClientWidth = 494
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object PnlMain: TPanel
    Left = 0
    Top = 27
    Width = 494
    Height = 306
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    ParentColor = True
    TabOrder = 0
    ExplicitHeight = 326
    object Image: TImgView32
      Left = 0
      Top = 0
      Width = 490
      Height = 302
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      RepaintMode = rmOptimizer
      Scale = 1.000000000000000000
      ScaleMode = smScale
      MousePan.Enabled = True
      MouseZoom.Enabled = True
      ScrollBars.Increment = 0
      ScrollBars.Size = 16
      OverSize = 0
      TabOrder = 0
      OnScaleChange = ImageScaleChange
      ExplicitHeight = 322
    end
  end
  object PnlSepartator: TPanel
    Left = 0
    Top = 25
    Width = 494
    Height = 2
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 494
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 2
    DesignSize = (
      494
      25)
    object Label1: TLabel
      Left = 9
      Top = 6
      Width = 30
      Height = 13
      Caption = 'Zoom:'
    end
    object Label2: TLabel
      Left = 176
      Top = 6
      Width = 38
      Height = 13
      Caption = 'Palette:'
    end
    object ScaleBar: TGaugeBar
      Left = 53
      Top = 4
      Width = 107
      Height = 18
      Hint = 'Zoom'
      Backgnd = bgPattern
      ButtonSize = 14
      LargeChange = 20
      Min = -100
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ScaleChange
    end
    object PaletteCombo: TComboBox
      Left = 220
      Top = 3
      Width = 73
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 1
      TabStop = False
      OnChange = PaletteComboChange
      Items.Strings = (
        'Grayscale'
        'Greens'
        'Reds'
        'Rainbow')
    end
  end
  object SavePictureDialog: TSavePictureDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 168
    Top = 148
  end
  object MainMenu: TMainMenu
    Left = 291
    Top = 158
    object mnFile: TMenuItem
      Caption = '&File'
      object mnNew: TMenuItem
        Caption = 'Generate New'
        ImageIndex = 0
        OnClick = NewClick
      end
      object mnOpen: TMenuItem
        Caption = 'Open...'
        ImageIndex = 1
        OnClick = OpenClick
      end
      object MenuItemSave: TMenuItem
        Caption = 'Save...'
        Enabled = False
        ImageIndex = 2
        OnClick = SaveClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnExit: TMenuItem
        Caption = 'Exit'
        OnClick = mnExitClick
      end
    end
    object mnEdit: TMenuItem
      Caption = '&Edit'
      object MenuItemCopy: TMenuItem
        Caption = 'Copy'
        Enabled = False
        ImageIndex = 3
        OnClick = CopyClick
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object MenuItemLinear: TMenuItem
        AutoCheck = True
        Caption = 'Linear resampler'
        OnClick = MenuItemLinearClick
      end
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 
      'All (*.gif;*.jpg;*.jpeg;*.bmp;*.ico)|*.gif;*.jpg;*.jpeg;*.bmp;*.' +
      'ico|CompuServe GIF Image (*.gif)|*.gif|JPEG Image File (*.jpg)|*' +
      '.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|Icons' +
      ' (*.ico)|*.ico'
    Left = 170
    Top = 98
  end
end
