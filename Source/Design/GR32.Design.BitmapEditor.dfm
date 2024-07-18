object PictureEditorForm: TPictureEditorForm
  Left = 247
  Top = 357
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Bitmap32 Editor'
  ClientHeight = 411
  ClientWidth = 477
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 310
  ParentFont = True
  Position = poScreenCenter
  TextHeight = 15
  object Bevel1: TBevel
    Left = 0
    Top = 42
    Width = 477
    Height = 6
    Align = alTop
    Shape = bsTopLine
    Style = bsRaised
    ExplicitTop = 54
    ExplicitWidth = 338
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 477
    Height = 42
    AutoSize = True
    BorderWidth = 1
    ButtonHeight = 38
    ButtonWidth = 42
    Caption = 'ToolBar'
    Images = ImageList
    ShowCaptions = True
    TabOrder = 0
    object ButtonLoad: TToolButton
      Left = 0
      Top = 0
      Action = ActionLoad
    end
    object ButtonSave: TToolButton
      Left = 42
      Top = 0
      Action = ActionSave
    end
    object ButtonClear: TToolButton
      Left = 84
      Top = 0
      Action = ActionClear
    end
    object ToolButton2: TToolButton
      Left = 126
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object ButtonCopy: TToolButton
      Left = 134
      Top = 0
      Action = ActionCopy
    end
    object ButtonPaste: TToolButton
      Left = 176
      Top = 0
      Action = ActionPaste
    end
    object ToolButton1: TToolButton
      Left = 218
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 5
      Style = tbsSeparator
    end
    object ButtonHelp: TToolButton
      Left = 226
      Top = 0
      Action = ActionHelp
    end
    object ButtonGrid: TToolButton
      Left = 268
      Top = 0
      Action = ActionGrid
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 48
    Width = 477
    Height = 311
    ActivePage = TabSheetRGBA
    Align = alClient
    HotTrack = True
    TabOrder = 1
    object TabSheetRGBA: TTabSheet
      Caption = 'All channels'
      ImageIndex = -1
    end
    object TabSheetRGB: TTabSheet
      Caption = 'RGB'
    end
    object TabSheetAlpha: TTabSheet
      Caption = 'Alpha'
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 359
    Width = 477
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      477
      33)
    object LabelZoom: TLabel
      Left = 4
      Top = 9
      Width = 66
      Height = 15
      Caption = 'Zoom: 100%'
      ShowAccelChar = False
    end
    object OKButton: TButton
      Left = 325
      Top = 6
      Width = 64
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Cancel: TButton
      Left = 400
      Top = 6
      Width = 65
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 392
    Width = 477
    Height = 19
    AutoHint = True
    Panels = <
      item
        Width = 105
      end
      item
        Width = 130
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object ImageList: TImageList
    DrawingStyle = dsTransparent
    Masked = False
    Left = 48
    Top = 138
  end
  object PopupMenu: TPopupMenu
    Images = ImageList
    Left = 46
    Top = 184
    object MenuItemLoad: TMenuItem
      Action = ActionLoad
    end
    object MenuItemSave: TMenuItem
      Action = ActionSave
    end
    object MenuItemClear: TMenuItem
      Action = ActionClear
    end
    object mnSeparator: TMenuItem
      Caption = '-'
    end
    object MenuItemCopy: TMenuItem
      Action = ActionCopy
    end
    object MenuItemPaste: TMenuItem
      Action = ActionPaste
    end
    object mnSeparator2: TMenuItem
      Caption = '-'
    end
    object MenuItemInvert: TMenuItem
      Action = ActionInvert
    end
  end
  object ActionList: TActionList
    Images = ImageList
    Left = 48
    Top = 88
    object ActionLoad: TAction
      Caption = 'Load...'
      ImageIndex = 0
      OnExecute = ActionLoadExecute
    end
    object ActionSave: TAction
      Caption = 'Save...'
      ImageIndex = 1
      OnExecute = ActionSaveExecute
      OnUpdate = ActionHasBitmapUpdate
    end
    object ActionClear: TAction
      Caption = 'Clear'
      ImageIndex = 2
      OnExecute = ActionClearExecute
      OnUpdate = ActionHasBitmapUpdate
    end
    object ActionCopy: TAction
      Caption = 'Copy'
      ImageIndex = 3
      OnExecute = ActionCopyExecute
      OnUpdate = ActionHasBitmapUpdate
    end
    object ActionPaste: TAction
      Caption = 'Paste'
      ImageIndex = 4
      OnExecute = ActionPasteExecute
      OnUpdate = ActionPasteUpdate
    end
    object ActionInvert: TAction
      Caption = 'Invert'
      ImageIndex = 5
      OnExecute = ActionInvertExecute
      OnUpdate = ActionHasBitmapUpdate
    end
    object ActionHelp: TAction
      Caption = 'Help'
      ImageIndex = 6
      OnExecute = ActionHelpExecute
    end
    object ActionGrid: TAction
      AutoCheck = True
      Caption = 'Grid'
      Hint = 'Display pixel grid'
      ImageIndex = 7
      OnExecute = ActionGridExecute
      OnUpdate = ActionGridUpdate
    end
  end
end
