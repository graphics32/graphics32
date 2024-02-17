object MainForm: TMainForm
  Left = 283
  Top = 174
  Caption = 'HTML Document Processor'
  ClientHeight = 362
  ClientWidth = 776
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PnlLog: TPanel
    Left = 363
    Top = 0
    Width = 413
    Height = 362
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 479
    ExplicitHeight = 345
    object Log: TMemo
      Left = 0
      Top = 0
      Width = 413
      Height = 338
      Align = alClient
      Color = 15204327
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      ExplicitWidth = 479
      ExplicitHeight = 321
    end
    object PnlProgress: TPanel
      Left = 0
      Top = 338
      Width = 413
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 321
      ExplicitWidth = 479
      DesignSize = (
        413
        24)
      object LblProgress: TLabel
        Left = 8
        Top = 5
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Progress:'
      end
      object Progress: TProgressBar
        Left = 61
        Top = 4
        Width = 352
        Height = 16
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
        ExplicitWidth = 418
      end
    end
  end
  object PnlControl: TPanel
    Left = 0
    Top = 0
    Width = 363
    Height = 362
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    ExplicitHeight = 345
    object PnlProjectInfo: TPanel
      Left = 2
      Top = 2
      Width = 359
      Height = 164
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        359
        164)
      object LblProjectTitle: TLabel
        Left = 10
        Top = 119
        Width = 94
        Height = 13
        Caption = 'Project &Description:'
        FocusControl = EdtProjectTitle
      end
      object LblProjectDirectory: TLabel
        Left = 10
        Top = 72
        Width = 85
        Height = 13
        Caption = '&Project Directory:'
        FocusControl = EdtProjectDirectory
      end
      object LblVersionString: TLabel
        Left = 174
        Top = 23
        Width = 39
        Height = 13
        Caption = '&Version:'
        FocusControl = EdtVersionString
      end
      object LblProjectFileName: TLabel
        Left = 10
        Top = 23
        Width = 97
        Height = 13
        Caption = 'Short Project &Name:'
        FocusControl = CmbProjectName
      end
      object Label1: TLabel
        Left = 250
        Top = 23
        Width = 52
        Height = 13
        Caption = '&Build Date:'
      end
      object PnlProjectInfoHead: TPanel
        Left = 0
        Top = 0
        Width = 359
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Project information'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
      end
      object EdtProjectTitle: TEdit
        Left = 10
        Top = 137
        Width = 340
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = 'My Project Library'
        OnChange = EdtProjectTitleChange
      end
      object EdtProjectDirectory: TEdit
        Left = 10
        Top = 90
        Width = 340
        Height = 21
        Hint = 'must contain '#39#39'source'#39#39' folder'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = 'MyProjectDirectory'
        OnChange = EdtProjectDirectoryChange
      end
      object CmbProjectName: TComboBox
        Left = 10
        Top = 41
        Width = 149
        Height = 21
        TabOrder = 0
        Text = 'My Project'
        OnChange = CmbProjectNameChange
        OnClick = CmbProjectNameClick
      end
      object EdtVersionString: TEdit
        Left = 174
        Top = 42
        Width = 59
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'v1.0'
        OnChange = EdtVersionStringChange
      end
      object dtpProjectBuildDate: TDateTimePicker
        Left = 250
        Top = 42
        Width = 100
        Height = 21
        Date = 44633.000000000000000000
        Time = 0.420905185186711600
        TabOrder = 2
        OnChange = EdtVersionStringChange
      end
    end
    object PnlTransComp: TPanel
      Left = 2
      Top = 166
      Width = 359
      Height = 86
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object BtnTransform: TButton
        Left = 10
        Top = 13
        Width = 160
        Height = 25
        Caption = 'Transform HTML (F7)'
        TabOrder = 0
        OnClick = BtnTransformClick
      end
      object CbxOpenAfterProcess: TCheckBox
        Left = 190
        Top = 62
        Width = 172
        Height = 17
        Caption = 'Op&en HTML after Compiling'
        TabOrder = 1
        OnClick = EdtVersionStringChange
      end
      object CbxReportBrokenLinks: TCheckBox
        Left = 190
        Top = 9
        Width = 139
        Height = 17
        Caption = 'Report &Broken Links'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = EdtProjectTitleChange
      end
      object cbxReportBrokenImages: TCheckBox
        Left = 190
        Top = 32
        Width = 139
        Height = 17
        Caption = 'Report Broken &Images'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = EdtProjectTitleChange
      end
    end
    object PnlMisc: TPanel
      Left = 2
      Top = 252
      Width = 359
      Height = 85
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object BtnParseMissing: TButton
        Left = 10
        Top = 18
        Width = 160
        Height = 25
        Caption = 'Parse Missing PAS &Units ...'
        TabOrder = 0
        OnClick = BtnParseMissingClick
      end
      object BtnOpen: TButton
        Left = 190
        Top = 19
        Width = 160
        Height = 24
        Caption = '&Open HTML'
        TabOrder = 1
        OnClick = BtnOpenClick
      end
      object BtnClose: TButton
        Left = 190
        Top = 51
        Width = 160
        Height = 24
        Cancel = True
        Caption = 'E&xit'
        TabOrder = 3
        OnClick = BtnCloseClick
      end
      object BtnSaveProjectInfo: TButton
        Left = 10
        Top = 51
        Width = 160
        Height = 24
        Cancel = True
        Caption = '&Save Project Information'
        Enabled = False
        TabOrder = 2
        OnClick = BtnSaveProjectInfoClick
      end
    end
    object StatusBar1: TStatusBar
      Left = 2
      Top = 341
      Width = 359
      Height = 19
      Panels = <>
      SimplePanel = True
      SimpleText = 'DocProcessor Version 2.0.1'
      ExplicitLeft = 144
      ExplicitTop = 344
      ExplicitWidth = 0
    end
  end
  object OpnDlgPAS: TOpenDialog
    DefaultExt = 'pas'
    Filter = 'Delphi Units (*.pas)|*.pas'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 387
    Top = 24
  end
end
