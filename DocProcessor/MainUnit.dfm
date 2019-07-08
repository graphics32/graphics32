object MainForm: TMainForm
  Left = 283
  Top = 174
  Caption = 'HTML Document Processor'
  ClientHeight = 459
  ClientWidth = 842
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
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PnlLog: TPanel
    Left = 363
    Top = 0
    Width = 479
    Height = 459
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Log: TMemo
      Left = 0
      Top = 0
      Width = 479
      Height = 435
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
    end
    object PnlProgress: TPanel
      Left = 0
      Top = 435
      Width = 479
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        479
        24)
      object LblProgress: TLabel
        Left = 5
        Top = 5
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Progress:'
      end
      object Progress: TProgressBar
        Left = 61
        Top = 4
        Width = 418
        Height = 16
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
      end
    end
  end
  object PnlControl: TPanel
    Left = 0
    Top = 0
    Width = 363
    Height = 459
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    object PnlProjectInfo: TPanel
      Left = 2
      Top = 2
      Width = 359
      Height = 157
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        359
        157)
      object LblProjectTitle: TLabel
        Left = 10
        Top = 127
        Width = 61
        Height = 13
        Caption = 'Project &Title:'
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
        Left = 246
        Top = 23
        Width = 70
        Height = 13
        Caption = '&Version String:'
        FocusControl = EdtVersionString
      end
      object LblProjectFileName: TLabel
        Left = 10
        Top = 23
        Width = 83
        Height = 13
        Caption = 'Project &Filename:'
        FocusControl = CmbProjectName
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
        TabOrder = 4
      end
      object EdtProjectTitle: TEdit
        Left = 82
        Top = 123
        Width = 268
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = 'MyProject Help'
        OnChange = EdtProjectTitleChange
      end
      object EdtProjectDirectory: TEdit
        Left = 10
        Top = 90
        Width = 340
        Height = 21
        Hint = 'must contain '#39#39'source'#39#39' folder'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'MyProjectDirectory'
        OnChange = EdtProjectDirectoryChange
      end
      object CmbProjectName: TComboBox
        Left = 10
        Top = 41
        Width = 226
        Height = 21
        TabOrder = 0
        Text = 'MyProjectName'
        OnChange = CmbProjectNameChange
        OnClick = CmbProjectNameClick
      end
      object EdtVersionString: TEdit
        Left = 246
        Top = 41
        Width = 104
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'v1.0'
        OnChange = EdtProjectTitleChange
      end
    end
    object PnlCompiler: TPanel
      Left = 2
      Top = 159
      Width = 359
      Height = 77
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        359
        77)
      object LblCompiler: TLabel
        Left = 10
        Top = 24
        Width = 70
        Height = 13
        Caption = 'CHM &Compiler:'
        FocusControl = EdtCHMCompiler
      end
      object EdtCHMCompiler: TEdit
        Left = 10
        Top = 42
        Width = 340
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'C:\Program Files\HTML Help Workshop\hhc.exe'
        OnChange = EdtCHMCompilerChange
      end
      object PnlCompilerHead: TPanel
        Left = 0
        Top = 0
        Width = 359
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Compiler'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
    end
    object PnlTransComp: TPanel
      Left = 2
      Top = 236
      Width = 359
      Height = 129
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object BtnProcess: TButton
        Left = 10
        Top = 93
        Width = 160
        Height = 24
        Caption = 'Transform && Compile (F9)'
        TabOrder = 2
        OnClick = BtnProcessClick
      end
      object PnlTransCompHead: TPanel
        Left = 0
        Top = 0
        Width = 359
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Transforming && Compiling'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
      end
      object BtnTransform: TButton
        Left = 10
        Top = 28
        Width = 160
        Height = 25
        Caption = 'Transform HTML only (F7)'
        TabOrder = 0
        OnClick = BtnTransformClick
      end
      object BtnCompile: TButton
        Left = 10
        Top = 60
        Width = 160
        Height = 25
        Caption = 'Compile CHM File only (F8)'
        TabOrder = 1
        OnClick = BtnCompileClick
      end
      object CbxOpenAfterProcess: TCheckBox
        Left = 186
        Top = 100
        Width = 172
        Height = 17
        Caption = 'Op&en CHM File after Compiling'
        TabOrder = 4
        OnClick = EdtProjectTitleChange
      end
      object CbxIncludeAlphabetClasses: TCheckBox
        Left = 186
        Top = 28
        Width = 150
        Height = 17
        Caption = 'Include &Alphabet Classes'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = EdtProjectTitleChange
      end
      object CbxBrokenLinks: TCheckBox
        Left = 186
        Top = 49
        Width = 139
        Height = 17
        Caption = 'Report &Broken Links'
        TabOrder = 6
        OnClick = EdtProjectTitleChange
      end
    end
    object PnlMisc: TPanel
      Left = 2
      Top = 365
      Width = 359
      Height = 93
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      object PnlMiscHead: TPanel
        Left = 0
        Top = 0
        Width = 359
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Miscellaneous'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object BtnParseMissing: TButton
        Left = 10
        Top = 28
        Width = 160
        Height = 25
        Caption = 'Parse Missing PAS &Units ...'
        TabOrder = 1
        OnClick = BtnParseMissingClick
      end
      object BtnOpen: TButton
        Left = 190
        Top = 28
        Width = 160
        Height = 24
        Caption = '&Open CHM File'
        TabOrder = 2
        OnClick = BtnOpenClick
      end
      object BtnClose: TButton
        Left = 190
        Top = 61
        Width = 160
        Height = 24
        Cancel = True
        Caption = 'E&xit'
        TabOrder = 4
        OnClick = BtnCloseClick
      end
      object BtnSaveProjectInfo: TButton
        Left = 10
        Top = 61
        Width = 160
        Height = 24
        Cancel = True
        Caption = '&Save Project Information'
        Enabled = False
        TabOrder = 3
        OnClick = BtnSaveProjectInfoClick
      end
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
