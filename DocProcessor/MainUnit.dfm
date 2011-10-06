object MainForm: TMainForm
  Left = 159
  Top = 150
  Width = 850
  Height = 490
  Caption = 'HTML Document Processor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 479
    Height = 463
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    object Log: TMemo
      Left = 0
      Top = 0
      Width = 479
      Height = 439
      Align = alClient
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
    object Panel2: TPanel
      Left = 0
      Top = 439
      Width = 479
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        479
        24)
      object Label7: TLabel
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
  object Panel3: TPanel
    Left = 479
    Top = 0
    Width = 363
    Height = 463
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    object Panel6: TPanel
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
      object Label8: TLabel
        Left = 244
        Top = 103
        Width = 70
        Height = 13
        Caption = '&Version String:'
        FocusControl = edVersionString
      end
      object Label2: TLabel
        Left = 8
        Top = 79
        Width = 61
        Height = 13
        Caption = 'Project &Title:'
        FocusControl = edProjectTitle
      end
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 85
        Height = 13
        Caption = '&Project Directory:'
        FocusControl = edProjectDirectory
      end
      object Label6: TLabel
        Left = 8
        Top = 105
        Width = 83
        Height = 13
        Caption = 'Project &Filename:'
        FocusControl = edProjectName
      end
      object Panel7: TPanel
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
      object edVersionString: TEdit
        Left = 244
        Top = 121
        Width = 104
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = 'v1.0'
      end
      object edProjectTitle: TEdit
        Left = 80
        Top = 75
        Width = 270
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'Project Help'
      end
      object edProjectDirectory: TEdit
        Left = 8
        Top = 42
        Width = 342
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'edProjectDirectory'
        OnChange = edProjectDirectoryChange
      end
      object edProjectName: TEdit
        Left = 8
        Top = 121
        Width = 226
        Height = 21
        TabOrder = 2
        Text = 'MyProject'
      end
    end
    object Panel8: TPanel
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
      object Label9: TLabel
        Left = 8
        Top = 24
        Width = 70
        Height = 13
        Caption = 'CHM &Compiler:'
        FocusControl = edCHMCompiler
      end
      object edCHMCompiler: TEdit
        Left = 8
        Top = 42
        Width = 342
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'C:\Programs\HTML Help Workshop\hhc.exe'
      end
      object Panel9: TPanel
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
    object Panel10: TPanel
      Left = 2
      Top = 236
      Width = 359
      Height = 129
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object bProcess: TButton
        Left = 9
        Top = 93
        Width = 160
        Height = 24
        Caption = 'Transform && Compile (F9)'
        TabOrder = 2
        OnClick = bProcessClick
      end
      object Panel11: TPanel
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
      object bTransform: TButton
        Left = 8
        Top = 28
        Width = 160
        Height = 25
        Caption = 'Transform HTML only (F7)'
        TabOrder = 0
        OnClick = bTransformClick
      end
      object bCompile: TButton
        Left = 8
        Top = 60
        Width = 160
        Height = 25
        Caption = 'Compile CHM File only (F8)'
        TabOrder = 1
        OnClick = bCompileClick
      end
      object cbOpenAfterProcess: TCheckBox
        Left = 186
        Top = 100
        Width = 172
        Height = 17
        Caption = 'Op&en CHM File after Compiling'
        TabOrder = 4
      end
      object cbIncludeAlphabetClasses: TCheckBox
        Left = 186
        Top = 28
        Width = 150
        Height = 17
        Caption = 'Include &Alphabet Classes'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object cbBrokenLinks: TCheckBox
        Left = 186
        Top = 49
        Width = 139
        Height = 17
        Caption = 'Check for &Broken Links'
        TabOrder = 6
      end
    end
    object Panel12: TPanel
      Left = 2
      Top = 365
      Width = 359
      Height = 93
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      object Panel13: TPanel
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
      object bParseMissing: TButton
        Left = 8
        Top = 28
        Width = 160
        Height = 25
        Caption = 'Parse Missing PAS &Units ...'
        TabOrder = 1
        OnClick = bParseMissingClick
      end
      object bOpen: TButton
        Left = 186
        Top = 28
        Width = 160
        Height = 24
        Caption = '&Open CHM File'
        TabOrder = 2
        OnClick = bOpenClick
      end
      object bClose: TButton
        Left = 186
        Top = 61
        Width = 160
        Height = 24
        Cancel = True
        Caption = 'E&xit'
        TabOrder = 3
        OnClick = bCloseClick
      end
    end
  end
end
