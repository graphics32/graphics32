object MainForm: TMainForm
  Left = 198
  Top = 135
  Width = 812
  Height = 568
  Caption = 'HTML Document Processor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 407
    Height = 541
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    object Log: TMemo
      Left = 0
      Top = 0
      Width = 407
      Height = 517
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
      OnKeyDown = LogKeyDown
    end
    object Panel2: TPanel
      Left = 0
      Top = 517
      Width = 407
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        407
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
        Width = 346
        Height = 16
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
      end
    end
  end
  object Panel3: TPanel
    Left = 407
    Top = 0
    Width = 397
    Height = 541
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    object Panel4: TPanel
      Left = 2
      Top = 2
      Width = 393
      Height = 183
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        393
        183)
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 85
        Height = 13
        Caption = 'Project Directory:'
      end
      object Label3: TLabel
        Left = 8
        Top = 102
        Width = 51
        Height = 13
        Caption = 'Index File:'
      end
      object Label4: TLabel
        Left = 8
        Top = 127
        Width = 44
        Height = 13
        Caption = 'TOC File:'
      end
      object Label5: TLabel
        Left = 7
        Top = 151
        Width = 66
        Height = 13
        Caption = 'Compiled File:'
      end
      object Label6: TLabel
        Left = 8
        Top = 77
        Width = 57
        Height = 13
        Caption = 'Project File:'
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 393
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Project files'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
      end
      object edProjectDirectory: TEdit
        Left = 8
        Top = 42
        Width = 376
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'edProjectDirectory'
        OnChange = edProjectDirectoryChange
      end
      object edIndexFile: TEdit
        Left = 83
        Top = 98
        Width = 302
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'Index.hhk'
      end
      object edTOCFile: TEdit
        Left = 83
        Top = 123
        Width = 302
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = 'TOC.hhc'
      end
      object edCompiledFile: TEdit
        Left = 83
        Top = 148
        Width = 302
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = 'My Help.chm'
      end
      object edProjectFile: TEdit
        Left = 83
        Top = 74
        Width = 302
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'My Help.hhp'
      end
    end
    object Panel6: TPanel
      Left = 2
      Top = 185
      Width = 393
      Height = 78
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        393
        78)
      object Label8: TLabel
        Left = 244
        Top = 24
        Width = 70
        Height = 13
        Caption = 'Version String:'
      end
      object Label2: TLabel
        Left = 8
        Top = 24
        Width = 61
        Height = 13
        Caption = 'Project Title:'
      end
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 393
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
        TabOrder = 2
      end
      object edVersionString: TEdit
        Left = 244
        Top = 42
        Width = 140
        Height = 21
        TabOrder = 1
        Text = 'v1.0'
      end
      object edProjectTitle: TEdit
        Left = 8
        Top = 42
        Width = 220
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'My Help Project'
      end
    end
    object Panel8: TPanel
      Left = 2
      Top = 263
      Width = 393
      Height = 77
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        393
        77)
      object Label9: TLabel
        Left = 8
        Top = 24
        Width = 70
        Height = 13
        Caption = 'CHM Compiler:'
      end
      object edCHMCompiler: TEdit
        Left = 8
        Top = 42
        Width = 376
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'C:\Programs\HTML Help Workshop\hhc.exe'
      end
      object Panel9: TPanel
        Left = 0
        Top = 0
        Width = 393
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
      Top = 340
      Width = 393
      Height = 102
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      DesignSize = (
        393
        102)
      object bProcess: TButton
        Left = 9
        Top = 62
        Width = 180
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Transform HTML && Compile CHM'
        TabOrder = 2
        OnClick = bProcessClick
      end
      object Panel11: TPanel
        Left = 0
        Top = 0
        Width = 393
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
        Width = 180
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Transform HTML only'
        TabOrder = 0
        OnClick = bTransformClick
      end
      object bCompile: TButton
        Left = 204
        Top = 28
        Width = 180
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Compile CHM File only'
        TabOrder = 1
        OnClick = bCompileClick
      end
      object cbOpenAfterProcess: TCheckBox
        Left = 204
        Top = 77
        Width = 172
        Height = 17
        Caption = 'Open CHM File after Compiling'
        TabOrder = 4
      end
      object cbIncludeAlphabetClasses: TCheckBox
        Left = 204
        Top = 59
        Width = 150
        Height = 17
        Caption = 'Include Alphabet Classes'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
    object Panel12: TPanel
      Left = 2
      Top = 442
      Width = 393
      Height = 93
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 4
      DesignSize = (
        393
        93)
      object Panel13: TPanel
        Left = 0
        Top = 0
        Width = 393
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
        Width = 180
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Parse Missing PAS Units ...'
        TabOrder = 1
        OnClick = bParseMissingClick
      end
      object bOpen: TButton
        Left = 204
        Top = 28
        Width = 180
        Height = 24
        Caption = 'Open CHM File'
        TabOrder = 2
        OnClick = bOpenClick
      end
      object bClose: TButton
        Left = 204
        Top = 61
        Width = 180
        Height = 24
        Cancel = True
        Caption = 'E&xit'
        TabOrder = 3
        OnClick = bCloseClick
      end
    end
  end
end
