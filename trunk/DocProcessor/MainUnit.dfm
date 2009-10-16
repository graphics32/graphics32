object MainForm: TMainForm
  Left = 132
  Top = 123
  Width = 877
  Height = 551
  Caption = 'HTML Document Processor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 573
    Height = 517
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    object Log: TMemo
      Left = 0
      Top = 0
      Width = 573
      Height = 493
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
      Top = 493
      Width = 573
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        573
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
        Left = 59
        Top = 4
        Width = 509
        Height = 16
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
      end
    end
  end
  object Panel3: TPanel
    Left = 573
    Top = 0
    Width = 296
    Height = 517
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    object Panel4: TPanel
      Left = 2
      Top = 2
      Width = 292
      Height = 185
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        292
        185)
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
        Width = 292
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
      object DirectoryEdit1: TEdit
        Left = 16
        Top = 42
        Width = 266
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'DirectoryEdit1'
        OnChange = DirectoryEdit1Change
      end
      object Edit2: TEdit
        Left = 83
        Top = 98
        Width = 199
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'Index.hhk'
      end
      object Edit3: TEdit
        Left = 83
        Top = 123
        Width = 199
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = 'TOC.hhc'
      end
      object Edit4: TEdit
        Left = 83
        Top = 148
        Width = 199
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = 'My Help.chm'
      end
      object Edit5: TEdit
        Left = 83
        Top = 74
        Width = 199
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'My Help.hhp'
      end
    end
    object Panel6: TPanel
      Left = 2
      Top = 187
      Width = 292
      Height = 128
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        292
        128)
      object Label8: TLabel
        Left = 8
        Top = 74
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
        Width = 292
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
      object Edit6: TEdit
        Left = 16
        Top = 91
        Width = 269
        Height = 21
        TabOrder = 1
        Text = 'v1.0'
      end
      object Edit1: TEdit
        Left = 16
        Top = 41
        Width = 266
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'My Help Project'
      end
    end
    object Panel8: TPanel
      Left = 2
      Top = 315
      Width = 292
      Height = 85
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        292
        85)
      object Label9: TLabel
        Left = 10
        Top = 27
        Width = 70
        Height = 13
        Caption = 'CHM Compiler:'
      end
      object Edit7: TEdit
        Left = 15
        Top = 48
        Width = 264
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'C:\Programs\HTML Help Workshop\hhc.exe'
      end
      object CheckBox1: TCheckBox
        Left = 155
        Top = 27
        Width = 121
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Compile on process'
        TabOrder = 0
      end
      object Panel9: TPanel
        Left = 0
        Top = 0
        Width = 292
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
        TabOrder = 2
      end
    end
    object Panel10: TPanel
      Left = 2
      Top = 400
      Width = 292
      Height = 123
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      DesignSize = (
        292
        123)
      object bProcess: TButton
        Left = 144
        Top = 60
        Width = 140
        Height = 24
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Process'
        TabOrder = 3
        OnClick = bProcessClick
      end
      object Panel11: TPanel
        Left = 0
        Top = 0
        Width = 292
        Height = 16
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Transformation'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
      end
      object bTransform: TButton
        Left = 8
        Top = 28
        Width = 125
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Transform only'
        TabOrder = 0
        OnClick = bTransformClick
      end
      object bCompile: TButton
        Left = 8
        Top = 60
        Width = 125
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Compile only'
        TabOrder = 2
        OnClick = bCompileClick
      end
      object cbOpenAfterProcess: TCheckBox
        Left = 8
        Top = 96
        Width = 129
        Height = 17
        Caption = 'Open after compiling'
        TabOrder = 4
      end
      object bOpen: TButton
        Left = 144
        Top = 92
        Width = 139
        Height = 24
        Caption = 'Open'
        TabOrder = 5
        OnClick = bOpenClick
      end
      object bParseMissing: TButton
        Left = 144
        Top = 28
        Width = 140
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Parse Missing Units ...'
        TabOrder = 1
        OnClick = bParseMissingClick
      end
    end
  end
end
