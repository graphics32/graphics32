object MainForm: TMainForm
  Left = 295
  Top = 297
  BorderStyle = bsDialog
  Caption = 'HTML Document Processor'
  ClientHeight = 521
  ClientWidth = 869
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
    Height = 521
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    object Log: TMemo
      Left = 0
      Top = 0
      Width = 573
      Height = 497
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
      Top = 497
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
        Height = 16
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
    Height = 521
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
        TabOrder = 0
      end
      object DirectoryEdit1: TEdit
        Left = 16
        Top = 42
        Width = 266
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
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
        TabOrder = 5
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
        TabOrder = 0
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
        TabOrder = 2
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
        TabOrder = 0
        Text = 'C:\Programs\HTML Help Workshop\hhc.exe'
      end
      object CheckBox1: TCheckBox
        Left = 155
        Top = 27
        Width = 121
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Compile on process'
        TabOrder = 1
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
      object Process: TButton
        Left = 144
        Top = 28
        Width = 140
        Height = 57
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Process'
        TabOrder = 0
        OnClick = ProcessClick
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
        TabOrder = 1
      end
      object Button1: TButton
        Left = 8
        Top = 28
        Width = 125
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Transform only'
        TabOrder = 2
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 8
        Top = 60
        Width = 125
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Compile only'
        TabOrder = 3
        OnClick = Button2Click
      end
    end
  end
end
