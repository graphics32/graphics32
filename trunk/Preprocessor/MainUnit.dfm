object MainForm: TMainForm
  Left = 202
  Top = 111
  BorderStyle = bsDialog
  Caption = 'HTML Doc Processor'
  ClientHeight = 438
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    417
    438)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 85
    Height = 13
    Caption = 'Project Directory:'
  end
  object Label2: TLabel
    Left = 8
    Top = 43
    Width = 61
    Height = 13
    Caption = 'Project Title:'
  end
  object Label3: TLabel
    Left = 8
    Top = 99
    Width = 51
    Height = 13
    Caption = 'Index File:'
  end
  object Label4: TLabel
    Left = 8
    Top = 75
    Width = 44
    Height = 13
    Caption = 'TOC File:'
  end
  object Label5: TLabel
    Left = 207
    Top = 99
    Width = 66
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Compiled File:'
  end
  object Label6: TLabel
    Left = 207
    Top = 75
    Width = 57
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Project File:'
  end
  object Label7: TLabel
    Left = 8
    Top = 415
    Width = 46
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Progress:'
  end
  object Label8: TLabel
    Left = 8
    Top = 123
    Width = 39
    Height = 13
    Caption = 'Version:'
  end
  object DirectoryEdit1: TDirectoryEdit
    Left = 96
    Top = 8
    Width = 312
    Height = 21
    DialogKind = dkWin32
    NumGlyphs = 1
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'C:\My Help Projects\G32'
    OnChange = DirectoryEdit1Change
  end
  object Process: TButton
    Left = 8
    Top = 148
    Width = 401
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Process'
    TabOrder = 1
    OnClick = ProcessClick
  end
  object Log: TMemo
    Left = 8
    Top = 180
    Width = 400
    Height = 227
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object Edit1: TEdit
    Left = 96
    Top = 40
    Width = 312
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'My Help Project'
  end
  object Edit2: TEdit
    Left = 64
    Top = 96
    Width = 131
    Height = 21
    TabOrder = 4
    Text = 'Index.hhk'
  end
  object Edit3: TEdit
    Left = 64
    Top = 72
    Width = 131
    Height = 21
    TabOrder = 5
    Text = 'TOC.hhc'
  end
  object Edit4: TEdit
    Left = 277
    Top = 96
    Width = 131
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 6
    Text = 'My Help.chm'
  end
  object Edit5: TEdit
    Left = 277
    Top = 72
    Width = 131
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 7
    Text = 'My Help.hhp'
  end
  object Progress: TProgressBar
    Left = 64
    Top = 414
    Width = 346
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    Min = 0
    Max = 100
    TabOrder = 8
  end
  object Edit6: TEdit
    Left = 64
    Top = 120
    Width = 131
    Height = 21
    TabOrder = 9
    Text = 'v1.0'
  end
  object FormStorage1: TFormStorage
    StoredProps.Strings = (
      'DirectoryEdit1.Text'
      'Edit1.Text'
      'Edit2.Text'
      'Edit3.Text'
      'Edit4.Text'
      'Edit5.Text'
      'Edit6.Text')
    StoredValues = <>
    Left = 24
    Top = 65520
  end
end
