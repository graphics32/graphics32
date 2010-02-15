object MainForm: TMainForm
  Left = 223
  Top = 109
  Caption = 'Sprites Example'
  ClientHeight = 684
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    852
    684)
  PixelsPerInch = 96
  TextHeight = 13
  object lbTotal: TLabel
    Left = 12
    Top = 654
    Width = 28
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Total:'
  end
  object lbFPS: TLabel
    Left = 272
    Top = 654
    Width = 81
    Height = 17
    Anchors = [akLeft, akBottom]
    AutoSize = False
  end
  object lbDimension: TLabel
    Left = 360
    Top = 654
    Width = 113
    Height = 17
    Anchors = [akLeft, akBottom]
    AutoSize = False
  end
  object Image32: TImage32
    Left = 8
    Top = 48
    Width = 836
    Height = 592
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTile
    Color = clWhite
    ParentColor = False
    Scale = 2.000000000000000000
    ScaleMode = smScale
    TabOrder = 0
    OnResize = Image32Resize
  end
  object bAdd: TButton
    Left = 627
    Top = 652
    Width = 69
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Add 10'
    TabOrder = 1
    OnClick = bAddClick
  end
  object edLayerCount: TEdit
    Left = 44
    Top = 652
    Width = 65
    Height = 19
    Anchors = [akLeft, akBottom]
    Ctl3D = False
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 2
    Text = '0 layers'
  end
  object bClearAll: TButton
    Left = 776
    Top = 652
    Width = 68
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Clear All'
    TabOrder = 3
    OnClick = bClearAllClick
  end
  object cbUseRepaintOpt: TCheckBox
    Left = 120
    Top = 654
    Width = 137
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Use Repaint Optimizer'
    TabOrder = 4
    OnClick = cbUseRepaintOptClick
  end
  object bRemove: TButton
    Left = 704
    Top = 652
    Width = 64
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Remove 10'
    TabOrder = 5
    OnClick = bRemoveClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 6
    Width = 835
    Height = 35
    Anchors = [akLeft, akTop, akRight]
    Color = clInfoBk
    Lines.Strings = (
      
        'This demonstration animates the sprites as fast as possible. It'#39 +
        's a simple throughput test that is mostly limited by the bus and' +
        ' memory bandwidth.'
      
        'Each sprite is an instance of the TBitmapLayer class. Use the ch' +
        'eckbox below to enable or disable the repaint optimizations.')
    ReadOnly = True
    TabOrder = 6
  end
  object bBenchmark: TButton
    Left = 520
    Top = 652
    Width = 96
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Benchmark'
    TabOrder = 7
    OnClick = bBenchmarkClick
  end
  object BitmapList: TBitmap32List
    Bitmaps = <
      item
        Bitmap.ResamplerClassName = 'TNearestResampler'
      end
      item
        Bitmap.ResamplerClassName = 'TNearestResampler'
      end
      item
        Bitmap.ResamplerClassName = 'TNearestResampler'
      end>
    Left = 432
    Top = 112
  end
  object TimerFPS: TTimer
    Interval = 5000
    OnTimer = TimerFPSTimer
    Left = 384
    Top = 584
  end
end
