object FormMain: TFormMain
  Left = 0
  Top = 0
  Cursor = crCross
  Caption = 'RepaintMode test'
  ClientHeight = 393
  ClientWidth = 994
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 101
    Width = 994
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 292
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 994
    Height = 101
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkFlat
    BevelOuter = bvNone
    FullRepaint = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object Panel5: TPanel
      Left = 809
      Top = 0
      Width = 185
      Height = 99
      Align = alRight
      BevelEdges = [beLeft]
      BevelKind = bkFlat
      BevelOuter = bvNone
      FullRepaint = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 0
      object ButtonClear: TButton
        Left = 4
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Clear all'
        TabOrder = 0
        OnClick = ButtonClearClick
      end
      object RadioGroupRepaint: TRadioGroup
        Left = 4
        Top = 30
        Width = 153
        Height = 65
        Caption = 'Repaint mode'
        ItemIndex = 2
        Items.Strings = (
          'Full repaint'
          'Direct repaint'
          'Optimized repaint')
        TabOrder = 1
        OnClick = RadioGroupRepaintClick
      end
      object ButtonDraw: TButton
        Left = 85
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Draw stuff'
        TabOrder = 2
        OnClick = ButtonDrawClick
      end
    end
    object Panel6: TPanel
      Left = 0
      Top = 0
      Width = 809
      Height = 99
      Align = alClient
      BevelOuter = bvNone
      FullRepaint = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 1
      object Label1: TLabel
        Left = 0
        Top = 0
        Width = 809
        Height = 15
        Align = alTop
        Caption = 
          'Left mouse button: Draw. Right mouse button: Draw stuff. Middle ' +
          'mouse button: Clear'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        ShowAccelChar = False
        Transparent = True
        WordWrap = True
        ExplicitWidth = 481
      end
      object MemoHelp: TMemo
        Left = 0
        Top = 15
        Width = 809
        Height = 84
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Enabled = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        Lines.Strings = (
          'Lorem ipsum dolor')
        ParentColor = True
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
        WantReturns = False
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 104
    Width = 250
    Height = 289
    Align = alLeft
    BevelOuter = bvNone
    FullRepaint = False
    Padding.Left = 2
    Padding.Right = 2
    Padding.Bottom = 2
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    object Label2: TLabel
      Left = 2
      Top = 0
      Width = 246
      Height = 15
      Align = alTop
      Caption = 'TPaintBox32'
      ShowAccelChar = False
      WordWrap = True
      ExplicitWidth = 65
    end
    object PaintBox32: TPaintBox32
      Left = 2
      Top = 15
      Width = 246
      Height = 272
      Cursor = crCross
      Align = alClient
      RepaintMode = rmOptimizer
      TabOrder = 0
      OnMouseDown = PaintBox32MouseDown
      OnMouseMove = PaintBox32MouseMove
      OnMouseUp = PaintBox32MouseUp
      OnPaintBuffer = PaintBox32PaintBuffer
    end
  end
  object Panel2: TPanel
    Left = 250
    Top = 104
    Width = 244
    Height = 289
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    Padding.Left = 2
    Padding.Right = 2
    Padding.Bottom = 2
    ParentColor = True
    ShowCaption = False
    TabOrder = 2
    object Label3: TLabel
      Left = 2
      Top = 0
      Width = 240
      Height = 15
      Align = alTop
      Caption = 'TImage32'
      ShowAccelChar = False
      WordWrap = True
      ExplicitWidth = 51
    end
    object Image32: TImage32
      Left = 2
      Top = 15
      Width = 240
      Height = 272
      Cursor = crCross
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      RepaintMode = rmOptimizer
      Scale = 1.000000000000000000
      ScaleMode = smScale
      MousePan.Enabled = True
      MousePan.ShiftState = [mssShift]
      MouseZoom.Enabled = True
      MouseZoom.MaintainPivot = False
      TabOrder = 0
      OnMouseDown = Image32MouseDown
      OnMouseMove = ImgView32MouseMove
      OnMouseUp = Image32MouseUp
      ExplicitLeft = 0
    end
  end
  object Panel3: TPanel
    Left = 494
    Top = 104
    Width = 250
    Height = 289
    Align = alRight
    BevelOuter = bvNone
    FullRepaint = False
    Padding.Left = 2
    Padding.Right = 2
    Padding.Bottom = 2
    ParentColor = True
    ShowCaption = False
    TabOrder = 3
    object Label4: TLabel
      Left = 2
      Top = 0
      Width = 246
      Height = 15
      Align = alTop
      Caption = 'TImgView32'
      ShowAccelChar = False
      WordWrap = True
      ExplicitWidth = 64
    end
    object ImgView32: TImgView32
      Left = 2
      Top = 15
      Width = 246
      Height = 272
      Cursor = crCross
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      Centered = False
      RepaintMode = rmOptimizer
      Scale = 1.000000000000000000
      ScaleMode = smScale
      MousePan.Enabled = True
      MousePan.ShiftState = [mssShift]
      MouseZoom.Enabled = True
      MouseZoom.MaintainPivot = False
      ScrollBars.ShowHandleGrip = False
      ScrollBars.Style = rbsDefault
      ScrollBars.Size = 17
      ScrollBars.Visibility = svHidden
      SizeGrip = sgNone
      OverSize = 0
      TabOrder = 0
      OnMouseDown = Image32MouseDown
      OnMouseMove = ImgView32MouseMove
      OnMouseUp = Image32MouseUp
    end
  end
  object Panel4: TPanel
    Left = 744
    Top = 104
    Width = 250
    Height = 289
    Align = alRight
    BevelOuter = bvNone
    FullRepaint = False
    Padding.Left = 2
    Padding.Right = 2
    Padding.Bottom = 2
    ParentColor = True
    ShowCaption = False
    TabOrder = 4
    object Label5: TLabel
      Left = 2
      Top = 0
      Width = 246
      Height = 15
      Align = alTop
      Caption = 'TImgView32 with layer'
      ShowAccelChar = False
      WordWrap = True
      ExplicitWidth = 118
    end
    object ImgView32Layers: TImgView32
      Left = 2
      Top = 15
      Width = 246
      Height = 272
      Cursor = crCross
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      Centered = False
      RepaintMode = rmOptimizer
      Scale = 1.000000000000000000
      ScaleMode = smScale
      MousePan.Enabled = True
      MousePan.ShiftState = [mssShift]
      MouseZoom.Enabled = True
      MouseZoom.MaintainPivot = False
      ScrollBars.ShowHandleGrip = False
      ScrollBars.Style = rbsDefault
      ScrollBars.Size = 17
      ScrollBars.Visibility = svHidden
      SizeGrip = sgNone
      OverSize = 0
      TabOrder = 0
      OnMouseDown = Image32MouseDown
      OnMouseMove = ImgView32MouseMove
      OnMouseUp = Image32MouseUp
    end
  end
end
