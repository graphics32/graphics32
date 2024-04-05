object MainForm: TMainForm
  Left = 833
  Top = 165
  Caption = 'Curves Example'
  ClientHeight = 560
  ClientWidth = 527
  Color = clBtnFace
  ParentFont = True
  OnShow = BtnDrawCurveClick
  TextHeight = 15
  object Paintbox: TPaintBox32
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 511
    Height = 503
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    TabOrder = 0
    OnClick = PaintboxClick
    OnPaintBuffer = PaintboxPaintBuffer
  end
  object Panel1: TPanel
    Left = 0
    Top = 519
    Width = 527
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    object BtnDrawCurve: TButton
      Left = 8
      Top = 7
      Width = 97
      Height = 25
      Caption = 'Draw Curve'
      TabOrder = 0
      OnClick = BtnDrawCurveClick
    end
    object CbxUpdate: TCheckBox
      Left = 119
      Top = 11
      Width = 88
      Height = 17
      Caption = 'Timer'
      TabOrder = 1
      OnClick = CbxUpdateClick
    end
  end
end
