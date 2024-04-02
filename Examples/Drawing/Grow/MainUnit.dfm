object FormGrow: TFormGrow
  Left = 224
  Top = 159
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Grow Example'
  ClientHeight = 459
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  TextHeight = 13
  object Image: TImage32
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 526
    Height = 443
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    MousePan.Enabled = True
    MouseZoom.Enabled = True
    TabOrder = 0
    OnClick = ImageClick
    OnResize = ImageResize
  end
  object MainMenu: TMainMenu
    Left = 74
    Top = 42
    object MenuItemFile: TMenuItem
      Caption = '&File'
      object MenuItemRefresh: TMenuItem
        Action = ActionRefresh
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuItemExit: TMenuItem
        Action = ActionFileExit
      end
    end
    object MenuItemOptions: TMenuItem
      Caption = '&Options'
      object MenuItemOptionsInflatePolygon: TMenuItem
        Action = ActionOptionShapePolygon
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
      end
      object MenuItemOptionsInflatePolyLine: TMenuItem
        Action = ActionOptionShapePolyLine
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object Joinstyle1: TMenuItem
        Action = ActionOptionJoinStyle
        GroupIndex = 1
        object Miterjoin1: TMenuItem
          Action = ActionOptionJoinMiter
          Checked = True
          GroupIndex = 2
          RadioItem = True
        end
        object Beveljoin1: TMenuItem
          Action = ActionOptionJoinBevel
          GroupIndex = 2
          RadioItem = True
        end
        object Beveljoin2: TMenuItem
          Action = ActionOptionJoinRound
          GroupIndex = 2
          RadioItem = True
        end
        object RoundExjoin2: TMenuItem
          Action = ActionOptionJoinRoundEx
          GroupIndex = 2
        end
        object RoundExjoin1: TMenuItem
          Action = ActionOptionJoinSquare
          GroupIndex = 2
          RadioItem = True
        end
      end
      object Endstyle1: TMenuItem
        Action = ActionOptionEndStyle
        GroupIndex = 1
        object Action51: TMenuItem
          Action = ActionOptionEndButt
          Checked = True
          GroupIndex = 3
          RadioItem = True
        end
        object Action52: TMenuItem
          Action = ActionOptionEndSquare
          GroupIndex = 3
          RadioItem = True
        end
        object Action71: TMenuItem
          Action = ActionOptionEndRound
          GroupIndex = 3
          RadioItem = True
        end
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object Growusing1: TMenuItem
        Caption = 'Offset using...'
        GroupIndex = 1
        object Graphics321: TMenuItem
          Action = ActionOptionGrowGraphics32
          AutoCheck = True
          GroupIndex = 4
          RadioItem = True
        end
        object Image321: TMenuItem
          Action = ActionOptionGrowAngus
          AutoCheck = True
          GroupIndex = 4
        end
        object Clipper1: TMenuItem
          Action = ActionOptionGrowClipper
          AutoCheck = True
          GroupIndex = 4
          RadioItem = True
        end
      end
    end
  end
  object ActionList: TActionList
    Left = 76
    Top = 96
    object ActionRefresh: TAction
      Caption = '&Refresh'
      ShortCut = 13
      OnExecute = ActionRefreshExecute
    end
    object ActionFileExit: TAction
      Caption = 'E&xit'
      ShortCut = 27
      OnExecute = ActionFileExitExecute
    end
    object ActionOptionShapePolygon: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = '&Polygon'
      GroupIndex = 1
      ShortCut = 16464
      OnExecute = ActionOptionShapeExecute
    end
    object ActionOptionShapePolyLine: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Poly&Line'
      Checked = True
      GroupIndex = 1
      ShortCut = 16460
      OnExecute = ActionOptionShapeExecute
    end
    object ActionOptionJoinStyle: TAction
      Category = 'Options'
      Caption = '&Join style'
      OnExecute = ActionDummyExecute
    end
    object ActionOptionJoinMiter: TAction
      Category = 'Options'
      Caption = '&Miter join'
      GroupIndex = 2
      OnExecute = ActionOptionJoinStyleExecute
      OnUpdate = ActionOptionJoinStyleUpdate
    end
    object ActionOptionJoinBevel: TAction
      Tag = 1
      Category = 'Options'
      Caption = '&Bevel join'
      GroupIndex = 2
      OnExecute = ActionOptionJoinStyleExecute
      OnUpdate = ActionOptionJoinStyleUpdate
    end
    object ActionOptionJoinRound: TAction
      Tag = 2
      Category = 'Options'
      Caption = '&Round join'
      GroupIndex = 2
      OnExecute = ActionOptionJoinStyleExecute
      OnUpdate = ActionOptionJoinStyleUpdate
    end
    object ActionOptionJoinRoundEx: TAction
      Tag = 3
      Category = 'Options'
      Caption = 'RoundEx join'
      GroupIndex = 2
      OnExecute = ActionOptionJoinStyleExecute
      OnUpdate = ActionOptionJoinStyleUpdate
    end
    object ActionOptionJoinSquare: TAction
      Tag = 4
      Category = 'Options'
      Caption = '&Square join'
      GroupIndex = 2
      OnExecute = ActionOptionJoinStyleExecute
      OnUpdate = ActionOptionJoinStyleUpdate
    end
    object ActionOptionEndStyle: TAction
      Category = 'Options'
      Caption = '&End style'
      OnExecute = ActionDummyExecute
      OnUpdate = ActionOptionEndStylesUpdate
    end
    object ActionOptionEndButt: TAction
      Category = 'Options'
      Caption = '&Butt end'
      GroupIndex = 3
      OnExecute = ActionOptionEndStyleExecute
      OnUpdate = ActionOptionEndStyleUpdate
    end
    object ActionOptionEndSquare: TAction
      Tag = 1
      Category = 'Options'
      Caption = '&Square end'
      GroupIndex = 3
      OnExecute = ActionOptionEndStyleExecute
      OnUpdate = ActionOptionEndStyleUpdate
    end
    object ActionOptionEndRound: TAction
      Tag = 2
      Category = 'Options'
      Caption = '&Round end'
      GroupIndex = 3
      OnExecute = ActionOptionEndStyleExecute
      OnUpdate = ActionOptionEndStyleUpdate
    end
    object ActionOptionGrowGraphics32: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Graphics32 reference'
      Checked = True
      GroupIndex = 4
      ShortCut = 32817
      OnExecute = ActionRedrawExecute
    end
    object ActionOptionGrowAngus: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Image32'
      GroupIndex = 4
      ShortCut = 32818
      OnExecute = ActionRedrawExecute
    end
    object ActionOptionGrowClipper: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Clipper'
      GroupIndex = 4
      ShortCut = 32819
      OnExecute = ActionRedrawExecute
    end
  end
end
