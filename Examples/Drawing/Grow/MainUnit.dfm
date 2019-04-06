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
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    542
    459)
  PixelsPerInch = 96
  TextHeight = 13
  object PnlImage: TPanel
    Left = 8
    Top = 8
    Width = 526
    Height = 443
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvLowered
    BorderWidth = 1
    TabOrder = 0
    object Image: TImage32
      Left = 2
      Top = 2
      Width = 522
      Height = 439
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCenter
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
      OnClick = ImageClick
      OnResize = ImageResize
    end
  end
  object MainMenu1: TMainMenu
    Left = 154
    Top = 154
    object File1: TMenuItem
      Caption = '&File'
      object Refresh1: TMenuItem
        Caption = '&Refresh'
        ShortCut = 13
        OnClick = Refresh1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = Exit1Click
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object mnuInflatePolygon: TMenuItem
        Caption = 'Inflate &Polygon'
        ShortCut = 16464
        OnClick = mnuInflatePolygonClick
      end
      object mnuInflatePolyLine: TMenuItem
        Caption = 'Inflate Poly&Line'
        Checked = True
        ShortCut = 16460
        OnClick = mnuInflatePolygonClick
      end
    end
  end
end
