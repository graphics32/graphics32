object MainForm: TMainForm
  Left = 225
  Top = 177
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Filling Polygons with Color Gradients Demo'
  ClientHeight = 445
  ClientWidth = 585
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 15
  object PnlControl: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 445
    Align = alLeft
    TabOrder = 0
    object LblColorStopsTop: TLabel
      Left = 14
      Top = 15
      Width = 68
      Height = 15
      Caption = 'Color Stops:'
      FocusControl = MemoColorStops
    end
    object MemoColorStops: TMemo
      Left = 14
      Top = 38
      Width = 108
      Height = 139
      Lines.Strings = (
        '0.0: clRed32'
        '0.1: clYellow32'
        '0.3: clLime32'
        '0.5: $AA00FFFF'
        '0.7: clBlue32'
        '0.9: clFuchsia32'
        '1.0: $80FF0000')
      TabOrder = 0
      WordWrap = False
      OnChange = MemoColorStopsChange
    end
    object RgpEllipseFillStyle: TRadioGroup
      Left = 14
      Top = 365
      Width = 110
      Height = 68
      Caption = 'Radial &Fill Style'
      ItemIndex = 1
      Items.Strings = (
        'Simple'
        'SVG')
      TabOrder = 1
      OnClick = RgpEllipseFillStyleClick
    end
    object RgpSpreadMethod: TRadioGroup
      Left = 14
      Top = 264
      Width = 110
      Height = 95
      Caption = 'Spread Method'
      ItemIndex = 0
      Items.Strings = (
        'Pad'
        'Reflect'
        'Repeat')
      TabOrder = 2
      OnClick = RgpSpreadMethodClick
    end
    object BtnDefaults: TButton
      Left = 32
      Top = 183
      Width = 75
      Height = 25
      Caption = '&Defaults'
      TabOrder = 3
      OnClick = BtnDefaultsClick
    end
  end
  object ImgView32: TImgView32
    Left = 145
    Top = 0
    Width = 440
    Height = 445
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 16
    ScrollBars.Visibility = svHidden
    OverSize = 0
    TabOrder = 1
    OnDblClick = ImgView32DblClick
    OnMouseDown = ImgView32MouseDown
    OnMouseMove = ImgView32MouseMove
    OnMouseUp = ImgView32MouseUp
  end
  object MainMenu: TMainMenu
    Left = 193
    Top = 16
    object MnuFile: TMenuItem
      Caption = '&File'
      object MnuFileOpen: TMenuItem
        Caption = 'Open...'
        OnClick = MnuFileOpenClick
      end
      object MnuFileSaveAs: TMenuItem
        Caption = 'Save As...'
        OnClick = MnuFileSaveAsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MnuExit: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = BtnExitClick
      end
    end
    object MnuSpreadMethod: TMenuItem
      Caption = '&Spread Method'
      object MnuPad: TMenuItem
        Caption = '&Pad'
        Checked = True
        RadioItem = True
        OnClick = MnuSpreadClick
      end
      object MnuReflect: TMenuItem
        Tag = 1
        Caption = '&Reflect'
        RadioItem = True
        OnClick = MnuSpreadClick
      end
      object MnuRepeat: TMenuItem
        Tag = 2
        Caption = '&Repeat'
        RadioItem = True
        OnClick = MnuSpreadClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.stops.txt'
    Left = 192
    Top = 80
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.stops.txt'
    Filter = 'Color Stops (*.stops.txt)|*.stops.txt'
    Left = 192
    Top = 136
  end
end
