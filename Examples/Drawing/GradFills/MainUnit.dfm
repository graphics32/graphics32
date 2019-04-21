object MainForm: TMainForm
  Left = 225
  Top = 177
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Filling Polygons with Color Gradients'
  ClientHeight = 449
  ClientWidth = 502
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
    Height = 449
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 445
    object LblColorStopsTop: TLabel
      Left = 14
      Top = 15
      Width = 68
      Height = 15
      Caption = 'Color Stops:'
      FocusControl = MemoColorStops
    end
    object LblLookupTableOrder: TLabel
      Left = 14
      Top = 225
      Width = 111
      Height = 15
      Caption = 'Lookup Table Order:'
    end
    object MemoColorStops: TMemo
      Left = 14
      Top = 38
      Width = 114
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
      Width = 114
      Height = 68
      Caption = 'Radial &Fill Style'
      ItemIndex = 1
      Items.Strings = (
        'Simple'
        'SVG')
      TabOrder = 1
      OnClick = RgpEllipseFillStyleClick
    end
    object RgpWrapMode: TRadioGroup
      Left = 14
      Top = 272
      Width = 114
      Height = 87
      Caption = '&Wrap Mode'
      ItemIndex = 0
      Items.Strings = (
        'Clamp'
        'Repeat'
        'Mirror')
      TabOrder = 2
      OnClick = RgpWrapModeClick
    end
    object BtnDefaults: TButton
      Left = 34
      Top = 183
      Width = 75
      Height = 25
      Caption = '&Defaults'
      TabOrder = 3
      OnClick = BtnDefaultsClick
    end
    object CmbLUT: TComboBox
      Left = 14
      Top = 243
      Width = 114
      Height = 23
      TabOrder = 4
      Text = '9 (= 512 Values)'
      OnChange = CmbLUTChange
      Items.Strings = (
        '4 (= 16 Values)'
        '5 (= 32 Values)'
        '6 (= 64 Values)'
        '7 (= 128 Values)'
        '8 (= 256 Values)'
        '9 (= 512 Values)'
        '10 (= 1024 Values)'
        '11 (= 2048 Values)'
        '12 (= 4096 Values)'
        '13 (= 8192 Values)')
    end
  end
  object ImgView32: TImgView32
    Left = 145
    Top = 0
    Width = 357
    Height = 449
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    RepaintMode = rmOptimizer
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
    ExplicitWidth = 440
    ExplicitHeight = 445
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
    object MnuWrapMode: TMenuItem
      Caption = '&Wrap Mode'
      object MnuPad: TMenuItem
        Caption = '&Clamp'
        Checked = True
        RadioItem = True
        OnClick = MnuSpreadClick
      end
      object MnuRepeat: TMenuItem
        Tag = 1
        Caption = '&Repeat'
        RadioItem = True
        OnClick = MnuSpreadClick
      end
      object MnuReflect: TMenuItem
        Tag = 2
        Caption = '&Mirror'
        RadioItem = True
        OnClick = MnuSpreadClick
      end
    end
    object MnuRadialFillStyle: TMenuItem
      Caption = 'Radial &Fill Style'
      object MnuSimple: TMenuItem
        Caption = '&Simple'
        RadioItem = True
        OnClick = MnuRadialFillStyleClick
      end
      object MnuSVG: TMenuItem
        Tag = 1
        Caption = 'S&VG'
        Checked = True
        RadioItem = True
        OnClick = MnuRadialFillStyleClick
      end
    end
    object MnuLookupTableOrder: TMenuItem
      Caption = '&Lookup Table Order'
      object MnuOrder4: TMenuItem
        Caption = '4 (= 16 Values)'
        RadioItem = True
        OnClick = MnuOrderClick
      end
      object MnuOrder5: TMenuItem
        Tag = 1
        Caption = '5 (= 32 Values)'
        RadioItem = True
        OnClick = MnuOrderClick
      end
      object MnuOrder6: TMenuItem
        Tag = 2
        Caption = '6 (= 64 Values)'
        RadioItem = True
        OnClick = MnuOrderClick
      end
      object MnuOrder7: TMenuItem
        Tag = 3
        Caption = '7 (= 128 Values)'
        RadioItem = True
        OnClick = MnuOrderClick
      end
      object MnuOrder8: TMenuItem
        Tag = 4
        Caption = '8 (= 256 Values)'
        RadioItem = True
        OnClick = MnuOrderClick
      end
      object MnuOrder9: TMenuItem
        Tag = 5
        Caption = '9 (= 512 Values)'
        Checked = True
        RadioItem = True
        OnClick = MnuOrderClick
      end
      object MnuOrder10: TMenuItem
        Tag = 6
        Caption = '10 (= 1024 Values)'
        RadioItem = True
        OnClick = MnuOrderClick
      end
      object MnuOrder11: TMenuItem
        Tag = 7
        Caption = '11 (= 2048 Values)'
        RadioItem = True
        OnClick = MnuOrderClick
      end
      object MnuOrder12: TMenuItem
        Tag = 8
        Caption = '12 (= 4096 Values)'
        RadioItem = True
        OnClick = MnuOrderClick
      end
      object MnuOrder13: TMenuItem
        Tag = 9
        Caption = '13 (= 8192 Values)'
        RadioItem = True
        OnClick = MnuOrderClick
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
