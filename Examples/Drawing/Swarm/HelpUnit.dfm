object FormHelp: TFormHelp
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Help'
  ClientHeight = 148
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Courier New'
  Font.Style = []
  Position = poDesigned
  OnKeyDown = FormKeyDown
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 276
    Height = 148
    Align = alClient
    BevelOuter = bvNone
    Enabled = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    ExplicitLeft = 56
    ExplicitTop = 44
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 276
      Height = 148
      Align = alClient
      BorderStyle = bsNone
      Lines.Strings = (
        '[F1] Show/hide help.'
        '[+] Add particles.'
        '[-] Remove particles.'
        '[Left mouse] Attract particles.'
        '[Right mouse] Repulse particles.'
        '[Middle mouse] Slow down particles.'
        '[C] Toggle color animation.'
        '[F] Toggle fade.'
        '[N] Toggle simplex noise view.')
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
      WantReturns = False
      ExplicitHeight = 89
    end
  end
end
