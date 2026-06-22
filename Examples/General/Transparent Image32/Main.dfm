object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Transparent TImage32 example'
  ClientHeight = 462
  ClientWidth = 796
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    796
    462)
  TextHeight = 15
  object Shape1: TShape
    Left = 8
    Top = 60
    Width = 233
    Height = 201
    Brush.Color = 11173888
    Brush.Style = bsDiagCross
    Pen.Color = 12658449
    Pen.Width = 3
    Shape = stRoundRect
    Visible = False
  end
  object Image1: TImage
    Left = 24
    Top = 24
    Width = 241
    Height = 229
    Center = True
    Proportional = True
  end
  object MemoRed: TMemo
    Left = 148
    Top = 235
    Width = 345
    Height = 205
    Anchors = [akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    Color = 4071127
    Ctl3D = False
    Lines.Strings = (
      'Sed ut perspiciatis unde omnis iste natus error sit voluptatem '
      'accusantium doloremque laudantium, totam rem aperiam, '
      'eaque ipsa quae ab illo inventore veritatis et quasi architecto '
      'beatae vitae dicta sunt explicabo. Nemo enim ipsam '
      'voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed '
      'quia consequuntur magni dolores eos qui ratione voluptatem '
      'sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum '
      'quia dolor sit amet, consectetur, adipisci velit, sed quia non '
      'numquam eius modi tempora incidunt ut labore et dolore '
      'magnam aliquam quaerat voluptatem. Ut enim ad minima '
      'veniam, quis nostrum exercitationem ullam corporis suscipit '
      'laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis '
      'autem vel eum iure reprehenderit qui in ea voluptate velit esse '
      'quam nihil molestiae consequatur, vel illum qui dolorem eum '
      'fugiat quo voluptas nulla pariatur?')
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 1
    WantReturns = False
  end
  object ImageSprite: TImage32
    Left = 40
    Top = 116
    Width = 429
    Height = 221
    Anchors = [akLeft, akRight]
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 2
    Visible = False
  end
  object Panel1: TPanel
    Left = 508
    Top = 0
    Width = 288
    Height = 462
    Align = alRight
    BevelKind = bkTile
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    ParentColor = True
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 268
      Height = 60
      Align = alTop
      Caption = 
        'This example demonstrates transparent TImage32 controls.'#13#10#13#10'The ' +
        'form contains (top to bottom):'
      ShowAccelChar = False
      WordWrap = True
      ExplicitWidth = 264
    end
    object LabelHatchInfo: TLabel
      Left = 8
      Top = 420
      Width = 268
      Height = 30
      Align = alBottom
      Caption = 
        'Note that the glitches in the hatch pattern are caused by a bug ' +
        'in the TShape control.'
      ShowAccelChar = False
      Visible = False
      WordWrap = True
      ExplicitWidth = 243
    end
    object CheckBoxSprite: TCheckBox
      Left = 8
      Top = 102
      Width = 268
      Height = 17
      Align = alTop
      Caption = 'Animated alien: TImage32 (TWinControl)'
      TabOrder = 0
      OnClick = CheckBoxSpriteClick
    end
    object CheckBoxDice: TCheckBox
      Left = 8
      Top = 136
      Width = 268
      Height = 17
      Align = alTop
      Caption = 'Dice: TImage (TGraphicControl)'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBoxDiceClick
    end
    object CheckBoxRedText: TCheckBox
      Left = 8
      Top = 119
      Width = 268
      Height = 17
      Align = alTop
      Caption = 'Red text: TMemo (TWinControl)'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBoxRedTextClick
    end
    object CheckBoxCoffeeCup: TCheckBox
      Left = 8
      Top = 85
      Width = 268
      Height = 17
      Align = alTop
      Caption = 'Coffee cup: TImage32 (TWinControl)'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBoxCoffeeCupClick
    end
    object CheckBoxBlueText: TCheckBox
      Left = 8
      Top = 68
      Width = 268
      Height = 17
      Align = alTop
      Caption = 'Blue text: TMemo (TWinControl)'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CheckBoxBlueTextClick
    end
    object CheckBoxShape: TCheckBox
      Left = 8
      Top = 153
      Width = 268
      Height = 17
      Align = alTop
      Caption = 'Hatch pattern: TShape (TGraphicControl)'
      TabOrder = 5
      OnClick = CheckBoxShapeClick
    end
  end
  object Image32: TImage32
    Left = 65
    Top = 48
    Width = 381
    Height = 364
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TLinearResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smResize
    MousePan.Enabled = True
    MouseZoom.Enabled = True
    MouseZoom.Animate = True
    TabOrder = 0
  end
  object MemoBlue: TMemo
    Left = 200
    Top = 175
    Width = 281
    Height = 121
    Anchors = [akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    Color = 15780006
    Ctl3D = False
    Lines.Strings = (
      'Sed ut perspiciatis unde omnis '
      'iste natus error sit voluptatem '
      'accusantium doloremque '
      'laudantium, totam rem aperiam, '
      'eaque ipsa quae ab illo inventore '
      'veritatis et quasi architecto '
      'beatae vitae dicta sunt explicabo. '
      'Nemo enim ipsam '
      'voluptatem quia voluptas sit '
      'aspernatur aut odit aut fugit, sed '
      'quia consequuntur magni dolores '
      'eos qui ratione voluptatem '
      'sequi nesciunt. Neque porro '
      'quisquam est, qui dolorem ipsum '
      'quia dolor sit amet, consectetur, '
      'adipisci velit, sed quia non '
      'numquam eius modi tempora '
      'incidunt ut labore et dolore '
      'magnam aliquam quaerat '
      'voluptatem. Ut enim ad minima '
      'veniam, quis nostrum '
      'exercitationem ullam corporis '
      'suscipit '
      'laboriosam, nisi ut aliquid ex ea '
      'commodi consequatur? Quis '
      'autem vel eum iure reprehenderit '
      'qui in ea voluptate velit esse '
      'quam nihil molestiae '
      'consequatur, vel illum qui '
      'dolorem eum '
      'fugiat quo voluptas nulla '
      'pariatur?')
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 3
    WantReturns = False
  end
  object TimerAnimate: TTimer
    Enabled = False
    Interval = 10
    OnTimer = TimerAnimateTimer
    Left = 208
    Top = 268
  end
end
