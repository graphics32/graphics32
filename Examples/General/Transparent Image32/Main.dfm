object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Transparent TImage32 example'
  ClientHeight = 543
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    640
    543)
  TextHeight = 15
  object Image1: TImage
    Left = 24
    Top = 24
    Width = 321
    Height = 289
    Center = True
    Proportional = True
    Stretch = True
  end
  object Memo1: TMemo
    Left = 268
    Top = 312
    Width = 345
    Height = 205
    Anchors = [akRight, akBottom]
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
    ReadOnly = True
    TabOrder = 1
    WantReturns = False
  end
  object Image32: TImage32
    Left = 64
    Top = 44
    Width = 493
    Height = 445
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Color = 15461355
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smScale
    MousePan.Enabled = True
    MouseZoom.Enabled = True
    MouseZoom.Animate = True
    TabOrder = 0
    OnClick = Image32Click
  end
end
