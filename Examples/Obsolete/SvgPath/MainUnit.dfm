object FrmSvgPathRenderer: TFrmSvgPathRenderer
  Left = 0
  Top = 0
  Caption = 'SVG Path Renderer'
  ClientHeight = 518
  ClientWidth = 761
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
    761
    518)
  PixelsPerInch = 96
  TextHeight = 13
  object LblPathData: TLabel
    Left = 8
    Top = 11
    Width = 52
    Height = 13
    Caption = 'Path Data:'
  end
  object ShpFillColor: TShape
    Left = 715
    Top = 11
    Width = 16
    Height = 16
    Anchors = [akTop, akRight]
    Brush.Color = 16711808
    OnMouseDown = ShpFillColorMouseDown
  end
  object ShpStrokeColor: TShape
    Left = 737
    Top = 11
    Width = 16
    Height = 16
    Anchors = [akTop, akRight]
    Brush.Color = 33023
    OnMouseDown = ShpStrokeColorMouseDown
  end
  object Image32: TImage32
    Left = 0
    Top = 35
    Width = 761
    Height = 483
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TLinearResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnResize = Image32Resize
  end
  object EditSVGPath: TEdit
    Left = 66
    Top = 8
    Width = 643
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 
      'm2.3e2 58c-3.3-4.3-2-10 4-7.3 8.2 2.6 22-1.7 16-12-4.5-4.2-20-2.' +
      '4-11-11 13 3.2 16-17 1.7-15-7.8 3.3-16-.20-6.3-6.9 9.4-2.6 24-1.' +
      '7 26 10 .78 9-13 15-1.5 19 6.7 11-2.9 26-16 24-4.3.03-8.6.05-13-' +
      '.7zm-1.4e2-13c-1-10-1-20-1-30 7.7 2 17-3.8 23 2.2 8.3 7.9 4.7 27' +
      '-8.3 27-11-6.8-5.4 6.8-8.6 12-6.8 2-4.3-7.7-5.4-11zm17-8.6c8.9-7' +
      '.2-3.9-24-9.6-12-2.7 5.8 2.3 19 9.6 12zm-1.1e2-21c5-11 20-16 30-' +
      '9.8 3 9.8-15-.4-20 7.8-9.3 9.3-1.3 28 13 25 6.3-5.1-1.3-9.8-6.2-' +
      '11-1.8-7.3 9.7-3.1 14-4.2 3.2.55.58 6.9 1.4 9.9 5 13-13 12-21 9-' +
      '11-4-13-19-11-27zm41 23c.79-6.2-5.8-26 5.4-22 3.8-.76 15-5.3 10 ' +
      '3.5-13 .43-6.2 16-9.6 23-4.5 2-5.8-1.3-5.9-5.1zm24 4c-11-9 3-20 ' +
      '12-18-.58-6.3-23-.49-11-9.2 9.4-3.6 21 2.5 19 14 1.7 8.2 1.1 20-' +
      '9.3 14-3.3 2-7.9 1.5-11-.04zm13-8.3c-1.5-9.2-17 3.2-4.5 3.2 1.9-' +
      '.02 4.5-.86 4.5-3.2zm50 8.1c-.83-14-.02-28 .07-42 3.8-.47 8.6-2.' +
      '2 6.9 3.5v13c6-6.8 20-3.6 19 6.7.13 6.5.05 13 .07 20-10 1.1-6.5-' +
      '12-7.9-18-.85-11-14-1.7-11 6.4-.24 4.8 1.8 17-7 10zm34 .78c-1.4-' +
      '9.2-.65-19-.83-28 2.9.66 9.1-2 7.5 3.1-.28 8.3.7 17-.72 25-1.4 1' +
      '.5-4.3.72-5.9-.04zm20-.57c-12-6.7-6.7-29 7.5-28 11-1.1 8.4 8.4-1' +
      '.2 6.6-12 5.5-2.4 21 8.6 17 3.5 8-11 5.3-15 4.4zm21 .57c-8.2-9.7' +
      ' 21-1.9 8.8-11-14-.47-10-20 2.6-18 7.5-1.8 12 9.2 1.8 6.7-9.2-1.' +
      '5-6.4 6.8.60 7.2 12 12 3.3 17-14 15zm86-30c-5.9-3.3-13 .04-19 1.' +
      '3-1.2-8.9 8.9-11 16-11 10-.030 16 11 10 21-3.9 4.7-15 9.4-8.9 9.' +
      '5h15c-.96 2.6 2.1 9.1-1.8 8.8h-32c.87-16 24-16 20-30zm-1.3e2-6.1' +
      'c-3.3-9 12-8.4 6.5 1-1.7 1.4-5.3.89-6.5-1z'
    OnChange = EditSVGPathChange
    OnKeyPress = EditSVGPathKeyPress
  end
  object ColorDialog: TColorDialog
    Left = 688
    Top = 48
  end
end
