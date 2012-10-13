object FormTranformExample: TFormTranformExample
  Left = 582
  Top = 275
  BorderStyle = bsDialog
  Caption = 'Transform Demo'
  ClientHeight = 529
  ClientWidth = 619
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 264
    Width = 617
    Height = 265
    ActivePage = TstAffine
    TabOrder = 2
    OnChange = PageControlChange
    object TstAffine: TTabSheet
      Caption = 'Affine'
      DesignSize = (
        609
        237)
      object LblCodeString: TLabel
        Left = 8
        Top = 212
        Width = 59
        Height = 13
        Caption = 'Code string:'
      end
      object LblTransformationMatrix: TLabel
        Left = 443
        Top = 96
        Width = 110
        Height = 13
        Caption = 'Transformation matrix:'
      end
      object LblResampler: TLabel
        Left = 413
        Top = 8
        Width = 78
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Resampler Class'
      end
      object LblKernel: TLabel
        Left = 413
        Top = 48
        Width = 96
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Kernel Class (if any)'
        Visible = False
      end
      object PnlTransformationMatrix: TPanel
        Left = 445
        Top = 112
        Width = 105
        Height = 73
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          105
          73)
        object Shape1: TShape
          Left = 0
          Top = 0
          Width = 9
          Height = 73
          Align = alLeft
          Brush.Style = bsClear
        end
        object Shape2: TShape
          Left = 96
          Top = 0
          Width = 9
          Height = 73
          Align = alRight
          Brush.Style = bsClear
        end
        object StringGrid: TStringGrid
          Left = 8
          Top = 1
          Width = 93
          Height = 71
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          ColCount = 4
          DefaultColWidth = 37
          DefaultRowHeight = 26
          Enabled = False
          FixedCols = 0
          RowCount = 3
          FixedRows = 0
          Options = []
          ParentColor = True
          ScrollBars = ssNone
          TabOrder = 0
        end
      end
      object ListBox: TListBox
        Left = 8
        Top = 8
        Width = 72
        Height = 161
        Style = lbOwnerDrawFixed
        BorderStyle = bsNone
        ItemHeight = 20
        Items.Strings = (
          'Operation 1'
          'Operation 2'
          'Operation 3'
          'Operation 4'
          'Operation 5'
          'Operation 6'
          'Operation 7'
          'Operation 8')
        ParentColor = True
        TabOrder = 1
        OnClick = ListBoxClick
      end
      object BtnClearAll: TButton
        Left = 8
        Top = 178
        Width = 65
        Height = 23
        Caption = 'Clear All'
        TabOrder = 2
        OnClick = BtnClearAllClick
      end
      object EdtCodeString: TEdit
        Left = 80
        Top = 208
        Width = 521
        Height = 21
        ReadOnly = True
        TabOrder = 3
        Text = 'Clear;'
      end
      object PnlOperation: TPanel
        Left = 80
        Top = 8
        Width = 321
        Height = 193
        TabOrder = 4
        object LblType: TLabel
          Left = 8
          Top = 11
          Width = 28
          Height = 13
          Caption = 'Type:'
        end
        object ComboBox: TComboBox
          Left = 48
          Top = 8
          Width = 265
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = ComboBoxChange
          Items.Strings = (
            'None'
            'Translate'
            'Scale'
            'Rotate'
            'Skew')
        end
        object Notebook: TNotebook
          Left = 8
          Top = 40
          Width = 305
          Height = 145
          PageIndex = 3
          TabOrder = 1
          object PageNone: TPage
            Left = 0
            Top = 0
            Caption = 'opNone'
            object LblNoOperation: TLabel
              Left = 16
              Top = 16
              Width = 235
              Height = 26
              Caption = 
                'No type is specified for this operation.'#13#10'Select operation type ' +
                'from the combo box above'
            end
          end
          object PageTranslate: TPage
            Left = 0
            Top = 0
            Caption = 'opTranslate'
            object LblInfoTranslate: TLabel
              Left = 8
              Top = 8
              Width = 195
              Height = 26
              Caption = 'method declaration:'#13#10'procedure Translate(Dx, Dy: Extended);'
            end
            object LblDx: TLabel
              Left = 16
              Top = 59
              Width = 17
              Height = 13
              Caption = 'Dx:'
            end
            object LblDy: TLabel
              Left = 16
              Top = 91
              Width = 17
              Height = 13
              Caption = 'Dy:'
            end
            object EdtDx: TEdit
              Left = 48
              Top = 56
              Width = 57
              Height = 21
              Ctl3D = True
              ParentCtl3D = False
              TabOrder = 0
              Text = '0'
              OnChange = TranslationChanged
            end
            object EdtDy: TEdit
              Left = 48
              Top = 88
              Width = 57
              Height = 21
              Ctl3D = True
              ParentCtl3D = False
              TabOrder = 1
              Text = '0'
              OnChange = TranslationChanged
            end
            object GbrDx: TGaugeBar
              Left = 120
              Top = 58
              Width = 173
              Height = 16
              Backgnd = bgPattern
              Max = 1000
              Min = -1000
              ShowHandleGrip = True
              Position = 0
              OnUserChange = TranslationScrolled
            end
            object GbrDy: TGaugeBar
              Left = 120
              Top = 90
              Width = 173
              Height = 16
              Backgnd = bgPattern
              Max = 1000
              Min = -1000
              ShowHandleGrip = True
              Position = 0
              OnUserChange = TranslationScrolled
            end
          end
          object PageScale: TPage
            Left = 0
            Top = 0
            Caption = 'opScale'
            object LblSy: TLabel
              Left = 16
              Top = 91
              Width = 16
              Height = 13
              Caption = 'Sy:'
            end
            object LblSx: TLabel
              Left = 16
              Top = 59
              Width = 16
              Height = 13
              Caption = 'Sx:'
            end
            object LblScale: TLabel
              Left = 8
              Top = 8
              Width = 173
              Height = 26
              Caption = 'method declaration:'#13#10'procedure Scale(Sx, Sy: Extended);'
            end
            object EdtSy: TEdit
              Left = 48
              Top = 88
              Width = 57
              Height = 21
              TabOrder = 0
              Text = '0'
              OnChange = ScaleChanged
            end
            object EdtSx: TEdit
              Left = 48
              Top = 56
              Width = 57
              Height = 21
              TabOrder = 1
              Text = '0'
              OnChange = ScaleChanged
            end
            object GbrSx: TGaugeBar
              Left = 116
              Top = 58
              Width = 177
              Height = 16
              Backgnd = bgPattern
              Max = 1000
              Min = -1000
              ShowHandleGrip = True
              Position = 0
              OnUserChange = ScaleScrolled
            end
            object GbrSy: TGaugeBar
              Left = 116
              Top = 90
              Width = 177
              Height = 16
              Backgnd = bgPattern
              Max = 1000
              Min = -1000
              ShowHandleGrip = True
              Position = 0
              OnUserChange = ScaleScrolled
            end
          end
          object PageRotate: TPage
            Left = 0
            Top = 0
            Caption = 'opRotate'
            object LblInfoRotate: TLabel
              Left = 8
              Top = 8
              Width = 217
              Height = 26
              Caption = 'method declaration: '#13#10'procedure Rotate(Cx, Cy, Alpha: Extended);'
            end
            object LblCx: TLabel
              Left = 16
              Top = 59
              Width = 17
              Height = 13
              Caption = 'Cx:'
            end
            object LblAlpha: TLabel
              Left = 16
              Top = 91
              Width = 31
              Height = 13
              Caption = 'Alpha:'
            end
            object LblCy: TLabel
              Left = 152
              Top = 59
              Width = 17
              Height = 13
              Caption = 'Cy:'
            end
            object EdtCx: TEdit
              Left = 48
              Top = 56
              Width = 57
              Height = 21
              TabOrder = 0
              Text = '0'
              OnChange = RotationChanged
            end
            object EdtAlpha: TEdit
              Left = 48
              Top = 88
              Width = 57
              Height = 21
              TabOrder = 1
              Text = '0'
              OnChange = RotationChanged
            end
            object EdtCy: TEdit
              Left = 184
              Top = 56
              Width = 57
              Height = 21
              TabOrder = 2
              Text = '0'
              OnChange = RotationChanged
            end
            object GbrAlpha: TGaugeBar
              Left = 112
              Top = 90
              Width = 181
              Height = 16
              Backgnd = bgPattern
              Max = 1000
              Min = -1000
              ShowHandleGrip = True
              Position = 0
              OnUserChange = RotationScrolled
            end
          end
          object PageSkew: TPage
            Left = 0
            Top = 0
            Caption = 'opSkew'
            object LblInfoSkew: TLabel
              Left = 8
              Top = 8
              Width = 173
              Height = 26
              Caption = 'method declaration:'#13#10'procedure Skew(Fx, Fy: Extended);'
            end
            object LblFx: TLabel
              Left = 16
              Top = 59
              Width = 16
              Height = 13
              Caption = 'Fx:'
            end
            object LblFy: TLabel
              Left = 16
              Top = 91
              Width = 16
              Height = 13
              Caption = 'Fy:'
            end
            object EdtFx: TEdit
              Left = 48
              Top = 56
              Width = 57
              Height = 21
              TabOrder = 0
              Text = '0'
              OnChange = SkewChanged
            end
            object EdtFy: TEdit
              Left = 48
              Top = 88
              Width = 57
              Height = 21
              TabOrder = 1
              Text = '0'
              OnChange = SkewChanged
            end
            object GbrFx: TGaugeBar
              Left = 116
              Top = 58
              Width = 177
              Height = 16
              Backgnd = bgPattern
              Min = -100
              ShowHandleGrip = True
              Position = 0
              OnUserChange = SkewScrolled
            end
            object GbrFy: TGaugeBar
              Left = 116
              Top = 90
              Width = 177
              Height = 16
              Backgnd = bgPattern
              Min = -100
              ShowHandleGrip = True
              Position = 0
              OnUserChange = SkewScrolled
            end
          end
        end
      end
      object CmbResamplerClassNames: TComboBox
        Left = 413
        Top = 24
        Width = 177
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        TabOrder = 5
        OnChange = CmbResamplerClassNamesChange
      end
      object CmbKernelClassNames: TComboBox
        Left = 413
        Top = 64
        Width = 177
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        TabOrder = 6
        Visible = False
        OnChange = CmbKernelClassNamesChange
      end
    end
    object TstProjective: TTabSheet
      Caption = 'Projective'
      ImageIndex = 1
      object LblProjectiveNote: TLabel
        Left = 48
        Top = 24
        Width = 230
        Height = 26
        Caption = 'Note:'#13#10'Only convex polygons are transormed normally!'
      end
    end
  end
  object Src: TImage32
    Left = 2
    Top = 2
    Width = 256
    Height = 256
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Color = clAppWorkSpace
    ParentColor = False
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnDblClick = SrcDblClick
    OnPaintStage = DstPaintStage
  end
  object Dst: TImage32
    Left = 264
    Top = 2
    Width = 351
    Height = 256
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Color = clAppWorkSpace
    ParentColor = False
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 1
    OnMouseDown = RubberLayerMouseDown
    OnMouseMove = RubberLayerMouseMove
    OnMouseUp = RubberLayerMouseUp
    OnPaintStage = DstPaintStage
  end
  object PnlOpacity: TPanel
    Left = 264
    Top = 260
    Width = 281
    Height = 20
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = '  Opacity:'
    TabOrder = 3
    object OpacityBar: TGaugeBar
      Left = 56
      Top = 2
      Width = 213
      Height = 16
      Backgnd = bgPattern
      Max = 255
      ShowHandleGrip = True
      Position = 255
      OnChange = OpacityChange
    end
  end
  object CbxRepeat: TCheckBox
    Left = 544
    Top = 260
    Width = 73
    Height = 20
    Caption = 'Repeat'
    TabOrder = 4
    OnClick = CbxRepeatClick
  end
end
