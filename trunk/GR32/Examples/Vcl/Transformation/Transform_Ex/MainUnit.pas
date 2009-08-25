unit MainUnit;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.INC}

uses
  {$IFDEF FPC} LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, GR32, GR32_Image,
  GR32_Transforms, GR32_Resamplers, GR32_Layers, ExtCtrls, StdCtrls, Buttons,
  ComCtrls, Grids, GR32_RangeBars;

type
  TOpType = (opNone, opTranslate, opScale, opRotate, opSkew);
  TOpRec = record
    OpType: TOpType;
    Dx, Dy: Extended;        // shifts for opTranslate mode
    Sx, Sy: Extended;        // scale factors for opScale mode
    Cx, Cy, Alpha: Extended; // rotation center and angle (deg) for opRotate mode
    Fx, Fy: Extended;        // skew factors for opSkew mode
  end;
  TOpRecs = array[0..7] of TOpRec;

const
  OpTypes: array [0..4] of TOpType = (opNone, opTranslate, opScale, opRotate,
    opSkew);

type
  TTransformMode = (tmAffine, tmProjective, tmBilinear);

  TForm1 = class(TForm)
    Src: TImage32;
    Dst: TImage32;
    OpacityBar: TGaugeBar;
    sbDx: TGaugeBar;
    sbDy: TGaugeBar;
    sbSx: TGaugeBar;
    sbSy: TGaugeBar;
    sbAlpha: TGaugeBar;
    sbFx: TGaugeBar;
    sbFy: TGaugeBar;
    PageControl1: TPageControl;
{$IFDEF FPC}
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    Page5: TPage;
{$ENDIF}
    TabSheet1: TTabSheet;
    Panel2: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    StringGrid: TStringGrid;
    ListBox: TListBox;
    Button1: TButton;
    Label9: TLabel;
    CodeString: TEdit;
    Panel1: TPanel;
    Label1: TLabel;
    ComboBox: TComboBox;
    Notebook: TNotebook;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    eDx: TEdit;
    eDy: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    eSy: TEdit;
    eSx: TEdit;
    Label11: TLabel;
    Label13: TLabel;
    Label16: TLabel;
    Label15: TLabel;
    eCx: TEdit;
    eAlpha: TEdit;
    eCy: TEdit;
    Label12: TLabel;
    Label14: TLabel;
    Label17: TLabel;
    eFx: TEdit;
    eFy: TEdit;
    Label10: TLabel;
    Panel3: TPanel;
    TabSheet2: TTabSheet;
    Label18: TLabel;
    ResamplerLabel: TLabel;
    ResamplerClassNamesList: TComboBox;
    KernelLabel: TLabel;
    KernelClassNamesList: TComboBox;
    cbRepeat: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure TranslationChanged(Sender: TObject);
    procedure ScaleChanged(Sender: TObject);
    procedure TranslationScrolled(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ScaleScrolled(Sender: TObject);
    procedure RotationChanged(Sender: TObject);
    procedure RotationScrolled(Sender: TObject);
    procedure SkewChanged(Sender: TObject);
    procedure SkewScrolled(Sender: TObject);
    procedure OpacityChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);

    procedure SrcRBResizingEvent(Sender: TObject; const OldLocation: TFloatRect;
      var NewLocation: TFloatRect; DragState: TDragState; Shift: TShiftState);

    procedure RubberLayerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure RubberLayerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure RubberLayerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure ResamplerClassNamesListClick(Sender: TObject);
    procedure ResamplerClassNamesListChange(Sender: TObject);
    procedure KernelClassNamesListChange(Sender: TObject);
    procedure DstPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure cbRepeatClick(Sender: TObject);
  protected
    LoadingValues: Boolean;
    DraggedVertex: Integer;
    LastMousePos: TPoint;
    StippleStart: Single;
    procedure PaintHandles(Sender: TObject; BackBuffer: TBitmap32);
  public
    SrcRubberBandLayer: TRubberBandLayer;
    Operation: TOpRecs;
    Current: ^TOpRec;
    AT: TAffineTransformation;
    PT: TProjectiveTransformation;
    TT: TTransformation;
    Vertices: array[0..3] of TPoint;
    Mode: TTransformMode;
    procedure ClearTransformations;
    procedure DoTransform;
    procedure GenTransform;
    procedure PrepareSource;
    procedure ShowSettings(OperationNum: Integer);
    procedure InitVertices; // for projective mapping
  end;

var
  Form1: TForm1;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
{$IFDEF Darwin}
  FPCMacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG;
{$ELSE}
  LazJPEG;
{$ENDIF}

function GetVal(Src: string; var Dst: Extended): Boolean;
var
  Code: Integer;
begin
  Val(Src, Dst, Code);
  Result := Code = 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
{$IFDEF Darwin}
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
  pathMedia: string;
begin
  // Under Mac OS X we need to get the location of the bundle
{$IFDEF Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
{$ENDIF}

  // On Lazarus we don't use design-time packages because they consume time to be installed
{$IFDEF FPC}
  Src := TImage32.Create(Self);
  Src.Parent := Self;
  Src.Left := 2;
  Src.Top := 2;
  Src.Width := 256;
  Src.Height := 256;
  Src.Bitmap.DrawMode := dmBlend;
  Src.Bitmap.ResamplerClassName := 'TNearestResampler';
  Src.BitmapAlign := baTopLeft;
  Src.Color := clAppWorkSpace;
  Src.ParentColor := False;
  Src.RepaintMode := rmOptimizer;
  Src.Scale := 1;
  Src.ScaleMode := smNormal;
  Src.TabOrder := 0;
  Src.OnPaintStage := DstPaintStage;

  Dst := TImage32.Create(Self);
  Dst.Parent := Self;
  Dst.Left := 264;
  Dst.Top := 2;
  Dst.Width := 351;
  Dst.Height := 256;
  Dst.Bitmap.DrawMode := dmBlend;
  Dst.Bitmap.ResamplerClassName := 'TNearestResampler';
  Dst.BitmapAlign := baTopLeft;
  Dst.Color := clAppWorkSpace;
  Dst.ParentColor := False;
  Dst.RepaintMode := rmOptimizer;
  Dst.Scale := 1;
  Dst.ScaleMode := smNormal;
  Dst.TabOrder := 1;
  Dst.OnMouseDown := RubberLayerMouseDown;
  Dst.OnMouseMove := RubberLayerMouseMove;
  Dst.OnMouseUp := RubberLayerMouseUp;
  Dst.OnPaintStage := DstPaintStage;

  sbDx := TGaugeBar.Create(Notebook.Page[0]);
  sbDx.Parent := Notebook.Page[0];
  sbDx.Left := 120;
  sbDx.Top := 58;
  sbDx.Width := 173;
  sbDx.Height := 16;
  sbDx.Backgnd := bgPattern;
  sbDx.Max := 1000;
  sbDx.Min := -1000;
  sbDx.ShowHandleGrip := True;
  sbDx.Position := 0;
  sbDx.OnUserChange := TranslationScrolled;

  sbDy := TGaugeBar.Create(Notebook.Page[0]);
  sbDy.Parent := Notebook.Page[0];
  sbDy.Left := 120;
  sbDy.Top := 90;
  sbDy.Width := 173;
  sbDy.Height := 16;
  sbDy.Backgnd := bgPattern;
  sbDy.Max := 1000;
  sbDy.Min := -1000;
  sbDy.ShowHandleGrip := True;
  sbDy.Position := 0;
  sbDy.OnUserChange := TranslationScrolled;

  sbSx := TGaugeBar.Create(Notebook.Page[2]);
  sbSx.Parent := Notebook.Page[2];
  sbSx.Left := 116;
  sbSx.Top := 58;
  sbSx.Width := 177;
  sbSx.Height := 16;
  sbSx.Backgnd := bgPattern;
  sbSx.Max := 1000;
  sbSx.Min := -1000;
  sbSx.ShowHandleGrip := True;
  sbSx.Position := 0;
  sbSx.OnUserChange := ScaleScrolled;

  sbSy := TGaugeBar.Create(Notebook.Page[2]);
  sbSy.Parent := Notebook.Page[2];
  sbSy.Left := 116;
  sbSy.Top := 90;
  sbSy.Width := 177;
  sbSy.Height := 16;
  sbSy.Backgnd := bgPattern;
  sbSy.Max := 1000;
  sbSy.Min := -1000;
  sbSy.ShowHandleGrip := True;
  sbSy.Position := 0;
  sbSy.OnUserChange := ScaleScrolled;

  sbAlpha := TGaugeBar.Create(Notebook.Page[3]);
  sbAlpha.Parent := Notebook.Page[3];
  sbAlpha.Left := 112;
  sbAlpha.Top := 90;
  sbAlpha.Width := 181;
  sbAlpha.Height := 16;
  sbAlpha.Backgnd := bgPattern;
  sbAlpha.Max := 1000;
  sbAlpha.Min := -1000;
  sbAlpha.ShowHandleGrip := True;
  sbAlpha.Position := 0;
  sbAlpha.OnUserChange := RotationScrolled;

  sbFx := TGaugeBar.Create(Notebook.Page[4]);
  sbFx.Parent := Notebook.Page[4];
  sbFx.Left := 116;
  sbFx.Top := 58;
  sbFx.Width := 177;
  sbFx.Height := 16;
  sbFx.Backgnd := bgPattern;
  sbFx.Min := -100;
  sbFx.ShowHandleGrip := True;
  sbFx.Position := 0;
  sbFx.OnUserChange := SkewScrolled;

  sbFy := TGaugeBar.Create(Notebook.Page[4]);
  sbFy.Parent := Notebook.Page[4];
  sbFy.Left := 116;
  sbFy.Top := 90;
  sbFy.Width := 177;
  sbFy.Height := 16;
  sbFy.Backgnd := bgPattern;
  sbFy.Min := -100;
  sbFy.ShowHandleGrip := True;
  sbFy.Position := 0;
  sbFy.OnUserChange := SkewScrolled;

  OpacityBar := TGaugeBar.Create(Panel3);
  OpacityBar.Parent := Panel3;
  OpacityBar.Left := 56;
  OpacityBar.Top := 2;
  OpacityBar.Width := 213;
  OpacityBar.Height := 16;
  OpacityBar.Backgnd := bgPattern;
  OpacityBar.Max := 255;
  OpacityBar.ShowHandleGrip := True;
  OpacityBar.Position := 255;
  OpacityBar.OnChange := OpacityChange;
{$ENDIF}

  // Different platforms store resource files on different locations
{$IFDEF Windows}
  pathMedia := '..\..\..\Media\';
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF Darwin}
    pathMedia := pathStr + '/Contents/Resources/Media/';
  {$ELSE}
    pathMedia := '../../../Media/';
  {$ENDIF}
{$ENDIF}

  Src.Bitmap.LoadFromFile(pathMedia + 'delphi.jpg');

  //Setup custom paintstages ("checkerboard" and border)
  with Dst do
  begin
    with PaintStages[0]^ do //Set up custom paintstage to draw checkerboard
    begin
      Stage := PST_CUSTOM;
      Parameter := 1; // use parameter to tag the stage, we inspect this in OnPaintStage
    end;
    with PaintStages.Add^ do  //Insert new paintstage on top of everything else, we use this to draw border
    begin
      Stage := PST_CUSTOM;
      Parameter := 2;
    end;
  end;

  with Src do
  begin
    with PaintStages[0]^ do
    begin
      Stage := PST_CUSTOM;
      Parameter := 1;
    end;
    with PaintStages.Add^ do
    begin
      Stage := PST_CUSTOM;
      Parameter := 2;
    end;
  end;

  ResamplerList.GetClassNames(ResamplerClassNamesList.Items);
  KernelList.GetClassNames(KernelClassNamesList.Items);
  ResamplerClassNamesList.ItemIndex := 0;
  KernelClassNamesList.ItemIndex := 0;

  SrcRubberBandLayer := TRubberBandLayer.Create(Src.Layers);
  SrcRubberBandLayer.OnResizing := SrcRBResizingEvent;
  SrcRubberBandLayer.Location := FloatRect(0, 0, Src.Bitmap.Width - 1, Src.Bitmap.Height - 1);
  with TCustomLayer.Create(Dst.Layers) do
  begin
    OnPaint := PaintHandles;
  end;

  DraggedVertex := -1;
  Dst.SetupBitmap; // set the destination bitmap size to match the image size
  PrepareSource;
  ClearTransformations;
  ShowSettings(0);
  AT := TAffineTransformation.Create;
  PT := TProjectiveTransformation.Create;
  TT := AT;
  DoTransform;

  Application.OnIdle := AppEventsIdle;
end;

procedure TForm1.ClearTransformations;
var
  I: Integer;
begin
  FillChar(Operation[0], SizeOf(TOpRecs), 0);
  for I := 0 to 7 do
  begin
    Operation[I].Sx := 1;
    Operation[I].Sy := 1;
    Operation[I].Cx := Src.Bitmap.Width / 2;
    Operation[I].Cy := Src.Bitmap.Height / 2;
  end;
end;

procedure TForm1.PrepareSource;
begin
  // make the border pixels transparent while keeping their RGB components
  if not cbRepeat.Checked then
    SetBorderTransparent(Src.Bitmap, Src.Bitmap.BoundsRect);
end;

procedure TForm1.DoTransform;
var
  i, j: Integer;
begin
  GenTransform;
  Dst.BeginUpdate;

  Dst.Bitmap.Clear($00000000);
  Transform(Dst.Bitmap, Src.Bitmap, TT);

  Dst.EndUpdate;
  Dst.Invalidate;

  if Mode = tmAffine then
  begin
    // fill the string grid
    for j := 0 to 2 do
      for i := 0 to 2 do
        StringGrid.Cells[i, j] := Format('%.3g', [AT.Matrix[i, j]]);
    StringGrid.Col := 3; // hide grid cursor
  end;
end;

procedure TForm1.GenTransform;
var
  I: Integer;
  Rec: TOpRec;
  S: string;
begin
  if Mode = tmProjective then
  begin
    PT.X0 := Vertices[0].X;
    PT.Y0 := Vertices[0].Y;
    PT.X1 := Vertices[1].X;
    PT.Y1 := Vertices[1].Y;
    PT.X2 := Vertices[2].X;
    PT.Y2 := Vertices[2].Y;
    PT.X3 := Vertices[3].X;
    PT.Y3 := Vertices[3].Y;
  end
  else
  begin
    // affine mode
    AT.Clear;
    for I := 0 to 7 do
    begin
      Rec := Operation[I];
      case Rec.OpType of
        opTranslate:  AT.Translate(Rec.Dx, Rec.Dy);
        opScale:      AT.Scale(Rec.Sx, Rec.Sy);
        opRotate:     AT.Rotate(Rec.Cx, Rec.Cy, Rec.Alpha);
        opSkew:       AT.Skew(Rec.Fx, Rec.Fy);
      end;
      case Rec.OpType of
        opTranslate:  s := s + Format('Translate(%.3g, %.3g); ', [Rec.Dx, Rec.Dy]);
        opScale:      s := s + Format('Scale(%.3g, %.3g); ', [Rec.Sx, Rec.Sy]);
        opRotate:     s := s + Format('Rotate(%.3g, %.3g, %3g); ', [Rec.Cx, Rec.Cy, Rec.Alpha]);
        opSkew:       s := s + Format('Skew(%.3g, %.3g); ', [Rec.Fx, Rec.Fy]);
      end;
    end;
    if Length(s) = 0 then s := 'Clear;';
    CodeString.Text := s;
  end;
  TT.SrcRect := SrcRubberBandLayer.Location;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AT.Free;
  PT.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ClearTransformations;
  ShowSettings(Listbox.ItemIndex);
  DoTransform;
end;

procedure TForm1.ListBoxClick(Sender: TObject);
begin
  ShowSettings(ListBox.ItemIndex);
end;

procedure TForm1.ShowSettings(OperationNum: Integer);
begin
  LoadingValues := True;
  ListBox.ItemIndex := OperationNum;
  Current := @Operation[OperationNum];
  Combobox.ItemIndex := Ord(Current.OpType);
  NoteBook.PageIndex := Ord(Current.OpType);
  eDx.Text := Format('%.4g', [Current.Dx]);
  eDy.Text := Format('%.4g', [Current.Dy]);
  sbDx.Position := Round(Current.Dx * 10);
  sbDy.Position := Round(Current.Dy * 10);
  eSx.Text := Format('%.4g', [Current.Sx]);
  eSy.Text := Format('%.4g', [Current.Sy]);
  sbSx.Position := Round(Current.Sx * 100);
  sbSy.Position := Round(Current.Sy * 100);
  eCx.Text := Format('%.4g', [Current.Cx]);
  eCy.Text := Format('%.4g', [Current.Cy]);
  eAlpha.Text := Format('%.4g', [Current.Alpha]);
  sbAlpha.Position := Round(Current.Alpha * 2);
  eFx.Text := Format('%.4g', [Current.Fx]);
  eFy.Text := Format('%.4g', [Current.Fy]);
  sbFx.Position := Round(Current.Fx * 100);
  sbFy.Position := Round(Current.Fy * 100);
  LoadingValues := False;
end;

procedure TForm1.ComboBoxChange(Sender: TObject);
begin
  Current.OpType := OpTypes[ComboBox.ItemIndex];
  ShowSettings(ListBox.ItemIndex);
  DoTransform;
end;

procedure TForm1.TranslationChanged(Sender: TObject);
var
  Tx, Ty: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(eDx.Text, Tx) and GetVal(eDy.Text, Ty) then
  begin
    Current.Dx := Tx;
    Current.Dy := Ty;
    DoTransform;
    LoadingValues := True;
    sbDx.Position := Round(Current.Dx * 10);
    sbDy.Position := Round(Current.Dy * 10);
    LoadingValues := False;
  end;
end;

procedure TForm1.TranslationScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Dx := sbDx.Position / 10;
  Current.Dy := sbDy.Position / 10;
  DoTransform;
  LoadingValues := True;
  eDx.Text := FloatToStr(Current.Dx);
  eDy.Text := FloatToStr(Current.Dy);
  LoadingValues := False;
end;

procedure TForm1.ScaleChanged(Sender: TObject);
var
  Sx, Sy: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(eSx.Text, Sx) and GetVal(eSy.Text, Sy) then
  begin
    Current.Sx := Sx;
    Current.Sy := Sy;
    DoTransform;
    LoadingValues := True;
    sbSx.Position := Round(Current.Sx * 100);
    sbSy.Position := Round(Current.Sy * 100);
    LoadingValues := False;
  end;
end;

procedure TForm1.ScaleScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Sx := sbSx.Position / 100;
  Current.Sy := sbSy.Position / 100;
  DoTransform;
  LoadingValues := True;
  eSx.Text := FloatToStr(Current.Sx);
  eSy.Text := FloatToStr(Current.Sy);
  LoadingValues := False;
end;

procedure TForm1.RotationChanged(Sender: TObject);
var
  Cx, Cy, Alpha: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(eCx.Text, Cx) and GetVal(eCy.Text, Cy) and
    GetVal(eAlpha.Text, Alpha) then
  begin
    Current.Cx := Cx;
    Current.Cy := Cy;
    Current.Alpha := Alpha;
    DoTransform;
    LoadingValues := True;
    sbAlpha.Position := Round(Alpha * 2);
    LoadingValues := False;
  end;
end;

procedure TForm1.RotationScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Alpha := sbAlpha.Position / 2;
  DoTransform;
  LoadingValues := True;
  eAlpha.Text := FloatToStr(Current.Alpha / 2);
  LoadingValues := False;
end;

procedure TForm1.SkewChanged(Sender: TObject);
var
  Fx, Fy: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(eFx.Text, Fx) and GetVal(eFy.Text, Fy) then
  begin
    Current.Fx := Fx;
    Current.Fy := Fy;
    DoTransform;
    LoadingValues := True;
    sbFx.Position := Round(Current.Fx * 10);
    sbFy.Position := Round(Current.Fy * 10);
    LoadingValues := False;
  end;
end;

procedure TForm1.SkewScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Fx := sbFx.Position / 10;
  Current.Fy := sbFy.Position / 10;
  DoTransform;
  LoadingValues := True;
  eFx.Text := FloatToStr(Current.Fx);
  eFy.Text := FloatToStr(Current.Fy);
  LoadingValues := False;
end;

procedure TForm1.OpacityChange(Sender: TObject);
begin
  OpacityBar.Update;
  Src.Bitmap.MasterAlpha := OpacityBar.Position;
  DoTransform;
end;

procedure TForm1.InitVertices;
begin
  Vertices[0].X := 0;
  Vertices[0].Y := 0;
  Vertices[1].X := Src.Bitmap.Width - 1;
  Vertices[1].Y := 0;
  Vertices[2].X := Src.Bitmap.Width - 1;
  Vertices[2].Y := Src.Bitmap.Height - 1;
  Vertices[3].X := 0;
  Vertices[3].Y := Src.Bitmap.Height - 1;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = TabSheet1 then
  begin
    Mode := tmAffine;
    TT := AT;
    ResamplerClassNamesList.Parent := TabSheet1;
    ResamplerLabel.Parent := TabSheet1;
    KernelClassNamesList.Parent := TabSheet1;
    KernelLabel.Parent := TabSheet1;
  end
  else
  begin
    // set current transformation as projective
    Mode := tmProjective;
    TT := PT;
    InitVertices;
    ResamplerClassNamesList.Parent := TabSheet2;
    ResamplerLabel.Parent := TabSheet2;
    KernelClassNamesList.Parent := TabSheet2;
    KernelLabel.Parent := TabSheet2;
  end;
  DoTransform;
end;

procedure TForm1.RubberLayerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  I: Integer;
begin
  if Mode = tmAffine then Exit;

  DraggedVertex := -1;

  // find the vertex to drag
  for I := 0 to 3 do
    if (Abs(Vertices[I].X - X) < 3) and (Abs(Vertices[I].Y - Y) < 3) then
    begin
      DraggedVertex := I;
      Break;
    end;

  // or drag all of them, (DragVertex = 4)
  if DraggedVertex = -1 then DraggedVertex := 4;

  // store current mouse position
  LastMousePos := Classes.Point(X, Y);
end;

procedure TForm1.RubberLayerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
var
  Dx, Dy, I: Integer;
begin
  if Mode = tmAffine then Exit;
  
  if DraggedVertex = -1 then Exit; // mouse is not pressed

  Dx := X - LastMousePos.X;
  Dy := Y - LastMousePos.Y;
  LastMousePos := Classes.Point(X, Y);

  // update coords
  if DraggedVertex = 4 then
  begin
    for I := 0 to 3 do
    begin
      Inc(Vertices[I].X, Dx);
      Inc(Vertices[I].Y, Dy);
    end;
  end
  else
  begin
    Inc(Vertices[DraggedVertex].X, Dx);
    Inc(Vertices[DraggedVertex].Y, Dy);
  end;

  DoTransform;
end;

procedure TForm1.RubberLayerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  DraggedVertex := -1;
end;

procedure TForm1.AppEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if DraggedVertex >= 0 then Exit;
  StippleStart := StippleStart - 0.05;
  Dst.Invalidate;
end;

procedure TForm1.PaintHandles(Sender: TObject; BackBuffer: TBitmap32);
var
  I, X0, Y0, X1, Y1: Integer;

  procedure PaintVertex(X, Y: Integer);
  begin
    BackBuffer.FillRectS(X - 2, Y - 2, X + 2, Y + 2, clWhite32);
    BackBuffer.FrameRectS(X - 3, Y - 3, X + 3, Y + 3, clBlack32);
  end;

begin
  if PageControl1.ActivePage = TabSheet1 then Exit;

  BackBuffer.SetStipple([clBlack32, clBlack32, clWhite32, clWhite32]);
  BackBuffer.StippleStep := 0.5;
  BackBuffer.StippleCounter := StippleStart;

  X0 := Vertices[3].X;
  Y0 := Vertices[3].Y;
  for I := 0 to 3 do
  begin
    X1 := Vertices[I].X;
    Y1 := Vertices[I].Y;
    BackBuffer.LineFSP(X0, Y0, X1, Y1);
    X0 := X1;
    Y0 := Y1;
  end;
  for I := 0 to 3 do PaintVertex(Vertices[I].X, Vertices[I].Y);
end;

procedure TForm1.ResamplerClassNamesListClick(Sender: TObject);
begin
  with ResamplerClassNamesList do
    if ItemIndex >= 0 then
      Src.Bitmap.ResamplerClassName:= Items[ ItemIndex ];
  DoTransform;
end;

procedure TForm1.SrcRBResizingEvent(Sender: TObject;
  const OldLocation: TFloatRect; var NewLocation: TFloatRect;
  DragState: TDragState; Shift: TShiftState);
begin
  Src.Invalidate;
  DoTransform;
end;

procedure TForm1.ResamplerClassNamesListChange(Sender: TObject);
var
  R: TCustomResampler;
begin
  with ResamplerClassNamesList do
    if ItemIndex >= 0 then
    begin
      Src.Bitmap.BeginUpdate;
      R := TCustomResamplerClass(ResamplerList[ItemIndex]).Create(Src.Bitmap);
      KernelClassNamesListChange(nil);
      Src.Bitmap.EndUpdate;
      Src.Bitmap.Changed;

      KernelClassNamesList.Visible := R is TKernelResampler;
      KernelLabel.Visible := KernelClassNamesList.Visible;
    end;
end;

procedure TForm1.KernelClassNamesListChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := KernelClassNamesList.ItemIndex;
  if Src.Bitmap.Resampler is TKernelResampler then
  begin
    TKernelResampler(Src.Bitmap.Resampler).Kernel := TCustomKernelClass(KernelList[Index]).Create;
  end;
  DoTransform;
end;

procedure TForm1.DstPaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
const            //0..1
  Colors: array [Boolean] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  R: TRect;
  I, J: Integer;
  OddY: Integer;
  TilesHorz, TilesVert: Integer;
  TileX, TileY: Integer;
  TileHeight, TileWidth: Integer;
begin
  if Sender is TImage32 then with TImage32(Sender) do
  begin
    BeginUpdate;
    R := GetViewportRect;
    case PaintStages[StageNum].Parameter of
      1: begin //Draw Checkerboard
           TileHeight := 8;
           TileWidth := 8;

           TilesHorz := (R.Right - R.Left) div TileWidth;
           TilesVert := (R.Bottom - R.Top) div TileHeight;
           TileY := 0;

           for J := 0 to TilesVert do
           begin
             TileX := 0;
             OddY := J and $1;
             for I := 0 to TilesHorz do
             begin
               Buffer.FillRectS(TileX, TileY, TileX + TileWidth, TileY + TileHeight,Colors[I and $1 = OddY]);
               Inc(TileX, TileWidth);
             end;
             Inc(TileY, TileHeight);
           end
         end;
      2: Buffer.FrameRectS(R , $FF000000); //Draw Frame
    end;
    EndUpdate;
  end;
end;

procedure TForm1.cbRepeatClick(Sender: TObject);
const
  AccessMode: array [Boolean] of TPixelAccessMode = (pamSafe, pamWrap);
begin
  Src.Bitmap.WrapMode := wmRepeat;
  Src.Bitmap.Resampler.PixelAccessMode := AccessMode[cbRepeat.Checked];
  DoTransform;
end;

{$IFDEF FPC}
initialization
  {$I MainUnit.lrs}
{$ENDIF}

end.
