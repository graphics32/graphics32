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
 * The Original Code is Nested Sampling Example
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Andre Beckedorf <Andre@metaException.de>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  TypInfo, SimplePropEdit, ComCtrls, Menus, ToolWin, ImgList, Buttons, ExtDlgs,
  GR32, GR32_Blend, GR32_Image, GR32_Math, GR32_Rasterizers, GR32_Resamplers,
  GR32_Transforms, GR32_Containers, GR32_ExtImage;
  
type
  TMainForm = class(TForm)
    btnRasterize: TSpeedButton;
    chkClear: TMenuItem;
    chkReset: TMenuItem;
    DisabledImages: TImageList;
    EnabledImages: TImageList;
    HotImages: TImageList;
    ImgView: TImgView32;
    LeftPanel: TPanel;
    lvSamplers: TListView;
    MainMenu: TMainMenu;
    miAdaptiveSuperSampler: TMenuItem;
    miAntialiasing: TMenuItem;
    miBloat: TMenuItem;
    miContour: TMenuItem;
    miContracter: TMenuItem;
    miConvolver: TMenuItem;
    miDilater: TMenuItem;
    miDisturbance: TMenuItem;
    miDraft: TMenuItem;
    miEdit: TMenuItem;
    miEroder: TMenuItem;
    miExit: TMenuItem;
    miExpander: TMenuItem;
    miFile: TMenuItem;
    miFisheye: TMenuItem;
    miJitteredPattern: TMenuItem;
    miKernel: TMenuItem;
    miLanczos: TMenuItem;
    miLinear: TMenuItem;
    miNearest: TMenuItem;
    miOpen: TMenuItem;
    miOptions: TMenuItem;
    miProgressive: TMenuItem;
    miProjective: TMenuItem;
    miRasterizer: TMenuItem;
    miRegular: TMenuItem;
    miResampler: TMenuItem;
    miRGBNoise: TMenuItem;
    miRotation: TMenuItem;
    miSaveImage: TMenuItem;
    miScale: TMenuItem;
    miSelectiveConvolver: TMenuItem;
    miSinsh: TMenuItem;
    miSkew: TMenuItem;
    miSpline: TMenuItem;
    miSupersampler: TMenuItem;
    miSwizzling: TMenuItem;
    miTesseral: TMenuItem;
    miTransformer: TMenuItem;
    miTranslation: TMenuItem;
    miTwirl: TMenuItem;
    N1: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N2: TMenuItem;
    N9: TMenuItem;
    NewItemMenu: TPopupMenu;
    OpenPictureDialog: TOpenPictureDialog;
    Panel1: TPanel;
    SavePictureDialog: TSavePictureDialog;
    Splitter1: TSplitter;
    stObjectInspector: TStaticText;
    stRasterizer: TStaticText;
    stSamplerManager: TStaticText;
    tbCopy: TToolButton;
    tbCut: TToolButton;
    tbDelete: TToolButton;
    tbDown: TToolButton;
    tbManager: TToolBar;
    tbNew: TToolButton;
    tbPaste: TToolButton;
    tbSplitter1: TToolButton;
    tbSplitter2: TToolButton;
    tbUp: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AntialiasClick(Sender: TObject);
    procedure btnRasterizeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Copy2Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure KernelClick(Sender: TObject);
    procedure lvSamplersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure miContourClick(Sender: TObject);
    procedure miEdit2Click(Sender: TObject);
    procedure miEditClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miLinearClick(Sender: TObject);
    procedure miNearestClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miProgressiveClick(Sender: TObject);
    procedure miRegularClick(Sender: TObject);
    procedure miRGBNoiseClick(Sender: TObject);
    procedure miSaveImageClick(Sender: TObject);
    procedure miSwizzlingClick(Sender: TObject);
    procedure miTesseralClick(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure SelectKernel(Sender: TObject);
    procedure tbDeleteClick(Sender: TObject);
    procedure tbUpDownClick(Sender: TObject);
    procedure TransformationClick(Sender: TObject);
  private
    procedure SetSourceResampler(const Value: TCustomResampler);
    function GetSourceResampler: TCustomResampler;
    procedure UpdateTransformations;
    function NewInstanceName(Sampler: TNestedSampler): string;
  public
    Source: TBitmap32;
    Source2: TBitmap32;
    Rasterizer: TRasterizer;
    Samplers: TList;
    PropertyEditor: TSimplePropertyEditor;
    RenderThread: TRenderThread;
    ClearBitmap: Boolean;
    IsRasterizing: Boolean;

    ClipBoardItem: TNestedSampler;
    ObjectName: string;
    property SourceResampler: TCustomResampler read GetSourceResampler write SetSourceResampler;
    procedure ThreadTerminated(Sender: TObject);
    procedure SetResampler(ResamplerClass: TCustomResamplerClass);
    procedure StopThread;
    function LastSampler: TCustomSampler;
    function SelectedSampler: TNestedSampler;
    function SelectedIndex: Integer;
    function ValidSelection: Boolean;
    procedure AddSampler(Sampler: TNestedSampler);
    procedure InsertSampler(Index: Integer; ObjName: string; Sampler: TNestedSampler);
    procedure DeleteSampler(Index: Integer; FreeItem: Boolean = True);
  end;


  { Simple implementation of a nested sampler }
  TNoiseSampler = class(TNestedSampler)
  public
    FRed, FGreen, FBlue: Integer;
    FRedNoise, FGreenNoise, FBlueNoise: Integer;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
  published
    property Red: Integer read FRed write FRed;
    property Green: Integer read FGreen write FGreen;
    property Blue: Integer read FBlue write FBlue;
    property RedNoise: Integer read FRedNoise write FRedNoise;
    property GreenNoise: Integer read FGreenNoise write FGreenNoise;
    property BlueNoise: Integer read FBlueNoise write FBlueNoise;
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math,
{$IFDEF DARWIN}
  MacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG,
{$ELSE}
  LazJPG,
{$ENDIF}
  GR32_OrdinalMaps, GR32_MediaPathLocator, GR32_LowLevel;

procedure SetupToolBar(ToolBar: TToolBar);
var
  I: Integer;
begin
  with ToolBar do
  begin
    ShowCaptions := False;
    for I := 0 to ButtonCount - 1 do
      with Buttons[I] do
        begin
          if MenuItem.Count > 0 then
            Style := tbsDropDown
          else if Caption = '|' then
            Style := tbsSeparator
          else
            Style := tbsButton;
          AutoSize := False;
          Width := 0;
          Height := 0;
        end;
  end;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  MediaPath: TFileName;
begin
  MediaPath := ExpandFileName(GetMediaPath);
  Assert(FileExists(MediaPath + 'stoneweed.jpg'));

  Source := TBitmap32.Create;
  Source.LoadFromFile(MediaPath + 'stoneweed.jpg');
  ImgView.Bitmap.SetSizeFrom(Source);
  Rasterizer := TRegularRasterizer.Create;
  TRegularRasterizer(Rasterizer).UpdateRowCount := 16;
  Rasterizer.Sampler := Source.Resampler;

  Samplers := TList.Create;
  PropertyEditor := TSimplePropertyEditor.Create(Self);
  PropertyEditor.Parent := LeftPanel;
  PropertyEditor.Align := alClient;

  with PropertyEditor do
  begin
    RegisterClassPropertyRange(TNoiseSampler, 'Red', -255, 255);
    RegisterClassPropertyRange(TNoiseSampler, 'Green', -255, 255);
    RegisterClassPropertyRange(TNoiseSampler, 'Blue', -255, 255);

    RegisterClassPropertyRange(TNoiseSampler, 'RedNoise', 0, 255);
    RegisterClassPropertyRange(TNoiseSampler, 'GreenNoise', 0, 255);
    RegisterClassPropertyRange(TNoiseSampler, 'BlueNoise', 0, 255);

    RegisterClassPropertyRange(TSuperSampler, 'SamplingX', 1, 8);
    RegisterClassPropertyRange(TSuperSampler, 'SamplingY', 1, 8);
    RegisterClassPropertyRange(TAdaptiveSuperSampler, 'Level', 0, 8);
    RegisterClassPropertyRange(TAdaptiveSuperSampler, 'Tolerance', 0, 255);

    RegisterClassPropertyRange(TKernelSampler, 'CenterX', 0, 4);
    RegisterClassPropertyRange(TKernelSampler, 'CenterY', 0, 4);

    RegisterClassPropertyRange(TSelectiveConvolver, 'Delta', 0, 255);

    RegisterClassPropertyRange(TTwirlTransformation, 'Twirl', 0, 0.1);
    RegisterClassPropertyRange(TBloatTransformation, 'BloatPower', 0, 1);

    RegisterClassPropertyRange(TWindowedSincKernel, 'Width', 0, 5);
    RegisterClassPropertyRange(TSinshKernel, 'Width', 0, 5);
    RegisterClassPropertyRange(TSinshKernel, 'Coeff', 0, 1);
    RegisterClassPropertyRange(TCubicKernel, 'Coeff', -1.5, 1.5);
  end;

  btnRasterizeClick(nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Samplers.Count - 1 do
    if not Assigned(Samplers[I]) then TCustomSampler(Samplers[I]).Free;
  Samplers.Clear;
  Samplers.Free;
end;

procedure TMainForm.lvSamplersSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected and (Item.Index >= 0) and (Item.Index < Samplers.Count) then
    PropertyEditor.SelectObject(Item.Caption, Samplers[Item.Index]);
end;

procedure TMainForm.SetResampler(ResamplerClass: TCustomResamplerClass);
begin
end;

procedure TMainForm.btnRasterizeClick(Sender: TObject);
begin
  if IsRasterizing then
  begin
    StopThread;
  end
  else
  begin
    miFile.Enabled := False;
    miResampler.Enabled := False;
    miRasterizer.Enabled := False;
    tbManager.Enabled := False;
    btnRasterize.Caption := 'Stop Rasterization';
    with ImgView do
    begin
      StopThread;
      if Assigned(RenderThread) then RenderThread.Free;
      if chkReset.Checked then
        SourceResampler := Source.Resampler
      else
      begin
        Source2 := TBitmap32.Create;
        Source2.Assign(Bitmap);
        SourceResampler := Source2.Resampler;
      end;
      if chkClear.Checked then Bitmap.Clear;
      RenderThread := TRenderThread.Create(Rasterizer, Bitmap, Bitmap.BoundsRect, False);
      RenderThread.OnTerminate := ThreadTerminated;
    end;
    IsRasterizing := True;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  PropertyEditor.SelectObject('Resampler', Source.Resampler);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  PropertyEditor.SelectObject('Rasterizer', Rasterizer);
end;

procedure TMainForm.tbDeleteClick(Sender: TObject);
begin
  if ValidSelection then
    DeleteSampler(SelectedIndex);
end;

procedure TMainForm.miOpenClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    Source.LoadFromFile(OpenPictureDialog.FileName);
    UpdateTransformations;
    ImgView.Bitmap.SetSizeFrom(Source);
    btnRasterizeClick(nil);
  end;
end;

procedure TMainForm.miSaveImageClick(Sender: TObject);
begin
  if SavePictureDialog.Execute then
    Source.SaveToFile(OpenPictureDialog.FileName);
end;

procedure TMainForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miNearestClick(Sender: TObject);
begin
  SourceResampler := TNearestResampler.Create(Source);
end;

procedure TMainForm.miLinearClick(Sender: TObject);
begin
  SourceResampler := TLinearResampler.Create(Source);
end;

procedure TMainForm.SelectKernel(Sender: TObject);
var
  R: TKernelResampler;
const
  KERNELS: array[0..3] of TCustomKernelClass =
    (TCubicKernel, TSplineKernel, TLanczosKernel, TSinshKernel);
begin
  R := TKernelResampler.Create(Source);
  R.Kernel := KERNELS[TComponent(Sender).Tag].Create;
  R.KernelMode := kmTableLinear;
  SourceResampler := R;
end;

procedure TMainForm.StopThread;
begin
  if Assigned(RenderThread) then
  begin
    RenderThread.Terminate;
    RenderThread.WaitFor;
  end;
end;

function TMainForm.LastSampler: TCustomSampler;
begin
  if Samplers.Count > 0 then
    Result := TCustomSampler(Samplers.Last)
  else
    Result := Source.Resampler;
end;

procedure TMainForm.miProgressiveClick(Sender: TObject);
begin
  Rasterizer.Free;
  Rasterizer := TProgressiveRasterizer.Create;
  Rasterizer.Sampler := LastSampler;
end;

procedure TMainForm.miRegularClick(Sender: TObject);
begin
  Rasterizer.Free;
  Rasterizer := TRegularRasterizer.Create;
  TRegularRasterizer(Rasterizer).UpdateRowCount := 16;  
  Rasterizer.Sampler := LastSampler;
end;

procedure TMainForm.miSwizzlingClick(Sender: TObject);
begin
  Rasterizer.Free;
  Rasterizer := TSwizzlingRasterizer.Create;
  Rasterizer.Sampler := LastSampler;
end;

procedure TMainForm.miTesseralClick(Sender: TObject);
begin
  Rasterizer.Free;
  Rasterizer := TTesseralRasterizer.Create;
  Rasterizer.Sampler := LastSampler;
end;

procedure TMainForm.miEditClick(Sender: TObject);
begin
  PropertyEditor.SelectObject('Resampler', Source.Resampler);
end;

procedure TMainForm.miEdit2Click(Sender: TObject);
begin
  PropertyEditor.SelectObject('Rasterizer', Rasterizer);
end;

procedure TMainForm.ThreadTerminated(Sender: TObject);
begin
  if Assigned(Source2) then FreeAndNil(Source2);
  tbManager.Enabled := True;
  miFile.Enabled := True;
  miResampler.Enabled := True;
  miRasterizer.Enabled := True;
  btnRasterize.Caption := 'Rasterize Image';
  IsRasterizing := False;
end;

procedure TMainForm.KernelClick(Sender: TObject);
type
  TKernelSamplerClass = class of TKernelSampler;
const
  Classes: array[0..5] of TKernelSamplerClass =
    (TConvolver, TDilater, TEroder, TExpander, TContracter, TSelectiveConvolver);
var
  Index, I, J: Integer;
  S: TKernelSampler;
begin
  Index := TComponent(Sender).Tag;
  S := Classes[Index].Create(LastSampler);
  S.CenterX := 2;
  S.CenterY := 2;
  S.Kernel.SetSize(5, 5);

  // initialize default kernels
  case Index of
    0: // convolver
      begin
        S.Kernel[2, 2] := 5 * 256;
        S.Kernel[2, 1] := -256;
        S.Kernel[2, 3] := -256;
        S.Kernel[1, 2] := -256;
        S.Kernel[3, 2] := -256;
      end;
    1, 2: // dilater, eroder
      for I := 0 to 4 do
        for J := 0 to 4 do
          S.Kernel[I, J] := Round(-128 + 128 * Sqrt(1/6*Max(6 - Sqr(I - 2) - Sqr(J - 2), 0)));
    3, 4, 5: // expander, contracter
      for I := 0 to 4 do
        for J := 0 to 4 do
          S.Kernel[I, J] := Round(256 * Sqrt(1/6*Max(6 - Sqr(I - 2) - Sqr(J - 2), 0)));
  end;
  AddSampler(S);
end;

procedure TMainForm.TransformationClick(Sender: TObject);
type
  TTransformationClass = class of TTransformation;
const
  Classes: array[0..8] of TTransformationClass =
    (nil, nil, nil, nil, TProjectiveTransformation, TTwirlTransformation,
     TBloatTransformation, TDisturbanceTransformation, TFishEyeTransformation);
var
  S: TTransformer;
  T: TTransformation;
begin
  if Classes[TComponent(Sender).Tag] = nil then Exit;
  T := Classes[TComponent(Sender).Tag].Create;
  T.SrcRect := FloatRect(Source.BoundsRect);
  S := TTransformer.Create(LastSampler, T);
  AddSampler(S);
end;

procedure TMainForm.AntialiasClick(Sender: TObject);
type
  TNestedSamplerClass = class of TNestedSampler;
const
  Classes: array[0..2] of TNestedSamplerClass =
    (TSuperSampler, TAdaptiveSuperSampler, TPatternSampler);
begin
  AddSampler(Classes[TComponent(Sender).Tag].Create(LastSampler));
end;

procedure TMainForm.miContourClick(Sender: TObject);
begin
  Rasterizer.Free;
  Rasterizer := TContourRasterizer.Create;
  Rasterizer.Sampler := LastSampler;
end;

procedure TMainForm.SetSourceResampler(const Value: TCustomResampler);
begin
  if Samplers.Count > 0 then
    TNestedSampler(Samplers[0]).Sampler := Value
  else
    Rasterizer.Sampler := Value;
end;

function TMainForm.GetSourceResampler: TCustomResampler;
begin
  Result := Source.Resampler;
end;

function TMainForm.SelectedSampler: TNestedSampler;
begin
  Result := nil;
  if Assigned(lvSamplers.Selected) then
    Result := TNestedSampler(lvSamplers.Selected.Data);
end;

function TMainForm.SelectedIndex: Integer;
begin
  Result := -1;
  if Assigned(lvSamplers.Selected) then
    Result := lvSamplers.Selected.Index;
end;

function TMainForm.ValidSelection: Boolean;
begin
  Result := Assigned(lvSamplers.Selected);
end;

procedure TMainForm.Copy2Click(Sender: TObject);
begin
  if ValidSelection then
  begin
    ClipBoardItem := SelectedSampler;
    ObjectName := lvSamplers.Selected.Caption;    
  end;
end;

procedure TMainForm.Cut1Click(Sender: TObject);
begin
  if ValidSelection then
  begin
    ClipBoardItem := SelectedSampler;
    ObjectName := lvSamplers.Selected.Caption;
    DeleteSampler(SelectedIndex, False);
  end;
end;

procedure TMainForm.Paste1Click(Sender: TObject);
begin
  if Assigned(ClipBoardItem) then
    if ValidSelection then
      InsertSampler(SelectedIndex, ObjectName, ClipBoardItem)
    else
      InsertSampler(MaxInt, ObjectName, ClipBoardItem);
end;

procedure TMainForm.tbUpDownClick(Sender: TObject);
var
  Index: Integer;
  S: TNestedSampler;
  ObjName: string;
begin
  if ValidSelection then
  begin
    Index := SelectedIndex;
    S := SelectedSampler;
    ObjName := lvSamplers.Selected.Caption;
    DeleteSampler(Index, False);
    InsertSampler(Index + TComponent(Sender).Tag, ObjName, S);
  end;
end;

procedure TMainForm.AddSampler(Sampler: TNestedSampler);
var
  NewItem: TListItem;
begin
  if Sampler is TPatternSampler then
    with Sampler as TPatternSampler do
      Pattern := CreateJitteredPattern(10, 10, 3, 3);
  Samplers.Add(Sampler);
  Rasterizer.Sampler := Sampler;
  NewItem := lvSamplers.Items.Add;
  NewItem.Caption := NewInstanceName(Sampler);
  NewItem.SubItems.Add(Sampler.ClassName);
  NewItem.Data := Sampler;
end;

procedure TMainForm.InsertSampler(Index: Integer; ObjName: string; Sampler: TNestedSampler);
var
  NewItem: TListItem;
begin
  Index := Constrain(Index, 0, lvSamplers.Items.Count);
  if Index = lvSamplers.Items.Count then
  begin
    Samplers.Add(Sampler);
    Sampler.Sampler := LastSampler;
    Rasterizer.Sampler := Sampler;
  end
  else if Index >= 0 then
  begin
    TNestedSampler(Samplers[Index]).Sampler := Sampler;
    if Index = 0 then
      Sampler.Sampler := Source.Resampler
    else
      Sampler.Sampler := Samplers[Index - 1];
    Samplers.Insert(Index, Sampler);
  end;
  NewItem := lvSamplers.Items.Insert(Index);
  NewItem.Caption := ObjName;
  NewItem.SubItems.Add(Sampler.ClassName);
  NewItem.Data := Sampler;
  lvSamplers.Selected := NewItem;
  lvSamplers.Selected.Focused := True;
end;

procedure TMainForm.DeleteSampler(Index: Integer; FreeItem: Boolean);
var
  S: TNestedSampler;
begin
  if Index >= 0 then
  begin
    S := Samplers[Index];
    if Index < Samplers.Count - 1 then
      TNestedSampler(Samplers[Index + 1]).Sampler := S.Sampler
    else
      Rasterizer.Sampler := S.Sampler;
    Samplers.Delete(Index);

    if FreeItem and (Samplers.IndexOf(S) = -1) then
      S.Free;
      
    lvSamplers.Selected.Delete;
    PropertyEditor.SelectObject('', nil);
  end;
end;

procedure TMainForm.miRGBNoiseClick(Sender: TObject);
begin
  AddSampler(TNoiseSampler.Create(LastSampler));
end;

procedure TMainForm.UpdateTransformations;
var
  I: Integer;
  SrcRect: TRect;
begin
  SrcRect := Source.BoundsRect;
  for I := 0 to Samplers.Count - 1 do
    if TPersistent(Samplers[I]) is TTransformer then
      TTransformer(Samplers[I]).Transformation.SrcRect := FloatRect(SrcRect);
end;

{ TNoiseSampler }

function TNoiseSampler.GetSampleFixed(X, Y: TFixed): TColor32;
begin
  Result := Sampler.GetSampleFixed(X, Y);
  with TColor32Entry(Result) do
  begin
    R := Constrain(R + FRed + Random(FRedNoise), 0, 255);
    G := Constrain(G + FGreen + Random(FGreenNoise), 0, 255);
    B := Constrain(B + FBlue + Random(FBlueNoise), 0, 255);
  end;
end;

function TMainForm.NewInstanceName(Sampler: TNestedSampler): string;
var
  S: string;
  I: Integer;
begin
  S := Sampler.ClassName;
  S := Copy(S, 2, Length(S) - 1);
  for I := 1 to MaxInt do
  begin
    Result := S + IntToStr(I);
    if lvSamplers.FindCaption(0, Result, False, True, False) = nil then
      Exit;
  end;
end;

end.
