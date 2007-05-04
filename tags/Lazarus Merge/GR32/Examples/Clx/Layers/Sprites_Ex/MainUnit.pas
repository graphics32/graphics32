{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

unit MainUnit;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is Sprites Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Andre Beckedorf - metaException OHG
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  {$IFNDEF CLX}QWindows,{$ENDIF}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs, GR32, GR32_Transforms,
  QStdCtrls, AppEvnts, GR32_Image, GR32_Layers, QExtCtrls, GR32_Containers,
  GR32_MicroTiles, Math, QButtons;

const
  MAX_RUNS = 3;

type
  TForm1 = class(TForm)
    Image32: TImage32;
    bAdd: TButton;
    edLayerCount: TEdit;
    bClearAll: TButton;
    BitmapList: TBitmap32List;
    Label1: TLabel;
    cbUseRepaintOpt: TCheckBox;
    bRemove: TButton;
    Memo1: TMemo;
    lbFPS: TLabel;
    TimerFPS: TTimer;
    lbDimension: TLabel;
    bBenchmark: TButton;
    procedure bRemoveClick(Sender: TObject);
    procedure cbUseRepaintOptClick(Sender: TObject);
    procedure bClearAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerFPSTimer(Sender: TObject);
    procedure Image32Resize(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bBenchmarkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    Velocities: TArrayOfFloatPoint;
    FramesDrawn: Integer;
    LastCheck: Cardinal;
    LastSeed: Integer;
    PriorityClass, Priority: Integer;

    BenchmarkMode: Boolean;
    BenchmarkRun: Cardinal;
    BenchmarkList: TStringList;

    procedure IdleHandler(Sender: TObject; var Done: Boolean);
    procedure AddLayers(Count: Integer);    
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

uses
  GR32_Filters, GR32_System, JPEG;

procedure TForm1.FormCreate(Sender: TObject);

  procedure LoadImage(Dst: TBitmap32; const Filename, AlphaFilename: String);
  var
    TempBitmap: TBitmap32;
  begin
    TempBitmap := TBitmap32.Create;
    try
      Dst.LoadFromFile(Filename);
      TempBitmap.LoadFromFile(AlphaFilename);
      IntensityToAlpha(Dst, TempBitmap);
    finally
      TempBitmap.Free;
    end;
  end;

begin
  Image32.Bitmap.LoadFromFile('..\..\..\Media\sprite_texture.bmp');

  LoadImage(BitmapList.Bitmap[0], '..\..\..\Media\sprite1.bmp', '..\..\..\Media\sprite1a.bmp');
  LoadImage(BitmapList.Bitmap[1], '..\..\..\Media\sprite2.bmp', '..\..\..\Media\sprite2a.bmp');
  LoadImage(BitmapList.Bitmap[2], '..\..\..\Media\sprite3.bmp', '..\..\..\Media\sprite3a.bmp');

  LastSeed := 0;
  BenchmarkList := TStringList.Create;
  Application.OnIdle := IdleHandler;
end;

procedure TForm1.AddLayers(Count: Integer);
var
  X: Integer;
  ALayer: TBitmapLayer;
  L: TFloatRect;
  I: Integer;
begin
  TimerFPS.Enabled := False;

  // make sure, we're creating reproducible randoms...
  RandSeed := LastSeed;

  Image32.BeginUpdate;
  for X := 1 to Count do
  begin
    // create a new layer...
    ALayer := TBitmapLayer.Create(Image32.Layers);
    with ALayer do
    begin
      Bitmap := BitmapList.Bitmaps[Random(BitmapList.Bitmaps.Count)].Bitmap;
      Bitmap.DrawMode := dmBlend;
      Bitmap.MasterAlpha := Random(255);

      // put it somethere
      L.Left := Random(Image32.Width);
      L.Top := Random(Image32.Height);
      L.Right := L.Left + Bitmap.Width;
      L.Bottom := L.Top + Bitmap.Height;
      ALayer.Location := L;

      I := Length(Velocities);
      SetLength(Velocities, I + 1);
      Velocities[I] := FloatPoint(Random - 0.5, Random - 0.5);
    end;
  end;
  Image32.EndUpdate;
  Image32.Changed;
  edLayerCount.Text := IntToStr(Image32.Layers.Count) + ' layers';

  // save current seed, so we can continue at this seed later...
  LastSeed := RandSeed;

  FramesDrawn := 0;
  TimerFPS.Enabled := True;
end;

procedure TForm1.IdleHandler(Sender: TObject; var Done: Boolean);
var
  I: Integer;
  R: TFloatRect;
begin
  if Image32.Layers.Count = 0 then Exit;
  Image32.BeginUpdate;
  for I := 0 to Image32.Layers.Count - 1 do
  begin
    with TBitmapLayer(Image32.Layers[I]) do
    begin
      Bitmap.MasterAlpha := (Bitmap.MasterAlpha + 1) mod 256;
      R := Location;
      with Velocities[I] do
      begin
        OffsetRect(R, X, Y);
        X := X + (Random - 0.5) * 0.9;
        Y := Y + (Random - 0.5) * 0.9;
        if (R.Left < 0) and (X < 0) then X := 1;
        if (R.Top < 0) and (Y < 0) then Y := 1;
        if (R.Right > Image32.Width) and (X > 0) then X := -1;
        if (R.Bottom > Image32.Height) and (Y > 0) then Y := -1;
      end;
      Location := R;
    end;
  end;
  Image32.EndUpdate;
  Image32.Invalidate;
  // because we're doing Invalidate in the IdleHandler and Invalidate has
  // higher priority, we can count the frames here, because we can be sure that
  // the deferred repaint is triggered once this method is exited.
  Inc(FramesDrawn);
end;

procedure TForm1.bClearAllClick(Sender: TObject);
begin
  Image32.Layers.Clear;
  Velocities := nil;
  edLayerCount.Text := '0 layers';
end;

procedure TForm1.bRemoveClick(Sender: TObject);
var
  I: Integer;
begin
  for I := Image32.Layers.Count - 1 downto Max(0, Image32.Layers.Count - 10) do
    Image32.Layers.Delete(I);
  edLayerCount.Text := IntToStr(Image32.Layers.Count) + ' layers';
end;

procedure TForm1.cbUseRepaintOptClick(Sender: TObject);
begin
  if cbUseRepaintOpt.Checked then
    Image32.RepaintMode := rmOptimizer
  else
    Image32.RepaintMode := rmFull;
end;

procedure TForm1.TimerFPSTimer(Sender: TObject);
var
  TimeElapsed: Cardinal;
  Diff: Integer;
  FPS: Single;
begin
  TimerFPS.Enabled := False;
  TimeElapsed := GetTickCount - LastCheck;

  FPS := FramesDrawn / (TimeElapsed / 1000);
  lbFPS.Caption := Format('%.2f fps', [FPS]);

  if BenchmarkMode then
  begin
    BenchmarkList.Add(Format('%d ' + #9 + '%.2f', [Image32.Layers.Count, FPS]));

    Diff := 0;  // stop complaining, ye my evil compiler!

    if Image32.Layers.Count = 10 then
      Diff := 4
    else if Image32.Layers.Count = 14 then
      Diff := 6
    else if Image32.Layers.Count < 100 then
      Diff := 10
    else if Image32.Layers.Count = 100 then
      Diff := 40
    else if Image32.Layers.Count = 140 then
      Diff := 60
    else if Image32.Layers.Count < 1000 then
      Diff := 100
    else if Image32.Layers.Count < 2000 then
      Diff := 500
    else if Image32.Layers.Count >= 2000 then
    begin
      bBenchmarkClick(nil);
      Exit;
    end;

    AddLayers(Diff);
  end;

  FramesDrawn := 0;
  LastCheck := GetTickCount;
  TimerFPS.Enabled := True;  
end;

procedure TForm1.Image32Resize(Sender: TObject);
begin
  lbDimension.Caption := IntToStr(Image32.Width) + ' x ' + IntToStr(Image32.Height); 
end;

procedure TForm1.bAddClick(Sender: TObject);
begin
  AddLayers(10);
end;

procedure TForm1.bBenchmarkClick(Sender: TObject);
begin
  if BenchmarkMode then
  begin
    {$IFNDEF CLX}
    SetThreadPriority(GetCurrentThread, Priority);
    SetPriorityClass(GetCurrentProcess, PriorityClass);
    {$ENDIF}

    bBenchmark.Caption := 'Benchmark';

    cbUseRepaintOpt.Enabled := True;
    bAdd.Enabled := True;
    bRemove.Enabled := True;
    bClearAll.Enabled := True;

    BenchmarkMode := False;
    TimerFPS.Interval := 5000;
    BenchmarkList.SaveToFile('Results.txt');
  end
  else if (MessageDlg('Do you really want to start benchmarking? ' +
    'This will take a considerable amount of time.' + #13#10 +
    'Benchmarking runs with a higher task priority. Your system might become unresponsive for several seconds.',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    {$IFNDEF CLX}
    PriorityClass := GetPriorityClass(GetCurrentProcess);
    Priority := GetThreadPriority(GetCurrentThread);

    SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
    SetThreadPriority(GetCurrentThread,
                      THREAD_PRIORITY_TIME_CRITICAL);
    {$ENDIF}

    bBenchmark.Caption := 'Stop';

    cbUseRepaintOpt.Enabled := False;
    bAdd.Enabled := False;
    bRemove.Enabled := False;
    bClearAll.Enabled := False;

    BenchmarkMode := True;
    BenchmarkList.Clear;
    bClearAllClick(nil);
    AddLayers(10);
    LastCheck := GetTickCount;    
    TimerFPS.Interval := MAX_RUNS * 5000;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  BenchmarkList.Free;
end;

initialization
  DecimalSeparator := '.';

end.
