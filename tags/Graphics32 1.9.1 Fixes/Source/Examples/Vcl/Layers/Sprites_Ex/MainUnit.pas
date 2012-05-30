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

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, Variants, {$ENDIF}
  {$IFNDEF FPC} AppEvnts, {$ENDIF}
  {$IFDEF Windows}Windows,{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, GR32, GR32_Transforms,
  StdCtrls, GR32_Image, GR32_Layers, ExtCtrls, GR32_Containers,
  GR32_MicroTiles, Math, Buttons;

const
  MAX_RUNS = 3;

type
  TMainForm = class(TForm)
    bAdd: TButton;
    bBenchmark: TButton;
    bClearAll: TButton;
    BitmapList: TBitmap32List;
    bRemove: TButton;
    cbUseRepaintOpt: TCheckBox;
    edLayerCount: TEdit;
    Image32: TImage32;
    lbDimension: TLabel;
    lbFPS: TLabel;
    lbTotal: TLabel;
    Memo1: TMemo;
    TimerFPS: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bBenchmarkClick(Sender: TObject);
    procedure bClearAllClick(Sender: TObject);
    procedure bRemoveClick(Sender: TObject);
    procedure cbUseRepaintOptClick(Sender: TObject);
    procedure Image32Resize(Sender: TObject);
    procedure TimerFPSTimer(Sender: TObject);
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
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  GR32_MediaPathLocator,
{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG,
{$ELSE}
  LazJPG,
{$ENDIF}
  GR32_Filters, GR32_System;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);

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

var
  MediaPath: TFileName;
begin
  MediaPath := ExpandFileName(GetMediaPath);

  Assert(FileExists(MediaPath + 'sprite_texture.bmp'));
  Image32.Bitmap.LoadFromFile(MediaPath + 'sprite_texture.bmp');

  Assert(FileExists(MediaPath + 'sprite1.bmp'));
  Assert(FileExists(MediaPath + 'sprite1a.bmp'));
  LoadImage(BitmapList.Bitmap[0], MediaPath + 'sprite1.bmp', MediaPath + 'sprite1a.bmp');

  Assert(FileExists(MediaPath + 'sprite2.bmp'));
  Assert(FileExists(MediaPath + 'sprite2a.bmp'));
  LoadImage(BitmapList.Bitmap[1], MediaPath + 'sprite2.bmp', MediaPath + 'sprite2a.bmp');

  Assert(FileExists(MediaPath + 'sprite3.bmp'));
  Assert(FileExists(MediaPath + 'sprite3a.bmp'));
  LoadImage(BitmapList.Bitmap[2], MediaPath + 'sprite3.bmp', MediaPath + 'sprite3a.bmp');

  LastSeed := 0;
  BenchmarkList := TStringList.Create;
  Application.OnIdle := IdleHandler;
end;

procedure TMainForm.AddLayers(Count: Integer);
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
      Bitmap := BitmapList.Bitmaps[System.Random(BitmapList.Bitmaps.Count)].Bitmap;
      Bitmap.DrawMode := dmBlend;
      Bitmap.MasterAlpha := System.Random(255);

      // put it somethere
      L.Left := System.Random(Image32.Width);
      L.Top := System.Random(Image32.Height);
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

procedure TMainForm.IdleHandler(Sender: TObject; var Done: Boolean);
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
        GR32.OffsetRect(R, X, Y);
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

procedure TMainForm.bClearAllClick(Sender: TObject);
begin
  Image32.Layers.Clear;
  Velocities := nil;
  edLayerCount.Text := '0 layers';
end;

procedure TMainForm.bRemoveClick(Sender: TObject);
var
  I: Integer;
begin
  for I := Image32.Layers.Count - 1 downto Max(0, Image32.Layers.Count - 10) do
    Image32.Layers.Delete(I);
  edLayerCount.Text := IntToStr(Image32.Layers.Count) + ' layers';
end;

procedure TMainForm.cbUseRepaintOptClick(Sender: TObject);
begin
  if cbUseRepaintOpt.Checked then
    Image32.RepaintMode := rmOptimizer
  else
    Image32.RepaintMode := rmFull;
end;

procedure TMainForm.TimerFPSTimer(Sender: TObject);
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

procedure TMainForm.Image32Resize(Sender: TObject);
begin
  lbDimension.Caption := IntToStr(Image32.Width) + ' x ' + IntToStr(Image32.Height); 
end;

procedure TMainForm.bAddClick(Sender: TObject);
begin
  AddLayers(10);
end;

procedure TMainForm.bBenchmarkClick(Sender: TObject);
begin
  if BenchmarkMode then
  begin
    {$IFNDEF FPC}
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
    {$IFNDEF FPC}
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

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  BenchmarkList.Free;
end;

initialization
  DecimalSeparator := '.';

end.
