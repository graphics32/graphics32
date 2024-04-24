unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ComCtrls, GR32_Image, System.Actions,
  Vcl.ActnList,

  GR32,
  GR32_Resamplers;

type
  TFormMain = class(TForm)
    ImageSource: TImgView32;
    ImageDest: TImgView32;
    Panel1: TPanel;
    TrackBarAngle: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    SpinEditWidth: TSpinEdit;
    Label3: TLabel;
    SpinEditHeight: TSpinEdit;
    TimerApply: TTimer;
    CheckBoxUpdate: TCheckBox;
    Button1: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    ActionList1: TActionList;
    ActionApply: TAction;
    Bevel3: TBevel;
    StatusBar: TStatusBar;
    Label4: TLabel;
    ComboBoxResampler: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure SettingChanged(Sender: TObject);
    procedure CheckBoxUpdateClick(Sender: TObject);
    procedure ActionApplyExecute(Sender: TObject);
    procedure ActionApplyUpdate(Sender: TObject);
    procedure TimerApplyTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FNeedUpdate: boolean;
    FLastResized: boolean;
    FLastRotated: boolean;

    procedure Status(const Msg: string);
    procedure QueueUpdate;
    procedure PerformUpdate;
    procedure PerformResize(BitmapSource, BitmapDest: TBitmap32; NewWidth, NewHeight: integer; ResamplerClass: TCustomResamplerClass = nil; KernelClass: TCustomKernelClass = nil);
    procedure PerformRotate(BitmapSource, BitmapDest: TBitmap32; Angle: Single; ResamplerClass: TCustomResamplerClass = nil; KernelClass: TCustomKernelClass = nil);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Diagnostics,
  Math,
  Types, // Inlining
  GR32_Math,
  GR32_Transforms,
  GR32_Rasterizers,
  GR32_Backends_Generic,
  GR32.Examples,
  GR32.ImageFormats.PNG32;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  ImageSource.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder+'\coffee.png');
  (*
  ImageSource.Bitmap.SetSize(3, 3);
  ImageSource.Bitmap.Clear(clBlue32);
  ImageSource.Bitmap.FillRect(1,1, 2,2, clRed32);
  *)

  ImageDest.Bitmap.Assign(ImageSource.Bitmap);

  SpinEditWidth.Value := ImageSource.Bitmap.Width;
  SpinEditHeight.Value := ImageSource.Bitmap.Height;

  for i := 0 to ResamplerList.Count-1 do
    ComboBoxResampler.Items.AddObject(ResamplerList[i].ClassName, TObject(ResamplerList[i]));
  ComboBoxResampler.ItemIndex := 0;

  FNeedUpdate := False;
  Status('');
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  ImageSource.Width := ClientWidth div 2;
end;

procedure TFormMain.ActionApplyExecute(Sender: TObject);
begin
  PerformUpdate;
end;

procedure TFormMain.ActionApplyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FNeedUpdate and (not CheckBoxUpdate.Checked);
end;

procedure TFormMain.SettingChanged(Sender: TObject);
begin
  QueueUpdate;
end;

procedure TFormMain.TimerApplyTimer(Sender: TObject);
begin
  TimerApply.Enabled := False;
  PerformUpdate;
end;

procedure TFormMain.CheckBoxUpdateClick(Sender: TObject);
begin
  TimerApply.Enabled := FNeedUpdate and CheckBoxUpdate.Checked;
end;

procedure TFormMain.Status(const Msg: string);
begin
  StatusBar.SimpleText := Msg;
  Update;
end;

type
  TBitmap32Cracker = class(TBitmap32);

type
  // A backend that allows us to create a bitmap with its own properties but
  // which uses the memory storage from a host bitmap.
  TGhostingBackend = class(TCustomBackend)
  public
    procedure GhostBitmap(ABitmap: TBitmap32);
  end;

procedure TGhostingBackend.GhostBitmap(ABitmap: TBitmap32);
begin
  FOwner.SetSizeFrom(ABitmap);
  TBitmap32Cracker(ABitmap).CopyPropertiesTo(FOwner);
  FBits := ABitmap.Bits;
  Changed;
end;

procedure TFormMain.PerformResize(BitmapSource, BitmapDest: TBitmap32; NewWidth, NewHeight: integer;
  ResamplerClass: TCustomResamplerClass; KernelClass: TCustomKernelClass);
var
  Resampler: TCustomResampler;
  SourceGhost: TBitmap32;
begin
  BitmapDest.SetSize(NewWidth, NewHeight);

  if (ResamplerClass = nil) then
    ResamplerClass := TCustomResamplerClass(BitmapSource.Resampler.ClassType);

  SourceGhost := TBitmap32.Create(TGhostingBackend);
  try
    TGhostingBackend(SourceGhost.Backend).GhostBitmap(BitmapSource);

    Resampler := ResamplerClass.Create(SourceGhost);

    if (Resampler is TKernelResampler) then
    begin
      TKernelResampler(Resampler).KernelMode := kmTableLinear;
      TKernelResampler(Resampler).TableSize := 256;

      if (KernelClass = nil) then
        KernelClass := TCubicKernel;
      TKernelResampler(Resampler).Kernel := KernelClass.Create;

      if (TKernelResampler(Resampler).Kernel is TWindowedSincKernel) then
        TWindowedSincKernel(TKernelResampler(Resampler).Kernel).Width := 4;
    end;

    Resampler.PixelAccessMode := pamTransparentEdge;
    // Note: pamSafe relies on BackgroundColor
    // Resampler.PixelAccessMode := pamSafe;

    StretchTransfer(BitmapDest, BitmapDest.BoundsRect, BitmapDest.BoundsRect, SourceGhost, SourceGhost.BoundsRect, Resampler, dmOpaque, nil);

  finally
    SourceGhost.Free;
  end;
end;

procedure TFormMain.PerformRotate(BitmapSource, BitmapDest: TBitmap32; Angle: Single; ResamplerClass: TCustomResamplerClass; KernelClass: TCustomKernelClass);
var
  Transformation: TAffineTransformation;
  Resampler: TCustomResampler;
  Rasterizer: TRasterizer;
  CombineInfo: TCombineInfo;
  Transformer: TTransformer;
  TransformedBounds: TFloatRect;
  TransformedFloatWidth, TransformedFloatHeight: Single;
  TransformedWidth, TransformedHeight: integer;
  SourceGhost: TBitmap32;
begin
  if (Abs(Frac(Angle / 360)) < 0.1/360) then
  begin
    TBitmap32Cracker(BitmapSource).CopyMapTo(BitmapDest);
    exit;
  end;

  SourceGhost := TBitmap32.Create(TGhostingBackend);
  try
    TGhostingBackend(SourceGhost.Backend).GhostBitmap(BitmapSource);

    Transformation := TAffineTransformation.Create;
    try
      Transformation.Clear;
      Transformation.SrcRect := FloatRect(0, 0, SourceGhost.Width, SourceGhost.Height);

      // Move origin so we will be rotating around center of bitmap
      Transformation.Translate(-SourceGhost.Width * 0.5, -SourceGhost.Height * 0.5);

      // Rotate
      Transformation.Rotate(0, 0, Angle);

      TransformedBounds := Transformation.GetTransformedBounds;

      // Size destination to fit transformed bitmap
      TransformedFloatWidth := TransformedBounds.Right-TransformedBounds.Left;
      TransformedWidth := Ceil(TransformedFloatWidth-0.00001);
      TransformedFloatHeight := TransformedBounds.Bottom-TransformedBounds.Top;
      TransformedHeight := Ceil(TransformedFloatHeight-0.00001);

      // Center in destination bitmap
      Transformation.Translate(-TransformedBounds.Left + (TransformedWidth-TransformedFloatWidth) * 0.5, -TransformedBounds.Top + (TransformedHeight-TransformedFloatHeight) * 0.5);

      if (ResamplerClass = nil) then
        ResamplerClass := TCustomResamplerClass(SourceGhost.Resampler.ClassType);

      Resampler := ResamplerClass.Create(SourceGhost);

      if (Resampler is TKernelResampler) then
      begin
        TKernelResampler(Resampler).KernelMode := kmTableLinear;
        TKernelResampler(Resampler).TableSize := 256;

        if (KernelClass = nil) then
          KernelClass := TCubicKernel;
        TKernelResampler(Resampler).Kernel := KernelClass.Create;

        if (TKernelResampler(Resampler).Kernel is TWindowedSincKernel) then
          TWindowedSincKernel(TKernelResampler(Resampler).Kernel).Width := 4;
      end;

      Resampler.PixelAccessMode := pamTransparentEdge;
      // Note: pamSafe relies on BackgroundColor
      // Resampler.PixelAccessMode := pamSafe;

      Transformer := TTransformer.Create(Resampler, Transformation);
      try

        // Rasterizer := DefaultRasterizerClass.Create;
        Rasterizer := TMultithreadedRegularRasterizer.Create;
        try
          Rasterizer.Sampler := Transformer;

          // We use CombineInfo so BufferSource's MasterAlpha isn't used in the rasterization.
          CombineInfo.SrcAlpha := 255;
          CombineInfo.DrawMode := dmOpaque;
          // cmMerge minimizes blend artifacts: rotate pure color rectangle on transparent background. Rotated edges does not retain original color.
          CombineInfo.CombineMode := cmMerge;
          CombineInfo.CombineCallBack := nil;
          CombineInfo.TransparentColor := 0;

          BitmapDest.BeginUpdate;
          try

            BitmapDest.SetSize(TransformedWidth, TransformedHeight);
            BitmapDest.Clear(0);
            Rasterizer.Rasterize(BitmapDest, BitmapDest.BoundsRect, CombineInfo);

          finally
            BitmapDest.EndUpdate;
          end;
        finally
          Rasterizer.Free;
        end;
      finally
        Transformer.Free;
      end;
    finally
      Transformation.Free;
    end;
  finally
    SourceGhost.Free;
  end;
end;

procedure TFormMain.PerformUpdate;
var
  ResamplerClass: TCustomResamplerClass;
  NeedResize, NeedRotate: boolean;
  StopWatch: TStopWatch;
  BitmapTemp: TBitmap32;
  BitmapRotateSource: TBitmap32;
  BitmapResizeDest: TBitmap32;
begin
  Cursor := crHourGlass;

  if (ComboBoxResampler.ItemIndex <> -1) then
    ResamplerClass := TCustomResamplerClass(ComboBoxResampler.Items.Objects[ComboBoxResampler.ItemIndex])
  else
    ResamplerClass := TCustomResamplerClass(ImageSource.Bitmap.Resampler.ClassType);

  NeedResize := (SpinEditWidth.Value <> ImageSource.Bitmap.Width) or (SpinEditHeight.Value <> ImageSource.Bitmap.height);
  NeedRotate := (TrackBarAngle.Position <> 0);

  if NeedResize and NeedRotate then
  begin
    BitmapTemp := TBitmap32.Create(TMemoryBackend);
    BitmapResizeDest := BitmapTemp;
    BitmapRotateSource := BitmapTemp;
  end else
  begin
    BitmapTemp := nil;
    BitmapResizeDest := ImageDest.Bitmap;
    BitmapRotateSource := ImageSource.Bitmap;
  end;
  try

    StopWatch := TStopWatch.StartNew;

    if (NeedResize) or (NeedRotate) then
    begin
      if NeedResize then
      begin
        Status('Resizing...');
        PerformResize(ImageSource.Bitmap, BitmapResizeDest, SpinEditWidth.Value, SpinEditHeight.Value, ResamplerClass);
      end;

      if NeedRotate then
      begin
        Status('Rotating...');
        PerformRotate(BitmapRotateSource, ImageDest.Bitmap, TrackBarAngle.Position, ResamplerClass);
      end;

    end else
    if (FLastResized or FLastRotated) then
      TBitmap32Cracker(ImageSource.Bitmap).CopyMapTo(ImageDest.Bitmap);

    StopWatch.Stop;

  finally
    BitmapTemp.Free;
  end;

  Status(Format('Completed in %.0n mS', [StopWatch.ElapsedMilliseconds * 1.0]));
  Cursor := crDefault;
  FNeedUpdate := False;
  FLastResized := NeedResize;
  FLastRotated := NeedRotate;
end;

procedure TFormMain.QueueUpdate;
begin
  FNeedUpdate := True;
  TimerApply.Enabled := False;

  if (CheckBoxUpdate.Checked) then
  begin
    TimerApply.Enabled := True;
    Status('Update queued...');
  end else
    Status('Update pending; Press Apply.');
end;

end.
