unit GR32_ExtImage;

interface

{$I GR32.INC}

uses
  GR32, GR32_Image, GR32_Rasterizers, Classes, Controls, Messages, Windows;

type
  {$IFNDEF CLX}
  TRenderThread = class;

  TRenderMode = (rnmFull, rnmConstrained);

  { TSyntheticImage32 }
  TSyntheticImage32 = class(TPaintBox32)
  private
    FRasterizer: TRasterizer;
    FAutoRasterize: Boolean;
    FDefaultProc: TWndMethod;
    FResized: Boolean;
    FRenderThread: TRenderThread;
    FOldAreaChanged: TAreaChangedEvent;
    FBitmapAlign: TBitmapAlign;
    FDstRect: TRect;
    FRenderMode: TRenderMode;
    procedure SetRasterizer(const Value: TRasterizer);
    procedure StopRenderThread;
    procedure SetBitmapAlign(const Value: TBitmapAlign);
    procedure SetDstRect(const Value: TRect);
    procedure SetRenderMode(const Value: TRenderMode);
  protected
    procedure RasterizerChanged(Sender: TObject);
    procedure SetParent(AParent: TWinControl); override;
    procedure FormWindowProc(var Message: TMessage);
    procedure DoRasterize;
    property RepaintMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure Rasterize;
  published
    property AutoRasterize: Boolean read FAutoRasterize write FAutoRasterize;
    property Rasterizer: TRasterizer read FRasterizer write SetRasterizer;
    property Buffer;
    property BitmapAlign: TBitmapAlign read FBitmapAlign write SetBitmapAlign;
    property RenderMode: TRenderMode read FRenderMode write SetRenderMode;
    property DstRect: TRect read FDstRect write SetDstRect;
  end;
  {$ENDIF}

  { TRenderThread }
  TRenderThread = class(TThread)
  private
    FBitmap: TBitmap32;
    FRasterizer: TRasterizer;
    FOldAreaChanged: TAreaChangedEvent;
    FArea: TRect;
    FDstRect: TRect;
    procedure SynchronizedAreaChanged;
    procedure AreaChanged(Sender: TObject; const Area: TRect; const Hint: Cardinal);
  protected
    procedure Execute; override;
    procedure Rasterize;
  public
    constructor Create(Rasterizer: TRasterizer; Bitmap: TBitmap32; DstRect: TRect; Suspended: Boolean);
  end;

procedure Rasterize(Rasterizer: TRasterizer; Bitmap: TBitmap32; DstRect: TRect);

implementation

uses
  Forms, SysUtils;

procedure Rasterize(Rasterizer: TRasterizer; Bitmap: TBitmap32; DstRect: TRect);
var
  R: TRenderThread;
begin
  R := TRenderThread.Create(Rasterizer, Bitmap, DstRect, True);
  R.FreeOnTerminate := True;
  R.Resume;
end;

{ TSyntheticImage32 }

{$IFNDEF CLX}
constructor TSyntheticImage32.Create(AOwner: TComponent);
begin
  inherited;
  FRasterizer := TRegularRasterizer.Create;
  FRasterizer.Sampler := Buffer.Resampler;
  FAutoRasterize := True;
  FResized := False;
  RepaintMode := rmDirect;
  RenderMode := rnmFull;
  BufferOversize := 0;
end;

destructor TSyntheticImage32.Destroy;
var
  ParentForm: TCustomForm;
begin
  if Assigned(FDefaultProc) then
  begin
    ParentForm := GetParentForm(Self);
    if ParentForm <> nil then
      ParentForm.WindowProc := FDefaultProc;
  end;
  StopRenderThread;
  if Assigned(FRenderThread) then FRenderThread.Free;
  FRasterizer.Free;
  inherited;
end;

procedure TSyntheticImage32.DoRasterize;
begin
  if FAutoRasterize then Rasterize;
end;

procedure TSyntheticImage32.FormWindowProc(var Message: TMessage);
var
  CmdType: Integer;
begin
  FDefaultProc(Message);
  case Message.Msg of
    WM_MOVING: FResized := False;
    WM_EXITSIZEMOVE:
      begin
        if FResized then
        begin
          DoRasterize;
        end;
        FResized := True;
      end;
    WM_SYSCOMMAND:
      begin
        CmdType := Message.WParam and $FFF0;
        if (CmdType = SC_MAXIMIZE) or (CmdType = SC_RESTORE) then
          DoRasterize;
      end;
  end;
end;

procedure TSyntheticImage32.Rasterize;
var
  R: TRect;
begin
  { clear buffer before rasterization }
  Buffer.Clear(clBlack32);
  Invalidate;

  { create rendering thread }
  StopRenderThread;
  FOldAreaChanged := Buffer.OnAreaChanged;
  if FRenderMode = rnmFull then
    R := Rect(0, 0, Width, Height)
  else
    R := FDstRect;

  if Assigned(FRenderThread) then FreeAndNil(FRenderThread);

  FRenderThread := TRenderThread.Create(FRasterizer, Buffer, R, False);
  FResized := True;
end;

procedure TSyntheticImage32.RasterizerChanged(Sender: TObject);
begin
  DoRasterize;
end;

procedure TSyntheticImage32.Resize;
begin
  StopRenderThread;
  inherited;
end;

procedure TSyntheticImage32.SetBitmapAlign(const Value: TBitmapAlign);
begin
  FBitmapAlign := Value;
end;

procedure TSyntheticImage32.SetDstRect(const Value: TRect);
begin
  FDstRect := Value;
end;

procedure TSyntheticImage32.SetParent(AParent: TWinControl);
var
  ParentForm: TCustomForm;
begin
  ParentForm := GetParentForm(Self);
  if ParentForm = AParent then Exit;
  if ParentForm <> nil then
    if Assigned(FDefaultProc) then
      ParentForm.WindowProc := FDefaultProc;
  inherited;
  if AParent <> nil then
  begin
    ParentForm := GetParentForm(Self);
    if ParentForm <> nil then
    begin
      FDefaultProc := ParentForm.WindowProc;
      ParentForm.WindowProc := FormWindowProc;
    end;
  end;
end;

procedure TSyntheticImage32.SetRasterizer(const Value: TRasterizer);
begin
  if Value <> FRasterizer then
  begin
    StopRenderThread;
    if Assigned(FRasterizer) then FRasterizer.Free;
    FRasterizer := Value;
    FRasterizer.OnChange := RasterizerChanged;
    DoRasterize;
    Changed;
  end;
end;

procedure TSyntheticImage32.SetRenderMode(const Value: TRenderMode);
begin
  FRenderMode := Value;
end;

procedure TSyntheticImage32.StopRenderThread;
begin
  if Assigned(FRenderThread) and (not FRenderThread.Terminated) then
  begin
    FRenderThread.Synchronize(FRenderThread.Terminate);
    FRenderThread.WaitFor;
    FreeAndNil(FRenderThread);
  end;
end;
{$ENDIF}

{ TRenderThread }

constructor TRenderThread.Create(Rasterizer: TRasterizer; Bitmap: TBitmap32;
  DstRect: TRect; Suspended: Boolean);
begin
  inherited Create(Suspended);
  FRasterizer := Rasterizer;
  FBitmap := Bitmap;
  FDstRect := DstRect;
  Priority := tpNormal;
end;

procedure TRenderThread.Execute;
begin
  Rasterize;
end;

procedure TRenderThread.Rasterize;
begin
  FRasterizer.Lock;

  // save current AreaChanged handler
  FOldAreaChanged := FBitmap.OnAreaChanged;

  FBitmap.OnAreaChanged := AreaChanged;
  try
    FRasterizer.Rasterize(FBitmap, FDstRect);
  except
    on EAbort do;
  end;

  // reset old AreaChanged handler
  FBitmap.OnAreaChanged := FOldAreaChanged;

  Synchronize(FRasterizer.Unlock);
end;

procedure TRenderThread.AreaChanged(Sender: TObject; const Area: TRect;
  const Hint: Cardinal);
begin
  if Terminated then Abort else
  begin
    FArea := Area;
    Synchronize(SynchronizedAreaChanged);
  end;
end;

procedure TRenderThread.SynchronizedAreaChanged;
begin
  FOldAreaChanged(FBitmap, FArea, AREAINFO_RECT);
end;

end.
