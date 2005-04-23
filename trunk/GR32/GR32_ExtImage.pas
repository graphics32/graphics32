unit GR32_ExtImage;

interface

uses
  GR32, GR32_Image, GR32_Rasterizers, Classes, Controls, Messages, Windows;

type
  TRenderThread = class;

  { TSyntheticImage32 }
  TSyntheticImage32 = class(TPaintBox32)
  private
    FRasterizer: TRasterizer;
    FAutoRasterize: Boolean;
    FDefaultProc: TWndMethod;
    FResized: Boolean;
    FRenderThread: TRenderThread;
    FOldAreaChanged: TAreaChangedEvent;
    procedure SetRasterizer(const Value: TRasterizer);
    procedure StopRenderThread;
  protected
    procedure RasterizerChanged(Sender: TObject);
    procedure SetParent(AParent: TWinControl); override;
    procedure FormWindowProc(var Message: TMessage);
    procedure DoRasterize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
  published
    property AutoRasterize: Boolean read FAutoRasterize write FAutoRasterize;
    property Rasterizer: TRasterizer read FRasterizer write SetRasterizer;
    property Buffer;
  end;

  TRenderThread = class(TThread)
  private
    FBitmap: TBitmap32;
    FRasterizer: TRasterizer;
    FOldAreaChanged: TAreaChangedEvent;
    FArea: TRect;
    FBoundsRect: TRect;
    procedure SynchronizedAreaChanged;
    procedure AreaChanged(Sender: TObject; const Area: TRect; const Hint: Cardinal);
  protected
    procedure Execute; override;
    procedure Rasterize;
  public
    constructor Create(Rasterizer: TRasterizer; Bitmap: TBitmap32; BoundsRect: TRect);
  end;

implementation

uses
  Forms, SysUtils;
  
{ TSyntheticImage32 }

constructor TSyntheticImage32.Create(AOwner: TComponent);
begin
  inherited;
  FRasterizer := TRegularRasterizer.Create;
  FRasterizer.Sampler := Buffer.Resampler;
  FAutoRasterize := True;
  FResized := False;
end;

destructor TSyntheticImage32.Destroy;
var
  ParentForm: TCustomForm;
begin
  FRasterizer.Free;
  if Assigned(FDefaultProc) then
  begin
    ParentForm := GetParentForm(Self);
    if ParentForm <> nil then
      ParentForm.WindowProc := FDefaultProc;
  end;
  inherited;
end;

procedure TSyntheticImage32.DoRasterize;
begin
  //if FAutoRasterize then
  begin
    { clear buffer before rasterization }
    Buffer.Clear(clBlack32);
    Invalidate;

    { create rendering thread }
    StopRenderThread;
    FOldAreaChanged := Buffer.OnAreaChanged;
    FRenderThread := TRenderThread.Create(FRasterizer, Buffer, BoundsRect);
    FResized := True;
  end;
end;

procedure TSyntheticImage32.FormWindowProc(var Message: TMessage);
var
  M: TMessage;
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

procedure TSyntheticImage32.RasterizerChanged(Sender: TObject);
begin
  DoRasterize;
end;

procedure TSyntheticImage32.Resize;
begin
  StopRenderThread;
  inherited;
end;

procedure TSyntheticImage32.SetParent(AParent: TWinControl);
var
  ParentForm: TCustomForm;
begin
  ParentForm := GetParentForm(Self);
  if ParentForm = AParent then Exit;
  if ParentForm <> nil then
  begin
    if Assigned(FDefaultProc) then
      ParentForm.WindowProc := FDefaultProc;
  end;
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

procedure TSyntheticImage32.StopRenderThread;
begin
  if Assigned(FRenderThread) and (not FRenderThread.Terminated) then
  begin
    FRenderThread.Terminate;
    FRenderThread.WaitFor;
    FRenderThread.Free;
  end;
end;

{ TRenderThread }

constructor TRenderThread.Create(Rasterizer: TRasterizer; Bitmap: TBitmap32;
  BoundsRect: TRect);
begin
  inherited Create(False);
  FRasterizer := Rasterizer;
  FBitmap := Bitmap;
  FBoundsRect := BoundsRect;
  Priority := tpNormal;
end;

procedure TRenderThread.Execute;
begin
  Rasterize;
end;

procedure TRenderThread.Rasterize;
begin
  FRasterizer.Lock;
  FBitmap.Lock;
  FOldAreaChanged := FBitmap.OnAreaChanged;
  FBitmap.OnAreaChanged := AreaChanged;
  try
    FRasterizer.Rasterize(FBitmap, FBoundsRect);
  except
    on EAbort do;
  end;
  FBitmap.OnAreaChanged := FOldAreaChanged;
  FBitmap.Unlock;
  FRasterizer.Unlock;
end;

procedure TRenderThread.AreaChanged(Sender: TObject; const Area: TRect;
  const Hint: Cardinal);
begin
  if Terminated then Abort else
  begin
    FArea := Area;
    FBitmap.Unlock;
    Synchronize(SynchronizedAreaChanged);
    FBitmap.Lock;
  end;
end;

procedure TRenderThread.SynchronizedAreaChanged;
begin
  FOldAreaChanged(FBitmap, FArea, 0);
end;

end.
