unit GR32_ExtImage;

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
 * The Original Code is Extended Image components for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  GR32, GR32_Image, GR32_Rasterizers, Classes, Controls;

type
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
    FDstRect: TRect;
    FRenderMode: TRenderMode;
    FClearBuffer: Boolean;
    procedure SetRasterizer(const Value: TRasterizer);
    procedure StopRenderThread;
    procedure SetDstRect(const Value: TRect);
    procedure SetRenderMode(const Value: TRenderMode);
  protected
    procedure RasterizerChanged(Sender: TObject);
    procedure SetParent(AParent: TWinControl); override;
    {$IFDEF FPC}
    procedure FormWindowProc(var Message: TLMessage);
    {$ELSE}
    procedure FormWindowProc(var Message: TMessage);
    {$ENDIF}
    procedure DoRasterize;
    property RepaintMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure Rasterize;
    property DstRect: TRect read FDstRect write SetDstRect;
  published
    property AutoRasterize: Boolean read FAutoRasterize write FAutoRasterize;
    property Rasterizer: TRasterizer read FRasterizer write SetRasterizer;
    property Buffer;
    property Color;
    property ClearBuffer: Boolean read FClearBuffer write FClearBuffer;
    property RenderMode: TRenderMode read FRenderMode write SetRenderMode;
  end;

  { TRenderThread }
  TRenderThread = class(TThread)
  private
    FDest: TBitmap32;
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
    constructor Create(Rasterizer: TRasterizer; Dst: TBitmap32; DstRect: TRect;
      Suspended: Boolean);
  end;

procedure Rasterize(Rasterizer: TRasterizer; Dst: TBitmap32; DstRect: TRect);

implementation

uses
  Forms, SysUtils;

procedure Rasterize(Rasterizer: TRasterizer; Dst: TBitmap32; DstRect: TRect);
var
  R: TRenderThread;
begin
  R := TRenderThread.Create(Rasterizer, Dst, DstRect, True);
  R.FreeOnTerminate := True;
{$IFDEF COMPILER2010}
  R.Start;
{$ELSE}
  R.Resume;
{$ENDIF}
end;

{ TSyntheticImage32 }

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
  StopRenderThread;
  if Assigned(FRenderThread) then FRenderThread.Free;
  if Assigned(FDefaultProc) then
  begin
    ParentForm := GetParentForm(Self);
    if ParentForm <> nil then
      ParentForm.WindowProc := FDefaultProc;
  end;
  FRasterizer.Free;
  inherited;
end;

procedure TSyntheticImage32.DoRasterize;
begin
  if FAutoRasterize then Rasterize;
end;

{$IFDEF FPC}
procedure TSyntheticImage32.FormWindowProc(var Message: TLMessage);
var
  CmdType: Integer;
begin
  FDefaultProc(Message);
  case Message.Msg of
    534: FResized := False;
    562:
      begin
        if FResized then DoRasterize;
        FResized := True;
      end;
    274:
      begin
        CmdType := Message.WParam and $FFF0;
        if (CmdType = SC_MAXIMIZE) or (CmdType = SC_RESTORE) then
          DoRasterize;
      end;
  end;
end;
{$ELSE}
procedure TSyntheticImage32.FormWindowProc(var Message: TMessage);
var
  CmdType: Integer;
begin
  FDefaultProc(Message);
  case Message.Msg of
    WM_MOVING: FResized := False;
    WM_EXITSIZEMOVE:
      begin
        if FResized then DoRasterize;
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
{$ENDIF}

procedure TSyntheticImage32.Rasterize;
var
  R: TRect;
begin
  { Clear buffer before rasterization }
  if FClearBuffer then
  begin
    Buffer.Clear(Color32(Color));
    Invalidate;
  end;

  { Create rendering thread }
  StopRenderThread;
  FOldAreaChanged := Buffer.OnAreaChanged;
  if FRenderMode = rnmFull then
    R := Rect(0, 0, Buffer.Width, Buffer.Height)
  else
    R := FDstRect;

  FRenderThread := TRenderThread.Create(FRasterizer, Buffer, R, False);
  FResized := True;
end;

procedure TSyntheticImage32.RasterizerChanged(Sender: TObject);
begin
  DoRasterize;
end;

procedure TSyntheticImage32.Resize;
begin
  if not FResized then StopRenderThread;
  inherited;
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

{ TRenderThread }

constructor TRenderThread.Create(Rasterizer: TRasterizer; Dst: TBitmap32;
  DstRect: TRect; Suspended: Boolean);
begin
{$IFDEF COMPILER2010}
  inherited Create(Suspended);
{$ELSE}
  inherited Create(True);
{$ENDIF}
  FRasterizer := Rasterizer;
  FDest := Dst;
  FDstRect := DstRect;
  Priority := tpNormal;
{$IFNDEF COMPILER2010}
  if not Suspended then Resume;
{$ENDIF}
end;

procedure TRenderThread.Execute;
begin
  Rasterize;
end;

procedure TRenderThread.Rasterize;
begin
  FRasterizer.Lock;

  { Save current AreaChanged handler }
  FOldAreaChanged := FDest.OnAreaChanged;

  FDest.OnAreaChanged := AreaChanged;
  try
    FRasterizer.Rasterize(FDest, FDstRect);
  except
    on EAbort do;
  end;

  { Reset old AreaChanged handler }
  FDest.OnAreaChanged := FOldAreaChanged;

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
  if Assigned(FOldAreaChanged) then
    FOldAreaChanged(FDest, FArea, AREAINFO_RECT);
end;

end.
