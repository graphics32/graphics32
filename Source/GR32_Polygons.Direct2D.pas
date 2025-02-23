unit GR32_Polygons.Direct2D;

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
 * The Original Code is Direct2D Polygon Rasterizer for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2025
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}


uses
  Types,

  Direct2D,
  D2D1,

  GR32,
  GR32_Polygons;

//------------------------------------------------------------------------------
//
// TPolygonRenderer32Direct2D
//
//------------------------------------------------------------------------------
// Direct2D polygon rasterizer.
//------------------------------------------------------------------------------
// Note: The Filler property of TPolygonRenderer32 is ignored; Fills are always
// solid.
//------------------------------------------------------------------------------
type
  TPolygonRenderer32Direct2D = class(TPolygonRenderer32, IPolygonRendererBatching)
  private
    FD2DCanvas: TDirect2DCanvas;
    FBatchLevel: integer;

  protected
    procedure SetBitmap(const Value: TCustomBitmap32); override;
    procedure SetColor(const Value: TColor32);  override;

    procedure ApplyColor;
  public
    destructor Destroy; override;

    procedure BeginDraw;
    procedure EndDraw;

    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect); override;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  SysUtils,

  GR32_VectorUtils,
  GR32_Backends;

//------------------------------------------------------------------------------
//
// TPolygonRenderer32Direct2D
//
//------------------------------------------------------------------------------
destructor TPolygonRenderer32Direct2D.Destroy;
begin
  FD2DCanvas.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TPolygonRenderer32Direct2D.BeginDraw;
begin
  Inc(FBatchLevel);

  if (FBatchLevel = 1) and (FD2DCanvas <> nil) then
    FD2DCanvas.BeginDraw;
end;

procedure TPolygonRenderer32Direct2D.EndDraw;
begin
  Dec(FBatchLevel);

  if (FBatchLevel = 0) and (FD2DCanvas <> nil) then
    FD2DCanvas.EndDraw;
end;

//------------------------------------------------------------------------------

procedure TPolygonRenderer32Direct2D.SetBitmap(const Value: TCustomBitmap32);
var
  DeviceContextSupport: IDeviceContextSupport;
begin
  // Bitmap is set for each call to TCanvas32.DrawPath so we need
  // to test for change in value or the recreation of the D2D
  // canvas will kill our performance.
  if (Value = Bitmap) then
    exit;

  if (FD2DCanvas <> nil) then
  begin
    if (FBatchLevel > 0) then
      FD2DCanvas.EndDraw;

    FD2DCanvas.Free;
    FD2DCanvas := nil;
  end;

  inherited;

  if (Bitmap <> nil) and (Supports(Bitmap.Backend, IDeviceContextSupport, DeviceContextSupport)) then
  begin
    FD2DCanvas := TDirect2DCanvas.Create(DeviceContextSupport.Handle, Bitmap.BoundsRect);
    ApplyColor;

    if (FBatchLevel > 0) then
      FD2DCanvas.BeginDraw;
  end;
end;

procedure TPolygonRenderer32Direct2D.ApplyColor;
const
  OneOver255: Single = 1 / 255;
begin
  if (FD2DCanvas = nil) then
    exit;

  FD2DCanvas.Brush.Color := WinColor(Color);
  FD2DCanvas.Brush.Handle.SetOpacity((Color shr 24) * OneOver255);
end;

//------------------------------------------------------------------------------

procedure TPolygonRenderer32Direct2D.SetColor(const Value: TColor32);
begin
  if (Value = Color) then
    exit;

  inherited;
  ApplyColor;
end;

//------------------------------------------------------------------------------

type
  TBitmap32Access = class(TBitmap32);

procedure TPolygonRenderer32Direct2D.PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
const
  d2dFillMode: array[TPolyFillMode] of TD2D1FillMode = (D2D1_FILL_MODE_ALTERNATE, D2D1_FILL_MODE_WINDING);
var
  i: Integer;
{$IFDEF CHANGENOTIFICATIONS}
  ChangeRect: TRect;
{$ENDIF}
  Geometry: ID2D1PathGeometry;
  Sink: ID2D1GeometrySink;
begin
  if (Length(Points) = 0) then
    Exit;

  if (FD2DCanvas = nil) then
    exit;

  if (not Bitmap.MeasuringMode) then
  begin
    // Calling FD2DCanvas.BeginDraw at this low level absolutely kills performance but
    // unfortunately our paint framework doesn't provide any granularity that involves
    // the polygon rasterizer.
    BeginDraw;

    D2DFactory.CreatePathGeometry(Geometry);

    Geometry.Open(Sink);
    Sink.SetFillMode(d2dFillMode[FillMode]);

    for i := 0 to High(Points) do
      if (Length(Points[i]) > 1) then
      begin
        Sink.BeginFigure(TD2D1Point2F(Points[i, 0]), D2D1_FIGURE_BEGIN_FILLED);
        Sink.AddLines(PD2D1Point2F(@Points[i, 1]), Length(Points[i])-1);
        Sink.EndFigure(D2D1_FIGURE_END_CLOSED);
      end;

    Sink.Close;

    FD2DCanvas.RenderTarget.FillGeometry(Geometry, FD2DCanvas.Brush.Handle);

    EndDraw;
  end;

{$IFDEF CHANGENOTIFICATIONS}
  if (TBitmap32Access(Bitmap).LockUpdateCount = 0) and
    ((Bitmap.MeasuringMode) or (TBitmap32Access(Bitmap).UpdateCount = 0)) then
  begin
    for i := 0 to High(Points) do
      if (Length(Points[i]) > 1) then
      begin
        if (GR32.IntersectRect(ChangeRect, MakeRect(ClipRect, rrOutside), MakeRect(PolygonBounds(Points[i])))) then
          Bitmap.Changed(ChangeRect);
      end;
  end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

initialization
  RegisterPolygonRenderer(TPolygonRenderer32Direct2D);

end.
