unit GR32_Polygons.GDI;

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
 * The Original Code is GDI Polygon Rasterizer for Graphics32
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
  Graphics,

  GR32,
  GR32_Polygons;

//------------------------------------------------------------------------------
//
// TPolygonRenderer32GDI
//
//------------------------------------------------------------------------------
// GDI polygon rasterizer.
//------------------------------------------------------------------------------
// Note: The Filler property of TPolygonRenderer32 is ignored; Fills are always
// solid.
// Semi-transparency is not supported; All fills are opaque.
//------------------------------------------------------------------------------
type
  TPolygonRenderer32GDI = class(TPolygonRenderer32)
  private
    FCanvas: TCanvas;

  protected
    procedure SetBitmap(const Value: TCustomBitmap32); override;
    procedure SetColor(const Value: TColor32);  override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect); override;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  SysUtils,
  UITypes,
  Windows,

  GR32_VectorUtils,
  GR32_Backends;

//------------------------------------------------------------------------------
//
// TPolygonRenderer32GDI
//
//------------------------------------------------------------------------------
constructor TPolygonRenderer32GDI.Create;
begin
  inherited Create;

  FCanvas := TCanvas.Create;
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Pen.Style := psClear;
end;

destructor TPolygonRenderer32GDI.Destroy;
begin
  FCanvas.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TPolygonRenderer32GDI.SetBitmap(const Value: TCustomBitmap32);
var
  DeviceContextSupport: IDeviceContextSupport;
begin
  if (Value = Bitmap) then
    exit;

  FCanvas.Handle := 0;

  inherited;

  if (Bitmap <> nil) and (Supports(Bitmap.Backend, IDeviceContextSupport, DeviceContextSupport)) then
    FCanvas.Handle := DeviceContextSupport.Handle;
end;

//------------------------------------------------------------------------------

procedure TPolygonRenderer32GDI.SetColor(const Value: TColor32);
begin
  if (TColor(Value and $00FFFFFF) = FCanvas.Brush.Color) then
    exit;

  inherited;

  FCanvas.Brush.Color := Value and $00FFFFFF;
end;

//------------------------------------------------------------------------------

type
  TBitmap32Access = class(TBitmap32);

procedure TPolygonRenderer32GDI.PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
const
  GdiFillMode: array[TPolyFillMode] of integer = (ALTERNATE, WINDING);
var
  i, j: Integer;
{$IFDEF CHANGENOTIFICATIONS}
  ChangeRect: TRect;
{$ENDIF}
  PolyPoints: array of tagPoint;
  PolyCount: array of longint;
  n: integer;
  Counts: integer;
begin
  if (Length(Points) = 0) then
    Exit;

  if (FCanvas = nil) then
    exit;

  if (not Bitmap.MeasuringMode) then
  begin

    n := 0;
    Counts := 0;
    for j := 0 to High(Points) do
    begin
      if (Length(Points[j]) = 0) then
        continue;

      Inc(Counts);
      Inc(n, Length(Points[j]));
    end;

    if (n = 0) then
      exit;

    SetLength(PolyPoints, n);
    SetLength(PolyCount, Counts);

    n := 0;
    Counts := 0;
    for j := 0 to High(Points) do
    begin
      if (Length(Points[j]) = 0) then
        continue;

      PolyCount[Counts] := Length(Points[j]);
      Inc(Counts);

      for i := 0 to High(Points[j]) do
      begin
        PolyPoints[n] := GR32.Point(Points[j, i]);
        Inc(n);
      end;
    end;

    SetPolyFillMode(FCanvas.Handle, GdiFillMode[FillMode]);

    if (not Windows.PolyPolygon(FCanvas.Handle, PolyPoints[0], PolyCount[0], Counts)) then
      RaiseLastOSError;

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
  RegisterPolygonRenderer(TPolygonRenderer32GDI);

end.
