unit GR32_Polygons.GDIPlus;

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
 * The Original Code is GDI+ Polygon Rasterizer for Graphics32
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

  GDIPAPI,
  GDIPOBJ,

  GR32,
  GR32_Polygons;

//------------------------------------------------------------------------------
//
// TCustomPolygonRenderer32GDIPlus
//
//------------------------------------------------------------------------------
// GDI+ polygon rasterizer base class.
//------------------------------------------------------------------------------
// Note: The Filler property of TPolygonRenderer32 is ignored; Fills are always
// solid.
//------------------------------------------------------------------------------
type
  TGDIPlusQuality = (gdipQualityLow, gdipQualityHigh);

  TCustomPolygonRenderer32GDIPlus = class(TPolygonRenderer32)
  private
    FGPGraphics: TGPGraphics;
    FPath: TGPGraphicsPath;
    FBrush: TGPSolidBrush;
    FQuality: TGDIPlusQuality;

  protected
    procedure SetBitmap(const Value: TCustomBitmap32); override;
    procedure SetColor(const Value: TColor32);  override;

    procedure ApplyQualitySetting;
    procedure SetQuality(const Value: TGDIPlusQuality);

    property Quality: TGDIPlusQuality read FQuality write SetQuality;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect); override;
  end;


//------------------------------------------------------------------------------
//
// TPolygonRenderer32GDIPlus
//
//------------------------------------------------------------------------------
// GDI+ polygon rasterizer. Quality can be set through the Quality property.
// This rasterizer isn't registered in the global rasterizer registry.
//------------------------------------------------------------------------------
type
  TPolygonRenderer32GDIPlus = class(TCustomPolygonRenderer32GDIPlus)
  public
    property Quality;
  end;


//------------------------------------------------------------------------------
//
// TPolygonRenderer32GDIPlusLowQuality
//
//------------------------------------------------------------------------------
// Fast GDI+ polygon rasterizer.
// Does not perform anti-aliasing.
// "Fast" means faster the the slow GDI+ rasterizer.
//------------------------------------------------------------------------------
type
  TPolygonRenderer32GDIPlusLowQuality = class(TCustomPolygonRenderer32GDIPlus)
  public
    constructor Create; override;
  end;


//------------------------------------------------------------------------------
//
// TPolygonRenderer32GDIPlusHighQuality
//
//------------------------------------------------------------------------------
// High quality GDI+ polygon rasterizer.
// Performs anti-aliasing (16 levels I think).
// Slower than the "fast" GDI+ rasterizer.
//------------------------------------------------------------------------------
type
  TPolygonRenderer32GDIPlusHighQuality = class(TCustomPolygonRenderer32GDIPlus)
  public
    constructor Create; override;
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
// TCustomPolygonRenderer32GDIPlus
//
//------------------------------------------------------------------------------
constructor TCustomPolygonRenderer32GDIPlus.Create;
begin
  inherited Create;

  FPath := TGPGraphicsPath.Create;
  FBrush := TGPSolidBrush.Create(Color);
end;

destructor TCustomPolygonRenderer32GDIPlus.Destroy;
begin
  FGPGraphics.Free;
  FPath.Free;
  FBrush.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomPolygonRenderer32GDIPlus.SetBitmap(const Value: TCustomBitmap32);
var
  DeviceContextSupport: IDeviceContextSupport;
begin
  FGPGraphics.Free;
  FGPGraphics := nil;

  inherited;

  if (Bitmap <> nil) and (Supports(Bitmap.Backend, IDeviceContextSupport, DeviceContextSupport)) then
  begin
    FGPGraphics := TGPGraphics.Create(DeviceContextSupport.Handle);
    ApplyQualitySetting;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomPolygonRenderer32GDIPlus.ApplyQualitySetting;
begin
  if (FGPGraphics = nil) then
    exit;

  case FQuality of
    gdipQualityLow:
      FGPGraphics.SetSmoothingMode(SmoothingModeHighSpeed);

    gdipQualityHigh:
      FGPGraphics.SetSmoothingMode(SmoothingModeHighQuality);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomPolygonRenderer32GDIPlus.SetColor(const Value: TColor32);
begin
  inherited;

  FBrush.SetColor(Color);
end;

//------------------------------------------------------------------------------

procedure TCustomPolygonRenderer32GDIPlus.SetQuality(const Value: TGDIPlusQuality);
begin
  FQuality := Value;
  ApplyQualitySetting;
end;

//------------------------------------------------------------------------------

type
  TBitmap32Access = class(TBitmap32);

procedure TCustomPolygonRenderer32GDIPlus.PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
const
  gdipFillMode: array[TPolyFillMode] of GDIPAPI.TFillMode = (FillModeAlternate, FillModeWinding);
var
  i, j: Integer;
{$IFDEF CHANGENOTIFICATIONS}
  ChangeRect: TRect;
{$ENDIF}
begin
  if (Length(Points) = 0) then
    Exit;

  if (FGPGraphics = nil) then
    exit;

  if (not Bitmap.MeasuringMode) then
  begin

    // FPath.Reset clears the fillmode (to Alternate), so we have to apply it every time
    FPath.SetFillMode(gdipFillMode[FillMode]);

    for j := 0 to High(Points) do
      if (Length(Points[j]) > 0) then
        FPath.AddPolygon(PGPPointF(@Points[j, 0]), Length(Points[j]));

    FGPGraphics.FillPath(FBrush, FPath);

    FPath.Reset;

  end;

{$IFDEF CHANGENOTIFICATIONS}
  if (TBitmap32Access(Bitmap).LockUpdateCount = 0) and
    ((Bitmap.MeasuringMode) or (TBitmap32Access(Bitmap).UpdateCount = 0)) then
  begin
    for i := 0 to High(Points) do
      if (Length(Points[i]) > 0) then
      begin
        if (GR32.IntersectRect(ChangeRect, MakeRect(ClipRect, rrOutside), MakeRect(PolygonBounds(Points[i])))) then
          Bitmap.Changed(ChangeRect);
      end;
  end;
{$ENDIF}
end;


//------------------------------------------------------------------------------
//
// TPolygonRenderer32GDIPlusLowQuality
//
//------------------------------------------------------------------------------
constructor TPolygonRenderer32GDIPlusLowQuality.Create;
begin
  inherited;
  SetQuality(gdipQualityLow);
end;


//------------------------------------------------------------------------------
//
// TPolygonRenderer32GDIPlusHighQuality
//
//------------------------------------------------------------------------------
constructor TPolygonRenderer32GDIPlusHighQuality.Create;
begin
  inherited;
  SetQuality(gdipQualityHigh);
end;

//------------------------------------------------------------------------------

initialization
  RegisterPolygonRenderer(TPolygonRenderer32GDIPlusLowQuality);
  RegisterPolygonRenderer(TPolygonRenderer32GDIPlusHighQuality);

end.
