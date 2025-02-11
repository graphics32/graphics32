unit GR32_Polygons.Blend2D;

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
 * The Original Code is Blend2D Polygon Rasterizer for Graphics32
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

  // Requires DelphiBlend2D
  // https://github.com/neslib/DelphiBlend2D
  Blend2D,

  GR32,
  GR32_Polygons;

//------------------------------------------------------------------------------
//
// TPolygonRenderer32Blend2D
//
//------------------------------------------------------------------------------
// Blend2D polygon rasterizer.
//------------------------------------------------------------------------------
// Note: The Filler property of TPolygonRenderer32 is ignored; Fills are always
// solid.
//------------------------------------------------------------------------------
type
//  TPolygonRenderer32Blend2D = class(TPolygonRenderer32)
  TPolygonRenderer32Blend2D = class(TPolygonRenderer32, IPolygonRendererBatching)
  private
    FImage: IBLImage;
    FContext: IBLContext;
    FBatchLevel: integer;
    FInnerBatchLevel: integer;

  private
    procedure InnerBeginDraw;
    procedure InnerEndDraw;

  protected
    procedure SetBitmap(const Value: TCustomBitmap32); override;
    procedure SetColor(const Value: TColor32); override;
    procedure SetFillMode(const Value: TPolyFillMode); override;

  public
    constructor Create; override;
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
// TPolygonRenderer32Blend2D
//
//------------------------------------------------------------------------------
constructor TPolygonRenderer32Blend2D.Create;
begin
  inherited Create;
  FContext := TBLContext.Create;
end;

destructor TPolygonRenderer32Blend2D.Destroy;
begin
  FImage := nil;
  FContext := nil;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TPolygonRenderer32Blend2D.BeginDraw;
begin
  Inc(FBatchLevel);

  if (FBatchLevel = 1) and (FImage <> nil) then
    FContext.Start(FImage);
end;

procedure TPolygonRenderer32Blend2D.EndDraw;
begin
  Dec(FBatchLevel);

  if (FBatchLevel = 0) and (FImage <> nil) then
    FContext.Finish;
end;

//------------------------------------------------------------------------------

procedure TPolygonRenderer32Blend2D.InnerBeginDraw;
const
  b2dFillMode: array[TPolyFillMode] of TBLFillRule = (TBLFillRule.EvenOdd, TBLFillRule.NonZero);
begin
  BeginDraw;

  Inc(FInnerBatchLevel);

  if (FInnerBatchLevel = 1) then
  begin
    FContext.FillColor := Color;
    FContext.FillRule := b2dFillMode[FillMode];
  end;
end;

procedure TPolygonRenderer32Blend2D.InnerEndDraw;
begin
  Dec(FInnerBatchLevel);
  EndDraw;
end;

//------------------------------------------------------------------------------

procedure TPolygonRenderer32Blend2D.SetBitmap(const Value: TCustomBitmap32);
begin
  if (Value = Bitmap) then
    exit;

  Assert(FInnerBatchLevel = 0, 'Cannot alter state inside InnerBeginDraw/InnerEndDraw');

  if (FImage <> nil) then
  begin
    if (FBatchLevel > 0) then
      FContext.Finish;

    FImage := nil;
  end;

  inherited;

  if (Bitmap <> nil) then
  begin
    FImage := TBLImage.Create;
    FImage.InitializeFromData(Bitmap.Width, Bitmap.Height, TBLFormat.PRGB32, Bitmap.Bits, Bitmap.Width*SizeOf(TColor32));

    if (FBatchLevel > 0) then
      FContext.Start(FImage);
  end;
end;

//------------------------------------------------------------------------------

procedure TPolygonRenderer32Blend2D.SetColor(const Value: TColor32);
begin
  if (Value = Color) then
    exit;

  Assert(FInnerBatchLevel = 0, 'Cannot alter state inside InnerBeginDraw/InnerEndDraw');

  inherited;
end;

procedure TPolygonRenderer32Blend2D.SetFillMode(const Value: TPolyFillMode);
begin
  if (Value = FillMode) then
    exit;

  Assert(FInnerBatchLevel = 0, 'Cannot alter state inside InnerBeginDraw/InnerEndDraw');

  inherited;
end;

//------------------------------------------------------------------------------

type
  TBitmap32Access = class(TBitmap32);

procedure TPolygonRenderer32Blend2D.PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
var
  i, j: Integer;
  Path: IBLPath;
{$IFDEF CHANGENOTIFICATIONS}
  ChangeRect: TRect;
{$ENDIF}
begin
  if (Length(Points) = 0) then
    Exit;

  if (FImage = nil) then
    exit;

  if (not Bitmap.MeasuringMode) then
  begin
    InnerBeginDraw;

    Path := TBLPath.Create;

    for j := 0 to High(Points) do
      if (Length(Points[j]) > 1) then
      begin
        Path.MoveTo(Points[j, 0].X, Points[j, 0].Y);
        for i := 1 to High(Points[j]) do
          Path.LineTo(Points[j, i].X, Points[j, i].Y);
        Path.Close;
      end;

    FContext.FillPath(Path);

    Path := nil;

    InnerEndDraw;
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
  RegisterPolygonRenderer(TPolygonRenderer32Blend2D);

end.
