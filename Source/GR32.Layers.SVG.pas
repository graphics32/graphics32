unit GR32.Layers.SVG;

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
 * The Original Code is SVG Image Format support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2025-2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  Classes, SysUtils,
  GR32,
  GR32_Layers,
  GR32.SVG.Types,
  GR32.SVG.Tree,
  GR32.SVG.Renderer;

//------------------------------------------------------------------------------
//
//      TSvgLayer
//
//------------------------------------------------------------------------------
// A svg rendering vector layer
//------------------------------------------------------------------------------
type
  TSvgLayer = class(TPositionedLayer)
  private
    FRenderer: TSvgRenderer;
    FSvgNode: TSvgDocumentNode;
    FCachedAdjustedLocation: TFloatRect;
    FCachedUpdateRect: TRect;
    procedure SetSvgNode(Value: TSvgDocumentNode);
  protected
    procedure Paint(Buffer: TBitmap32); override;
    function DoGetUpdateRect: TRect; override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    property Renderer: TSvgRenderer read FRenderer;
    property SvgDocument: TSvgDocumentNode read FSvgNode write SetSvgNode;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  System.Types,
  System.Math,
  GR32_Image;

//------------------------------------------------------------------------------
//
//      TSvgLayer
//
//------------------------------------------------------------------------------
constructor TSvgLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited Create(ALayerCollection);
  FRenderer := TSvgRenderer.Create;
  FSvgNode := nil;
end;

destructor TSvgLayer.Destroy;
begin
  FSvgNode := nil;
  FRenderer.Free;
  inherited Destroy;
end;

procedure TSvgLayer.SetSvgNode(Value: TSvgDocumentNode);
var
  W, H: TFloat;
  Image: TCustomImage32;
begin
  if Value = FSvgNode then
    exit;

  FCachedAdjustedLocation := Default(TFloatRect);

  Changing;

  if (LayerCollection <> nil) and (LayerOptions and LOB_NO_UPDATE = 0) then
    Update;

  FSvgNode := Value;

  if (FSvgNode <> nil) and (Location.IsEmpty) then
  begin
    Image := TCustomImage32(LayerCollection.Owner); // This sucks!

    W := FSvgNode.GetSizePx(FSvgNode.Width, Image.ClientWidth);
    H := FSvgNode.GetSizePx(FSvgNode.Height, Image.ClientHeight);
    if (W > 0) and (H > 0) then
      Location := FloatRect(0, 0, W, H);
  end;

  Changed;
end;

function TSvgLayer.DoGetUpdateRect: TRect;
var
  UpdateRect, RenderedBounds: TFloatRect;
begin
  if (FSvgNode <> nil) then
  begin
    UpdateRect := GetAdjustedLocation;
    
    if (FCachedAdjustedLocation <>  UpdateRect) then
    begin
      FCachedAdjustedLocation := UpdateRect;
      
      RenderedBounds := FRenderer.GetDocumentBounds(FSvgNode, UpdateRect);
      UpdateRect.Union(RenderedBounds);

      Result := MakeRect(UpdateRect, rrOutside);

      // Inflate by 1 pixel to ensure anti-aliased edge pixels on polygon borders
      // are completely encompassed when invalidating layer update rects.
      GR32.InflateRect(RenderedBounds, 1.0, 1.0);
    
      FCachedUpdateRect := Result;
    end else
      Result := FCachedUpdateRect;
  end else
    Result := inherited DoGetUpdateRect;
end;

procedure TSvgLayer.Paint(Buffer: TBitmap32);
var
  TargetRect: TFloatRect;
begin
  if (FSvgNode = nil) or (Buffer = nil) then
    Exit;

  TargetRect := GetAdjustedLocation;

  FRenderer.RenderDocument(Buffer, FSvgNode, TargetRect);
end;

end.
