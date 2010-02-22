unit GR32_RepaintOpt;

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
 * The Original Code is Repaint Optimizer Extension for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Andre Beckedorf - metaException
 * Andre@metaException.de
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
  LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
  Classes, SysUtils, GR32, GR32_LowLevel, GR32_Containers, GR32_Layers;

type
  { TCustomRepaintOptimizer }
  TCustomRepaintOptimizer = class
  private
    FEnabled: Boolean;
    FLayerCollections: TList;
    FInvalidRects: TRectList;
    FBuffer: TBitmap32;
  protected
    function GetEnabled: Boolean; virtual;
    procedure SetEnabled(const Value: Boolean); virtual;
    property LayerCollections: TList read FLayerCollections write FLayerCollections;
    property Buffer: TBitmap32 read FBuffer write FBuffer;
    property InvalidRects: TRectList read FInvalidRects write FInvalidRects;

    // LayerCollection handler
    procedure LayerCollectionNotifyHandler(Sender: TLayerCollection;
      Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer); virtual; abstract;
  public
    constructor Create(Buffer: TBitmap32; InvalidRects: TRectList); virtual;
    destructor Destroy; override;

    procedure RegisterLayerCollection(Layers: TLayerCollection); virtual;
    procedure UnregisterLayerCollection(Layers: TLayerCollection); virtual;

    procedure BeginPaint; virtual;
    procedure EndPaint; virtual;
    procedure BeginPaintBuffer; virtual;
    procedure EndPaintBuffer; virtual;

    procedure Reset; virtual; abstract;
    function  UpdatesAvailable: Boolean; virtual; abstract;
    procedure PerformOptimization; virtual; abstract;

    // handlers
    procedure AreaUpdateHandler(Sender: TObject; const Area: TRect; const Info: Cardinal); virtual; abstract;
    procedure LayerUpdateHandler(Sender: TObject; Layer: TCustomLayer); virtual; abstract;
    procedure BufferResizedHandler(const NewWidth, NewHeight: Integer); virtual; abstract;

    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  TCustomRepaintOptimizerClass = class of TCustomRepaintOptimizer;

// differs from InflateRect in the way that it does also handle negative rects
procedure InflateArea(var Area: TRect; Dx, Dy: Integer);

implementation

procedure InflateArea(var Area: TRect; Dx, Dy: Integer);
begin
  if Area.Left > Area.Right then
    Dx := -Dx;

  if Area.Top > Area.Bottom then
    Dy := -Dy;

  Dec(Area.Left, Dx); Dec(Area.Top, Dy);
  Inc(Area.Right, Dx); Inc(Area.Bottom, Dy);
end;

type
  TLayerCollectionAccess = class(TLayerCollection);

{ TCustomRepaintManager }

constructor TCustomRepaintOptimizer.Create(Buffer: TBitmap32; InvalidRects: TRectList);
begin
  FLayerCollections := TList.Create;
  FInvalidRects := InvalidRects;
  FBuffer := Buffer;
end;

destructor TCustomRepaintOptimizer.Destroy;
var
  I: Integer;
begin
  for I := 0 to FLayerCollections.Count - 1 do
    UnregisterLayerCollection(TLayerCollection(FLayerCollections[I]));

  FLayerCollections.Free;
  inherited;
end;

function TCustomRepaintOptimizer.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TCustomRepaintOptimizer.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TCustomRepaintOptimizer.RegisterLayerCollection(Layers: TLayerCollection);
begin
  if FLayerCollections.IndexOf(Layers) = -1 then
  begin
    FLayerCollections.Add(Layers);
    TLayerCollectionAccess(Layers).OnListNotify := LayerCollectionNotifyHandler;
  end;
end;

procedure TCustomRepaintOptimizer.UnregisterLayerCollection(Layers: TLayerCollection);
begin
  TLayerCollectionAccess(Layers).OnListNotify := nil;
  FLayerCollections.Remove(Layers);
end;

procedure TCustomRepaintOptimizer.BeginPaint;
begin
  // do nothing by default
end;

procedure TCustomRepaintOptimizer.EndPaint;
begin
  // do nothing by default
end;

procedure TCustomRepaintOptimizer.BeginPaintBuffer;
begin
  // do nothing by default
end;

procedure TCustomRepaintOptimizer.EndPaintBuffer;
begin
  // do nothing by default
end;

end.
