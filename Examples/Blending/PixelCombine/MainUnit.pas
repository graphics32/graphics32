unit MainUnit;

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
 * The Original Code is PixelCombine Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  {$IFNDEF FPC} Windows, {$ELSE} LCLIntf, LCLType, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  Generics.Collections,
  GR32,
  GR32_Blend,
  GR32.Blend.Modes,
  GR32_Image,
  GR32_Layers;

type
  TFormPixelCombine = class(TForm)
    ImgView: TImgView32;
    Splitter1: TSplitter;
    PanelOptions: TPanel;
    ListViewOperation: TListView;
    PanelLayer: TPanel;
    ComboBoxLayer: TComboBox;
    Label1: TLabel;
    TrackBarAlpha: TTrackBar;
    ButtonLayerAdd: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListViewOperationItemChecked(Sender: TObject; Item: TListItem);
    procedure ListViewOperationSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ComboBoxLayerSelect(Sender: TObject);
    procedure ImgViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure TrackBarAlphaChange(Sender: TObject);
    procedure ButtonLayerAddClick(Sender: TObject);
  protected
  private
    FRubberbandLayer: TRubberbandLayer;
    FBlenders: TObjectDictionary<TBitmapLayer, TCustomGraphics32Blender>;
    FCurrentItem: TListItem;
    FPatternCount: integer;

    function AddLayer(const Name: string; DrawMode: TDrawMode): TBitmapLayer;

    function GetCurrentLayer: TBitmapLayer;
    procedure SetCurrentLayer(const Value: TBitmapLayer);

    property CurrentLayer: TBitmapLayer read GetCurrentLayer write SetCurrentLayer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormPixelCombine: TFormPixelCombine;

implementation

{$R *.dfm}

uses
  Types,
  Math,
  GR32_RangeBars,
  GR32.ImageFormats.JPG,
  GR32_VectorUtils,
  GR32_Polygons,
  GR32.Blend.Modes.PhotoShop,
  GR32.Blend.Modes.PorterDuff;

const
  BitmapSize = 200;
  BitmapOffset = 20;

{ TFormPixelCombine }

constructor TFormPixelCombine.Create(AOwner: TComponent);
begin
  inherited;
  FBlenders := TObjectDictionary<TBitmapLayer, TCustomGraphics32Blender>.Create([doOwnsValues]);
end;

destructor TFormPixelCombine.Destroy;
begin
  FBlenders.Free;
  inherited;
end;

function TFormPixelCombine.AddLayer(const Name: string; DrawMode: TDrawMode): TBitmapLayer;
begin
  // Add new layer but ensure rubberband remains the last layer
  if (FRubberbandLayer <> nil) then
    Result := TBitmapLayer(ImgView.Layers.Insert(FRubberbandLayer.Index, TBitmapLayer))
  else
    Result := TBitmapLayer.Create(ImgView.Layers);

  Result.Visible := False;
  Result.Bitmap.DrawMode := DrawMode;
  Result.Bitmap.CombineMode := cmMerge;

  ComboBoxLayer.Items.AddObject(Name, Result);

  FBlenders.Add(Result, nil);

  Result.Visible := True;
end;

procedure TFormPixelCombine.ButtonLayerAddClick(Sender: TObject);

  procedure GenerateBitmap(Bitmap: TBitmap32);
  var
    X, Y: Integer;
    SinY, SinX: Double;
    Color: TColor32;
  begin
    Assert(Bitmap.Height <= 255); // Prevent DivMul255Table overflow
    Assert(Bitmap.Width <= 255);

    // Just a pattern with some variation
    for Y := 0 to Bitmap.Height-1 do
    begin
      SinY := Sin(Y * 0.1);

      for X := 0 to Bitmap.Width-1 do
      begin
        SinX := Sin(X * 0.1);

        // 2D sine wave pattern
        Color :=  Gray32(Round(((SinX + SinY) * 0.25 + 0.5) * 255));
        // Alpha gradient
        Color := SetAlpha(Color, DivMul255Table[Bitmap.Height-1, Y]); // MulDiv(255, Y, Bitmap.Height-1)

        Bitmap[X, Y] := Color;
      end;
    end;
  end;

var
  Layer: TBitmapLayer;
  r: TRect;
  Viewport: TRect;
begin
  Inc(FPatternCount);
  Layer := AddLayer(Format('Pattern %d', [FPatternCount]), dmCustom);
  Layer.Scaled := True;
  Layer.Bitmap.SetSize(BitmapSize, BitmapSize);

  GenerateBitmap(Layer.Bitmap);

  // Random position within viewport
  r := Layer.Bitmap.BoundsRect;
  Viewport := ImgView.GetViewportRect;
  r.Offset(Random(Viewport.Width - Layer.Bitmap.Width), Random(Viewport.Height - Layer.Bitmap.Height));
  Layer.Location := ImgView.ControlToBitmap(r);

  CurrentLayer := Layer;
end;

procedure TFormPixelCombine.ComboBoxLayerSelect(Sender: TObject);
begin
  if (ComboBoxLayer.ItemIndex <> -1) then
    CurrentLayer := TBitmapLayer(ComboBoxLayer.Items.Objects[ComboBoxLayer.ItemIndex])
  else
    CurrentLayer := nil;
end;

procedure TFormPixelCombine.FormCreate(Sender: TObject);

  procedure DrawCircle(ABitmap: TBitmap32; Alpha: integer = 255);
  var
    Shape: TArrayOfFloatPoint;
  begin
    ABitmap.SetSize(BitmapSize, BitmapSize, True);
    Shape := Circle(ABitmap.Width / 2, ABitmap.Height / 2, Min(ABitmap.Width, ABitmap.Height) / 2 - 4);
    PolygonFS(ABitmap, Shape, SetAlpha(clOrange32, Alpha), pfWinding);
    PolylineFS(ABitmap, Shape, clYellow32, True, 4);
  end;

  procedure DrawBox(ABitmap: TBitmap32; Alpha: integer = 255);
  var
    r: TRect;
    Shape: TArrayOfFloatPoint;
  begin
    ABitmap.SetSize(BitmapSize, BitmapSize, True);
    r := ABitmap.BoundsRect;
    GR32.InflateRect(r, -10, -10);
    Shape := GR32_VectorUtils.Rectangle(r);
    PolygonFS(ABitmap, Shape, SetAlpha(clLightSkyblue32, Alpha), pfWinding);
    PolylineFS(ABitmap, Shape, clBlue32, True, 4);
  end;

var
  Layer: TBitmapLayer;
  r: TRect;
  Location: TFloatRect;
  Group: IGraphics32BlendGroup;
  ListGroup: TListGroup;
  Item: TListItem;
  BlenderClass: TGraphics32BlenderClass;
begin
  // Note: Layer.Scaled makes no difference in this concrete example since
  // we have no bitmap (in ImgView.Bitmap) to scale/position relative to.

  (*
  ** Create unscaled background bitmap layer
  *)
  Layer := AddLayer('(background)', dmOpaque);
  Layer.Scaled := False;
  Layer.Bitmap.LoadFromResourceName(HInstance, 'Runner', RT_RCDATA);
  Layer.Bitmap.CombineMode := cmBlend;
  // Center, relative to viewport
  Location := FloatRect(ImgView.GetViewportRect);
  Location.Inflate(-Location.Width / 4, -Location.Height / 4);
  Layer.Location := Location;


  (*
  ** Create scaled foreground bitmap layers
  *)

  // First layer: Circle
  Layer := AddLayer('Layer 1', dmCustom);
  Layer.Scaled := True;
  DrawCircle(Layer.Bitmap, 255);
  // Position top-left, relative to bitmap
  r := Layer.Bitmap.BoundsRect;
  r.Offset(r.Width div 4, r.Height div 4);
  Layer.Location := ImgView.ControlToBitmap(r);

  // Second layer: Box
  Layer := AddLayer('Layer 2', dmCustom);
  Layer.Scaled := True;
  DrawBox(Layer.Bitmap, 255);
  // Position bottom-right, relative to bitmap
  r := Layer.Bitmap.BoundsRect;
  r.Offset(r.Width - r.Width div 4, r.Height - r.Height div 4);
  Layer.Location := ImgView.ControlToBitmap(r);


  (*
  ** Create rubberband layer so we can move the foreground layers around
  *)
  FRubberbandLayer := TRubberbandLayer.Create(ImgView.Layers);
  FRubberbandLayer.Visible := False;
  FRubberbandLayer.Handles := [rhCenter, rhFrame, rhCorners];


  (*
  ** Populate blend mode list
  *)
  FCurrentItem := pointer(-1);
  ListViewOperation.Items.BeginUpdate;
  try

    for Group in Graphics32BlendService.Groups do
    begin
      ListGroup := ListViewOperation.Groups.Add;
      ListGroup.Header := Group.Name;

      for BlenderClass in Group do
      begin
        Item := ListViewOperation.Items.Add;
        Item.Caption := BlenderClass.Name;
        Item.SubItems.Add(BlenderClass.ID);
        Item.Data := pointer(BlenderClass);
        Item.GroupID := ListGroup.GroupID;
      end;
    end;

  finally
    ListViewOperation.Items.EndUpdate;
  end;
  FCurrentItem := nil;
end;

function TFormPixelCombine.GetCurrentLayer: TBitmapLayer;
begin
  if (FRubberbandLayer.ChildLayer <> nil) then
    Result := TBitmapLayer(FRubberbandLayer.ChildLayer)
  else
    Result := nil;
end;

procedure TFormPixelCombine.ImgViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if (Layer is TBitmapLayer) then
    CurrentLayer := TBitmapLayer(Layer);
end;

procedure TFormPixelCombine.SetCurrentLayer(const Value: TBitmapLayer);
var
  i: integer;
  Blender: TCustomGraphics32Blender;
  Found: boolean;
begin
  if (FRubberbandLayer.ChildLayer = Value) then
    exit; // Guards against endless recursion via ComboBoxLayer

  FRubberbandLayer.ChildLayer := Value;
  FRubberbandLayer.Visible := (Value <> nil);

  Found := False;

  if (Value <> nil) and (Value.Bitmap.DrawMode <> dmOpaque) then
  begin
    ListViewOperation.Enabled := (Value.Bitmap.DrawMode <> dmOpaque);
    TrackBarAlpha.Position := Value.Bitmap.MasterAlpha;

    Blender := FBlenders[Value];

    if (Blender <> nil) then
    begin
      for i := 0 to ListViewOperation.Items.Count-1 do
        if (TGraphics32BlenderClass(ListViewOperation.Items[i].Data) = Blender.ClassType) then
        begin
          ListViewOperation.Items[i].Checked := True;
          ListViewOperation.Items[i].MakeVisible(False);
          Found := True;
          break;
        end;

    end;

    TrackBarAlpha.Position := Value.Bitmap.MasterAlpha;
    TrackBarAlpha.Enabled := True;
  end else
  begin
    ListViewOperation.Enabled := False;
    TrackBarAlpha.Position := 0;
    TrackBarAlpha.Enabled := False;
  end;

  if (not Found) then
  begin
    FCurrentItem := pointer(-1);

    for i := 0 to ListViewOperation.Items.Count-1 do
      ListViewOperation.Items[i].Checked := False;

    FCurrentItem := nil;
  end;

  if (Value <> nil) then
    ComboBoxLayer.ItemIndex := ComboBoxLayer.Items.IndexOfObject(Value)
  else
    ComboBoxLayer.ItemIndex := -1;
end;

procedure TFormPixelCombine.TrackBarAlphaChange(Sender: TObject);
begin
  if (CurrentLayer <> nil) then
    CurrentLayer.Bitmap.MasterAlpha := TrackBarAlpha.Position;
end;

procedure TFormPixelCombine.ListViewOperationItemChecked(Sender: TObject; Item: TListItem);
var
  i: integer;
  Layer: TBitmapLayer;
  BlenderClass: TGraphics32BlenderClass;
  CurrentBlender: TCustomGraphics32Blender;
  NewBlender: TCustomGraphics32Blender;
  BlendFunc: TPixelCombineEvent;
begin
  if (FCurrentItem <> nil) then
    exit; // Avoid recursion

  FCurrentItem := Item;
  try
    // Can only uncheck by checking another item
    if (not FCurrentItem.Checked) then
    begin
      FCurrentItem.Checked := True;
      exit;
    end;

    for i := 0 to ListViewOperation.Items.Count-1 do
      if (Item <> ListViewOperation.Items[i]) then
        ListViewOperation.Items[i].Checked := False;

  finally
    FCurrentItem := nil;
  end;

  Layer := CurrentLayer;
  if (Layer = nil) then
    exit;

  BlenderClass := TGraphics32BlenderClass(Item.Data);
  CurrentBlender := FBlenders[Layer];

  if (CurrentBlender <> nil) and (CurrentBlender.ClassType = BlenderClass) then
    exit; // Same blender; Do nothing

  NewBlender := TGraphics32BlenderClass(Item.Data).Create;
  try

    // Replace existing blender with new one
    FBlenders.Items[Layer] := NewBlender;

  except
    NewBlender.Free;
    raise;
  end;

  NewBlender.GetPixelCombiner(BlendFunc);

  Layer.Bitmap.OnPixelCombine := BlendFunc;

  if (Assigned(BlendFunc)) then
    Layer.Bitmap.DrawMode := dmCustom
  else
    // No blend function implies that we should use the standard blend op
    Layer.Bitmap.DrawMode := dmBlend;

  Layer.Changed;
end;

procedure TFormPixelCombine.ListViewOperationSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if (Selected) then
    Item.Checked := True;
end;

end.
