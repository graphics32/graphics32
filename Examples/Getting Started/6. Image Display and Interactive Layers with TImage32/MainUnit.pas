unit MainUnit;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32, GR32_Image, GR32_Layers;

type
  TFormInteractiveLayers = class(TForm)
    Image32: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure Image32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
    FRubberbandLayer: TRubberbandLayer;
  public
    function AddOverlayLayer(AImageControl: TImage32; AColor: TColor32): TPositionedLayer;
  end;

var
  FormInteractiveLayers: TFormInteractiveLayers;

implementation

{$R *.dfm}

function TFormInteractiveLayers.AddOverlayLayer(AImageControl: TImage32; AColor: TColor32): TPositionedLayer;
var
  Layer: TBitmapLayer;
  r: TFloatRect;
begin
  // Create a new bitmap layer owned by ImageControl.Layers
  Layer := TBitmapLayer.Create(AImageControl.Layers);

  // Configure layer/bitmap size and color
  Layer.Bitmap.SetSize(120, 120, False);
  Layer.Bitmap.Clear(AColor);

  // Setup bitmap blending so the transparency works
  Layer.Bitmap.DrawMode := dmBlend;
  Layer.Bitmap.CombineMode := cmMerge;

  // Make layer position relative to image bitmap and follow its scale
  Layer.Scaled := True;

  // Position layer at a random position within the image control
  r.Left := Random(AImageControl.Width - Layer.Bitmap.Width);
  r.Top := Random(AImageControl.Height - Layer.Bitmap.Height);
  r.Right := r.Left + Layer.Bitmap.Width;
  r.Bottom := r.Top + Layer.Bitmap.Height;
  Layer.Location := r;

  Result := Layer;
end;

procedure TFormInteractiveLayers.FormCreate(Sender: TObject);
var
  Layer1: TPositionedLayer;
  Layer2: TPositionedLayer;
begin
  // Size background bitmap and clear to opaque white
  Image32.Bitmap.SetSize(400, 300, False);
  Image32.Bitmap.Clear(clWhite32);

  // Draw blue-ish opaque box onto background bitmap
  Image32.Bitmap.FillRect(20, 20, 200, 200, Color32(52, 152, 219));
RandSeed := 5;
  // Add two bitmap layers
  Layer1 := AddOverlayLayer(Image32, clTrRed32); // Semi-transparent red
  Layer2 := AddOverlayLayer(Image32, clTrBlue32); // Semi-transparent blue

  // Add interactive rubberband layer and attach it to the first layer
  FRubberbandLayer := TRubberbandLayer.Create(Image32.Layers);
  FRubberbandLayer.ChildLayer := Layer1;
end;

procedure TFormInteractiveLayers.Image32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  // Only react to left-click
  if (Button <> mbLeft) then
    exit;

  // Did we click on a layer - and is it one that can be moved?
  if (Layer <> nil) and (Layer is TPositionedLayer) then
  begin
    // Attach the rubberband to the layer we just clicked on (unless it's
    // the rubberband itself)
    if (Layer <> FRubberbandLayer) then
    begin
      FRubberbandLayer.ChildLayer := TPositionedLayer(Layer);
      FRubberbandLayer.Visible := True;
    end;
  end else
  begin
    // Detach and hide the rubberband when we click outside a layer
    FRubberbandLayer.ChildLayer := nil;
    FRubberbandLayer.Visible := False;
  end;
end;

end.
