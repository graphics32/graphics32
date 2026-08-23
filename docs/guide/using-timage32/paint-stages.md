# Paint Stages

The order in which `TImage32` blends layers and the bitmap image to its back-buffer is specified by paint stages.

For example, the default sequence of operations includes:

  * Clearing the visible area of the background (that is the parts of the buffered area which are not covered by the bitmap image), or the whole buffer (if the bitmap image is not in dmOpaque draw mode).
  * Drawing the dotted frame around the control boundaries (design-time only).
  * Drawing the scaled bitmap image.
  * Framing the area of the scaled bitmap image with the dotted frame (design-time only).
  * Drawing layers.

It is possible to change the order in which stages execute at run-time, add new stages, delete old ones, etc., using the [PaintStages](/api/GR32_Image/TCustomImage32/Properties/PaintStages) property of `TImage32`, which is basically a dynamic indexed list of stages.

## TPaintStage Record

Each paint stage is defined with a [TPaintStage](/api/GR32_Image/Types/TPaintStage) record:

```pascal
type
  TPaintStage = record
  DsgnTime: Boolean;
  RunTime: Boolean;
  Stage: Cardinal;     // a PST_* constant
  Parameter: Cardinal; // an optional parameter
end;
```

The `Stage` field holds one of the [Paint Stage Constants](/api/GR32_Image/Constants/Paint%20Stage%20Constants) and defines the action associated with the stage.

All stages include additional parameter, which may be ignored or may be used to store additional stage options. For example, the `PST_DRAW_LAYERS` stage uses its parameter as a 32-bit mask to filter out invisible layers.

By default, `TImage32` contains the following stages:

#| DsgnTime| RunTime| Stage| Parameter
---|---|---|---|---
0| True | True | `PST_CLEAR_BACKGND` | not used
1| False | True | `PST_CONTROL_FRAME` | not used
2| True | True | `PST_DRAW_BITMAP` | not used
3| False | True | `PST_BITMAP_FRAME` | not used
4| True | True | `PST_DRAW_LAYERS` | $80000000

See ‘[Using Layers](Using%20Layers)‘ for an explanation of the parameter value in `PST_DRAW_LAYERS` stage.

## Customizing TImage32 at Run-Time

The `PST_CUSTOM` stage deserves a little bit deeper explanation. It causes the control to issue an [OnPaintStage](/api/GR32_Image/TCustomImage32/Events/OnPaintStage) event, thus allowing you to change `TImage32` behavior at run-time.

The `OnPaintStage` event is declared as a `TPaintStageEvent` delegate type:

```pascal
type
  TPaintStageEvent = procedure(Sender: TObject; Dest: TBitmap32; StageNum: Cardinal) of object;
```

In the `OnPaintStage` event handler, the application can draw on the back-buffer (the `Dest` parameter) of the control.

Note, that by default, TImage32 does not generate [OnPaintStage](/api/GR32_Image/TCustomImage32/Events/OnPaintStage) events. In order to make it do so, you have to insert a new stage in the [PaintStages](/api/GR32_Image/TCustomImage32/Properties/PaintStages) list, and set its Stage to PST_CUSTOM, or change one of the existing stages, for example:

```pascal:line-numbers
type
  TForm1 = class(TForm)
    Image32: TImage32;
    procedure Image32InitStages(Sender: TObject); // OnInitStages
    procedure Image32PaintStage(Sender: TObject; Dest: TBitmap32; StageNum: Cardinal); // OnPaintStage
  private
    { Private declarations }
  public
    { Public declarations }
  end;
 
var
  Form1: TForm1;
 
implementation
 
{$R *.DFM}

procedure TForm1.Image32InitStages(Sender: TObject);
begin
  // change default PST_CLEAR_BACKGND (0-th stage) to a custom handler
  with Image32.PaintStages[0] do
  begin
    Stage := PST_CUSTOM;
    Parameter := 1; // use parameter to tag the stage
  end;

  // insert another custom stage after the bitmap image
  // was drawn, but before the control starts painting layers
  with Image32.PaintStages.Insert(4) do
  begin
    // Note that for new stages RunTime = True by default
    Stage := PST_CUSTOM;
    Parameter := 2; // use parameter to tag the stage
  end;
end ;
 
procedure TForm1.Image32PaintStage(Sender: TObject; Dest: TBitmap32; StageNum: Cardinal);
begin
  // OnPaintStage Handler
  case Image32.PaintStages[StageNum].Parameter of
    1: // do something with the background
    2: // call another handler
  end;
end;
```

## GDI Overlays

A final step in `TImage32` repainting is the drawing of GDI overlays. This operation is performed after the bitmap image and layers have been combined in a back buffer and copied to the screen canvas.

At this stage, `TImage32` fires the `OnGDIOverlay` event, where you can perform drawing using the standard `Canvas` of the `TImage32`.

The main reason for introducing this stage is that painting of GDI overlays does not affect the contents of the buffer, that is changes in overlay image will not cause buffer invalidation. However, GDI overlays have to be repainted each time the control repaints itself, and they are not flicker-free. Basically, you can think of them as a `TPaintBox` on top of a `TImage32`.

:::warning
GDI overlays has been marked for deprecation and might not be included in future versions.
Use at your own risk (although this warning has been here for at least 10 years).
:::