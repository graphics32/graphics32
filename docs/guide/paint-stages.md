# Paint stages

# Paint Stages

An order in which TImage32 blends layers and bitmap image to its back-buffer is specified by paint stages.

For example, the default sequence of operations includes:

  * Clearing the visible area of the background, that is the parts of the buffered area which are not covered by the bitmap image, or the whole buffer, if the bitmap image is not in dmOpaque draw mode;
  * Drawing the dotted frame around the control boundaries (design-time only);
  * Drawing the scaled bitmap image;
  * Framing the area of the scaled bitmap image with the dotted frame (design-time only);
  * Drawing layers;



It is possible to change the order in which stages execute at run-time, add new stages, delete old ones, etc., using the [PaintStages](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Properties/PaintStages.htm) property of TImage32, which is basically a dynamic indexed list of stages.

## TPaintStage Record

Each paint stage is defined with a [TPaintStage](https://graphics32.github.io/Docs/Units/GR32_Image/Types/TPaintStage.htm) record:

**type** TPaintStage = **record**
DsgnTime: Boolean;
RunTime: Boolean;
Stage: Cardinal; // a PST_* constant
Parameter: Cardinal; // an optional parameter
**end** ;

where the Stage member holds one of the [Paint Stage Constants](https://graphics32.github.io/Docs/Units/GR32_Image/Constants/Paint%20Stage%20Constants.htm) and defines the action associated with the stage.

All stages include additional parameter, which may be ignored or may be used to store additional stage options. For example, PST_DRAW_LAYERS stage uses its parameter as a 32-bit mask to filter out invisible layers.

By default, TImage32 contains the following stages:

#| DsgnTime| RunTime| Stage| Parameter
---|---|---|---|---
0| True| True| PST_CLEAR_BACKGND| not used
1| False| True| PST_CONTROL_FRAME| not used
2| True| True| PST_DRAW_BITMAP| not used
3| False| True| PST_BITMAP_FRAME| not used
4| True| True| PST_DRAW_LAYERS| $80000000

See ‘[Using Layers](https://graphics32.github.io/Docs/Additional%20Topics/Using%20TImage32/Using%20Layers.htm)‘ for an explanation of the parameter value in PST_DRAW_LAYERS stage.

## Customizing TImage32 at Run-Time

A PST_CUSTOM stage deserves a little bit deeper explanation. It causes the control to issue an [OnPaintStage](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Events/OnPaintStage.htm) event, thus allowing to change TImage32 behavior at run-time.

The [OnPaintStage](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Events/OnPaintStage.htm) event is of a TPaintStageEvent type:

**type** TPaintStageEvent = **procedure**(Sender: TObject; Dest: TBitmap32; StageNum: Cardinal) **of** **object** ;

In the event handler, the application can perform some custom operations over the back-buffer of the control.

Note, that by default, TImage32 does not generate [OnPaintStage](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Events/OnPaintStage.htm). In order to make it do so, you have to insert a new stage in the [PaintStages](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Properties/PaintStages.htm) list, and set its Stage to PST_CUSTOM, or change one of the existing stages, for example:
