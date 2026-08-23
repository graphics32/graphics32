# Using layers

The `TImage32` control contains an indexed collection of layers, referenced by the [Layers](/api/GR32_Image/TCustomImage32/Properties/Layers) property. The number of layers is limited only by the amount of free memory.

Each layer is an entity, which basically ‘knows’ how to paint itself onto the back-buffer of the control and how to interact with the mouse. Layers are indexed and their indexes are similar to the Z-order of standard controls. Layers with smaller indexes are considered to lie deeper.

## Common Properties and Methods

All layers inherit from the [TCustomLayer](/api/GR32_Layers/TCustomLayer) class. This class implements behavior and properties common to all layer types.

The basic layer’s behavior is controlled by its [LayerOptions](/api/GR32_Layers/TCustomLayer/Properties/LayerOptions) property, which is a 32-bit unsigned integer value composed of [Layer Options Bits](/api/GR32_Layers/Constants/Layer%20Options%20Bits). This property allows for fast and relatively simple referencing of layers and groups of layers.

Consider, for example, the `LOB_VISIBLE` bit (31-st bit in layer options). When `TImage32` repaints its layers at the `PST_DRAW_LAYERS` stage, it uses the stage parameter from corresponding [TPaintStage record](Paint-Stages#TPaintStage%20Record) as a bit-mask (default value is `LOB_VISIBLE=$80000000`). Being compared (logical AND operation) with [LayerOptions](/api/GR32_Layers/TCustomLayer/Properties/LayerOptions) of each layer, this mask determines whether the layer should be painted or not.

Similar situation is with reaction to mouse messages (see below).

Using [LayerOptions](/api/GR32_Layers/TCustomLayer/Properties/LayerOptions) you can easily customize the appearance and modify the order in which `TImage32` repaints layers. For example, you may assign several categories to layers, using lower 24-bits of [LayerOptions](/api/GR32_Layers/TCustomLayer/Properties/LayerOptions) and then just change the mask in corresponding [TPaintStage record](Paint-Stages#TPaintStage%20Record) to specify exactly which category you want to be displayed.

Note that you can insert several `PST_DRAW_LAYERS` paint stages (even before the `PST_DRAW_BITMAP` stage), each with its own mask, or, alternatively, call [ExecDrawLayers](/api/GR32_Image/TCustomImage32/Methods/ExecDrawLayers) while handling `PST_CUSTOM` stages. Basically it means, that you can realize almost any complex repainting scheme, like overlay layers, underlay layers, layers which are always on top or always in the background, etc.

## Tracking the Mouse

Layers are capable of responding to mouse-down/move/up messages. When the layer container (the `TImage32` control) receives a mouse event it searches the layer collection, from top to bottom, for a single layer that can handle the mouse event.

In order for a layer to be eligible to receive mouse events the layer must satisfy both of the following conditions:

  * The layer has `LOB_MOUSE_EVENTS` bit activated in its [LayerOptions](/api/GR32_Layers/TCustomLayer/Properties/LayerOptions).
  * The layer passes a hit test (see below).

If a suitable layer is found then the event is passed on to the layer and the layer generates a corresponding OnMouseDown, OnMouseMove or OnMouseUp event.

Once the layer mouse handling is done the `TImage32` control continues its normal mouse handling and generate the relevant [OnMouseDown](/api/GR32_Image/TCustomImage32/Events/OnMouseDown), [OnMouseMove](/api/GR32_Image/TCustomImage32/Events/OnMouseMove) or [OnMouseUp](/api/GR32_Image/TCustomImage32/Events/OnMouseUp) event.

Note that layer mouse events can be completely disabled by setting the layer collection [MouseEvents](/api/GR32_Layers/TLayerCollection/Properties/MouseEvents) property to _False_. When layer mouse events has been disabled, the `TImage32` control will generate mouse events right away, without checking for a possible receiving layer.

## Layer hit testing

As mentioned, in order for a layer to be eligible to receive mouse events, the layer must pass a _hit test_. Hit testing is performed by calling the layer’s [HitTest](/api/GR32_Layers/TCustomLayer/Methods/HitTest) method:

```pascal
function HitTest(X, Y: Integer): Boolean; virtual;
```

The **X** and **Y** parameters here are the coordinates of the mouse pointer in pixels, relative to the top-left corner of the `TImage32` control. The function returns a value that indicates if the layer wants to process the mouse event.

By default `HitTest` returns True regardless of the mouse coordinates but most layer types override `HitTest` to take the layer content and position into account. For example the `TPositionedLayer` layer type only returns `True` if the mouse position is within the layer bounds and the [TBitmapLayer](/api/GR32_Layers/TBitmapLayer) type extends this to optionally also take the transparency of its bitmap into account; Clicks or movement over fully transparent bitmap pixels does not generate mouse events.

The built in hit test logic can easily be customized using the layer’s [OnHitTest](/api/GR32_Layers/TCustomLayer/Events/OnHitTest) event.

## Mouse capture

Similar to standard controls, layers can also _capture_ mouse messages.

Mouse capture is necessary in order for a control to continue to receive mouse messages even after the mouse leaves the control. For example, without mouse capture, if a user clicked on a control and then moved the mouse pointer out of the control and released the mouse button, the control would only receive a mouse-down message and mouse-move messages until the mouse left the control. It would never receive the mouse-up message. With mouse capture the control will receive mouse messages until it releases the mouse capture.

By default, a layer that has satisfied the hit test criteria mentioned above, will automatically acquire mouse capture when the layer is clicked and release mouse capture when the mouse button is released.
A layer can disable automatic mouse capture by setting the `LOB_NO_CAPTURE` layer option. This is commonly done for layers that are visible but which should not interact with the mouse.

## Painting Layers

Layers are painted in bottom-to-top order, starting from lower indexes. By default, only visible layers are painted, that is the ones with `LOB_VISIBLE` bit set, but as it was shown above, this order may be changed.

## Positioned Layers

Positioned layers are the layers you will probably use most of the time. The base class for positioned layers is [TPositionedLayer](/api/GR32_Layers/TPositionedLayer). In addition to basic layer behavior, it introduces a [Location](/api/GR32_Layers/TPositionedLayer/Properties/Location) property (of the [TFloatRect](/api/GR32/Types/Rectangle%20Types#TFloatRect) type), which specifies the layer’s position and size. Having location specified as a floating point rectangle helps to avoid rounding errors when layers are resized.

The location can be specified in pixels, relative to the `TImage32` control’s top-left corner, or in scaled pixels, relative to the top-left corner of the [bitmap image](bitmap-image). In the second case, the scale of the layer coincides with the scale of the bitmap image and the actual location of the layer relative to the top-left corner of `TImage32` may be obtained with its [GetAdjustedLocation](/api/GR32_Layers/TPositionedLayer/Methods/GetAdjustedLocation) method.
