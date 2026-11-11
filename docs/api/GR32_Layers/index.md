---
layout: doc
docType: api
unit: GR32_Layers
entity: GR32_Layers
kind: Unit
summary: "Provides classes, interfaces, and types for managing interactive 2D image and vector layers, mouse handling, and rubberband selection controls."
---

## Description

The `GR32_Layers` unit implements the layer framework in Graphics32. It provides a comprehensive set of classes and tools for creating and managing visual layers rendered on top of visual controls or bitmaps.

Key capabilities provided by `GR32_Layers` include:

- **Layer Management**: `TLayerCollection` manages ordered lists of layers, Z-ordering, visibility, mouse capture, coordinate transformation, and invalidation notifications.
- **Base Layer Framework**: `TCustomLayer` provides fundamental layer behaviors, visibility options (`LOB_*`), hit testing, mouse/keyboard event dispatching, and invalidation hooks.
- **Positioned & Bitmap Layers**: `TPositionedLayer`, `TCustomIndirectBitmapLayer`, `TIndirectBitmapLayer`, `TCustomBitmapLayer`, and `TBitmapLayer` support moving, scaling, clipping, and rendering 32-bit bitmaps with optional alpha-channel hit testing.
- **Rubberband Selection & Manipulation**: `TCustomRubberBandLayer`, `TPolygonRubberbandLayer`, and `TRubberbandLayer` provide design-time or interactive visual frames with draggable control vertices and sizing handles for interactive object transformation.
- **Notification Interfaces**: Standard interfaces (`ILayerNotification`, `IUpdateRectNotification`, `ILayerUpdateNotification`, `ILayerListNotification`, etc.) enable low-coupling event propagation and customization.

[members]
