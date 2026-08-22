# Unit GR32_Layers

The `GR32_Layers` unit implements a layer management system for `TImage32`, supporting image overlays, vector shapes, rubber-banding, and interactive positioning.

---

## Classes

| Class | Description |
|---|---|
| `TLayers` | Container class managing a z-ordered collection of layers attached to a `TImage32`. |
| `TCustomLayer` | Abstract base class for interactive layers. |
| `TBitmapLayer` | Layer containing a sub-bitmap with independent position, scaling, and opacity. |
| `TPositionedLayer` | Layer with spatial bounds (`Location`) and scaling handles. |
