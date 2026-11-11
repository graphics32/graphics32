---
layout: doc
docType: api
unit: GR32_Layers
entity: TRubberbandLayer
kind: Class
inheritance:
  - TPersistent
  - TNotifiablePersistent
  - TCustomLayer
  - TPositionedLayer
  - TCustomRubberBandLayer
  - TRubberbandLayer
summary: "Standard 8-handle rectangular selection and sizing layer."
---

## Description

`TRubberbandLayer` is a rectangular rubberband layer providing 8 side and corner sizing handles plus body dragging. Key features include:

- **Handle Sets**: `Handles` (`TRBHandles`) toggles visibility/enablement for corner, side, and frame elements.
- **Constraints & Aspect Ratios**: `Options` (`TRBOptions`) enables `roProportional`, `roConstrained`, and `roQuantized` sizing modes.
- **Min/Max Size**: Properties `MinWidth`, `MaxWidth`, `MinHeight`, `MaxHeight` enforce size limits.
- **Events**: `OnResizing` and `OnConstrain` allow fine-grained customization during interactive resizing.

[members]
