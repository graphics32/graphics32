---
layout: doc
docType: api
unit: GR32_Paths
entity: TFlattenedPath
kind: Class
declaration: "TFlattenedPath = class(TCustomPath)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomPath
  - TFlattenedPath
summary: "Converts vector path commands into flattened polygonal vertex contours stored in dynamic float point arrays."
---

## Description

`TFlattenedPath` extends [[TCustomPath]] to flatten mathematical curves (cubic Beziers, quadratic Beziers, arcs, circles, ellipses) into discrete polygonal vertex lines stored in dynamic float point arrays (`TArrayOfArrayOfFloatPoint`).

Key features include:
- **Tessellated Vertex Storage**: Stores flattened vector path contours in the [[Path]] array (`TArrayOfArrayOfFloatPoint`) accompanied by closure state flags in [[PathClosed]] (`TBooleanArray`).
- **Path Lifecycle Events**: Triggers [[OnBeginPath]] when starting a new sub-path and [[OnEndPath]] when completing a sub-path segment.
- **Dynamic Buffer Management**: Utilizes internal vertex buffering with automatic grow strategies to minimize memory reallocations during curve tessellation.

[members]
