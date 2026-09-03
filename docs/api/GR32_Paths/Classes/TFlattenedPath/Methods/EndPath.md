---
layout: doc
docType: api
unit: GR32_Paths
parent: TFlattenedPath
entity: TFlattenedPath.EndPath
kind: Method
declaration: "procedure EndPath(Close: boolean = False); override;"
summary: "Finalizes the current sub-path contour, copying accumulated vertices into the Path array."
---

## Description

`EndPath` completes the active sub-path segment being constructed.

- If `Close` is `True`, the initial point of the current sub-path is appended to close the contour, and the closed path counter is incremented.
- Flushes the internal working vertex buffer into a new entry in [[Path]], updates [[PathClosed]], and triggers the [[OnEndPath]] event.

## Parameters

| Parameter | Type | Description |
| --- | --- | --- |
| `Close` | `Boolean` | Set to `True` to close the contour by connecting back to the initial vertex; `False` leaves the contour open. |

## Example

```pascal
var
  Path: TFlattenedPath;
begin
  Path := TFlattenedPath.Create;
  try
    Path.MoveTo(10.0, 10.0);
    Path.LineTo(100.0, 10.0);
    Path.LineTo(100.0, 100.0);
    Path.EndPath(True); // Flushes 4-point closed contour into Path[0]
  finally
    Path.Free;
  end;
end;
```
