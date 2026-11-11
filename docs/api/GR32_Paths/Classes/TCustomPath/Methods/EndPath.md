---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.EndPath
kind: Method
declaration: "procedure EndPath(Close: boolean = False); virtual;"
summary: "Finalizes the current sub-path contour, optionally closing it back to its starting coordinate."
---

## Description

`EndPath` completes the current sub-path segment being built.

If `Close` is `True`, a closing line segment is appended from the current position back to the initial starting vertex of the active sub-path, and the path contour is marked as closed.

In derived classes such as [[TFlattenedPath]] and [[TCanvas32]], ending a path flushes accumulated vertices into the internal polygon list and notifies change handlers.

## Parameters

| Parameter | Type | Description |
| --- | --- | --- |
| `Close` | `Boolean` | Set to `True` to automatically append a line to the start point and mark the path closed; `False` leaves the path open. |

## Example

```pascal
var
  Canvas: TCanvas32;
begin
  Canvas := TCanvas32.Create(MyBitmap);
  try
    // Construct a closed triangle
    Canvas.MoveTo(100.0, 50.0);
    Canvas.LineTo(150.0, 150.0);
    Canvas.LineTo(50.0, 150.0);
    Canvas.EndPath(True); // Closes path back to (100, 50)
  finally
    Canvas.Free;
  end;
end;
```
