---
layout: doc
docType: api
unit: GR32_Paths
parent: TFlattenedPath
entity: TFlattenedPath.MoveTo
kind: Method
declaration: "procedure MoveTo(const P: TFloatPoint); override;"
summary: "Starts a new sub-path contour at coordinate P, implicitly finalizing any prior open sub-path."
parameters:
  - name: P
    type: TFloatPoint
    description: "Target starting point for the new sub-path."
---

## Description

`MoveTo` implicitly ends any currently active sub-path segment by calling [[EndPath]], updates [[CurrentPoint]] to `P`, and registers `P` as the first vertex of a new sub-path contour.

## Example

```pascal
var
  Path: TFlattenedPath;
begin
  Path := TFlattenedPath.Create;
  try
    Path.MoveTo(FloatPoint(20.0, 20.0));
    Path.LineTo(80.0, 20.0);
    // Calling MoveTo implicitly ends the previous segment
    Path.MoveTo(FloatPoint(20.0, 60.0));
    Path.LineTo(80.0, 60.0);
    Path.EndPath;
  finally
    Path.Free;
  end;
end;
```
