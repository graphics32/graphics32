---
layout: doc
docType: api
unit: GR32_Paths
parent: TFlattenedPath
entity: TFlattenedPath.Clear
kind: Method
declaration: "procedure Clear; override;"
summary: "Clears all stored path vertex contours, closure flags, and temporary point buffers."
---

## Description

`Clear` purges all flattened polygonal contours from the [[Path]] array, resets [[PathClosed]], clears closed path counters, and resets internal vertex working buffers in `TFlattenedPath`.

## Example

```pascal
var
  Path: TFlattenedPath;
begin
  Path := TFlattenedPath.Create;
  try
    Path.Rectangle(FloatRect(10.0, 10.0, 100.0, 100.0));
    // Path.Path now contains 1 polygon contour

    Path.Clear;
    // Path.Path is now empty
  finally
    Path.Free;
  end;
end;
```
