---
layout: doc
docType: api
unit: GR32_Paths
parent: TCustomPath
entity: TCustomPath.Create
kind: Constructor
declaration: "constructor Create; override;"
summary: "Creates and initializes a new instance of TCustomPath."
---

## Description

`Create` instantiates a new `TCustomPath` object and initializes internal state variables, resetting [[CurrentPoint]] to `(0, 0)`.

## Example

```pascal
var
  Path: TFlattenedPath;
begin
  Path := TFlattenedPath.Create;
  try
    Path.MoveTo(100.0, 100.0);
    Path.LineTo(200.0, 100.0);
    Path.EndPath(False);
  finally
    Path.Free;
  end;
end;
```
