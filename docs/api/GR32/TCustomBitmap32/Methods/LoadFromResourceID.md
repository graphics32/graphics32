---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LoadFromResourceID
kind: Method
scope: Public
declaration: "procedure LoadFromResourceID(Instance: THandle; ResID: Integer; ResType: TResourceType = RT_BITMAP);"
summary: "Loads bitmap data from an executable module resource by integer resource ID."
parameters:
  - name: Instance
    type: THandle
    description: "Module handle containing the resource."
  - name: ResID
    type: Integer
    description: "Integer resource ID."
  - name: ResType
    type: TResourceType
    description: "Resource type identifier (default RT_BITMAP)."
---

## Description

`LoadFromResourceID` loads bitmap contents from a compiled executable/DLL resource.

## Example

```pascal
Bitmap.LoadFromResourceID(HInstance, 101);
```
