---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LoadFromResourceName
kind: Method
scope: Public
declaration: "procedure LoadFromResourceName(Instance: THandle; const ResName: string; ResType: TResourceType = RT_BITMAP);"
summary: "Loads bitmap data from an executable module resource by string resource name."
parameters:
  - name: Instance
    type: THandle
    description: "Module handle containing the resource."
  - name: ResName
    type: string
    description: "Resource name string."
  - name: ResType
    type: TResourceType
    description: "Resource type identifier (default RT_BITMAP)."
---

## Description

`LoadFromResourceName` loads bitmap contents from a compiled executable resource specified by string name.

## Example

```pascal
Bitmap.LoadFromResourceName(HInstance, 'MY_BITMAP');
```
