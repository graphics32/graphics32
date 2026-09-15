---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: Graphics32BlendService
kind: Function
declaration: "function Graphics32BlendService: IGraphics32BlendService;"
summary: "Returns the global singleton instance of the Graphics32 blend service."
returns:
  type: IGraphics32BlendService
  description: "The global [[IGraphics32BlendService]] singleton interface."
seealso:
  - "[[IGraphics32BlendService]]"
---

## Description

`Graphics32BlendService` returns the global [[IGraphics32BlendService]] singleton instance. It provides access to the global blend mode registry, enabling application code to register custom blender classes, query blenders by ID, enumerate registered blenders, or access category groups.

## Example

```pascal
var
  Service: IGraphics32BlendService;
  BlenderClass: TGraphics32BlenderClass;
begin
  Service := Graphics32BlendService;
  BlenderClass := Service.BlenderByID('Multiply');
  if (BlenderClass <> nil) then
    ShowMessage('Found Multiply blender: ' + BlenderClass.Name);
end;
```
