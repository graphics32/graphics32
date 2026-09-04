---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: ImageFormatManager
kind: Function
declaration: "function ImageFormatManager: IImageFormatManager;"
summary: "Returns the global singleton IImageFormatManager instance for Graphics32."
returns:
  - type: IImageFormatManager
    description: "The calculated [[IImageFormatManager]] result."
---

## Description

`ImageFormatManager` provides access to the global central registry instance implementing [[IImageFormatManager]]. Use this function to register custom image format handlers, query registered formats, or perform image loading and saving.

## Example

```pascal
var
  Filter: string;
begin
  // Build a file filter string for open dialogs including all image format readers
  Filter := ImageFormatManager.BuildFileFilter(IImageFormatReader, True);
end;
```
