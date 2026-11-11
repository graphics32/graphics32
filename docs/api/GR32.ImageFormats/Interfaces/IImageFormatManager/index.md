---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: IImageFormatManager
kind: Interface
declaration: "IImageFormatManager = interface"
summary: "Central manager interface for image format registration, priority management, and file filter construction."
---

## Description

`IImageFormatManager` is the central image format registry interface in Graphics32. It maintains registered image formats ordered by priority, dispatches adapter/reader/writer operations, and constructs file filter strings for open/save file dialogs. The global singleton instance is accessed via [[ImageFormatManager]].

## Example

```pascal
var
  OpenFilter, SaveFilter: string;
  Writer: IImageFormatWriter;
begin
  // Construct dialog filters for file dialogs
  OpenFilter := ImageFormatManager.BuildFileFilter(IImageFormatReader, True);
  SaveFilter := ImageFormatManager.BuildFileFilter(IImageFormatWriter, True);

  // Locate an image format writer for PSD format
  Writer := ImageFormatManager.Writers.FindWriter('psd');
  if Writer <> nil then
    Writer.SaveToStream(MyBitmap, OutputFileStream);
end;
```

[members]
