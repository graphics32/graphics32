---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: IImageFormatReader
kind: Interface
declaration: "IImageFormatReader = interface"
summary: "Interface for reading image data from a stream into a TCustomBitmap32."
---

## Description

`IImageFormatReader` defines stream inspection and decoding contracts for image formats that can parse image files from streams into [[TCustomBitmap32]] pixel buffers.

## Example

```pascal
var
  Reader: IImageFormatReader;
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create('image.png', fmOpenRead or fmShareDenyWrite);
  try
    Reader := ImageFormatManager.Readers.FindReader(FileStream);
    if (Reader <> nil) and Reader.CanLoadFromStream(FileStream) then
      Reader.LoadFromStream(MyBitmap32, FileStream);
  finally
    FileStream.Free;
  end;
end;
```

[members]
