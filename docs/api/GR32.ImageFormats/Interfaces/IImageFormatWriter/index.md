---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: IImageFormatWriter
kind: Interface
declaration: "IImageFormatWriter = interface"
summary: "Interface for encoding and writing TCustomBitmap32 pixel data out to a stream."
---

## Description

`IImageFormatWriter` provides the contract for encoding [[TCustomBitmap32]] graphics into specific image format file streams (such as PNG, JPEG, BMP, or PSD).

## Example

```pascal
var
  Writer: IImageFormatWriter;
  FileStream: TFileStream;
begin
  Writer := ImageFormatManager.Writers.FindWriter('png');
  if Writer <> nil then
  begin
    FileStream := TFileStream.Create('output.png', fmCreate);
    try
      Writer.SaveToStream(MyBitmap32, FileStream);
    finally
      FileStream.Free;
    end;
  end;
end;
```

[members]
