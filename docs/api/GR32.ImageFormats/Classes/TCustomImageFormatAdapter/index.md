---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: TCustomImageFormatAdapter
kind: Class
scope: Public
abstract: true
declaration: "TCustomImageFormatAdapter = class abstract(TCustomImageFormat, IImageFormatAdapter);"
inheritance:
  - TObject
  - TInterfacedObject
  - TCustomImageFormat
  - TCustomImageFormatAdapter
summary: "Abstract base class for image format adapter implementations handling object-to-object image assignments."
---

## Description

`TCustomImageFormatAdapter` implements [[IImageFormatAdapter]] and provides virtual method stubs for assigning graphic objects to and from [[TCustomBitmap32]].

Custom format handlers derive from `TCustomImageFormatAdapter` to integrate third-party graphic classes into the Graphics32 format architecture.

## Example

```pascal
type
  TMyImageAdapter = class(TCustomImageFormatAdapter)
  protected
    function CanAssignFrom(Source: TPersistent): Boolean; override;
    function AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): Boolean; override;
  end;

function TMyImageAdapter.CanAssignFrom(Source: TPersistent): Boolean;
begin
  Result := (Source is TMyGraphicClass);
end;

function TMyImageAdapter.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): Boolean;
begin
  if CanAssignFrom(Source) then
  begin
    // Convert TMyGraphicClass into Dest
    Dest.SetSize(TMyGraphicClass(Source).Width, TMyGraphicClass(Source).Height);
    // ... Copy pixel data ...
    Result := True;
  end
  else
    Result := False;
end;
```
