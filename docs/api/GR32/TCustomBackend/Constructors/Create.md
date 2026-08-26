---
layout: doc
docType: api
unit: GR32
parent: TCustomBackend
entity: TCustomBackend.Create
kind: Constructor
scope: Public
summary: "Initializes a new instance of TCustomBackend, optionally setting the owner bitmap."
overloads:
  - signature: "constructor Create; overload; override;"
    summary: "Initializes a standalone TCustomBackend instance."
  - signature: "constructor Create(Owner: TCustomBitmap32); reintroduce; overload; virtual;"
    summary: "Initializes a TCustomBackend instance and sets it as the active backend of Owner."
    parameters:
      - name: Owner
        type: TCustomBitmap32
        description: "The bitmap instance that owns and uses this backend."
---

## Description

`Create` initializes a new `TCustomBackend` instance.

When constructed with `Create(Owner)`, the backend attaches itself to `Owner` by assigning `Owner.Backend := Self`.

## Example

```pascal
var
  Backend: TMemoryBackend;
begin
  Backend := TMemoryBackend.Create(Bitmap);
end;
```
