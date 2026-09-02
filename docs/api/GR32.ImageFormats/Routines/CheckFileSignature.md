---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: CheckFileSignature
kind: Function
summary: "Helper function that inspects magic bytes or headers in a stream at a given offset without altering the stream position."
overloads:
  - signature: "function CheckFileSignature(Stream: TStream; const Signature, Mask: array of Byte; Offset: Int64 = 0): Boolean; overload;"
    summary: "Checks if a stream matches a byte array signature and bitmask at the specified offset."
    parameters:
      - name: Stream
        type: TStream
        description: "Input stream to inspect."
      - name: Signature
        type: "array of Byte"
        description: "Expected magic byte sequence."
      - name: Mask
        type: "array of Byte"
        description: "Bitmask array for bitwise signature comparison."
      - name: Offset
        type: Int64
        description: "Byte offset from stream start (defaults to 0)."

  - signature: "function CheckFileSignature(Stream: TStream; const Signature: array of Byte; Offset: Int64 = 0): Boolean; overload;"
    summary: "Checks if a stream matches a byte array signature at the specified offset."
    parameters:
      - name: Stream
        type: TStream
        description: "Input stream to inspect."
      - name: Signature
        type: "array of Byte"
        description: "Expected magic byte sequence."
      - name: Offset
        type: Int64
        description: "Byte offset from stream start (defaults to 0)."

  - signature: "function CheckFileSignature(Stream: TStream; const Signature, Mask: TBytes; Offset: Int64 = 0): Boolean; overload;"
    summary: "Checks if a stream matches a dynamic byte array signature and bitmask."
    parameters:
      - name: Stream
        type: TStream
        description: "Input stream."
      - name: Signature
        type: TBytes
        description: "Expected signature bytes."
      - name: Mask
        type: TBytes
        description: "Bitmask bytes."
      - name: Offset
        type: Int64
        description: "Stream offset."

  - signature: "function CheckFileSignature(Stream: TStream; const Signature: TBytes; Offset: Int64 = 0): Boolean; overload;"
    summary: "Checks if a stream matches a dynamic byte array signature."
    parameters:
      - name: Stream
        type: TStream
        description: "Input stream."
      - name: Signature
        type: TBytes
        description: "Expected signature bytes."
      - name: Offset
        type: Int64
        description: "Stream offset."

  - signature: "function CheckFileSignature(Stream: TStream; const Signature; Size: Cardinal; const Mask; MaskSize: Cardinal; Offset: Int64 = 0): Boolean; overload;"
    summary: "Checks raw untyped memory buffer signatures and masks against a stream."
    parameters:
      - name: Stream
        type: TStream
        description: "Input stream."
      - name: Signature
        type: Untyped
        description: "Signature buffer."
      - name: Size
        type: Cardinal
        description: "Signature size in bytes."
      - name: Mask
        type: Untyped
        description: "Mask buffer."
      - name: MaskSize
        type: Cardinal
        description: "Mask size in bytes."
      - name: Offset
        type: Int64
        description: "Stream offset."

  - signature: "function CheckFileSignature(Stream: TStream; const Signature; Size: Cardinal; Offset: Int64): Boolean; overload;"
    summary: "Checks raw untyped memory buffer signatures against a stream."
    parameters:
      - name: Stream
        type: TStream
        description: "Input stream."
      - name: Signature
        type: Untyped
        description: "Signature buffer."
      - name: Size
        type: Cardinal
        description: "Signature size in bytes."
      - name: Offset
        type: Int64
        description: "Stream offset."
---

## Description

`CheckFileSignature` tests magic numbers or headers at a specified offset in `Stream`. The original stream position is automatically saved and restored when `CheckFileSignature` finishes. Returns `True` if the signature matches.

## Example

```pascal
const
  PngSignature: array [0..7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);
begin
  // Verify if stream starts with standard 8-byte PNG header
  if CheckFileSignature(Stream, PngSignature) then
    ShowMessage('Valid PNG header detected!');
end;
```
