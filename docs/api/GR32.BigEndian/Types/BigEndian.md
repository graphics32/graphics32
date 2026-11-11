---
layout: doc
docType: api
unit: GR32.BigEndian
entity: BigEndian
kind: Type
summary: "Record providing static helper methods for big-endian integer stream reading and writing."
declaration: |
  type
    BigEndian = record
    public
      class function ReadByte(Stream: TStream): Byte; static; inline;
      class function ReadWord(Stream: TStream): Word; static; inline;
      class function ReadSmallInt(Stream: TStream): SmallInt; static; inline;
      class function ReadCardinal(Stream: TStream): Cardinal; static; inline;
      class function ReadInt64(Stream: TStream): Int64; static; inline;

      class procedure WriteByte(Stream: TStream; Value: Byte); static; inline;
      class procedure WriteWord(Stream: TStream; Value: Word); static; inline;
      class procedure WriteSmallInt(Stream: TStream; Value: SmallInt); static; inline;
      class procedure WriteCardinal(Stream: TStream; Value: Cardinal); static; inline;
      class procedure WriteInt64(Stream: TStream; Value: Int64); static; inline;
    end;
---

## Description

`BigEndian` is a static record helper providing methods to read and write fundamental integer types (`Byte`, `Word`, `SmallInt`, `Cardinal`, and `Int64`) to and from Delphi `TStream` objects in Big Endian (network) byte order.

Multi-byte values are automatically byte-swapped using optimized `Swap16`, `Swap32`, and `Swap64` routines when reading or writing.

## Static Methods

### Stream Readers

| Method | Return Type | Description |
| --- | --- | --- |
| `ReadByte(Stream: TStream)` | `Byte` | Reads a single byte from the stream. |
| `ReadWord(Stream: TStream)` | `Word` | Reads a 16-bit unsigned word from the stream and converts from big-endian to host byte order. |
| `ReadSmallInt(Stream: TStream)` | `SmallInt` | Reads a 16-bit signed integer from the stream and converts from big-endian to host byte order. |
| `ReadCardinal(Stream: TStream)` | `Cardinal` | Reads a 32-bit unsigned cardinal from the stream and converts from big-endian to host byte order. |
| `ReadInt64(Stream: TStream)` | `Int64` | Reads a 64-bit signed integer from the stream and converts from big-endian to host byte order. |

### Stream Writers

| Method | Parameters | Description |
| --- | --- | --- |
| `WriteByte(Stream: TStream; Value: Byte)` | `Stream: TStream`, `Value: Byte` | Writes a single byte to the stream. |
| `WriteWord(Stream: TStream; Value: Word)` | `Stream: TStream`, `Value: Word` | Converts a 16-bit word from host to big-endian byte order and writes to stream. |
| `WriteSmallInt(Stream: TStream; Value: SmallInt)` | `Stream: TStream`, `Value: SmallInt` | Converts a 16-bit smallint from host to big-endian byte order and writes to stream. |
| `WriteCardinal(Stream: TStream; Value: Cardinal)` | `Stream: TStream`, `Value: Cardinal` | Converts a 32-bit cardinal from host to big-endian byte order and writes to stream. |
| `WriteInt64(Stream: TStream; Value: Int64)` | `Stream: TStream`, `Value: Int64` | Converts a 64-bit integer from host to big-endian byte order and writes to stream. |
