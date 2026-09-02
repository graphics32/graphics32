---
layout: doc
docType: api
unit: GR32
entity: TFixedRec
kind: Type
summary: "Packed record overlaying a 16.16 fixed-point integer with integer and fractional parts."
declaration: |
  type
    TFixedRec = packed record
      case Integer of
        0: (Fixed: TFixed);
        1: (Frac: Word; Int: SmallInt);
    end;
    PFixedRec = ^TFixedRec;
aliases: [PFixedRec]
---

## Description

`TFixedRec` is a packed variant record providing low-level access to the integer and fractional parts of a 16.16 fixed-point value ([[TFixed]]).

## Variant Fields

**case 0**

| Field | Type | Description |
| --- | --- | --- |
| `Fixed` | `TFixed` | Combined 16.16 fixed-point value as a 32-bit signed integer. |

**case 1**

| Field | Type | Description |
| --- | --- | --- |
| `Frac` | `Word` | Fractional component (lower 16 bits). |
| `Int` | `SmallInt` | Signed integer component (upper 16 bits). |
