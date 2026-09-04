---
layout: doc
docType: api
unit: GR32
entity: Color32
kind: Function
summary: "Constructs a TColor32 value"
overloads:
  - signature: "function Color32(R, G, B: Byte; A: Byte = $FF): TColor32; overload;"
    summary: "Constructs a TColor32 value from individual Red, Green, Blue, and optional Alpha byte components."
    parameters:
      - name: R
        type: Byte
        description: "Red component (0..255)."
      - name: G
        type: Byte
        description: "Green component (0..255)."
      - name: B
        type: Byte
        description: "Blue component (0..255)."
      - name: A
        type: Byte
        description: "Alpha component (0..255, defaults to $FF / opaque)."
    returns:
      - type: TColor32
        description: "The constructed 32-bit ARGB `TColor32` value."

  - signature: "function Color32(WinColor: TColor): TColor32; overload;"
    summary: "Converts a native VCL/FCL TColor value to a 32-bit TColor32 value with 100% opacity (Alpha = $FF)."
    parameters:
      - name: WinColor
        type: TColor
        description: "VCL/FCL 24-bit color value."
    returns:
      - type: TColor32
        description: "The converted opaque 32-bit ARGB `TColor32` value."

  - signature: "function Color32(Index: Byte; var Palette: TPalette32): TColor32; overload;"
    summary: "Retrieves a TColor32 value from a 256-color TPalette32 array by index."
    parameters:
      - name: Index
        type: Byte
        description: "Palette index (0..255)."
      - name: Palette
        type: TPalette32
        description: "Palette lookup table."
    returns:
      - type: TColor32
        description: "The 32-bit ARGB `TColor32` value stored at `Index` in the palette table."
---

## Description

`Color32` constructs a 32-bit ARGB [[TColor32]] value from RGB/Alpha components, VCL `TColor` values, or palette entries.
