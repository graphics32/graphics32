---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TNearestResampler
entity: TNearestResampler.GetSampleInt
kind: Method
scope: Public
summary: "Retrieves the nearest pixel color sample at integer, fixed-point, or float coordinates."
overloads:
  - signature: "function GetSampleInt(X, Y: Integer): TColor32; override;"
    summary: "Retrieves the nearest pixel color sample at integer coordinates."
    parameters:
      - name: X, Y
        type: Integer
        description: "Integer pixel coordinates."
    returns:
      - type: TColor32
        description: "The 32-bit ARGB `TColor32` pixel color sample at integer coordinate `(X, Y)`."
  - signature: "function GetSampleFixed(X, Y: TFixed): TColor32; override;"
    summary: "Retrieves the nearest pixel color sample at fixed-point coordinates."
    parameters:
      - name: X, Y
        type: TFixed
        description: "Fixed-point pixel coordinates in 16.16 format."
    returns:
      - type: TColor32
        description: "The 32-bit ARGB `TColor32` pixel color sample at fixed-point coordinate `(X, Y)`."
  - signature: "function GetSampleFloat(X, Y: TFloat): TColor32; override;"
    summary: "Retrieves the nearest pixel color sample at floating-point coordinates."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Floating-point pixel coordinates."
    returns:
      - type: TColor32
        description: "The 32-bit ARGB `TColor32` pixel color sample at floating-point coordinate `(X, Y)`."
---

## Description

Returns the nearest pixel value from the bound bitmap at coordinate $(X, Y)$.
