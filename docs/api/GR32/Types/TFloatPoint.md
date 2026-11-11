---
layout: doc
docType: api
unit: GR32
entity: TFloatPoint
kind: Type
summary: "2D single-precision floating-point point structure."
declaration: |
  type
    TFloatPoint = record
      X, Y: TFloat;
    public
      constructor Create(const P: TPoint); overload;
      constructor Create(X, Y: Integer); overload;
      constructor Create(X, Y: Single); overload;

      class operator Equal(const Lhs, Rhs: TFloatPoint): Boolean;
      class operator NotEqual(const Lhs, Rhs: TFloatPoint): Boolean;
      class operator Add(const Lhs, Rhs: TFloatPoint): TFloatPoint;
      class operator Subtract(const Lhs, Rhs: TFloatPoint): TFloatPoint;

      class function Zero: TFloatPoint; static;
      function Distance(const APoint: TFloatPoint): Single;
      function Length: Single;
    end;
    PFloatPoint = ^TFloatPoint;
aliases: [PFloatPoint, PFloatPointArray, TFloatPointArray, TArrayOfFloatPoint, TArrayOfArrayOfFloatPoint]
---

## Description

`TFloatPoint` represents a 2D point using single-precision floating-point coordinates ([[TFloat]]).

## Fields

| Field | Type | Description |
| --- | --- | --- |
| `X` | `TFloat` | Single precision X-coordinate. |
| `Y` | `TFloat` | Single precision Y-coordinate. |

## Related Types & Arrays

| Type | Declaration | Description |
| --- | --- | --- |
| `PFloatPoint` | `^TFloatPoint` | Pointer to a `TFloatPoint` record. |
| `TFloatPointArray` | `array [0..0] of TFloatPoint` | Static un-sized array type. |
| `PFloatPointArray` | `^TFloatPointArray` | Pointer to an un-sized array of `TFloatPoint` records. |
| `TArrayOfFloatPoint` | `array of TFloatPoint` | Dynamic array of `TFloatPoint` records; A polyline/polygon. |
| `TArrayOfArrayOfFloatPoint` | `array of TArrayOfFloatPoint` | 2D dynamic array of `TFloatPoint` records; A poly-polyline/poly-polygon; |
