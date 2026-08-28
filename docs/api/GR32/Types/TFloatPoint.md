---
layout: doc
docType: api
unit: GR32
entity: TFloatPoint
kind: Type
summary: "2D single-precision floating-point point structure."
declaration: |
  type
    PFloatPoint = ^TFloatPoint;
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
---

## Description

`TFloatPoint` represents a 2D point using single-precision floating-point coordinates ([[TFloat]]).

## Fields

| Field | Type | Description |
| --- | --- | --- |
| `X` | `TFloat` | Single precision X-coordinate. |
| `Y` | `TFloat` | Single precision Y-coordinate. |
