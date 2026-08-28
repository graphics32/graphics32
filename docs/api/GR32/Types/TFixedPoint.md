---
layout: doc
docType: api
unit: GR32
entity: TFixedPoint
kind: Type
summary: "2D point structure using 16.16 fixed-point coordinates."
declaration: |
  type
    TFixedPoint = record
      X, Y: TFixed;
    public
      constructor Create(const P: TFloatPoint); overload;
      constructor Create(X, Y: TFixed); overload;
      constructor Create(X, Y: Integer); overload;
      constructor Create(X, Y: TFloat); overload;

      class operator Equal(const Lhs, Rhs: TFixedPoint): Boolean;
      class operator NotEqual(const Lhs, Rhs: TFixedPoint): Boolean;
      class operator Add(const Lhs, Rhs: TFixedPoint): TFixedPoint;
      class operator Subtract(const Lhs, Rhs: TFixedPoint): TFixedPoint;

      class function Zero: TFixedPoint; static;
    end;
---

## Description

`TFixedPoint` represents a 2D point using 16.16 fixed-point coordinates ([[TFixed]]).

## Fields

| Field | Type | Description |
| --- | --- | --- |
| `X` | `TFixed` | X-coordinate in 16.16 fixed-point format. |
| `Y` | `TFixed` | Y-coordinate in 16.16 fixed-point format. |
