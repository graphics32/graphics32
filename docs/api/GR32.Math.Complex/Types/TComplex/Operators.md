---
layout: doc
docType: api
unit: GR32.Math.Complex
parent: TComplex
entity: TComplex Operators
kind: Operator
summary: "Overloaded arithmetic, comparison, implicit/explicit conversion, and rounding operators for TComplex."
---

## Description

The `TComplex` structure provides comprehensive operator overloads in Delphi and Free Pascal (FPC) for seamless mathematical expressions, comparisons, implicit type widening, and explicit conversions.

---

## Comparison & Equality Operators

Complex numbers do not possess a natural total ordering in the complex plane $\mathbb{C}$. Equality operations (`Equal`, `NotEqual`) evaluate whether both real and imaginary parts match using `Math.SameValue`. Ordered comparison operators (`LessThan`, `GreaterThan`) return `False`, while `LessThanOrEqual` and `GreaterThanOrEqual` evaluate to equality (`Left = Right`).

| Operator | Pascal Signature | Description |
| --- | --- | --- |
| `=` | `operator Equal(const Left, Right: TComplex): Boolean` | Returns `True` if both real and imaginary components are equal within floating-point tolerance (`SameValue`). |
| `<>` | `operator NotEqual(const Left, Right: TComplex): Boolean` | Returns `True` if either real or imaginary components differ. |
| `<` | `operator LessThan(const Left, Right: TComplex): Boolean` | Always returns `False` (complex plane is unordered). |
| `<=` | `operator LessThanOrEqual(const Left, Right: TComplex): Boolean` | Evaluates equality `(Left = Right)`. |
| `>` | `operator GreaterThan(const Left, Right: TComplex): Boolean` | Always returns `False` (complex plane is unordered). |
| `>=` | `operator GreaterThanOrEqual(const Left, Right: TComplex): Boolean` | Evaluates equality `(Left = Right)`. |

---

## Arithmetic Operators

Overloaded arithmetic operators support binary addition, subtraction, multiplication, and division between `TComplex` operands, as well as mixed-mode operations combining `TComplex` and scalar `Double` values.

| Operator | Pascal Signature | Description |
| --- | --- | --- |
| `+` | `operator Add(const Left, Right: TComplex): TComplex` | Adds two complex numbers: $(x_1 + x_2) + i(y_1 + y_2)$. |
| `+` | `operator Add(const Left: Double; const Right: TComplex): TComplex` | Adds a scalar double to a complex number: $(x_1 + x_2) + i y_2$. |
| `+` | `operator Add(const Left: TComplex; const Right: Double): TComplex` | Adds a complex number to a scalar double: $(x_1 + x_2) + i y_1$. |
| `-` | `operator Subtract(const Left, Right: TComplex): TComplex` | Subtracts two complex numbers: $(x_1 - x_2) + i(y_1 - y_2)$. |
| `-` | `operator Subtract(const Left: Double; const Right: TComplex): TComplex` | Subtracts a complex number from a scalar double: $(x_1 - x_2) - i y_2$. |
| `-` | `operator Subtract(const Left: TComplex; const Right: Double): TComplex` | Subtracts a scalar double from a complex number: $(x_1 - x_2) + i y_1$. |
| `*` | `operator Multiply(const Left, Right: TComplex): TComplex` | Multiplies two complex numbers: $(x_1 x_2 - y_1 y_2) + i(x_1 y_2 + x_2 y_1)$. |
| `*` | `operator Multiply(const Left: Double; const Right: TComplex): TComplex` | Scales a complex number by a scalar double. |
| `*` | `operator Multiply(const Left: TComplex; const Right: Double): TComplex` | Scales a complex number by a scalar double. |
| `/` | `operator Divide(const Left, Right: TComplex): TComplex` | Divides two complex numbers: $\frac{z_1 \cdot \overline{z_2}}{|z_2|^2}$. Raises `EZeroDivide` if denominator is zero. |
| `/` | `operator Divide(const Left: Double; const Right: TComplex): TComplex` | Divides a scalar double by a complex number. |
| `/` | `operator Divide(const Left: TComplex; const Right: Double): TComplex` | Divides a complex number by a scalar double. |
| `-` | `operator Negative(const AValue: TComplex): TComplex` | Unary negation: $-x - iy$. |

---

## Implicit Conversion Operators

Implicit conversion operators allow automatic assignment of numeric scalar types and strings to `TComplex` without explicit casting.

| Target Type | Pascal Signature | Description |
| --- | --- | --- |
| `TComplex` | `operator Implicit(const AValue: Double): TComplex` | Promotes a double scalar to complex $x + 0i$. |
| `TComplex` | `operator Implicit(const AValue: Integer): TComplex` | Promotes an integer to complex $x + 0i$. |
| `TComplex` | `operator Implicit(const AValue: Int64): TComplex` | Promotes a 64-bit integer to complex $x + 0i$. |
| `TComplex` | `operator Implicit(const AValue: Variant): TComplex` | Converts a variant value to complex real part. |
| `TComplex` | `operator Implicit(const AValue: string): TComplex` | Parses a complex string expression via `TComplex.Parse`. |

---

## Explicit Conversion Operators

Explicit cast operators convert a `TComplex` instance to scalar numeric types or string representations. Numerical conversions require that the imaginary component is zero (`Math.IsZero(Imaginary)`); otherwise, an `EConvertError` is raised.

| Target Type | Pascal Signature | Description |
| --- | --- | --- |
| `Double` | `operator Explicit(const AValue: TComplex): Double` | Casts real part to `Double` (asserts `Imaginary = 0`). |
| `Integer` | `operator Explicit(const AValue: TComplex): Integer` | Rounds real part to `Integer` (asserts `Imaginary = 0`). |
| `Int64` | `operator Explicit(const AValue: TComplex): Int64` | Rounds real part to `Int64` (asserts `Imaginary = 0`). |
| `string` | `operator Explicit(const AValue: TComplex): string` | Formats complex number to string via `ToString`. |

---

## Rounding & Truncation Operators (Delphi)

| Operator | Pascal Signature | Description |
| --- | --- | --- |
| `Round` | `operator Round(const AValue: TComplex): Int64` | Rounds real component to `Int64` (asserts `Imaginary = 0`). |
| `Trunc` | `operator Trunc(const AValue: TComplex): Int64` | Truncates real component to `Int64` (asserts `Imaginary = 0`). |
