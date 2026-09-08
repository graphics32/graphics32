---
layout: doc
docType: api
unit: GR32_Math
entity: CumSum
kind: Procedure
declaration: "procedure CumSum(Values: PSingleArray; Count: Integer);"
summary: "Computes the cumulative sum (prefix sum) in-place for an array of single-precision floating-point values."
parameters:
  - name: Values
    type: PSingleArray
    description: "Pointer to the array of Single values."
  - name: Count
    type: Integer
    description: "Number of elements in the array."
---

## Description

`CumSum` calculates the cumulative sum (prefix sum) in-place for `Count` elements in the single-precision floating-point array pointed to by `Values`.

After execution, each element $i$ in `Values` contains the sum of all elements from index $0$ to $i$:

$$Values[i] = \sum_{k=0}^{i} Values_{initial}[k]$$

## Common Use Cases

1. **Summed-Area Tables & Fast Box Blurs**:
   - Prefix sums enable constant-time $O(1)$ range sum queries over array intervals.
   - In image processing, horizontal and vertical cumulative sums are the foundational building block for multi-pass box blurs, fast Gaussian approximations, and 2D summed-area tables (integral images).
2. **Cumulative Histograms & Image Equalization**:
   - Constructing cumulative distribution functions (CDF) from pixel brightness or color histograms for histogram equalization and tone mapping.
3. **Integral Curve & Spline Traversal**:
   - Accumulating segment lengths along polylines or parametric paths to perform constant-velocity arc-length reparameterization or distance-based stippling.
