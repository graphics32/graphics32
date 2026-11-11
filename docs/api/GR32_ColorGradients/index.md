---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: GR32_ColorGradients
kind: Unit
summary: "Provides smooth color gradient generation, lookup tables, spatial gradient samplers, and polygon gradient fillers for Graphics32."
---

## Description

The `GR32_ColorGradients` unit implements a flexible color gradient framework in Graphics32. It provides multi-stop color gradient definitions, high-performance lookup table generation, spatial 2D gradient sampling, and polygon gradient filler renderers.

### Key Capabilities

1. **Color Stop & Lookup Table Management**:
   - [[TColor32Gradient]]: Manages an ordered list of color stops with position offsets ($0.0 \dots 1.0$), alpha blending support, and event notifications.
   - [[TColor32LookupTable]]: Pre-calculates 256 or 1024-entry ARGB color lookup tables for fast scanline rendering.

2. **Analytical & Center-Based Gradient Samplers**:
   - Linear gradients ([[TLinearGradientSampler]], [[TXGradientSampler]]).
   - Radial and focal-offset radial gradients ([[TRadialGradientSampler]], [[TRadialExGradientSampler]]).
   - Angular, conic, and sweep gradients ([[TConicGradientSampler]], [[TSweepGradientSampler]]).
   - Geometric shapes: Diamond ([[TDiamondGradientSampler]]), XY/XY-Sqrt ([[TXYGradientSampler]], [[TXYSqrtGradientSampler]]).

3. **Sparse-Point & Polygon Gradient Fillers**:
   - [[TBarycentricGradientSampler]] / [[TBarycentricGradientPolygonFiller]]: 3-point planar triangular interpolation.
   - [[TBilinearGradientSampler]]: 4-point quadrilateral bilinear interpolation.
   - [[TInvertedDistanceWeightingSampler]]: Shepard's method for arbitrary scatter point sets.
   - [[TVoronoiSampler]]: Voronoi diagram cell partitioning using Euclidean or Manhattan distance metrics.
   - [[TGourandShadedDelaunayTrianglesSampler]] / [[TGourandShadedDelaunayTrianglesPolygonFiller]]: Delaunay triangulation mesh shading.
   - Polygon fillers for high-speed scanline rasterization with anti-aliasing ([[TLinearGradientPolygonFiller]], [[TRadialGradientPolygonFiller]], [[TSVGRadialGradientPolygonFiller]]).

[members]
