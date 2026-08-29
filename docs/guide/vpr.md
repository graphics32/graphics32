# Vectorial Polygon Rasterizer (VPR)
## Technical Architecture and Algorithmic Reference Overview

This document provides a technical, deep-dive description of the **Vectorial Polygon Rasterizer (VPR)** algorithm implemented in Graphics32 (`Source/GR32_VPR.pas`). Designed and implemented by Mattias Andersson, VPR is an analytical coverage-based polygon rasterizer that computes exact pixel coverage for optimal anti-aliasing without the performance bottlenecks of traditional scanline edge-sorting rasterizers.

---

## 1. Introduction & Theoretical Background

Traditional high-quality vector rasterizers, such as those in **FreeType** or **Anti-Grain Geometry (AGG)**, compute anti-aliased coverage by tracking and sorting the horizontal crossings of polygon edges for every scanline. While highly accurate, the sorting step introduces an $O(N \log N)$ complexity bottleneck per scanline, where $N$ is the number of edge crossings.

**VPR** eliminates horizontal sorting entirely by leveraging a fundamental property of calculus: **the cumulative prefix sum (prefix integration)**.

### Core Architectural Concepts:
1. **Vertical Subdivision**
   All polygon edges are subdivided vertically so that each segment fragment is exactly bounded within a single scanline (local $Y \in [0, 1]$).
2. **Horizontal Crossing Propagation (1D Delta Buffers)**
   When an edge crosses a scanline boundary (specifically the bottom boundary at local $Y = 1$), it changes the winding state for *all* pixels to the right of that crossing. VPR represents this change as a local delta at the crossing $X$ coordinate. A subsequent horizontal cumulative prefix sum propagates this winding state across the scanline in $O(W)$ time (where $W$ is the scanline width).
3. **Local Signed Area Integration**
   For pixels that contain an active edge fragment, VPR analytically computes the local signed trapezoidal area under the segment within that pixel column and adds it directly to the cumulative-summed base.

By separating the **global boundary crossings** (handled via horizontal 1D delta propagation) and **local edge integration** (handled analytically per pixel containing an edge), VPR achieves perfect analytical accuracy with outstanding linear $O(W + N)$ performance.

---

## 2. Architectural Pipeline & Data Structures

VPR's rendering pipeline flows through several distinct phases:
```
+-----------------------------------------------------------+
| 1. Clipping: Clip polygon vertices against ClipRect       |
+-----------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------+
| 2. Y-Range Determination: Find YMin and YMax of vertices  |
+-----------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------+
| 3. Prefix-Count Allocation: Calculate exact segment counts|
|    per scanline using Y-interval delta accumulation       |
+-----------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------+
| 4. Subdivision (DivideSegment): Split edges into          |
|    scanline-high fragments (local Y within [0, 1])        |
+-----------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------+
| 5. Span Extraction (ExtractSingleSpan):                   |
|    - Apply crossing deltas at bottom boundary (Y = 1)     |
|    - Perform horizontal Cumulative Prefix Sum             |
|    - Apply analytical local trapezoidal integration       |
+-----------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------+
| 6. Rendering (FillSpan / RenderSpan):                     |
|    Convert analytical coverages to alpha and blend        |
+-----------------------------------------------------------+
```

### Key Data Structures

* **`TFloatPoint`**
  A 2D point coordinate represented by single or double-precision floats (`X, Y: TFloat`).
* **`TLineSegment`**
  An array of two `TFloatPoint` coordinates representing a segment bounded within a single scanline:
  
  ```pascal
  TLineSegment = array[0..1] of TFloatPoint;
  ```
* **`TScanLine`**
  Represents a horizontal scanline bucket containing divided edge segments:
  
  ```pascal
  TScanLine = record
    Segments: PLineSegmentArray;
    Count: Integer;
    Y: Integer;
  end;
  ```
* **`TValueSpan`**
  Defines the horizontal span of pixels on a scanline that require rendering:
  
  ```pascal
  TValueSpan = record
    LowX, HighX: Integer;
    Values: PSingleArray; // Pointer to the coverage values
  end;
  ```

---

## 3. Step-by-Step Algorithmic Walkthrough

### Step 1: Polygon Clipping
The input polygons (represented as `TArrayOfArrayOfFloatPoint`) are first clipped against the target bounding box (`ClipRect`) using `ClipPolygon`. This ensures that all vertices processed by the rasterizer lie within or on the boundaries of the viewport, preventing out-of-bounds memory accesses during coordinate-to-pixel mapping.

### Step 2: Y-Range & Segment-Count Determination
Rather than using dynamic resizing arrays or linked lists (which cause heap fragmentation and cache misses), VPR uses a **two-pass prefix count** optimization to pre-allocate memory for all scanlines.

1. **First Pass (Range Determination)**
   VPR scans all vertices to find the minimum (`YMin`) and maximum (`YMax`) integer scanlines.
2. **Second Pass (Segment-Count Delta Accumulation)**
   For each edge going from $Y_0$ to $Y_1$:
   
   - If the edge goes downwards ($Y_0 \le Y_1$), it increments the count at $Y_0$ and decrements it at $Y_1 + 1$:
     ```pascal
     Inc(pScanLines[Y0].Count);
     Dec(pScanLines[Y1 + 1].Count);
     ```
   - If the edge goes upwards ($Y_0 > Y_1$), it increments the count at $Y_1$ and decrements it at $Y_0 + 1$:
     ```pascal
     Inc(pScanLines[Y1].Count);
     Dec(pScanLines[Y0 + 1].Count);
     ```
3. **Prefix Sum Allocation**
   VPR computes the prefix sum of these counts across all scanlines. The prefix sum yields the exact number of segments that intersect each scanline. A single, contiguous block of memory is allocated for each scanline's segments:
   
   ```pascal
   SegmentCount := 0;
   for i := 0 to High(ScanLines) do
   begin
     Inc(SegmentCount, ScanLines[i].Count);
     GetMem(ScanLines[i].Segments, SegmentCount * SizeOf(TLineSegment));
     ScanLines[i].Count := 0; // Reset for actual population
   end;
   ```

### Step 3: Vertical Subdivision (`DivideSegment`)
Every clipped polygon edge is subdivided into scanline-high fragments where the local $Y$ coordinate is mapped to $[0, 1]$.

For a segment from $P_1$ to $P_2$:
* Let $Y_1 = \lfloor P_1.Y \rfloor$ and $Y_2 = \lfloor P_2.Y \rfloor$.
* If $Y_1 = Y_2$, the segment lies entirely within a single scanline. It is added directly with its $Y$ coordinates offset by $-Y_1$.
* If $Y_1 \ne Y_2$, the segment crosses scanline boundaries. The inverse slope $k = \frac{P_2.X - P_1.X}{P_2.Y - P_1.Y}$ is calculated.
* The segment is split at the horizontal grid lines $Y = \lfloor Y \rfloor + 1$ (for downward segments) or $Y = \lfloor Y \rfloor$ (for upward segments).
* The intermediate $X$ intersection coordinates are calculated linearly:
  $$X_{\text{intersect}} = P_1.X + (Y_{\text{grid}} - P_1.Y) \cdot k$$
* Each fractional and whole-scanline segment is added to its corresponding scanline bucket. To protect against floating-point rounding errors accumulating over long edges, $X$ is clamped using `Max(0, ...)` to prevent negative coordinate index underflow.

### Step 4: Span Extraction (`ExtractSingleSpan`)
For each scanline, VPR extracts the horizontal span of pixel coverage values. This is the heart of the VPR algorithm.

```
Scanline Y
+-------------------------------------------------------------------+
| Pixel X-1     | Pixel X       | Pixel X+1     | Pixel X+2         |
|               |               |               |                   |
|               |   Segment Start (Y=0)         |                   |
|               |       \       |               |                   |
|               |        \      |               |                   |
|               |         \     |               |                   |
|               |   Segment End (Y=1)           |                   |
+---------------+---------+-----+---------------+-------------------+
                          |
                   Crossing at Bottom Boundary
```

#### A. Apply Crossing Deltas at Bottom Boundary ($Y = 1$)
If an edge fragment intersects the bottom of the scanline ($P.Y = 1$ in local coordinates), it acts as a transition point between scanlines.
Let $X = \lfloor P.X \rfloor$ and $\text{fracX} = P.X - X$.
* For **downward segments** (odd-indexed points in the segment array, i.e., end-points):
  $$\text{SpanData}[X] \leftarrow \text{SpanData}[X] + (1 - \text{fracX})$$
  $$\text{SpanData}[X+1] \leftarrow \text{SpanData}[X+1] + \text{fracX}$$
* For **upward segments** (even-indexed points in the segment array, i.e., start-points):
  $$\text{SpanData}[X] \leftarrow \text{SpanData}[X] - (1 - \text{fracX})$$
  $$\text{SpanData}[X+1] \leftarrow \text{SpanData}[X+1] - \text{fracX}$$

#### B. Perform Horizontal Cumulative Prefix Sum
VPR runs a prefix integration (`CumSum`) from the minimum active $X$ coordinate (`LowX`) to the maximum active $X$ coordinate (`HighX`).
$$\text{SpanData}[x] \leftarrow \sum_{i = \text{LowX}}^{x} \text{SpanData}[i]$$
This propagates the boundary crossing transitions to all pixels to the right, establishing the base winding number coverage.

#### C. Local Trapezoidal Area Integration
Finally, VPR iterates over all segment fragments belonging to this scanline and accumulates their local analytical trapezoidal areas into the same `SpanData` buffer (see Section 4 for detailed math).

### Step 5: Color Mapping & Raster Blending
Once the exact coverage values are extracted into `SpanData`, they are mapped to alpha values based on the polygon's fill rule:
* **Even-Odd Fill Rule (`pfEvenOdd`)**:
  The coverage value $C$ is mapped using:
  $$V = | \lfloor C \cdot \text{0x10000} \rfloor | \pmod{\text{0x20000}}$$
  $$\text{if } V \ge \text{0x10000} \text{ then } V \leftarrow \text{0x1FFFF} \oplus V$$
* **Non-Zero / Winding Fill Rule (`pfWinding` / `pfNonZero`)**:
  The coverage value $C$ is mapped using:
  $$V = | \lfloor C \cdot \text{0x10000} \rfloor |$$
  $$\text{if } V > \text{0x10000} \text{ then } V \leftarrow \text{0x10000}$$

The resulting alpha value is combined with the paint color's alpha channel and blended onto the destination scanline buffer using `BlendLine` or `MergeLine` depending on the `CombineMode`.

---

## 4. Mathematical Principles of Local Edge Integration

The local analytical area integration within a pixel column is performed by the `IntegrateSegment` procedure. Let's analyze the exact mathematics behind it.

For a segment from $P_1 = (x_1, y_1)$ to $P_2 = (x_2, y_2)$ inside a single scanline, we know that $y_1, y_2 \in [0, 1]$.

```
        x1    x2
      +---------+
  y1  | *       |
      |   *     |
      |     *   |
  y2  |       * |
      +---------+
```

### Case A: Vertical Segment ($x_1 = x_2$)
If the segment is perfectly vertical within a pixel column, its horizontal width is zero. Thus, the segment itself covers zero area inside the vertical column strip.
$$\text{Area} = 0$$

### Case B: Non-Vertical Segment ($x_1 \ne x_2$)
The slope parameters are:
$$\Delta x = x_2 - x_1, \quad \Delta y = y_2 - y_1, \quad \frac{dy}{dx} = \frac{\Delta y}{\Delta x}$$

The line equation within the scanline is:
$$y(x) = y_1 + \frac{dy}{dx} \cdot (x - x_1)$$

#### 1. Left-to-Right Segments ($x_1 < x_2$)
* Let $X_1 = \lfloor x_1 \rfloor$ and $X_2 = \lfloor x_2 \rfloor$.
* **First Pixel ($X_1$)**:
  The horizontal span of the segment within the first pixel column is from $x_1$ to $X_1 + 1$.
  The width is $\text{fracX}\_1 = 1 - (x_1 - X_1)$.
  At the right boundary of the pixel ($x = X_1 + 1$), the vertical height is:
  $$Y_{\text{boundary}} = y_1 + \text{fracX}_1 \cdot \frac{dy}{dx}$$
  Using the trapezoidal rule, the area under the segment from $x_1$ to $X_1 + 1$ is:
  $$\text{Area} = \text{fracX}_1 \cdot \frac{y_1 + Y\_{\text{boundary}}}{2}$$
  The code computes this as:
  ```pascal
  Values[X1] := Values[X1] + 0.5 * (P1.Y + Y) * fracX1;
  ```

* **Intermediate Pixels ($i \in [X_1 + 1, X_2 - 1]$)**:
  For intermediate pixels, the horizontal span covers the entire pixel column width ($\Delta x_{\text{pixel}} = 1$).
  At the left boundary of pixel column $i$, the height is $Y$. At the right boundary, the height is $Y + \frac{dy}{dx}$.
  The trapezoidal area under the segment within this pixel is:
  $$\text{Area} = 1 \cdot \frac{Y + (Y + \frac{dy}{dx})}{2} = Y + 0.5 \cdot \frac{dy}{dx}$$
  The code accumulates this efficiently and increments the running $Y$:
  ```pascal
  Values[i] := Values[i] + (Y + DyDx * 0.5);
  Y := Y + DyDx;
  ```

* **Last Pixel ($X_2$)**:
  The horizontal span of the segment within the last pixel column is from $X_2$ to $x_2$.
  The width is $\text{fracX}_2 = x_2 - X_2$.
  The starting height is the running $Y$, and the ending height is $y_2$.
  The trapezoidal area under the segment within this pixel is:
  $$\text{Area} = \text{fracX}_2 \cdot \frac{Y + y_2}{2}$$
  The code computes this as:
  ```pascal
  Values[X2] := Values[X2] + 0.5 * (Y + P2.Y) * fracX2;
  ```

#### 2. Right-to-Left Segments ($x_1 > x_2$)
For right-to-left segments, the orientation is reversed. VPR computes the identical trapezoidal areas but **subtracts** them from the `SpanData` buffer. This elegant signed area formulation naturally implements the winding number mathematics without needing separate code paths or conditional branching for area orientation.

---

## 5. Algorithmic Pseudocode

The following pseudocode details the core logic of the VPR rasterizer.

### Extracting and Rasterizing Scanline Spans

```python
def ExtractSingleSpan(scanline, span_data):
    # Initialize span bounds
    low_x = infinity
    high_x = -infinity

    # Step A: Apply bottom-boundary (Y = 1) crossing deltas
    # scanline.segments consists of segment endpoints: [P0, P1, P2, P3, ...]
    points = scanline.segments
    n = scanline.count * 2

    for i in range(n):
        P = points[i]
        X = floor(P.X)

        # Track active horizontal boundaries of the scanline
        if X < low_x:
            low_x = X

        if P.Y == 1:
            fracX = P.X - X
            if i % 2 == 1: # Right edge (downward segment endpoint)
                span_data[X] += (1.0 - fracX)
                X += 1
                span_data[X] += fracX
            else:          # Left edge (upward segment startpoint)
                span_data[X] -= (1.0 - fracX)
                X += 1
                span_data[X] -= fracX

        if X > high_x:
            high_x = X

    # Step B: Perform horizontal Cumulative Prefix Sum
    # This propagates 1D boundary crossing deltas across the scanline
    cumulative_sum = 0.0
    for x in range(low_x, high_x + 1):
        cumulative_sum += span_data[x]
        span_data[x] = cumulative_sum

    # Step C: Integrate local segment areas analytically
    for i in range(scanline.count):
        segment = scanline.segments[i]
        IntegrateSegment(segment[0], segment[1], span_data)

    return low_x, high_x

def IntegrateSegment(P1, P2, span_data):
    X1 = floor(P1.X)
    X2 = floor(P2.X)

    # Perfectly vertical segment inside a single pixel strip
    if X1 == X2:
        span_data[X1] += 0.5 * (P2.X - P1.X) * (P1.Y + P2.Y)
        return

    Dx = P2.X - P1.X
    Dy = P2.Y - P1.Y
    DyDx = Dy / Dx

    # Left-to-Right orientation
    if X1 < X2:
        fracX1 = 1.0 - (P1.X - X1)
        fracX2 = P2.X - X2

        Y = P1.Y + fracX1 * DyDx

        # Integrate first fractional pixel column
        span_data[X1] += 0.5 * (P1.Y + Y) * fracX1

        # Integrate intermediate whole pixel columns
        for x in range(X1 + 1, X2):
            span_data[x] += Y + DyDx * 0.5
            Y += DyDx

        # Integrate last fractional pixel column
        span_data[X2] += 0.5 * (Y + P2.Y) * fracX2

    # Right-to-Left orientation
    else:
        fracX1 = P1.X - X1
        fracX2 = 1.0 - (P2.X - X2)

        Y = P1.Y - fracX1 * DyDx

        # Subtract integrated first fractional pixel column
        span_data[X1] -= 0.5 * (P1.Y + Y) * fracX1

        # Subtract integrated intermediate whole pixel columns
        for x in range(X1 - 1, X2, -1):
            span_data[x] -= Y - DyDx * 0.5
            Y -= DyDx

        # Subtract integrated last fractional pixel column
        span_data[X2] -= 0.5 * (Y + P2.Y) * fracX2
```

---

## 6. Key Performance Optimizations

VPR contains several low-level optimizations that make it one of the fastest analytical vector rasterizers available:

1. **No Horizontal Sorting**:
   By using 1D delta buffers and a single horizontal `CumSum` pass, VPR completely avoids sorting crossing coordinates horizontally. This turns an $O(N \log N)$ operation into an $O(N)$ population pass plus a fast $O(W)$ prefix integration.
2. **Segment Count Prefix Pre-allocation**:
   The use of the Y-interval delta accumulation (`pScanLines[Y0].Count++`, `pScanLines[Y1+1].Count--`) allows VPR to compute the exact size of the segment array needed for each scanline *before* allocating memory. This enables allocating a single, contiguous block of memory per scanline, maximizing CPU L1/L2 cache locality.
3. **Optimized Floor/Ceil Routines (`PolyFloor`/`PolyCeil`)**:
   Delphi’s standard `Trunc` function is notoriously slow on x86 because it modifies the hardware FPU control word. VPR bypasses this by implementing optimized assembly or fast SSE routines (`FastFloorSingle`/`FastFloorDouble`) to perform floor and ceil operations without altering the FPU state.
4. **Active Span Clipping**:
   The horizontal rendering bounds are clamped to `ClipX1` and `ClipX2` immediately after span extraction. This ensures that regions outside the clipping rectangle are skipped during cumulative summing and local integration, minimizing wasted CPU cycles.
5. **Fast Reset of Coverage Buffers**:
   After a span is rendered, the `SpanData` buffer is reset back to zero only within the active horizontal span bounds using `FillLongWord`. This avoids the overhead of clearing the entire viewport width, keeping the algorithm cache-friendly even on extremely wide viewports.
