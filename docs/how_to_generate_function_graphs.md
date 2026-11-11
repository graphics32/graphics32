# How to Generate Function Graphs

This internal documentation guide describes how the SVG function plot diagrams (such as `docs/images/plot-fmod.svg`, `docs/images/plot-floatmod.svg`, `docs/images/plot-wrapfloat.svg`, `docs/images/plot-clamp.svg`, `docs/images/plot-wrap.svg`, `docs/images/plot-mirror.svg`, and `docs/images/plot-reflect.svg`) are generated and maintained. It is intended for maintainers and AI coding agents.

---

## 1. Overview & Purpose

Mathematical routines in `GR32_Math` and `GR32_LowLevel` (e.g., modulo operations like `FMod` and `FloatMod`, floating point `Wrap`, and integer range boundary functions like `Clamp`, `Wrap`, `Mirror`, and `Reflect`) include 2D plot diagrams to visually demonstrate their exact input-to-output mapping behavior across positive and negative input domains.

- **Output Directory**: `docs/images/`
- **Filename Convention**: `plot-<function_name>.svg` (e.g. `plot-clamp.svg`, `plot-wrap.svg`, `plot-wrapfloat.svg`, `plot-mirror.svg`, `plot-reflect.svg`, `plot-fmod.svg`, `plot-floatmod.svg`)
- **Format**: Scalable Vector Graphics (SVG), 600px $\times$ 300px (`viewBox="0 0 600 300"`)

---

## 2. Integer Function Plots Specifications (`Clamp`, `Wrap`, `Mirror`, `Reflect`)

For integer boundary functions (`Clamp`, `Wrap`, `Mirror`, `Reflect` in `GR32_LowLevel`), plot diagrams follow strict visual parameters:

| Parameter / Spec | Value / Description |
|---|---|
| **X-Axis Range** | $X \in [0..20]$ (20 units domain). |
| **Parameters** | $Min = 0$, $Max = 3$. |
| **Tick Marks** | Every 1 integer unit on both X and Y axes. |
| **Text Labels** | Every 5 units on X-axis ($0, 5, 10, 15, 20$), every 1 unit on Y-axis ($0, 1, 2, 3$). |
| **Integer Markers** | Small round red circle markers (`#D40000` / `$FFD40000`) for each integer result $x \in 0..20$ (`r="3.5"`). |
| **Line Between Markers** | Blue stroke (`#0066cc`, `stroke-width="2"`), rendered below markers in Z-order. |
| **Axes & Text Labels** | `#7f7f7f` (`$FF7F7F7F` in ARGB) with gridlines (`stroke-dasharray="4,4"`). |

---

## 3. Function Behavior & Domain Specifications

### A. Floating-Point Modulo & Wrap Routines (`GR32_Math` & `GR32_LowLevel`)
1. **`FMod(x, 3.5)`**:
   - Truncating division modulo: $Result = x - 3.5 \cdot \text{Trunc}(x / 3.5)$.
   - Domain $x \in [-10, 10]$.
   - Output range is $[0, 3.5)$ for positive $x$, and $(-3.5, 0]$ for negative $x$.
2. **`FloatMod(x, 3.5)`**:
   - Floored division modulo: $Result = x - 3.5 \cdot \text{Floor}(x / 3.5)$.
   - Domain $x \in [-10, 10]$.
   - Periodic sawtooth wave strictly constrained to $[0, 3.5)$ for all $x$.
3. **`Wrap(x, 3.5: Single)`**:
   - Single-precision floating-point wrap: constrained to $[0, 3.5)$ range (`plot-wrapfloat.svg`).
   - Domain $x \in [-10, 10]$. Sawtooth wave repeating every $Max = 3.5$ units.

### B. Integer Range Constraining Routines (`GR32_LowLevel`)
1. **`Clamp(x, 0, 3)`**:
   - Clamps integer values below $0$ to $0$ and values above $3$ to $3$.
   - Output for $x \in [0..20]$: `[0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]`.
2. **`Wrap(x, 0, 3)`**:
   - Wraps integer values into $[0, 3]$ range with modulo/wrap-around ($Range = Max - Min + 1 = 4$).
   - Output for $x \in [0..20]$: `[0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0]`.
3. **`Mirror(x, 0, 3)`**:
   - Mirrors integer values at range boundaries with symmetry around the edge value. Cycle length is $2 \cdot Max = 6$.
   - Output for $x \in [0..20]$: `[0, 1, 2, 3, 2, 1, 0, 1, 2, 3, 2, 1, 0, 1, 2, 3, 2, 1, 0, 1, 2]`.
4. **`Reflect(x, 0, 3)`**:
   - Reflects integer values with symmetry around the outer edge pixel ($Max + 1$). Cycle length is $2 \cdot (Max + 1) = 8$.
   - Output for $x \in [0..20]$: `[0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3]`.

---

## 4. Embedding in API Documentation

To embed generated function graphs in API documentation pages, use standard HTML `<img />` tags in the markdown body:

```html
<img src="/images/plot-wrapfloat.svg" alt="Wrap Float Plot" style="width:100%; max-width:600px; margin:1rem 0;" />
```

or simply as markdown:

```markdown
![Wrap Float Plot](/images/plot-wrapfloat.svg)
```