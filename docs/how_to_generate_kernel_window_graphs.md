# How to Generate Kernel Window Graphs

This internal documentation guide outlines how the SVG kernel window function diagrams in `docs/images/kernel-window-<name>.svg` are generated and maintained. It is intended for internal use by maintainers and AI coding agents.

---

## 1. Overview & Location

Resampling kernels in `GR32_Resamplers` (such as `TBoxKernel`, `TLinearKernel`, `TCubicKernel`, `TLanczosKernel`, `TSinshKernel`, etc.) include visual 2D plot diagrams depicting their 1D spatial window function curves $f(x)$ over their effective radius range.

- **Output Directory**: `docs/images/`
- **Filename Convention**: `kernel-window-<kernel_name>.svg` (e.g. `kernel-window-lanczos.svg`, `kernel-window-sinsh.svg`)
- **Format**: Scalable Vector Graphics (SVG), 600px $\times$ 360px (`viewBox="0 0 600 360"`)

---

## 2. Color Palette & Styling Specification

All kernel window SVG graphs follow strict visual branding guidelines:

| Element | Color Code / Spec | Description |
|---|---|---|
| **Background Fill** | `#1B1B1F` with 25% opacity | Dark container background (`fill-opacity="0.25"`). |
| **Grid Lines** | `#5F9F5F` (opacity 0.6) | Minor gridlines representing coordinate steps. |
| **Coordinate Axes ($x=0, y=0$)** | `#BFFFBF` (opacity 0.6) | Prominent axis lines at zero origin (`stroke-width="2"`). |
| **Kernel Curve** | `#008800` | Smooth plot curve path (`stroke-width="2.5"`). |
| **Area Fill** | `#004000` with 75% opacity | Filled area bounded under the curve down to $y=0$ axis. |

---

## 3. Mathematical Models & Python Generator Script

The SVG diagrams are generated programmatically using a Python script that evaluates the exact filter equations defined in `Source/GR32_Resamplers.pas`.

### Generation Script Example (`generate_kernel_svgs.py`)

```python
import math
import os

os.makedirs("docs/images", exist_ok=True)

# Kernel mathematical filter functions according to Source/GR32_Resamplers.pas
def box(x):
    return 1.0 if abs(x) <= 0.5 else 0.0

def linear(x):
    x = abs(x)
    return 1.0 - x if x <= 1.0 else 0.0

def cubic(x, b=0.0, c=0.5): # Catmull-Rom default (b=0, c=0.5)
    x = abs(x)
    if x < 1.0:
        return ((12 - 9*b - 6*c)*x**3 + (-18 + 12*b + 6*c)*x**2 + (6 - 2*b)) / 6.0
    elif x < 2.0:
        return ((-b - 6*c)*x**3 + (6*b + 30*c)*x**2 + (-12*b - 48*c)*x + (8*b + 24*c)) / 6.0
    return 0.0

def lanczos(x, n=3):
    x = abs(x)
    if x == 0:
        return 1.0
    if x < n:
        return (n * math.sin(math.pi * x) * math.sin(math.pi * x / n)) / ((math.pi * x) ** 2)
    return 0.0

def mitchell(x):
    return cubic(x, b=1.0/3.0, c=1.0/3.0)

def gaussian(x, sigma=0.5):
    x = abs(x)
    if x <= 1.5:
        return math.exp(-x*x / (2 * sigma * sigma))
    return 0.0

def sinsh(x):
    v = abs(x)
    if v < 1e-5:
        return 1.0
    elif v < 3.0:
        # Pascal: Sin(Value * Pi) / (Value * Pi) * Sinh(Value * Pi) / (Value * Pi)
        term1 = math.sin(v * math.pi) / (v * math.pi)
        term2 = math.sinh(v * math.pi) / (v * math.pi)
        return term1 * term2
    return 0.0

kernels = {
    "box": (box, 1.5, 1.2, 0.4),
    "linear": (linear, 1.5, 1.2, 0.4),
    "cubic": (cubic, 2.5, 1.2, 0.4),
    "lanczos": (lanczos, 3.5, 1.2, 0.4),
    "mitchell": (mitchell, 2.5, 1.2, 0.4),
    "gaussian": (gaussian, 2.0, 1.2, 0.4),
    "sinsh": (sinsh, 3.5, 2.5, 2.5),
}

width = 600
height = 360

for name, (fn, max_x, max_y, min_y) in kernels.items():
    filename = f"docs/images/kernel-window-{name}.svg"
    margin_x, margin_y = 40, 30
    
    def map_x(x):
        return margin_x + (x + max_x) / (2 * max_x) * (width - 2 * margin_x)
    
    def map_y(y):
        return (height - margin_y) - (y + min_y) / (max_y + min_y) * (height - 2 * margin_y)
    
    svg_parts = []
    svg_parts.append(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}" width="100%" height="100%">')
    svg_parts.append(f'  <rect width="{width}" height="{height}" fill="#1B1B1F" fill-opacity="0.25" rx="6" />')
    
    # Render axes, gridlines, curve paths, and filled polygon...
    # (See generate_kernel_svgs.py for full rendering details)
```

---

## 4. Maintenance & Workflow Instructions

1. **Adding a New Kernel**:
   - Add the kernel filter formula function into the script.
   - Specify the domain radius `max_x`, upper Y range `max_y`, and lower Y range `min_y` matching the kernel's peak and negative sidelobe excursions.
   - Run the script to output the corresponding SVG file into `docs/images/kernel-window-<name>.svg`.
2. **Embedding in API Documentation**:
   - Reference the SVG in the class overview page (`docs/api/GR32_Resamplers/Classes/<ClassName>/index.md`):
     ```html
     <img src="/images/kernel-window-<name>.svg" alt="<Kernel> Window" style="width:100%; max-width:600px; margin:1rem 0;" />
     ```
