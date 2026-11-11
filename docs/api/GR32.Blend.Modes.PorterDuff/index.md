---
layout: doc
docType: api
unit: GR32.Blend.Modes.PorterDuff
entity: GR32.Blend.Modes.PorterDuff
kind: Unit
aliases: [TGraphics32BlenderClear, TGraphics32BlenderSrc, TGraphics32BlenderSrcOver, TGraphics32BlenderSrcIn, TGraphics32BlenderSrcOut, TGraphics32BlenderSrcAtop, TGraphics32BlenderDest, TGraphics32BlenderDestOver, TGraphics32BlenderDestIn, TGraphics32BlenderDestOut, TGraphics32BlenderDestAtop, TGraphics32BlenderXor]
summary: "Provides the 12 classic Porter-Duff compositing operators for Graphics32."
seealso:
  - "[[GR32_Blend]]"
  - "[[GR32.Blend.Modes]]"
  - "[[GR32.Blend.Modes.PhotoShop]] Adobe Photoshop blend modes"
  - "[[GR32.Blend.Modes.Extra]] Additional blend modes"
---

## Description

The `GR32.Blend.Modes.PorterDuff` unit implements the 12 fundamental digital image compositing operators defined by Thomas Porter and Tom Duff in their seminal 1984 SIGGRAPH paper, *"Compositing Digital Images"*.

### Understanding Porter-Duff Compositing

When two semi-transparent image layers (a **Source** foreground layer $S$ and a **Destination** background layer $D$) overlap, each pixel can be divided geometrically into four distinct coverage regions based on the opacity of the source ($\alpha_S$) and destination ($\alpha_D$):

1. **Neither shape**: Completely transparent background where neither $S$ nor $D$ is present.
2. **Source only**: Region covered by $S$ but outside $D$, with coverage $(1 - \alpha_D)$.
3. **Destination only**: Region covered by $D$ but outside $S$, with coverage $(1 - \alpha_S)$.
4. **Both shapes**: Overlapping intersection region covered by both $S$ and $D$, with coverage $\alpha_S \cdot \alpha_D$.

By assigning binary inclusion factors ($0$ or $1$) to regions 2, 3, and 4, Porter and Duff established an algebraic framework yielding exactly 12 primary compositing operators.

In general, the resulting alpha $\alpha_R$ and color components $C_R$ are calculated using source weight factor $F_S$ and destination weight factor $F_D$:

$$\alpha_R = \alpha_S \cdot F_S + \alpha_D \cdot F_D$$

$$C_R = \frac{C_S \cdot \alpha_S \cdot F_S + C_D \cdot \alpha_D \cdot F_D}{\alpha_R}$$

### Common Applications

Porter-Duff blend modes are universally used across modern 2D computer graphics systems, including HTML5 Canvas 2D (`globalCompositeOperation`), SVG compositing, Android (`PorterDuff.Mode`), Apple Quartz 2D / Core Graphics, and Skia:

- **Standard Layer Alpha Blending**: `SrcOver` is the default compositing mode for UI rendering, drawing elements over existing backgrounds.
- **Masking & Clipping**: `SrcIn` clips a source texture into a destination mask shape, while `DestIn` uses a source brush to mask existing background content.
- **Punch-outs & Erasing**: `SrcOut` and `DestOut` create cutouts, transparency holes, or rubber-erase effects where layers intersect.
- **Texture Overlays**: `SrcAtop` composites source patterns exclusively onto visible parts of an existing destination graphic (e.g. adding specular highlights or textures to an icon without spilling past its boundary).
- **Background Insertion**: `DestOver` inserts new background graphics behind existing rendered elements.
- **Clearing Regions**: `Clear` zeroes out pixels to make rectangular or polygonal regions completely transparent.

---

## Summary Table

| ID | Name | Class | Example |
| --- | --- | --- | --- |
| `Clear` | Clear | [[TGraphics32BlenderClear]] | ![](/images/compositing-porter-duff-clear.svg) |
| `Src` | Source | [[TGraphics32BlenderSrc]] | ![](/images/compositing-porter-duff-src.svg) |
| `SrcOver` | Source Over | [[TGraphics32BlenderSrcOver]] | ![](/images/compositing-porter-duff-srcover.svg) |
| `SrcIn` | Source In | [[TGraphics32BlenderSrcIn]] | ![](/images/compositing-porter-duff-srcin.svg) |
| `SrcOut` | Source Out | [[TGraphics32BlenderSrcOut]] | ![](/images/compositing-porter-duff-srcout.svg) |
| `SrcAtop` | Source Atop | [[TGraphics32BlenderSrcAtop]] | ![](/images/compositing-porter-duff-srcatop.svg) |
| `Dest` | Destination | [[TGraphics32BlenderDest]] | ![](/images/compositing-porter-duff-dest.svg) |
| `DestOver` | Destination Over | [[TGraphics32BlenderDestOver]] | ![](/images/compositing-porter-duff-destover.svg) |
| `DestIn` | Destination In | [[TGraphics32BlenderDestIn]] | ![](/images/compositing-porter-duff-destin.svg) |
| `DestOut` | Destination Out | [[TGraphics32BlenderDestOut]] | ![](/images/compositing-porter-duff-destout.svg) |
| `DestAtop` | Destination Atop | [[TGraphics32BlenderDestAtop]] | ![](/images/compositing-porter-duff-destatop.svg) |
| `Xor` | Xor | [[TGraphics32BlenderXor]] | ![](/images/compositing-porter-duff-xor.svg) |

---

## The 12 Porter-Duff Blend Modes

### Source

::: right
![](/images/compositing-porter-duff-src.svg)
:::

**ID**: `Src`<br>
**Name**: Source<br>
**Class**: [[TGraphics32BlenderSrc]]

The source pixel is copied directly to the destination pixel, completely replacing the destination color and alpha regardless of opacity. The destination pixel is not used as input.

---

### Source Over

::: right
![](/images/compositing-porter-duff-srcover.svg)
:::

**ID**: `SrcOver`<br>
**Name**: Source Over<br>
**Class**: [[TGraphics32BlenderSrcOver]]

The source pixel is composited over the destination pixel. This is the standard, default alpha blending operation where semi-transparent source colors blend smoothly on top of existing background pixels.

---

### Source In

::: right
![](/images/compositing-porter-duff-srcin.svg)
:::

**ID**: `SrcIn`<br>
**Name**: Source In<br>
**Class**: [[TGraphics32BlenderSrcIn]]

The source pixel replaces the destination pixel only where the destination pixel has alpha coverage. Regions of the source lying outside the destination boundary are discarded, effectively using the destination as a clip mask for the source.

---

### Source Out

::: right
![](/images/compositing-porter-duff-srcout.svg)
:::

**ID**: `SrcOut`<br>
**Name**: Source Out<br>
**Class**: [[TGraphics32BlenderSrcOut]]

The source pixel replaces the destination pixel only where the destination pixel is transparent (outside the destination boundary). Regions of the source that overlap the destination are rendered transparent.

---

### Source Atop

::: right
![](/images/compositing-porter-duff-srcatop.svg)
:::

**ID**: `SrcAtop`<br>
**Name**: Source Atop<br>
**Class**: [[TGraphics32BlenderSrcAtop]]

The source pixel is composited onto the destination pixel only within the bounds of the destination alpha coverage. The destination backdrop remains visible outside the source, but where they overlap, the source is blended over the destination.

---

### Destination

::: right
![](/images/compositing-porter-duff-dest.svg)
:::

**ID**: `Dest`<br>
**Name**: Destination<br>
**Class**: [[TGraphics32BlenderDest]]

The destination pixel is left completely untouched. The source pixel is ignored and has no visual effect.

---

### Destination Over

::: right
![](/images/compositing-porter-duff-destover.svg)
:::

**ID**: `DestOver`<br>
**Name**: Destination Over<br>
**Class**: [[TGraphics32BlenderDestOver]]

The destination pixel is composited over the source pixel. The source pixel appears underneath the existing background, filling in transparent areas of the destination.

---

### Destination In

::: right
![](/images/compositing-porter-duff-destin.svg)
:::

**ID**: `DestIn`<br>
**Name**: Destination In<br>
**Class**: [[TGraphics32BlenderDestIn]]

The destination pixel is retained only where the source pixel has alpha coverage. Areas of the destination outside the source shape are rendered completely transparent, effectively using the source as a cutting mask.

---

### Destination Out

::: right
![](/images/compositing-porter-duff-destout.svg)
:::

**ID**: `DestOut`<br>
**Name**: Destination Out<br>
**Class**: [[TGraphics32BlenderDestOut]]

The destination pixel is retained only where the source pixel is transparent. Areas of the destination overlapping the source shape are erased, creating a transparent cutout or hole.

---

### Destination Atop

::: right
![](/images/compositing-porter-duff-destatop.svg)
:::

**ID**: `DestAtop`<br>
**Name**: Destination Atop<br>
**Class**: [[TGraphics32BlenderDestAtop]]

The destination pixel is composited over the source pixel within the bounds of the source alpha coverage. Areas of the source outside the destination remain visible, while overlapping regions blend the destination over the source.

---

### Clear

::: right
![](/images/compositing-porter-duff-clear.svg)
:::

**ID**: `Clear`<br>
**Name**: Clear<br>
**Class**: [[TGraphics32BlenderClear]]

The target region is cleared to complete transparency ($00000000$). Neither the source nor the destination pixel colors contribute to the result.

---

### Xor

::: right
![](/images/compositing-porter-duff-xor.svg)
:::

**ID**: `Xor`<br>
**Name**: Xor<br>
**Class**: [[TGraphics32BlenderXor]]

Combines the non-overlapping parts of the source and destination. Regions where both source and destination overlap are cleared to transparent, while non-overlapping portions of both shapes remain visible.

[members]


