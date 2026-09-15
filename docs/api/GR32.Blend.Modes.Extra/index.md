---
layout: doc
docType: api
unit: GR32.Blend.Modes.Extra
entity: GR32.Blend.Modes.Extra
kind: Unit
aliases: [TGraphics32BlenderErase, TGraphics32BlenderMask, TGraphics32BlenderAlpha]
summary: "Provides utility alpha channel masking and erasing blend modes for Graphics32."
seealso:
  - "[[GR32_Blend]]"
  - "[[GR32.Blend.Modes]]"
  - "[[GR32.Blend.Modes.PorterDuff]]"
  - "[[GR32.Blend.Modes.PhotoShop]]"
---

## Description

The `GR32.Blend.Modes.Extra` unit provides specialized utility blend modes for alpha-channel manipulation, transparency masking, and image erasing in Graphics32.

### Alpha Channel Modulation

Unlike standard color blend modes (such as Multiply, Screen, or Overlay) that modify RGB color channels, the extra blend modes (`Erase`, `Mask`, and `Alpha`) preserve the original backdrop RGB color components intact ($C_R = C_B$) while directly modulating the backdrop's alpha channel ($\alpha_B$):

- **Erase**: Inverse alpha scaling. Reduces backdrop opacity proportional to source opacity ($\alpha_R = \alpha_B \cdot (1 - \alpha_S)$), creating rubber-erase, cutout, or dissolve effects.
- **Mask**: Direct alpha scaling. Masks backdrop opacity proportional to source opacity ($\alpha_R = \alpha_B \cdot \alpha_S$), preserving backdrop colors only where the source mask is opaque.
- **Alpha**: Modulates backdrop opacity based on both source alpha and source color intensity ($\alpha_R = \alpha_B \cdot \alpha_S \cdot I_S$), allowing grayscale images or luminance textures to act as dynamic transparency masks.

---

## Summary Table

| ID | Name | Class | Formula / Behavior |
| --- | --- | --- | --- |
| `erase` | Erase | [[TGraphics32BlenderErase]] | $\alpha_R = \frac{\alpha_B \cdot (255 - \alpha_S)}{255}$ |
| `mask` | Mask | [[TGraphics32BlenderMask]] | $\alpha_R = \frac{\alpha_B \cdot \alpha_S}{255}$ |
| `alpha` | Alpha | [[TGraphics32BlenderAlpha]] | $\alpha_R = \frac{\alpha_B \cdot \alpha_S \cdot \text{Intensity}(S)}{255^2}$ |

---

## Extra Blend Modes

### Erase

**ID**: `erase`<br>
**Name**: Erase<br>
**Class**: [[TGraphics32BlenderErase]]

Scales the backdrop alpha channel by the inverse of the source alpha channel ($\alpha_R = \alpha_B \cdot (1 - \alpha_S)$). The source RGB color channels are ignored. Painting with an opaque brush ($\alpha_S = 255$) completely erases the target backdrop pixel to zero alpha, while semi-transparent brushes partially erase the background.

---

### Mask

**ID**: `mask`<br>
**Name**: Mask<br>
**Class**: [[TGraphics32BlenderMask]]

Scales the backdrop alpha channel directly by the source alpha channel ($\alpha_R = \alpha_B \cdot \alpha_S$). The source RGB color channels are ignored. Retains backdrop opacity where the source is opaque, and renders backdrop pixels transparent where the source mask is transparent.

---

### Alpha

**ID**: `alpha`<br>
**Name**: Alpha<br>
**Class**: [[TGraphics32BlenderAlpha]]

Scales the backdrop alpha channel by both the source alpha channel and the average RGB color intensity of the source pixel ($\alpha_R = \alpha_B \cdot \alpha_S \cdot \frac{R_S + G_S + B_S}{3}$). Allows grayscale images, alpha maps, or color textures to serve directly as transparency masks.

[members]
