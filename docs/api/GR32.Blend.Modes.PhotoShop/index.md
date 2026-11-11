---
layout: doc
docType: api
unit: GR32.Blend.Modes.PhotoShop
entity: GR32.Blend.Modes.PhotoShop
kind: Unit
aliases: [TGraphics32BlenderDarken, TGraphics32BlenderMultiply, TGraphics32BlenderColorBurn, TGraphics32BlenderLinearBurn, TGraphics32BlenderDarkerColor, TGraphics32BlenderLighten, TGraphics32BlenderScreen, TGraphics32BlenderColorDodge, TGraphics32BlenderLinearDodge, TGraphics32BlenderLighterColor, TGraphics32BlenderOverlay, TGraphics32BlenderSoftLight, TGraphics32BlenderHardLight, TGraphics32BlenderVividLight, TGraphics32BlenderLinearLight, TGraphics32BlenderPinLight, TGraphics32BlenderHardMix, TGraphics32BlenderDifference, TGraphics32BlenderExclusion, TGraphics32BlenderNegation, TGraphics32BlenderHue, TGraphics32BlenderSaturation, TGraphics32BlenderColor, TGraphics32BlenderLuminance, ColorLightness, ColorLuminance, ColorAverage, ColorBrightness]
summary: "Provides Adobe Photoshop and W3C compliant separable, non-separable, and component blend modes for Graphics32."
seealso:
  - "[[GR32_Blend]]"
  - "[[GR32.Blend.Modes]]"
  - "[[GR32.Blend.Modes.PorterDuff]] Porter-Duff blend modes"
  - "[[GR32.Blend.Modes.Extra]] Additional blend modes"
---

## Description

The `GR32.Blend.Modes.PhotoShop` unit implements the full suite of standard Adobe Photoshop and W3C Compositing and Blending Specification blending algorithms for Graphics32.

### Overview of Photoshop Blend Modes

Unlike Porter-Duff operators, which primarily determine geometry-based alpha coverage cutouts and layer intersections, Photoshop blend modes blend color channel values ($C_S$ foreground/source and $C_B$ background/backdrop) according to non-linear color algebra and component-based color space transformations.

The blend modes in `GR32.Blend.Modes.PhotoShop` are organized into 5 functional groups:

| Group | Description |
| --- | --- |
| **Darken** | Always produce a result color that is at least as dark as either constituent color. |
| **Lighten** | Always produce a result color that is at least as light as either constituent color. |
| **Contrast** | Increase or decrease contrast based on whether the source color component is lighter or darker than 50% neutral gray. |
| **Inversion** | Highlight differences or invert colors between source and backdrop layers. |
| **Component** | Blend color attributes based on Hue, Saturation, and Luminosity (Luminance). |

---

## Luminance & Lightness Functions

The unit includes cross-platform inline helper routines to compute lightness, luminance, brightness, and average intensity values from 32-bit `TColor32` values or RGB byte components:

| Routine | Formula / Standard | Description |
| --- | --- | --- |
| `ColorLightness` | $\frac{\max(R,G,B) + \min(R,G,B)}{2}$ | Calculates HSL lightness (Chroma midpoint). |
| `ColorLuminance` | $0.30 R + 0.59 G + 0.11 B$ | Calculates perceived Rec. 601 NTSC luminance (Luma). |
| `ColorAverage` | $\frac{R + G + B}{3}$ | Calculates unweighted average RGB intensity. |
| `ColorBrightness` | $\max(R,G,B)$ | Calculates HSV Value (maximum color component). |

---

## Summary Table

### Darken group

| ID | Name | Class | Formula / Behavior |
| --- | --- | --- | --- |
| `Darken` | Darken | [[TGraphics32BlenderDarken]] | $B \le F ? B : F$ |
| `multiply` | Multiply | [[TGraphics32BlenderMultiply]] | $\frac{B \cdot F}{255}$ |
| `ColorBurn` | Color Burn | [[TGraphics32BlenderColorBurn]] | $255 - \frac{(255 - B) \cdot 255}{F}$ |
| `LinearBurn` | Linear Burn | [[TGraphics32BlenderLinearBurn]] | $\max(0, B + F - 255)$ |
| `Darker` | Darker Color | [[TGraphics32BlenderDarkerColor]] | Compares overall luminance; retains darker pixel. |

### Lighten group

| ID | Name | Class | Formula / Behavior |
| --- | --- | --- | --- |
| `Lighten` | Lighten | [[TGraphics32BlenderLighten]] | $B \ge F ? B : F$ |
| `screen` | Screen | [[TGraphics32BlenderScreen]] | $255 - \frac{(255 - B)(255 - F)}{255}$ |
| `ColorDodge` | Color Dodge | [[TGraphics32BlenderColorDodge]] | $\min(255, \frac{B \cdot 255}{255 - F})$ |
| `Add` | Linear Dodge (Add) | [[TGraphics32BlenderLinearDodge]] | $\min(255, B + F)$ |
| `Lighter` | Lighter Color | [[TGraphics32BlenderLighterColor]] | Compares overall luminance; retains lighter pixel. |

### Contrast group

| ID | Name | Class | Formula / Behavior |
| --- | --- | --- | --- |
| `Overlay` | Overlay | [[TGraphics32BlenderOverlay]] | Screens or multiplies based on backdrop $B$. |
| `SoftLight` | Soft Light | [[TGraphics32BlenderSoftLight]] | Diffused spotlight effect based on source $F$. |
| `HardLight` | Hard Light | [[TGraphics32BlenderHardLight]] | Harsh spotlight effect based on source $F$. |
| `VividLight` | Vivid Light | [[TGraphics32BlenderVividLight]] | Dodges or burns by adjusting contrast. |
| `LinearLight` | Linear Light | [[TGraphics32BlenderLinearLight]] | Dodges or burns by adjusting brightness. |
| `PinLight` | Pin Light | [[TGraphics32BlenderPinLight]] | Replaces color depending on 50% gray threshold. |
| `HardMix` | Hard Mix | [[TGraphics32BlenderHardMix]] | Thresholds channels to 0 or 255 primary colors. |

### Inversion group

| ID | Name | Class | Formula / Behavior |
| --- | --- | --- | --- |
| `Difference` | Difference | [[TGraphics32BlenderDifference]] | $\|B - F\|$ |
| `Exclusion` | Exclusion | [[TGraphics32BlenderExclusion]] | $F + B - \frac{2 \cdot F \cdot B}{255}$ |
| `Negation` | Negation | [[TGraphics32BlenderNegation]] | $255 - \|B - F\|$ |

### Component group

| ID | Name | Class | Formula / Behavior |
| --- | --- | --- | --- |
| `Hue` | Hue | [[TGraphics32BlenderHue]] | Combines source Hue with backdrop Saturation and Luminance. |
| `Saturation` | Saturation | [[TGraphics32BlenderSaturation]] | Combines source Saturation with backdrop Hue and Luminance. |
| `Color` | Color | [[TGraphics32BlenderColor]] | Combines source Hue and Saturation with backdrop Luminance. |
| `Luminosity` | Luminosity | [[TGraphics32BlenderLuminance]] | Combines source Luminance with backdrop Hue and Saturation. |

---

## Darken Blend Modes

### Darken

**ID**: `Darken`  
**Name**: Darken  
**Class**: [[TGraphics32BlenderDarken]]  

Compares each channel value of the source and backdrop color and selects the smaller (darker) of the two values.

---

### Multiply

**ID**: `multiply`  
**Name**: Multiply  
**Class**: [[TGraphics32BlenderMultiply]]  

Multiplies the source and backdrop color values. The result is always a darker color. Blending with white leaves the image unchanged; blending with black produces black.

---

### Color Burn

**ID**: `ColorBurn`  
**Name**: Color Burn  
**Class**: [[TGraphics32BlenderColorBurn]]  

Darkens the backdrop color to reflect the source color by increasing the contrast between the two. Painting with white produces no change.

---

### Linear Burn

**ID**: `LinearBurn`  
**Name**: Linear Burn  
**Class**: [[TGraphics32BlenderLinearBurn]]  

Darkens the backdrop color to reflect the source color by decreasing brightness. Painting with white produces no change. Also known as the Subtract blend mode.

---

### Darker Color

**ID**: `Darker`  
**Name**: Darker color  
**Class**: [[TGraphics32BlenderDarkerColor]]  

Compares the overall perceived luminance of the source and backdrop pixels and displays the lower-luminance color in its entirety.

---

## Lighten Blend Modes

### Lighten

**ID**: `Lighten`  
**Name**: Lighten  
**Class**: [[TGraphics32BlenderLighten]]  

Compares each channel value of the source and backdrop color and selects the larger (lighter) of the two values.

---

### Screen

**ID**: `screen`  
**Name**: Screen  
**Class**: [[TGraphics32BlenderScreen]]  

Multiplies the inverse of the source and backdrop colors, then complements the result. The result color is always lighter. Screening with black leaves the color unchanged; screening with white yields white.

---

### Color Dodge

**ID**: `ColorDodge`  
**Name**: Color Dodge  
**Class**: [[TGraphics32BlenderColorDodge]]  

Brightens the backdrop color to reflect the source color by decreasing contrast between them. Painting with black produces no change.

---

### Linear Dodge (Add)

**ID**: `Add`  
**Name**: Linear Dodge (Add)  
**Class**: [[TGraphics32BlenderLinearDodge]]  

Sums the channel values of the backdrop and source colors. Blending with black causes no change; blending with white yields pure white.

---

### Lighter Color

**ID**: `Lighter`  
**Name**: Lighter color  
**Class**: [[TGraphics32BlenderLighterColor]]  

Compares the overall perceived luminance of the source and backdrop pixels and displays the higher-luminance color in its entirety.

---

## Contrast Blend Modes

### Overlay

**ID**: `Overlay`  
**Name**: Overlay  
**Class**: [[TGraphics32BlenderOverlay]]  

Multiplies or screens the colors depending on the backdrop color value. Patterns or colors overlay the existing backdrop while preserving its highlights and shadows.

---

### Soft Light

**ID**: `SoftLight`  
**Name**: Soft Light  
**Class**: [[TGraphics32BlenderSoftLight]]  

Darkens or lightens colors depending on the source color value. The visual effect is similar to shining a diffused spotlight on the backdrop image.

---

### Hard Light

**ID**: `HardLight`  
**Name**: Hard Light  
**Class**: [[TGraphics32BlenderHardLight]]  

Multiplies or screens colors depending on the source color value. The visual effect is similar to shining a harsh spotlight on the backdrop image.

---

### Vivid Light

**ID**: `VividLight`  
**Name**: Vivid Light  
**Class**: [[TGraphics32BlenderVividLight]]  

Burns or dodges colors by increasing or decreasing contrast depending on whether the source color is darker or lighter than 50% gray.

---

### Linear Light

**ID**: `LinearLight`  
**Name**: Linear Light  
**Class**: [[TGraphics32BlenderLinearLight]]  

Burns or dodges colors by increasing or decreasing brightness depending on whether the source color is darker or lighter than 50% gray.

---

### Pin Light

**ID**: `PinLight`  
**Name**: Pin Light  
**Class**: [[TGraphics32BlenderPinLight]]  

Replaces colors depending on the source color. If the source is lighter than 50% gray, darker backdrop pixels are replaced; if darker than 50% gray, lighter backdrop pixels are replaced.

---

### Hard Mix

**ID**: `HardMix`  
**Name**: Hard Mix  
**Class**: [[TGraphics32BlenderHardMix]]  

Adds the red, green, and blue channel values of the source to the backdrop. Values $\ge 255$ become 255, and values $< 255$ become 0, reducing all pixels to primary additive colors (Red, Green, Blue, White, Black).

---

## Inversion Blend Modes

### Difference

**ID**: `Difference`  
**Name**: Difference  
**Class**: [[TGraphics32BlenderDifference]]  

Subtracts the darker of the source and backdrop colors from the lighter color. Blending with white inverts backdrop colors; blending with black produces no change.

---

### Exclusion

**ID**: `Exclusion`  
**Name**: Exclusion  
**Class**: [[TGraphics32BlenderExclusion]]  

Creates an effect similar to Difference mode but lower in contrast. Blending with white inverts base color values; blending with black produces no change.

---

### Negation

**ID**: `Negation`  
**Name**: Negation  
**Class**: [[TGraphics32BlenderNegation]]  

Subtracts the darker color from the lighter color and complements the result. Blending with black inverts backdrop colors; blending with white produces no change.

---

## Component (HSL) Blend Modes

### Hue

**ID**: `Hue`  
**Name**: Hue  
**Class**: [[TGraphics32BlenderHue]]  

Creates a result color with the luminance and saturation of the backdrop color and the hue of the source color.

---

### Saturation

**ID**: `Saturation`  
**Name**: Saturation  
**Class**: [[TGraphics32BlenderSaturation]]  

Creates a result color with the luminance and hue of the backdrop color and the saturation of the source color.

---

### Color

**ID**: `Color`  
**Name**: Color  
**Class**: [[TGraphics32BlenderColor]]  

Creates a result color with the luminance of the backdrop color and the hue and saturation of the source color. Preserves gray levels and is useful for tinting monochrome or color images.

---

### Luminosity

**ID**: `Luminosity`  
**Name**: Luminosity  
**Class**: [[TGraphics32BlenderLuminance]]  

Creates a result color with the hue and saturation of the backdrop color and the luminance of the source color. This mode creates the inverse effect of the Color blend mode.

[members]

