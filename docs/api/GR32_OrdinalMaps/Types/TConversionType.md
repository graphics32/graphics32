---
layout: doc
docType: api
unit: GR32_OrdinalMaps
entity: TConversionType
kind: Type
declaration: "TConversionType = (ctRed, ctGreen, ctBlue, ctAlpha, ctUniformRGB, ctWeightedRGB);"
summary: "Specifies color channel extraction and conversion modes when copying pixels between TBitmap32 and TByteMap."
---

## Description

`TConversionType` defines how 32-bit ARGB color pixels in [[TBitmap32]] are converted into 8-bit grayscale values when reading into or writing from a [[TByteMap]].

### Enum Values

| Value | Description |
| --- | --- |
| `ctRed` | Extracts or writes the red color component ($R$). |
| `ctGreen` | Extracts or writes the green color component ($G$). |
| `ctBlue` | Extracts or writes the blue color component ($B$). |
| `ctAlpha` | Extracts or writes the alpha transparency component ($A$). |
| `ctUniformRGB` | Computes or applies uniform unweighted average grayscale values: $(R + G + B) / 3$. |
| `ctWeightedRGB` | Computes or applies perceived intensity weighted grayscale values using standard luminance formula: [[GR32.Intensity\|Intensity(Color)]]. |
