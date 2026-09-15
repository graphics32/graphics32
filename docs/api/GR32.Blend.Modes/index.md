---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: GR32.Blend.Modes
kind: Unit
summary: "Provides an extensible blend mode registry service and object-oriented blender class hierarchy for Graphics32."
seealso:
  - "[[GR32_Blend]]"
  - "[[Graphics32BlendService]]"
  - "[[GR32.Blend.Modes.PorterDuff]] Porter-Duff blend modes"
  - "[[GR32.Blend.Modes.PhotoShop]] Adobe Photoshop blend modes"
  - "[[GR32.Blend.Modes.Extra]] Additional blend modes"
---

## Description

The `GR32.Blend.Modes` unit provides an object-oriented framework for custom pixel blending modes and compositing algorithms in Graphics32.

Key features provided by `GR32.Blend.Modes` include:

- **Blender Class Hierarchy**: Base abstract classes [[TCustomGraphics32Blender]], [[TCustomGraphics32ComponentBlender]], [[TGraphics32ComponentBlender]], and [[TGraphics32SeparableBlender]] for implementing custom blending algorithms.
- **Normal Blender**: The default [[TGraphics32BlenderNormal]] implementation that delegates standard pixel blending to Graphics32's fast core routines.
- **Adobe / Photoshop Compositing**: [[TGraphics32ComponentBlender]] implements the standard Adobe Photoshop separable alpha compositing formula for multi-layer color blending.
- **Global Blend Service**: The [[Graphics32BlendService]] singleton and related interfaces ([[IGraphics32BlendService]], [[IGraphics32BlendGroups]], [[IGraphics32BlendGroup]], [[IGraphics32BlendEnumerator]], [[IGraphics32BlendGroupsEnumerator]]) enable dynamic registration, enumeration, and discovery of blend modes and blend groups across Photoshop, Porter-Duff, and extra blend mode libraries.

---

[members]
