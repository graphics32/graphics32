---
layout: doc
docType: api
unit: GR32_Image
entity: TPaintStageMask
kind: Type
aliases: [TPaintStageMaskValue, psmDesignTime, psmRunTime, psmExport]
declaration: |
  TPaintStageMaskValue = (psmDesignTime, psmRunTime, psmExport);
  TPaintStageMask = set of TPaintStageMaskValue;
summary: "Set of execution contexts defining when a paint stage is active."
seealso:
  - "[[TPaintStages]]"
---

## Description

`TPaintStageMask` specifies the execution environments in which a given `TPaintStage` is rendered.

| Value | Description |
|---|---|
| `psmDesignTime` | Stage is painted at design-time inside the IDE form designer. |
| `psmRunTime` | Stage is painted at application runtime. |
| `psmExport` | Stage is painted when exporting the image surface (e.g. via `PaintTo`). |

---
