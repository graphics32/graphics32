---
layout: doc
docType: api
unit: GR32_Image
entity: TPaintStage
kind: Type
aliases: [PPaintStage]
declaration: |
  TPaintStage = record
    Mask: TPaintStageMask;
    Stage: Cardinal;
    Parameter: Cardinal;
    DsgnTime: Boolean;
    RunTime: Boolean;
  end;
  PPaintStage = ^TPaintStage;
summary: "Record structure representing an individual rendering pipeline stage."
seealso:
  - "[[TPaintStages]]"
  - "[[Paint Stage Constants]]"
---

## Description

`TPaintStage` defines a single rendering step in the image paint pipeline, including execution mask (`Mask`), stage identifier (`Stage`), and optional parameter (`Parameter`).

| Field | Type | Description |
|---|---|---|
| `Mask` | `TPaintStageMask` | Set of contexts (`psmDesignTime`, `psmRunTime`, `psmExport`) where stage is executed. |
| `Stage` | `Cardinal` | Stage identifier constant (`PST_CUSTOM`, `PST_CLEAR_BACKGND`, `PST_DRAW_BITMAP`, `PST_DRAW_LAYERS`, etc.). |
| `Parameter` | `Cardinal` | Optional stage parameter (e.g. layer option mask for `PST_DRAW_LAYERS`). |
| `DsgnTime` | `Boolean` | Backward compatibility property for `psmDesignTime in Mask`. |
| `RunTime` | `Boolean` | Backward compatibility property for `psmRunTime in Mask`. |

---
