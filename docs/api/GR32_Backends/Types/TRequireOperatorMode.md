---
layout: doc
docType: api
unit: GR32_Backends
entity: TRequireOperatorMode
kind: Type
declaration: "TRequireOperatorMode = (romAnd, romOr);"
summary: "Enumeration specifying logical operator mode when matching backend interface requirements."
---

# Type TRequireOperatorMode

`TRequireOperatorMode` specifies whether all required interface GUIDs must be supported (`romAnd`) or if supporting any single interface is sufficient (`romOr`) when calling [[RequireBackendSupport]].

## Values

| Value | Description |
| --- | --- |
| `romAnd` | All interface GUIDs must be supported by the backend. |
| `romOr` | At least one specified interface GUID must be supported by the backend. |
