---
layout: doc
docType: api
unit: GR32_System
entity: GetProcessorCount
kind: Function
declaration: "function GetProcessorCount: Cardinal;"
summary: "Returns the number of logical processors or cores configured by the operating system."
returns:
  - type: Cardinal
    description: "Number of active logical processors."
---

## Description

`GetProcessorCount` retrieves the total number of logical CPU cores reported by the operating system.

It is used internally by multi-threaded rasterizers and parallel rendering algorithms to determine thread count allocation and workload distribution across CPU cores.
