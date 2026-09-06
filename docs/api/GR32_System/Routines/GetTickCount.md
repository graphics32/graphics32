---
layout: doc
docType: api
unit: GR32_System
entity: GetTickCount
kind: Function
declaration: "function GetTickCount: UInt64;"
summary: "Returns the number of milliseconds elapsed since system boot or timer initialization."
returns:
  - type: UInt64
    description: "64-bit integer representing elapsed milliseconds."
seealso:
  - "[[TStopwatch]]"
---

## Description

`GetTickCount` returns a 64-bit unsigned integer representing elapsed milliseconds.

On Windows and FPC systems, it calls `GetTickCount64` or uses high-precision monotonic stopwatch counters to avoid 32-bit integer wraparound issues (which occur after ~49.7 days with standard 32-bit `GetTickCount`).
