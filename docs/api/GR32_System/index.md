---
layout: doc
docType: api
unit: GR32_System
entity: GR32_System
kind: Unit
summary: "Provides high-resolution stopwatch timing, processor affinity control, tick counts, and CPU detection instance."
---

## Description

The `GR32_System` unit provides core system utilities for performance measurement, multi-core CPU affinity optimization, system clock ticks, and global hardware detection.

Key capabilities provided by this unit include:

- **High-Resolution Timing**: Accurate elapsed time measurement via [[TStopwatch]].
- **System Tick Counts**: Portable millisecond tick counter retrieval via [[GetTickCount]].
- **CPU & Core Topology Management**: Queries logical processor count via [[GetProcessorCount]] and manages performance core affinity masks via [[SetPerformanceAffinityMask]] and [[RestoreAffinityMask]].
- **Global Hardware Instance**: Exposes the global [[CPU]] hardware information record instance initialized at startup.

---

[members]
