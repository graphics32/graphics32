---
layout: doc
docType: api
unit: GR32_System
entity: SetPerformanceAffinityMask
kind: Function
aliases: [RestoreAffinityMask]
declaration: |
  function SetPerformanceAffinityMask(Force: Boolean = False): Boolean;
  procedure RestoreAffinityMask;
summary: "Configures process affinity to restrict worker threads to high-performance CPU cores."
parameters:
  - name: Force
    type: Boolean
    description: "If True, overrides existing custom process affinity masks."
returns:
  - type: Boolean
    description: "Returns True if process affinity mask was successfully modified."
---

## Description

`SetPerformanceAffinityMask` adjusts the current process affinity mask on hybrid heterogeneous CPU architectures (such as Intel Alder Lake/Raptor Lake with Performance and Efficiency cores on Windows 10/11) so that compute-heavy rendering threads run exclusively on high-performance cores.

`RestoreAffinityMask` restores the original process affinity mask, allowing threads to run across all system CPU cores.
