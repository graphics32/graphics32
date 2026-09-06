---
layout: doc
docType: api
unit: GR32.CPUID
entity: TCPUVendor
kind: Type
summary: "Enumerates hardware CPU vendors and virtual machine hypervisors."
declaration: |
  TCPUVendor = (
    cvUnknown, cvAMD, cvCentaur, cvCyrix, cvIntel, cvTransmeta,
    cvNexGen, cvRise, cvUMC, cvNSC, cvSiS, cvAMDEarly, cvVIA, cvVortex,
    cvVM_KVM, cvVM_Microsoft, cvVM_Parallels, cvVM_VMWare, cvVM_XEN,
    cvVM_XTA, cvVM_Rosetta2, cvZhaoxin, cvHygon, cvRDC, cvElbrus
  );
seealso:
  - "[[TCPU]]"
---

## Description

`TCPUVendor` identifies the hardware CPU vendor or virtual machine hypervisor retrieved via CPUID query leaf 0.

## Enumeration Values

| Value | Description |
| --- | --- |
| `cvUnknown` | Unknown or unrecognized CPU vendor. |
| `cvAMD` | Advanced Micro Devices (AMD). |
| `cvCentaur` | Centaur Technology / VIA. |
| `cvCyrix` | Cyrix Corporation. |
| `cvIntel` | Intel Corporation. |
| `cvTransmeta` | Transmeta Corporation. |
| `cvNexGen` | NexGen. |
| `cvRise` | Rise Technology. |
| `cvUMC` | United Microelectronics Corporation (UMC). |
| `cvNSC` | National Semiconductor. |
| `cvSiS` | Silicon Integrated Systems (SiS). |
| `cvAMDEarly` | AMD K5 engineering sample identification. |
| `cvVIA` | VIA Technologies. |
| `cvVortex` | Vortex86 SoC. |
| `cvVM_KVM` | Linux Kernel-based Virtual Machine (KVM). |
| `cvVM_Microsoft` | Microsoft Hyper-V hypervisor. |
| `cvVM_Parallels` | Parallels Desktop hypervisor. |
| `cvVM_VMWare` | VMware hypervisor. |
| `cvVM_XEN` | Xen hypervisor. |
| `cvVM_XTA` | Microsoft X86-to-ARM (XTA) translation. |
| `cvVM_Rosetta2` | Apple Rosetta 2 translation environment. |
| `cvZhaoxin` | Shanghai Zhaoxin Semiconductor. |
| `cvHygon` | Hygon Information Technology. |
| `cvRDC` | RDC Semiconductor Co. Ltd. |
| `cvElbrus` | MCST Elbrus architecture. |
