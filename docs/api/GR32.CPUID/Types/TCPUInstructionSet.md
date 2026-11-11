---
layout: doc
docType: api
unit: GR32.CPUID
entity: TCPUInstructionSet
kind: Type
aliases: [TInstructionSupport]
summary: "Enumerates hardware CPU instruction sets, vector extensions, and synthetic binding features."
declaration: |
  TCPUInstructionSet = (
    isReference, isPascal, isAssembler,
    isFPU, isTSC, isCX8, isSEP, isCMOV, isMMX, isFXSR, isSSE, isSSE2, isSSE3,
    isMONITOR, isCX16, isX64, isExMMX, isEx3DNow, is3DNow, isSSSE3, isSSE41,
    isSSE42, isAES, isAVX, isPopCnt, isXSAVE, isRDTSCP, isTBM, isFMA4, isXOP,
    isSSE4A, isABM, isLAHF, isPCLMULQDQ, isFMA, isMOVBE, isF16C, isRDRAND,
    isBMI1, isAVX2, isBMI2, isERMS, isINVPCID, isRTM, isMPX, isAVX512f,
    isAVX512dq, isRDSEED, isADX, isPCOMMIT, isCLFLUSHOPT, isCLWB, isAVX512pf,
    isAVX512er, isAVX512cd, isSHA, isAVX512bw, isAVX512vl, isPREFETCHWT1, isAVX512vbmi
  );
  TInstructionSupport = set of TCPUInstructionSet;
seealso:
  - "[[TCPU]]"
---

## Description

`TCPUInstructionSet` enumerates hardware instruction set extensions, SIMD vector capabilities, and synthetic feature flags used by Graphics32's CPU function binding system.

`TInstructionSupport` is a Pascal `set of TCPUInstructionSet` indicating the set of features supported by the host CPU and operating system kernel.

## Enumeration Values

### Synthetic Features
| Value | Description |
| --- | --- |
| `isReference` | Reference implementation flag used by CPU dispatch bindings. |
| `isPascal` | Pure Pascal fallback implementation flag. |
| `isAssembler` | Base x86/x64 assembly implementation flag without specialized vector requirements. |

### Hardware Features
| Value | Description |
| --- | --- |
| `isFPU` | x87 floating-point unit present. |
| `isTSC` | Time Stamp Counter (`RDTSC`). |
| `isCX8` | `CMPXCHG8B` instruction. |
| `isSEP` | `SYSENTER` / `SYSEXIT` instructions. |
| `isCMOV` | Conditional Move instructions (`CMOVcc`, `FCMOVcc`). |
| `isMMX` | MMX technology instructions. |
| `isFXSR` | `FXSAVE` and `FXRSTOR` instructions. |
| `isSSE` | Streaming SIMD Extensions (SSE). |
| `isSSE2` | Streaming SIMD Extensions 2 (SSE2). |
| `isSSE3` | Streaming SIMD Extensions 3 (SSE3). |
| `isMONITOR` | `MONITOR` and `MWAIT` instructions. |
| `isCX16` | `CMPXCHG16B` instruction. |
| `isX64` | 64-bit architecture extensions (AMD64 / Intel 64). |
| `isExMMX` | Extended MMX (AMD MMX+). |
| `isEx3DNow` | Extended 3DNow! (AMD). |
| `is3DNow` | 3DNow! instructions (AMD). |
| `isSSSE3` | Supplemental Streaming SIMD Extensions 3 (SSSE3). |
| `isSSE41` | SSE 4.1 instructions. |
| `isSSE42` | SSE 4.2 instructions. |
| `isAES` | AES instruction set support. |
| `isAVX` | Advanced Vector Extensions (AVX). |
| `isPopCnt` | `POPCNT`, `LZCNT`, `TZCNT` bit counting instructions. |
| `isXSAVE` | Extended State Save/Restore (`XSAVE`, `XRSTOR`). |
| `isRDTSCP` | Synchronized Read Time Stamp Counter (`RDTSCP`). |
| `isTBM` | Trailing Bit Manipulation instructions. |
| `isFMA4` | 4-operand FMA instructions (AMD). |
| `isXOP` | eXtended Operations instructions (AMD). |
| `isSSE4A` | SSE 4a support (AMD). |
| `isABM` | Advanced Bit Manipulation. |
| `isLAHF` | `LAHF` and `SAHF` in 64-bit mode. |
| `isPCLMULQDQ` | Carryless Multiplication instruction. |
| `isFMA` | Fused Multiply-Add (3-operand FMA3). |
| `isMOVBE` | Move Big Endian instruction. |
| `isF16C` | Half-precision floating-point conversion instructions. |
| `isRDRAND` | On-chip hardware random number generator (`RDRAND`). |
| `isBMI1` | Bit Manipulation Instruction Set 1. |
| `isAVX2` | Advanced Vector Extensions 2 (AVX2). |
| `isBMI2` | Bit Manipulation Instruction Set 2. |
| `isERMS` | Enhanced REP MOVSB/STOSB. |
| `isINVPCID` | Invalidate Process-Context Identifier instruction. |
| `isRTM` | Restricted Transactional Memory (TSX). |
| `isMPX` | Memory Protection Extensions. |
| `isAVX512f` | AVX-512 Foundation instructions. |
| `isAVX512dq` | AVX-512 Doubleword and Quadword instructions. |
| `isRDSEED` | Hardware random seed generator (`RDSEED`). |
| `isADX` | Multi-Precision Add-Carry Instruction Extensions. |
| `isPCOMMIT` | `PCOMMIT` instruction. |
| `isCLFLUSHOPT` | Optimized Cache Line Flush instruction. |
| `isCLWB` | Cache Line Write Back instruction. |
| `isAVX512pf` | AVX-512 Prefetch instructions. |
| `isAVX512er` | AVX-512 Exponential and Reciprocal instructions. |
| `isAVX512cd` | AVX-512 Conflict Detection instructions. |
| `isSHA` | SHA Extensions. |
| `isAVX512bw` | AVX-512 Byte and Word instructions. |
| `isAVX512vl` | AVX-512 Vector Length extensions. |
| `isPREFETCHWT1` | Prefetch Vector Data Into Caches with Intent to Write instruction. |
| `isAVX512vbmi` | AVX-512 Vector Bit Manipulation instructions. |
