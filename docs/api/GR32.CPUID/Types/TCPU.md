---
layout: doc
docType: api
unit: GR32.CPUID
entity: TCPU
kind: Type
summary: "Record structure holding CPU model metadata, vendor, cache sizes, and hardware instruction support flags."
declaration: |
  TCPU = record
  public
    Vendor: TCPUVendor;
    Signature: Cardinal;

    // Signature unpacked
    Stepping: Byte;
    Model: Byte;
    ProcessorType: Byte;
    Family: Byte;
    ExtendedModel: Byte;
    ExtendedFamily: Byte;

    // Calculated fields
    ActualFamily: Byte;
    ActualModel: Byte;

    CodeL1CacheSize,
    DataL1CacheSize,
    L2CacheSize,
    L3CacheSize: Word;
    PrefetchSize: Word;
    InstructionSupport: TInstructionSupport;

    property VendorName: string read GetVendorName;

    class function GetCPUInfo: TCPU; static;
    class function GetVendorName(Vendor: TCPUVendor): string; overload; static;
    class function GetInstructionSetName(InstructionSet: TCPUInstructionSet): string; static;
  end;
seealso:
  - "[[TCPUVendor]]"
  - "[[TCPUInstructionSet]]"
---

## Description

`TCPU` represents detailed hardware identification and feature introspection for the host processor.

## Fields

| Field | Type | Description |
| --- | --- | --- |
| `Vendor` | `TCPUVendor` | Identified CPU vendor or hypervisor enumeration value. |
| `Signature` | `Cardinal` | Raw CPUID leaf 1 signature returned in `EAX`. |
| `Stepping` | `Byte` | Processor stepping revision number. |
| `Model` | `Byte` | Base processor model number. |
| `ProcessorType` | `Byte` | Processor type (e.g. Primary, OverDrive, Dual). |
| `Family` | `Byte` | Base processor family identifier. |
| `ExtendedModel` | `Byte` | Extended model bits. |
| `ExtendedFamily` | `Byte` | Extended family bits. |
| `ActualFamily` | `Byte` | Calculated effective family ID combining `Family` and `ExtendedFamily`. |
| `ActualModel` | `Byte` | Calculated effective model ID combining `Model` and `ExtendedModel`. |
| `CodeL1CacheSize` | `Word` | L1 instruction/code cache size in kilobytes (or micro-ops where applicable). |
| `DataL1CacheSize` | `Word` | L1 data cache size in kilobytes. |
| `L2CacheSize` | `Word` | L2 unified/data cache size in kilobytes. |
| `L3CacheSize` | `Word` | L3 cache size in kilobytes. |
| `PrefetchSize` | `Word` | Cache line prefetch size in bytes. |
| `InstructionSupport` | `TInstructionSupport` | Set of supported hardware instruction set extensions and synthetic feature flags. |

## Properties

| Property | Type | Description |
| --- | --- | --- |
| `VendorName` | `string` | Read-only string description for the identified CPU vendor. |

## Class Methods

### GetCPUInfo
```pascal
class function GetCPUInfo: TCPU; static;
```
Queries the host hardware via CPUID instructions and returns a populated `TCPU` structure containing vendor, family, cache sizes, and verified instruction set support.

### GetVendorName
```pascal
class function GetVendorName(Vendor: TCPUVendor): string; overload; static;
```
Returns the display string associated with a given `TCPUVendor` enum value.

### GetInstructionSetName
```pascal
class function GetInstructionSetName(InstructionSet: TCPUInstructionSet): string; static;
```
Returns the text representation for a given `TCPUInstructionSet` enum value.
