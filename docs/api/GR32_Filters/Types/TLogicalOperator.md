---
layout: doc
docType: api
unit: GR32_Filters
entity: TLogicalOperator
kind: Type
declaration: "TLogicalOperator = (loXOR, loAND, loOR);"
summary: "Enumeration specifying bitwise logical operations for bitmask filtering."
---

## Description

`TLogicalOperator` specifies the logical bitwise operation applied during `ApplyBitmask` filter execution.

### Enum Values

| Value | Description |
| --- | --- |
| `loXOR` | Performs a bitwise Exclusive-OR (`xor`) operation between pixel colors and bitmask. |
| `loAND` | Performs a bitwise AND (`and`) operation between pixel colors and bitmask. |
| `loOR` | Performs a bitwise OR (`or`) operation between pixel colors and bitmask. |
