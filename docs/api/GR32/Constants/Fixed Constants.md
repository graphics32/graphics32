---
layout: doc
docType: api
unit: GR32
entity: Fixed
kind: Constant
aliases: [FixedOne,FixedHalf,FixedPI,FixedToFloat,COne255th]
summary: "16.16 fixed-point mathematical constants."
---

## Description

Constants used for 16:16 fixed-point arithmetic ([[TFixed]]) and floating-point conversions in Graphics32.

## Constants Table

| Constant | Value | Type | Description |
| --- | --- | --- | --- |
| `FixedOne` | `$10000` (`65536`) | `Integer` | Represents the value `1.0` in 16.16 fixed-point precision. |
| `FixedHalf` | `$7FFF` (`32767`) | `Integer` | Represents the value `0.5` (approximate) in 16.16 fixed-point precision. |
| `FixedPI` | `Round(PI * FixedOne)` (`205887`) | `Double` | The value of $\pi$ in 16.16 fixed-point format. |
| `FixedToFloat` | `1 / FixedOne` (`1 / 65536`) | `Double` | Multiplier used to convert 16.16 fixed-point numbers to floating-point (`Single`/`Double`). |
| `COne255th` | `1 / $FF` (`1 / 255`) | `Double` | Reciprocal of 255, used for fast channel normalization to `[0..1]`. |
