---
layout: doc
docType: api
unit: GR32_Geometry
entity: Geometry Constants
kind: Constant
aliases: [CRad01,CRad30,CRad45,CRad60,CRad90,CRad180,CRad270,CRad360,CDegToRad,CRadToDeg]
summary: "Predefined angular constants and conversion factors for degrees and radians."
---

## Description

The `GR32_Geometry` unit provides predefined angular constants in radians for common angles ($1^\circ$, $30^\circ$, $45^\circ$, $60^\circ$, $90^\circ$, $180^\circ$, $270^\circ$, $360^\circ$) as well as conversion multipliers between degrees and radians.

## Constants Table

| Constant | Value | Description |
| --- | --- | --- |
| `CRad01` | $\pi / 180$ ($\approx 0.0174532925$) | Angle of $1^\circ$ expressed in radians. |
| `CRad30` | $\pi / 6$ ($\approx 0.5235987756$) | Angle of $30^\circ$ expressed in radians. |
| `CRad45` | $\pi / 4$ ($\approx 0.7853981634$) | Angle of $45^\circ$ expressed in radians. |
| `CRad60` | $\pi / 3$ ($\approx 1.0471975512$) | Angle of $60^\circ$ expressed in radians. |
| `CRad90` | $\pi / 2$ ($\approx 1.5707963268$) | Angle of $90^\circ$ expressed in radians. |
| `CRad180` | $\pi$ ($\approx 3.1415926535$) | Angle of $180^\circ$ expressed in radians. |
| `CRad270` | $3\pi / 2$ ($\approx 4.7123889804$) | Angle of $270^\circ$ expressed in radians. |
| `CRad360` | $2\pi$ ($\approx 6.2831853072$) | Angle of $360^\circ$ expressed in radians. |
| `CDegToRad` | $\pi / 180$ ($\approx 0.0174532925$) | Multiplier to convert degrees to radians (`Radians := Degrees * CDegToRad`). |
| `CRadToDeg` | $180 / \pi$ ($\approx 57.2957795131$) | Multiplier to convert radians to degrees (`Degrees := Radians * CRadToDeg`). |
