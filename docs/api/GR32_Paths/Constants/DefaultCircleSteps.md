---
layout: doc
docType: api
unit: GR32_Paths
entity: DefaultCircleSteps
kind: Constant
declaration: "const DefaultCircleSteps = 100;"
summary: "Default number of line segments used to approximate circular and elliptical arcs."
---

## Description

`DefaultCircleSteps` specifies the default number of linear steps (segments) used when generating polygons for circular and elliptical paths in [[TCustomPath.Circle]] and [[TCustomPath.Ellipse]].

A higher value produces smoother circular arcs at the expense of generating more vertex coordinates.
