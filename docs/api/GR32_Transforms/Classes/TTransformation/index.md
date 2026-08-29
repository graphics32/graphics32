---
layout: doc
docType: api
unit: GR32_Transforms
entity: TTransformation
kind: Class
aliases: [TTransformationClass]
declaration: "TTransformation = class abstract(TNotifiablePersistent)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
summary: "Abstract base class for 2D spatial coordinate mappings in Graphics32."
---

## Description

`TTransformation` is the abstract base class for all 2D spatial coordinate mappings in Graphics32. It provides abstract forward (`Transform`) and reverse (`ReverseTransform`) coordinate mapping methods across integer, fixed-point (`TFixedPoint`), and floating-point (`TFloatPoint`) precisions.

Derived classes implement specific spatial mappings (affine, projective, twirl, bloat, polar, sphere, etc.).

[members]
