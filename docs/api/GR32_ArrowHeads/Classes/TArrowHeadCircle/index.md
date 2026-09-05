---
layout: doc
docType: api
unit: GR32_ArrowHeads
entity: TArrowHeadCircle
kind: Class
declaration: |
  TArrowHeadCircle = class(TArrowHeadAbstract)
  protected
    function GetPointsInternal: TArrayOfFloatPoint; override;
  end;
inheritance:
  - TObject
  - TArrowHeadAbstract
  - TArrowHeadCircle
summary: "Generates a circular endpoint decoration."
---

## Description

`TArrowHeadCircle` is a concrete subclass of [[TArrowHeadAbstract]] that generates a circular polygon centered between the tip and base points at polyline endpoints.

![](/images/arrowhead-circle.png)

[members]
