---
layout: doc
docType: api
unit: GR32_ArrowHeads
entity: TArrowHeadDiamond
kind: Class
declaration: |
  TArrowHeadDiamond = class(TArrowHeadAbstract)
  protected
    function GetPointsInternal: TArrayOfFloatPoint; override;
  end;
inheritance:
  - TObject
  - TArrowHeadAbstract
  - TArrowHeadDiamond
summary: "Generates a 4-point diamond endpoint decoration."
---

## Description

`TArrowHeadDiamond` is a concrete subclass of [[TArrowHeadAbstract]] that generates a 4-point diamond-shaped polygon at polyline endpoints.

![](/images/arrowhead-diamond.png)

[members]
