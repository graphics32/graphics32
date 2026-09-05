---
layout: doc
docType: api
unit: GR32_ArrowHeads
entity: TArrowHeadSimple
kind: Class
declaration: |
  TArrowHeadSimple = class(TArrowHeadAbstract)
  protected
    function GetPointsInternal: TArrayOfFloatPoint; override;
  end;
inheritance:
  - TObject
  - TArrowHeadAbstract
  - TArrowHeadSimple
summary: "Generates a simple 3-point triangular arrowhead."
---

## Description

`TArrowHeadSimple` is a concrete subclass of [[TArrowHeadAbstract]] that generates a basic 3-point triangle arrowhead shape at polyline endpoints.

![](/images/arrowhead-3point.png)

[members]
