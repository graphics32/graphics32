---
layout: doc
docType: api
unit: GR32_ArrowHeads
entity: TArrowHeadFourPt
kind: Class
declaration: |
  TArrowHeadFourPt = class(TArrowHeadAbstract)
  protected
    function GetPointsInternal: TArrayOfFloatPoint; override;
  end;
inheritance:
  - TObject
  - TArrowHeadAbstract
  - TArrowHeadFourPt
summary: "Generates a 4-point dart-style arrowhead with a recessed base."
---

## Description

`TArrowHeadFourPt` is a concrete subclass of [[TArrowHeadAbstract]] that generates a 4-point dart/barbed arrowhead polygon where the base point recedes into the body of the arrowhead.

![](/images/arrowhead-4point.png)

[members]
