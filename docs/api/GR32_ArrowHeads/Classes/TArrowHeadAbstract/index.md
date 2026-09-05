---
layout: doc
docType: api
unit: GR32_ArrowHeads
entity: TArrowHeadAbstract
kind: Class
abstract: true
declaration: |
  TArrowHeadAbstract = class
  protected
    function GetPointsInternal: TArrayOfFloatPoint; virtual; abstract;
  public
    constructor Create(Size: TFloat); virtual;
    function GetPoints(const Line: TArrayOfFloatPoint; AtEnd: Boolean): TArrayOfFloatPoint;
    property Size: TFloat read FSize write FSize;
  end;
inheritance:
  - TObject
  - TArrowHeadAbstract
summary: "Abstract base class for calculating arrowhead vector geometry along line endpoints."
---

## Description

`TArrowHeadAbstract` serves as the abstract base class for vector arrowhead generators in Graphics32. It handles the alignment, vector scaling, and position calculation required to place an arrowhead shape at either the start or end point of a line or polyline.

Derived classes override the protected `GetPointsInternal` method to compute the shape points based on the tip and base points determined by [[GetPoints]].

[members]
