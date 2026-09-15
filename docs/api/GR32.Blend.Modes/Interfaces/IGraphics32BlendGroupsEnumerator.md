---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: IGraphics32BlendGroupsEnumerator
kind: Interface
declaration: "IGraphics32BlendGroupsEnumerator = interface"
summary: "Enumerator interface for iterating over registered blend mode groups."
---

## Description

`IGraphics32BlendGroupsEnumerator` enables sequence enumeration over registered [[IGraphics32BlendGroup]] instances.

## Properties

| Property | Type | Description |
| --- | --- | --- |
| `Current` | [[IGraphics32BlendGroup]] | Read-only. Returns the current [[IGraphics32BlendGroup]] in the iteration sequence. |

## Methods

### GetCurrent
```pascal
function GetCurrent: IGraphics32BlendGroup;
```
Returns the active [[IGraphics32BlendGroup]] referenced by the current enumerator position.

### MoveNext
```pascal
function MoveNext: Boolean;
```
Advances the enumerator position to the next blend group. Returns `True` if successful, or `False` if the end of the collection has been reached.
