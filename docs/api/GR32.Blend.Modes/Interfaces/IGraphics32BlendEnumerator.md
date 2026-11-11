---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: IGraphics32BlendEnumerator
kind: Interface
declaration: "IGraphics32BlendEnumerator = interface"
summary: "Enumerator interface for iterating over registered blender classes."
---

## Description

`IGraphics32BlendEnumerator` enables sequence enumeration over registered [[TCustomGraphics32Blender]] classes.

## Properties

| Property | Type | Description |
| --- | --- | --- |
| `Current` | [[TGraphics32BlenderClass]] | Read-only. Returns the current [[TGraphics32BlenderClass]] in the iteration sequence. |

## Methods

### GetCurrent
```pascal
function GetCurrent: TGraphics32BlenderClass;
```
Returns the active [[TGraphics32BlenderClass]] referenced by the current enumerator position.

### MoveNext
```pascal
function MoveNext: Boolean;
```
Advances the enumerator position to the next registered blender class. Returns `True` if successful, or `False` if the end of the collection has been reached.
