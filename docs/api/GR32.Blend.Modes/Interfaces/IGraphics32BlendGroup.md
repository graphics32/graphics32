---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: IGraphics32BlendGroup
kind: Interface
declaration: "IGraphics32BlendGroup = interface"
summary: "Interface representing a named group or category of blender classes."
---

## Description

`IGraphics32BlendGroup` represents a category or group (such as Photoshop, Porter-Duff, or Extra blend modes) that groups related blender classes together.

## Properties

| Property | Type | Description |
| --- | --- | --- |
| `Name` | string | Read-only. Returns the name of the blend mode group. |

## Methods

### GetEnumerator
```pascal
function GetEnumerator: IGraphics32BlendEnumerator;
```
Returns an [[IGraphics32BlendEnumerator]] interface for iterating through all blender classes registered within this group.

### GetName
```pascal
function GetName: string;
```
Returns the group name string.
