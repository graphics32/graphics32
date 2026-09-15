---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: IGraphics32BlendGroups
kind: Interface
declaration: "IGraphics32BlendGroups = interface"
summary: "Interface for registering and managing named groups of blender classes."
---

## Description

`IGraphics32BlendGroups` provides registry management for grouping blender classes by category names.

## Methods

### Register
```pascal
procedure Register(const GroupName: string; BlenderClass: TGraphics32BlenderClass);
```
Registers a [[TGraphics32BlenderClass]] into the specified named group `GroupName`.

### GroupByName
```pascal
function GroupByName(const GroupName: string): IGraphics32BlendGroup;
```
Looks up and returns the [[IGraphics32BlendGroup]] corresponding to `GroupName`, or `nil` if not found.

### GetEnumerator
```pascal
function GetEnumerator: IGraphics32BlendGroupsEnumerator;
```
Returns an [[IGraphics32BlendGroupsEnumerator]] interface for iterating across all registered blend groups.
