---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: IGraphics32BlendService
kind: Interface
declaration: "IGraphics32BlendService = interface"
summary: "Central service interface for registering, querying, and enumerating Graphics32 pixel blenders and groups."
---

## Description

`IGraphics32BlendService` is the primary blend mode management interface in Graphics32. It allows registration of custom [[TCustomGraphics32Blender]] classes, lookup of blenders by unique string ID, enumeration of registered blenders, and access to named blend mode groups.

## Properties

| Property | Type | Description |
| --- | --- | --- |
| `Groups` | [[IGraphics32BlendGroups]] | Read-only. Provides access to the [[IGraphics32BlendGroups]] group registry manager. |

## Methods

### Register
```pascal
procedure Register(BlenderClass: TGraphics32BlenderClass; Groups: TArray<string> = []);
```
Registers `BlenderClass` with the global blend service and optionally adds it to one or more named group categories specified in `Groups`.

### BlenderByID
```pascal
function BlenderByID(const ID: string): TGraphics32BlenderClass;
```
Retrieves the [[TGraphics32BlenderClass]] registered with the specified unique string identifier `ID`, or returns `nil` if no matching blender is registered.

### GetEnumerator
```pascal
function GetEnumerator: IGraphics32BlendEnumerator;
```
Returns an [[IGraphics32BlendEnumerator]] interface for iterating through all registered blender classes.

### GetGroups
```pascal
function GetGroups: IGraphics32BlendGroups;
```
Returns the [[IGraphics32BlendGroups]] interface for accessing group-based blender organization.
