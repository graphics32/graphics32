---
layout: doc
docType: api
unit: GR32
parent: TCustomMap
entity: TCustomMap.Create
kind: Constructor
summary: "Initializes a new TCustomMap instance."
overloads:
  - signature: "constructor Create;"
    summary: "Creates a new map instance with default zero dimensions."
  - signature: "constructor Create(Width, Height: Integer); reintroduce; overload;"
    summary: "Creates a new map instance and sets initial width and height."
    parameters:
      - name: Width
        type: Integer
        description: "Initial width of the map."
      - name: Height
        type: Integer
        description: "Initial height of the map."
---

## Description

Initializes a new `TCustomMap` instance.
The overloaded constructor allows setting initial map dimensions during instantiation.
