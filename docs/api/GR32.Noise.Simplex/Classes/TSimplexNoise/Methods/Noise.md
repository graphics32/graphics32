---
layout: doc
docType: api
unit: GR32.Noise.Simplex
parent: TSimplexNoise
entity: TSimplexNoise.Noise
kind: Method
summary: "Evaluates smooth continuous Simplex Noise at 2D, 3D, or 4D coordinates."
overloads:
  - signature: "function Noise(const x, y: Double): Double; overload;"
    summary: "Evaluates 2D Simplex Noise at spatial coordinates (x, y)."
    parameters:
      - name: x
        type: Double
        description: "X spatial coordinate in Euclidean space."
      - name: y
        type: Double
        description: "Y spatial coordinate in Euclidean space."
    returns:
      - type: Double
        description: "Continuous scalar noise value in range [-1.0, 1.0]."

  - signature: "function Noise(const x, y, z: Double): Double; overload;"
    summary: "Evaluates 3D Simplex Noise at spatial coordinates (x, y, z)."
    parameters:
      - name: x
        type: Double
        description: "X spatial coordinate."
      - name: y
        type: Double
        description: "Y spatial coordinate."
      - name: z
        type: Double
        description: "Z spatial coordinate (or temporal dimension)."
    returns:
      - type: Double
        description: "Continuous scalar noise value in range [-1.0, 1.0]."

  - signature: "function Noise(const x, y, z, w: Double): Double; overload;"
    summary: "Evaluates 4D Simplex Noise at coordinates (x, y, z, w)."
    parameters:
      - name: x
        type: Double
        description: "X coordinate."
      - name: y
        type: Double
        description: "Y coordinate."
      - name: z
        type: Double
        description: "Z coordinate."
      - name: w
        type: Double
        description: "W coordinate (e.g. time or extra parameter)."
    returns:
      - type: Double
        description: "Continuous scalar noise value in range [-1.0, 1.0]."
---

## Description

The `Noise` method samples continuous gradient noise at the specified coordinates:

1. **2D Noise `Noise(x, y)`**: Partitions 2D Euclidean space into equilateral triangles. Useful for planar texture generation, heightfield maps, and 2D particle drift.
2. **3D Noise `Noise(x, y, z)`**: Partitions 3D space into tetrahedrons. Ideal for volumetric textures (such as 3D fog, marble, or cloud density) or animating 2D textures smoothly over time ($z = \text{time}$).
3. **4D Noise `Noise(x, y, z, w)`**: Partitions 4D space into 5-cell hyper-simplices. Useful for animating 3D volumes over time, 4D vector fields, or multi-parameter procedural synthesis.

All variants return smooth $C^2$ continuous scalar values normalized approximately within $[-1.0, 1.0]$.

## Example

```pascal
var
  Simplex: TSimplexNoise;
  Time: Double;
  Vx, Vy: Double;
begin
  Simplex := TSimplexNoise.Create;
  try
    Time := 1.5; // Seconds elapsed

    // Animate 2D flow field over time using 3D Simplex noise
    Vx := Simplex.Noise(10.0, 20.0, Time);
    Vy := Simplex.Noise(10.0 + 100.0, 20.0 + 100.0, Time);
  finally
    Simplex.Free;
  end;
end;
```
