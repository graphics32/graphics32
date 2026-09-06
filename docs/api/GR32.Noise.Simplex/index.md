---
layout: doc
docType: api
unit: GR32.Noise.Simplex
entity: GR32.Noise.Simplex
kind: Unit
summary: "Provides smooth multi-dimensional Simplex Noise evaluation (2D, 3D, and 4D) for procedural textures, animations, and vector fields."
---

## Description

The `GR32.Noise.Simplex` unit implements Ken Perlin's **Simplex Noise** algorithm for 2D, 3D, and 4D evaluation spaces. It provides the [[TSimplexNoise]] class, offering fast, smooth, pseudo-random continuous gradient noise with reduced computational overhead and visually superior isotropic isotropy compared to traditional grid-based Perlin noise.

### Common Use Cases

1. **Procedural Texture Generation**: Synthesizing natural patterns such as clouds, smoke, marble, wood grain, fire, and liquid surfaces without visible grid artifacts or directional bias.
2. **Terrain & Heightmap Generation**: Creating continuous landscape elevation maps and heightfields across 2D spatial coordinates.
3. **Organic Motion & Particle Swarms**: Driving organic motion in 2D or 3D vector fields where 3D or 4D noise (using time $t$ as an extra dimension) produces smoothly evolving forces over time.
4. **Domain Warping & Distortion**: Displacing spatial coordinates ($x, y$) prior to sampling other patterns or images to simulate turbulence, rippling water, or heat haze.

### Mathematical Background

Standard classical Perlin noise divides space into a hypercubic grid ($N$-dimensional cubes). In $N$ dimensions, a hypercube has $2^N$ vertices, causing the computational complexity of classical noise to scale exponentially as $O(2^N)$. Consequently, sampling classical 3D noise requires evaluating 8 corners, while 4D noise requires evaluating 16 corners.

Simplex noise replaces hypercubic grids with a **simplical grid** (tessellation composed of $N$-dimensional simplices):
- In 2D, a simplex is an equilateral triangle (3 vertices).
- In 3D, a simplex is a tetrahedron (4 vertices).
- In 4D, a simplex is a 5-cell / pentatope (5 vertices).

Because an $N$-dimensional simplex has only $N + 1$ vertices, the computational complexity of Simplex noise scales as $O(N^2)$, making higher-dimensional sampling (3D and 4D) significantly faster.

#### 1. Coordinate Space Skewing and Unskewing

To partition space into simplices, the input coordinate vector $\mathbf{x} = (x_1, x_2, \dots, x_n)$ is transformed from standard Euclidean space to a skewed simplical space using a skew factor $F_n$:

$$s = \left(\sum_{i=1}^n x_i\right) \cdot F_n$$

$$x'_i = x_i + s, \quad \text{where } F_n = \frac{\sqrt{n+1}-1}{n}$$

The cell coordinates in skewed space are determined by taking the floor values: $i_k = \lfloor x'_k \rfloor$. To calculate the unskewed displacement vector from the cell origin back to Euclidean space, an unskewing factor $G_n$ is applied:

$$t = \left(\sum_{i=1}^n i_k\right) \cdot G_n$$

$$x_k = i_k - t, \quad \text{where } G_n = \frac{1 - \frac{1}{\sqrt{n+1}}}{n}$$

The exact skew and unskew constants for 2D, 3D, and 4D spaces implemented in [[TSimplexNoise]] are:

| Dimension $N$ | Skew Factor $F_n$ | Unskew Factor $G_n$ | Simplex Shape | Vertices ($N+1$) |
|---|---|---|---|---|
| **2D** | $F_2 = \frac{\sqrt{3}-1}{2} \approx 0.3660254038$ | $G_2 = \frac{3-\sqrt{3}}{6} \approx 0.2113248654$ | Equilateral Triangle | 3 |
| **3D** | $F_3 = \frac{1}{3} \approx 0.3333333333$ | $G_3 = \frac{1}{6} \approx 0.1666666667$ | Tetrahedron | 4 |
| **4D** | $F_4 = \frac{\sqrt{5}-1}{4} \approx 0.3090169944$ | $G_4 = \frac{5-\sqrt{5}}{20} \approx 0.1381966011$ | 5-Cell (Pentatope) | 5 |

#### 2. Simplex Traversal & Gradient Kernel Summation

1. **Cell Partitioning**: By comparing relative magnitudes of fractional displacements (e.g. $dx > dy > dz$), the algorithm determines the precise simplex traversal order through the $N+1$ corners of the containing simplex.
2. **Pseudo-Random Gradient Hashing**: Each vertex index is hashed through a seed-based 512-byte permutation table (`FPerm`) to look up a unit gradient vector $\mathbf{g}_k$.
3. **Distance Attenuation Radial Kernel**: For each vertex $k$, the distance vector $\mathbf{dx}_k$ from the vertex to the evaluation point is calculated in unskewed Euclidean space. The contribution $n_k$ of vertex $k$ is governed by a radially symmetric polynomial kernel:

$$t_k = \max\left(0, r^2 - |\mathbf{dx}_k|^2\right)$$

$$n_k = t_k^4 \cdot \left(\mathbf{g}_k \cdot \mathbf{dx}_k\right)$$

where $r^2 = 0.5$ for 2D and 3D (or $0.6$ for 4D).

Summing the corner contributions $n = \sum_{k=0}^N n_k$ yields a smooth $C^2$ continuous scalar value scaled within the range $[-1.0, 1.0]$.

### Reference Links

- **Ken Perlin**: *"Noise Hardware"*, Real-Time Shading SIGGRAPH Course Notes (2001). [http://www.csee.umbc.edu/~olano/s2002c36/ch02.pdf](http://www.csee.umbc.edu/~olano/s2002c36/ch02.pdf)
- **Stefan Gustavson**: *"Simplex noise demystified"*, Linköping University, Sweden (2005/2012). [https://github.com/stegu/perlin-noise/blob/master/simplexnoise.pdf](https://github.com/stegu/perlin-noise/blob/master/simplexnoise.pdf)
- **Stefan Gustavson**: *Simplex Noise 1D-4D Reference Implementation in C*. [https://github.com/stegu/perlin-noise/blob/master/src/simplexnoise1234.c](https://github.com/stegu/perlin-noise/blob/master/src/simplexnoise1234.c)

[members]
