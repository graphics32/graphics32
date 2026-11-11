# Graphics32

https://github.com/graphics32/graphics32

Graphics32 is a high-performance graphics library for Delphi and Lazarus/Free Pascal.

Optimized for 32-bit pixel formats, it provides fast operations with pixel, vector, and polygon graphic primitives. The library significantly outperforms GDI, GDI+, and the standard TBitmap and TCanvas classes.

## Key Features

- **Performance**: High-speed per-pixel access and optimized drawing routines for 32-bit device-independent bitmaps (DIBs).
- **Alpha Blending**: Comprehensive support for bitmap alpha blending, including per-pixel alpha channels.
- **Antialiasing**: High-quality line and polygon antialiasing with sub-pixel accuracy.
- **Transformations**: Support for affine (rotation, scaling), projective, and arbitrary remapping transformations (warping, morphing) with sub-pixel accuracy.
- **Resampling**: Bitmap resampling using high-quality reconstruction filters, including Lanczos, Cubic, and Mitchell.
- **Polygon Rendering**: A state-of-the-art polygon rasterizer with flexible polygon transformation and custom filling options.
- **Repaint Optimization**: Flicker-free image display controls utilizing an advanced MicroTiles-based repaint optimizer.
- **Layers**: Multi-layer support with customizable overlay layers.
- **Text**: Crystal clear anti-aliased, vector based text rendering with advanced layout.
- **Multithreading**: Safe bitmap locking mechanism for multithreaded applications.
- **Backends**: Extensible surface-managing backends for cross-platform and framework compatibility.
- **Optimized**: Hot spots are implemented in highly optimized SIMD assembler (SSE2 and SSE4.1) with optional fallback to pure Pascal.

## Supported Platforms and Frameworks

Graphics32 supports a wide range of development environments and platforms:

|                        | Delphi                                                       | Lazarus / Free Pascal                                      |
| ---------------------- | ------------------------------------------------------------ | ---------------------------------------------------------- |
| **Versions supported** | 10.2 and later.<br />Generally, versions more than 10 years old are not supported. | Generally, only the last few releases are supported.       |
| **Platforms**          | Windows 7 and later, 32- and 64-bit.                         | Windows 7 and later, 32- and 64-bit.<br />Linux<br />macOS |
| **Framework**          | VCL                                                          | LCL                                                        |

## Documentation

The documentation for Graphics32 is available online at [https://graphics32.github.io](https://graphics32.github.io).

In addition we have [an extensive suite of examples](./Examples/README.md) that showcases much of Graphics32's features.

## License

This project is dual-licensed under the **Mozilla Public License (MPL) 1.1** or the **GNU Lesser General Public License (LGPL) 2.1 with linking exception**.

`SPDX-License-Identifier: MPL-1.1+ OR LGPL-2.1-linking-exception+`
