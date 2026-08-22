# Simd optimizations

In order to achieve the best possible performance on a given host platform, Graphics32 implements most low level bottlenecks using [SIMD](https://en.wikipedia.org/wiki/Single_instruction,_multiple_data) optimized assembly instructions. SIMD instructions enable the CPU to execute multiple similar calculations in parallel and is thus particularly well suited for graphics processing where the same operation is often performed on the R, G, B, and possibly A channels, of an image.

Since the level of SIMD support varies with different CPU models, Graphics32 provides different implementations for different SIMD support levels and [automatically uses the best possible variant](https://melander.dk/graphics32/cpu-feature-detection/).
The most basic SIMD support is based on the [MMX](https://en.wikipedia.org/wiki/MMX_\(instruction_set\)) instruction set. Since MMX was introduced in 1997 it is supported by all processors supported by Delphi and thus Graphics32. Newer processors all support the [SSE](https://en.wikipedia.org/wiki/Streaming_SIMD_Extensions) (introduced 1999) and [SSE2](https://en.wikipedia.org/wiki/SSE2) (introduced 2000) instruction sets and today MMX support is _only_ included in Graphics32 for backward compatibility and as a reference.

### MMX vs SSE

The reason to favor SSE* optimizations over MMX optimizations is mainly due to MMX’s use of the x87 FPU’s floating point registers. As long as all processing is done solely in fixed point or integer registers, there is no problem, but if floating point calculations needs to be done after MMX has used the FPU registers then an expensive reset of the FPU registers must first be performed or else a floating point exception will be thrown. This reset is performed within the [EMMS](https://www.felixcloutier.com/x86/emms) function (an intrinsic to the assembler mnemonic with the same name).

It should be noted that even though a call to EMMS uses only a handful of cycles, frequent calls can slow down simple processing such as alpha blending a lot.

### The OMIT_MMX compiler conditional

The support for MMX optimizations can be controlled with the `OMIT_MMX` compiler conditional:

  * When `OMIT_MMX` is defined, the MMX optimizations will be entirely disabled.
The EMMS function becomes an empty static function that does nothing and which, due to compiler inlining, has no cost.


  * When `OMIT_MMX` is not defined, the MMX optimizations will be included and by default the EMMS function will execute the _emms_ assembler instruction.
However if Graphics32’s CPU feature detection has selected a newer SIMD technology, such as SSE or SSE2, then the EMMS function just becomes an empty virtual function that does nothing and the cost of calling it is nothing more that a call and return.



Note: As all x86 64-bit processors feature SSE2, `OMIT_MMX` is defined by default here. This not only results in better performance, but also in smaller executables.
