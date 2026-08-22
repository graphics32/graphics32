# Alpha composition

**Alpha composition** , or **alpha blending** as it is perhaps better known, is the process of combining one image with another while taking the transparency of the images into account.

Graphics32 supports three fundamental blend operations: **Blend** , **Merge** , and **Combine**.

## Blend

The Blend operation mixes a foreground color **F** with a background color **B** using the alpha component of the foreground color.

SRGB=FA∗FRGB+(1–FA)∗BRGBSRGB=FA∗FRGB+(1–FA)∗BRGB

The blend operation is a simplification of the merge operation; It is an optimization that, by assuming that **the background is fully opaque** (i.e. has an alpha value A = 255), avoids the merge operation’s costly calculation of the result alpha.

Note that while the blend operation assume that the background alpha is 255, for performance reasons, many blend functions still calculate the result alpha using the blend formula:

SA=FA∗FA+(1–FA)∗BASA=FA∗FA+(1–FA)∗BA

In most cases this alpha calculation only makes sense when the background is opaque.

Note
---
The **Blend** operation is almost exclusively used when drawing onto an opaque (i.e. non-transparent) background.
If you are drawing onto a semi-transparent background then the **Merge** operation is most likely what you want to use.

## Merge

The Merge operation merges a foreground color **F** with the background color **B** using the alpha component of the foreground color. It merges both the RGB and alpha-channels.

SA=1–(1–FA)∗(1–BA)SA=1–(1–FA)∗(1–BA) SRGB=FA∗FRGB+BA∗(1–FA)∗BRGBSASRGB=FA∗FRGB+BA∗(1–FA)∗BRGBSA

Note
---
The **Merge** operation is expensive. If you are drawing onto a fully opaque (i.e. non-transparent) background then it is much more efficient to use the **Blend** operation.

## Combine

The Combine operation performs _linear interpolation_ between two colors: **X** and **Y**. The **W** parameter, which must be in [0..255] range, specifies the weight of the first color (**X**). The alpha channel is interpolated as well.

SRGBA=W∗XRGBA+(1–W)∗YRGBA=W∗(XRGBA–YRGBA)+YRGBASRGBA=W∗XRGBA+(1–W)∗YRGBA=W∗(XRGBA–YRGBA)+YRGBA

The Combine operation is also known as _Lerp_ or _Cross-fade_.

## Alpha composition functions

For both **Blend** and **Merge** , Graphics32 implements a suite of highly optimized low-level functions.
Implementations of these functions are available in both Pascal and hand optimized 32-bit and 64-bit assembler using various levels of processor features, such as MMX, SSE, SSE2, SSE4.1, etc. At run-time Graphics32 will [automatically select](https://melander.dk/graphics32/cpu-feature-detection/) the fastest version to use based on what the host system supports.

Note
---
The Blend and Merge functions that are implemented using MMX instructions alters the CPU floating point flags. Therefore, after using _any_ of these functions, you _must_ call the [EMMS](https://graphics32.github.io/Docs/Units/GR32_Blend/Routines/EMMS.htm) function to reset the floating point state. Otherwise the CPU will be unable to handle subsequent floating point instructions.

#### Blend

`function` `BlendReg(F, B: TColor32): TColor32;``procedure` `BlendMem(F: TColor32; ``var` `B: TColor32);``procedure` `BlendMems(F: TColor32; B: PColor32; Count: ``Integer``);``function` `BlendRegEx(F, B: TColor32; M: ``Cardinal``): TColor32;``procedure` `BlendMemEx(F: TColor32; ``var` `B: TColor32; M: ``Cardinal``);``procedure` `BlendLine(Src, Dst: PColor32; Count: ``Integer``);``procedure` `BlendLineEx(Src, Dst: PColor32; Count: ``Integer``; M: ``Cardinal``);``procedure` `BlendLine1(Src: TColor32; Dst: PColor32; Count: ``Integer``);``function` `BlendRegRGB(F, B: TColor32; W: ``Cardinal``): TColor32;``procedure` `BlendMemRGB(F: TColor32; ``var` `B: TColor32; W: ``Cardinal``);`
---

#### Merge

`function` `MergeReg(F, B: TColor32): TColor32;``procedure` `MergeMem(F: TColor32; ``var` `B: TColor32);``function` `MergeRegEx(F, B: TColor32; M: ``Cardinal``): TColor32;``procedure` `MergeMemEx(F: TColor32; ``var` `B: TColor32; M: ``Cardinal``);``procedure` `MergeLine(Src, Dst: PColor32; Count: ``Integer``);``procedure` `MergeLineEx(Src, Dst: PColor32; Count: ``Integer``; M: ``Cardinal``);``procedure` `MergeLine1(Src: TColor32; Dst: PColor32; Count: ``Integer``);`
---

#### Combine

12| `function` `CombineReg(X, Y: TColor32; W: ``Cardinal``): TColor32;``procedure` `CombineMem(X: TColor32; ``var` `Y: TColor32; W: ``Cardinal``);`
---|---

### Master alpha functions

Some blend functions, notably those with the **Ex** suffix, takes an extra parameter **M** specifying a _master alpha_ value.

This master alpha is used to scale the alpha values used in the Blend or Merge operations. Thus the Blend operation becomes:

SARGB=(M∗FA)∗FARGB+(1–(M∗FA))∗BARGBSARGB=(M∗FA)∗FARGB+(1–(M∗FA))∗BARGB

and the Merge operation becomes:

SA=1–(1–(M∗FA))∗(1–BA)SA=1–(1–(M∗FA))∗(1–BA) SRGB=(M∗FA)∗FRGB+BA∗(1–(M∗FA))∗BRGBSASRGB=(M∗FA)∗FRGB+BA∗(1–(M∗FA))∗BRGBSA

Note that while **M** is declared as a Cardinal, it should only contain values in the [0..255] range when used with the standard blend and merge functions; They do not perform range checking and the result in case **M** > 255 is undefined. For custom blend functions, such as those you write yourself, there are no restriction on the value of **M** ; The interpretation of the value depends entirely on the function.

### RGB alpha functions

Most blend functions applies a single alpha value to each of the R, G, and B channels. However in some situations it is necessary to blend each channel independently and for this we have the functions with the **RGB** suffix; These functions does not use the alpha channel of **F** when blending. Instead they use the individual R, G, and B components of the **W** parameter as the alpha value:

SRGB=(FRGB–BRGB)∗WRGB+BRGBSRGB=(FRGB–BRGB)∗WRGB+BRGB

Note that the **W** parameter, while declared as a Cardinal, is in fact a TColor32 value where the A channel is ignored.

#### BlendReg, MergeReg

`function` `BlendReg(F, B: TColor32): TColor32;``function` `MergeReg(F, B: TColor32): TColor32;`
---

Blends or Merges **F** onto **B** , returning **S**.

#### BlendMem, MergeMem

`procedure` `BlendMem(F: TColor32; ``var` `B: TColor32);``procedure` `MergeMem(F: TColor32; ``var` `B: TColor32);`
---

Blends or Merges **F** onto **B** , returning **S** in **B**.

BlendMem is optimized so that it excludes writing operation for fully transparent foreground pixels and reading operation for fully opaque ones.

#### BlendMems

`procedure` `BlendMems(F: TColor32; B: PColor32; Count: ``Integer``);`
---

Blends **F** onto an array of **Count** pixels pointed to by **B**.
Corresponds to the following pseudo code:

123456789| `procedure` `BlendMems(F: TColor32; B: PColor32; Count: ``Integer``);``begin`` ``while` `(Count > ``0``) ``do`` ``begin`` ``BlendMem(F, B^);`` ``Inc(B);`` ``Dec(Count);`` ``end``;``end``;`
---|---

#### BlendRegEx, MergeRegEx

`function` `BlendRegEx(F, B: TColor32; M: ``Cardinal``): TColor32;``function` `MergeRegEx(F, B: TColor32; M: ``Cardinal``): TColor32;`
---

Blends or Merges, applying the master alpha **M** , **F** onto **B** , returning **S**.

#### BlendMemEx. MergeMemEx

`procedure` `BlendMemEx(F: TColor32; ``var` `B: TColor32; M: ``Cardinal``);``procedure` `MergeMemEx(F: TColor32; ``var` `B: TColor32; M: ``Cardinal``);`
---

Blends or Merges, applying the master alpha **M** , **F** onto **B** , returning **S** in **B**.

#### BlendRegRGB

`function` `BlendRegRGB(F, B: TColor32; W: ``Cardinal``): TColor32;`
---

Blends **F** onto **B** , using the alpha specified by **W** , returning **S**.

#### BlendMemRGB

`procedure` `BlendMemRGB(F: TColor32; ``var` `B: TColor32; W: ``Cardinal``);`
---

Blends or Merges **F** onto **B** , using the alpha specified by **W** , returning **S** in **B**.

#### BlendLine, MergeLine

`procedure` `BlendLine(Src, Dst: PColor32; Count: ``Integer``);``procedure` `MergeLine(Src, Dst: PColor32; Count: ``Integer``);`
---

Blends or Merges an array of **Count** pixels pointed to by **Src** onto an array of pixels pointed to by **Dst**.
Corresponds to the following pseudo code:

12345678910| `procedure` `BlendLine(Src, Dst: PColor32; Count: ``Integer``);``begin`` ``while` `(Count > ``0``) ``do`` ``begin`` ``BlendMem(Src^, Dst^);`` ``Inc(Src);`` ``Inc(Dst);`` ``Dec(Count);`` ``end``;``end``;`
---|---

#### BlendLineEx, MergeLineEx

`procedure` `BlendLineEx(Src, Dst: PColor32; Count: ``Integer``; M: ``Cardinal``);``procedure` `MergeLineEx(Src, Dst: PColor32; Count: ``Integer``; M: ``Cardinal``);`
---

Blends or Merges, applying the master alpha **M** , an array of **Count** pixels pointed to by **Src** onto an array of pixels pointed to by **Dst**.
Corresponds to the following pseudo code:

12345678910| `procedure` `BlendLineEx(Src, Dst: PColor32; Count: ``Integer``; M: ``Cardinal``);``begin`` ``while` `(Count > ``0``) ``do`` ``begin`` ``BlendMemEx(Src^, Dst^, M);`` ``Inc(Src);`` ``Inc(Dst);`` ``Dec(Count);`` ``end``;``end``;`
---|---

#### BlendLine1, MergeLine1

`procedure` `BlendLine1(Src: TColor32; Dst: PColor32; Count: ``Integer``);``procedure` `MergeLine1(Src: TColor32; Dst: PColor32; Count: ``Integer``);`
---

Blends or Merges the single color **Src** onto an array of **Count** pixels pointed to by **Dst**.
Corresponds to the following pseudo code:

123456789| `procedure` `BlendLine1(Src: TColor32; Dst: PColor32; Count: ``Integer``);``begin`` ``while` `(Count > ``0``) ``do`` ``begin`` ``BlendMem(Src, Dst^);`` ``Inc(Dst);`` ``Dec(Count);`` ``end``;``end``;`
---|---

#### CombineReg

`function` `CombineReg(X, Y: TColor32; W: ``Cardinal``): TColor32;`
---

Interpolates between **X** and **Y** , using the weight **W** , returning **S**.

#### CombineMem

`procedure` `CombineMem(X: TColor32; ``var` `Y: TColor32; W: ``Cardinal``);`
---

Interpolates between **X** and **Y** , using the weight **W** , returning **S** in **Y**.

#### CombineLine

`procedure` `CombineLine(Src, Dst: PColor32; Count: ``Integer``; W: ``Cardinal``);`
---

Interpolates between the colors of the **Count** individual pixels in the arrays **Src** and **Dst** , using the weight **W** , and stores the result in the **Dst** pixels.
Corresponds to the following pseudo code:

12345678910| `procedure` `CombineLine(Src, Dst: PColor32; Count: ``Integer``; W: ``Cardinal``);``begin`` ``while` `(Count > ``0``) ``do`` ``begin`` ``CombineMem(Src^, Dst^, W);`` ``Inc(Src);`` ``Inc(Dst);`` ``Dec(Count);`` ``end``;``end``;`
---|---
