# Back ends

## Introduction

Traditionally bitmaps in Graphics32 have been using resources managed by the Windows GDI. While this is feasible for most usage scenarios in Windows there are some that require certain less resource-restricted approaches like handle-less bitmaps or bitmaps that rely on memory-mapped files. Prior to Graphics32 1.9 several community-driven patches existed that added these features.
With the arrival of broader platform support in Graphics32 we have separated the memory management as well as OS or graphics subsystem specific methods from the generic methods in [TBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TBitmap32/_Body.htm) and have moved these portions into so called back-end classes.

## Approach and compatibility

In order to keep backwards compatibility the [TBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TBitmap32/_Body.htm) class still exposes the same external interface. In order to get a clean cut from the platform specific methods and properties we implemented a new in-between class [TCustomBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomBitmap32/_Body.htm) which is truly platform agnostic and relies solely on the associated back-end class to handle its needs. As a matter of fact [TCustomBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomBitmap32/_Body.htm) is missing all Text, Canvas and Handle methods. Back-ends may implement predefined interfaces to implement the functionality.

The back-end instance is switchable during the life-cycle of a [TCustomBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomBitmap32/_Body.htm) instance. Conversion to the new back-end instance is handled transparently, i.e. without loosing the bitmap’s surface contents. For instance this allows for temporarily switching a handle-less memory-only bitmap to a GDI bitmap with handle and vice versa. [TCustomBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomBitmap32/_Body.htm) exposes the current back-end via its [Backend](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomBitmap32/Properties/Backend.htm) property.

123456789101112| `begin`` ``MyBitmap := TBitmap32``.``Create;`` ``TMMFBackend``.``Create(MyBitmap); ``// Switch to a handle-less memory mapped file back-end...`` ``MyBitmap``.``SetSize(``5000``, ``5000``);`` ` ` ``// Draw onto your new big bitmap...`` ``// Note: No text or canvas drawing is allowed because TMMFBackend does not implement those operations.`` ``// Use TGDIMMFBackend instead...`` ` ` ``TGDIBackend``.``Create(MyBitmap); ``// Switch to a GDI back-end and convert the current buffer...`` ``MyBitmap``.``SaveToFile(``'test.bmp'``);``end``;`
---|---

## Class and interface overview

Currently Graphics32 ships with the following back-end classes which are subclasses of [TBackend](https://graphics32.github.io/Docs/Units/GR32/Classes/TBackend/_Body.htm) and implement several interfaces (see below):

  * **Generic back-ends**
    * [TMemoryBackend](https://graphics32.github.io/Docs/Units/GR32_Backends_Generic/Classes/TMemoryBackend/_Body.htm) (default for [TCustomBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomBitmap32/_Body.htm))
    * [TMMFBackend](https://graphics32.github.io/Docs/Units/GR32_Backends_Generic/Classes/TMMFBackend/_Body.htm)
  * **Windows GDI/VCL back-ends**
    * [TGDIBackend](https://graphics32.github.io/Docs/Units/GR32_Backends_VCL/Classes/TGDIBackend/_Body.htm) (default for [TBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TBitmap32/_Body.htm))
    * [TGDIMMFBackend](https://graphics32.github.io/Docs/Units/GR32_Backends_VCL/Classes/TGDIMMFBackend/_Body.htm)
    * [TGDIMemoryBackend](https://graphics32.github.io/Docs/Units/GR32_Backends_VCL/Classes/TGDIMemoryBackend/_Body.htm)
  * **Lazarus back-ends (currently Windows, OS X Carbon, GTK)**
    * TLCLBackend (default for [TBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TBitmap32/_Body.htm))
    * TLCLMMFBackend (Windows only)



By default [TBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TBitmap32/_Body.htm) now uses the back-end class TGDIBackend on Delphi/VCL/Windows and TLCLBackend on FreePascal/LCL/[supported OS (see above)].

Each of these back-ends may or may not implement certain pre-defined interfaces which can be queried for at runtime, either directly via the back-end or indirectly via the bitmap instance:

  * [IPaintSupport](https://graphics32.github.io/Docs/Units/GR32_Backends/Interfaces/IPaintSupport/_Body.htm)
  * [ICopyFromBitmapSupport](https://graphics32.github.io/Docs/Units/GR32_Backends/Interfaces/ICopyFromBitmapSupport/_Body.htm)
  * [IBitmapContextSupport](https://graphics32.github.io/Docs/Units/GR32_Backends/Interfaces/IBitmapContextSupport/_Body.htm)
  * [IDeviceContextSupport](https://graphics32.github.io/Docs/Units/GR32_Backends/Interfaces/IDeviceContextSupport/_Body.htm)
  * [ITextSupport](https://graphics32.github.io/Docs/Units/GR32_Backends/Interfaces/ITextSupport/_Body.htm)
  * [IFontSupport](https://graphics32.github.io/Docs/Units/GR32_Backends/Interfaces/IFontSupport/_Body.htm)
  * [ICanvasSupport](https://graphics32.github.io/Docs/Units/GR32_Backends/Interfaces/ICanvasSupport/_Body.htm)



##### Note

Most of the methods and properties left in [TBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TBitmap32/_Body.htm) query the back-end for these specific interfaces. Failing to implement the required interfaces in the back-end class will cause the method call or property read to fail with an exception. We recommend to change your custom routines or methods to use [TCustomBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomBitmap32/_Body.htm) instead of [TBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TBitmap32/_Body.htm) wherever possible.
