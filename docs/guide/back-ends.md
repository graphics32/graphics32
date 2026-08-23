# Back-ends

## Introduction

Traditionally bitmaps in Windows are GDI resources, managed by the GDI and limited by GDI resource restrictions and performance. While this is acceptable for many usage scenarios, there are many other that require less resource-restricted approaches, like handle-less bitmaps or bitmaps that rely on memory-mapped files. Additionally, since the GDI is a Windows-only technology, the need for broader platform support in Graphics32 dictate that we isolate all bitmap memory management, as well as OS or graphics subsystem specific methods, from the generic methods in [TBitmap32](/api/GR32/TBitmap32). All these platform specific parts are therefore delegated to so called *back-end* classes.

## Approach and compatibility

The Graphics32 bitmap base class is [TCustomBitmap32](/api/GR32/TCustomBitmap32). This class is truly platform agnostic and relies solely on the associated back-end class to handle its needs. For that reason it also missing all Text, Canvas and Handle methods. However, back-ends may implement predefined interfaces to implement this functionality, which is what the derived class [TBitmap32](/api/GR32/TBitmap32) does.

This approach maintains backwards compatibility with the library before the cross-platform back-end architecture was introduced; The [TBitmap32](/api/GR32/TBitmap32) class still exposes the same few properties and methods that rely on the Windows GDI, but now it does so with the help of a back-end.

## Switching the back-end

The back-end instance is switchable during the life-cycle of a [TCustomBitmap32](/api/GR32/TCustomBitmap32) instance. Transition from one back-end instance to another is handled transparently, i.e. without losing the bitmap’s surface contents. This, for instance, allows for temporarily switching a handle-less memory-only bitmap to a GDI bitmap with a handle and vice versa. [TCustomBitmap32](/api/GR32/TCustomBitmap32) exposes the current back-end via its [Backend](/api/GR32/TCustomBitmap32/Properties/Backend.htm) property.

```pascal:line-numbers
begin
  MyBitmap := TBitmap32.Create;
  TMMFBackend.Create(MyBitmap); // Switch to a handle-less memory mapped file back-end...
  MyBitmap.SetSize(5000, 5000);
  
  // Draw onto your new big bitmap...
  // Note: No text or canvas drawing is allowed because TMMFBackend does not implement those operations.
  // Use TGDIMMFBackend instead...
  
  TGDIBackend.Create(MyBitmap); // Switch to a GDI back-end and convert the current buffer...
  MyBitmap.SaveToFile('test.bmp');
end;
```

For the cases where the back-end temporarily might need to be switched to another, the `RequireBackendSupport` utility function can be used.
In the following example, we have a function that takes a bitmap and needs one of the `IDeviceContextSupport` or `ICanvasSupport` back-end interfaces in order to do its thing.

```pascal:line-numbers
procedure DoSomething(Bitmap: TCustomBitmap32);
var
  SavedBackend: TCustomBackend;
  DeviceContextSupport: IDeviceContextSupport;
  CanvasSupport: ICanvasSupport;
begin
  ASSERT(Bitmap <> nil);

  // Query the bitmap back-end for one of the required interfaces.
  // - We allow the back-end switch to be destructive (content is not preserved).
  // - The current back-is saved in SavedBackend.
  RequireBackendSupport(Bitmap, [IInteroperabilitySupport, ICanvasSupport],
    romOr, True, SavedBackend);
  try

    if Supports(Bitmap.Backend, IDeviceContextSupport, DeviceContextSupport) then
    begin
      // Do something through IDeviceContextSupport
      ...
      InteroperabilitySupport := nil;
    end else
    if Supports(Bitmap.Backend, ICanvasSupport, CanvasSupport) then
    begin
      // Do something through ICanvasSupport
      ...
      CanvasSupport := nil;
    end else
      raise Exception.Create('Required back-end not supported');

  finally
    // Switch back to the original back-end
    RestoreBackend(Bitmap, SavedBackend);
  end;
end;
```

::: info
Note that switching back-end is a relative expensive operation, even when the bitmap content isn't preserved across the switch, which is why `TBitmap` doesn't just do it automatically when it needs to.
:::

## Class and interface overview

Currently Graphics32 ships with the following back-end classes which are subclasses of [TBackend](/api/GR32/TBackend) and implement several interfaces (see below):

  * **Generic back-ends**
    * [TMemoryBackend](/api/GR32_Backends_Generic/TMemoryBackend) (default for [TCustomBitmap32](/api/GR32/TCustomBitmap32))
    * [TMMFBackend](/api/GR32_Backends_Generic/TMMFBackend) (Windows only)
  * **Delphi back-ends (Windows GDI/VCL)**
    * [TGDIBackend](/api/GR32_Backends_VCL/TGDIBackend) (default for [TBitmap32](/api/GR32/TBitmap32))
    * [TGDIMMFBackend](/api/GR32_Backends_VCL/TGDIMMFBackend)
    * [TGDIMemoryBackend](/api/GR32_Backends_VCL/TGDIMemoryBackend)
  * **Lazarus back-ends (currently Windows, OS X Carbon, GTK, Custom Drawn)**
    * TLCLBackend (default for [TBitmap32](/api/GR32/TBitmap32))
    * TLCLMMFBackend (Windows only)
    * TLCLMemoryBackend (Windows only)


To summarize, [TBitmap32](/api/GR32/TBitmap32) by default uses the back-end class `TGDIBackend` on Delphi/VCL/Windows and `TLCLBackend` on FreePascal/LCL.

Each of these back-ends may or may not implement certain pre-defined interfaces which can be queried for at runtime, either directly via the back-end or indirectly via the `Backend` property on the bitmap instance.
At the time of writing, these interfaces include:

  * [IPaintSupport](/api/GR32_Backends/Interfaces/IPaintSupport)
  * [ICopyFromBitmapSupport](/api/GR32_Backends/Interfaces/ICopyFromBitmapSupport)
  * [IBitmapContextSupport](/api/GR32_Backends/Interfaces/IBitmapContextSupport)
  * [IDeviceContextSupport](/api/GR32_Backends/Interfaces/IDeviceContextSupport)
  * [ICanvasSupport](/api/GR32_Backends/Interfaces/ICanvasSupport)
  * [ITextSupport](/api/GR32_Backends/Interfaces/ITextSupport)
  * [IFontSupport](/api/GR32_Backends/Interfaces/IFontSupport)
  * [ITextToPathSupport](/api/GR32_Backends/Interfaces/ITextToPathSupport)
  * [ITextToPathSupport2](/api/GR32_Backends/Interfaces/ITextToPathSupport2)
  * [IFontHintingSupport](/api/GR32_Backends/Interfaces/IFontHintingSupport)
  * [IUpdateRectSupport](/api/GR32_Backends/Interfaces/IUpdateRectSupport)
  * [IInteroperabilitySupport](/api/GR32_Backends/Interfaces/IInteroperabilitySupport)

::: info
Most of the platform backed methods and properties in [TBitmap32](/api/GR32/TBitmap32) query the back-end for the above interfaces.
If the back-end does not support the required interface then the operation will fail with an *Interface not supported* exception.

For example, the getter function for the `TBitmap.Handle` property, which returns a GDI Device Context handle, looks like this:
```pascal
function TBitmap32.GetHDC: HDC;
begin
  Result := (Backend as IDeviceContextSupport).Handle;
end;
```
:::