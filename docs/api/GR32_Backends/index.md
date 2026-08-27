---
layout: doc
docType: api
unit: GR32_Backends
entity: GR32_Backends
kind: Unit
summary: "Provides interface contracts, abstraction layers, helper routines, and exception types for TBitmap32 surface backends."
---

# Unit GR32_Backends

The `GR32_Backends` unit defines the core interface contracts, helper routines, and exception types for surface backend management in Graphics32. Backends abstract raw pixel buffer allocations, device context access, text rendering capabilities, canvas integration, and platform-specific surface operations.

---

## Classes

- [[EBackend]]: Exception class raised when backend allocation or initialization fails.

## Interfaces

- [[IBitmapContextSupport]]: Interface for backends providing DIB bitmap headers and handles.
- [[ICanvasSupport]]: Interface for backends providing VCL/LCL `TCanvas` integration.
- [[IDeviceContextSupport]]: Interface for backends providing native OS device context handles (`HDC`) and drawing routines.
- [[IFontHintingSupport]]: Interface for configuring text font hinting settings (deprecated).
- [[IFontSupport]]: Interface for backends managing font configuration and font change notifications.
- [[IInteroperabilitySupport]]: Interface for backends supporting pixel buffer copying from VCL/LCL `TGraphic` objects.
- [[IPaintSupport]]: Interface for backends executing repainting operations onto `TCanvas` controls.
- [[ITextSupport]]: Interface for backends executing GDI/LCL text rendering and measurement.
- [[ITextToPathSupport]]: Interface for converting rendered text glyph outlines into vector paths.
- [[ITextToPathSupport2]]: Extended interface for text-to-path conversion with advanced layout parameters.
- [[IUpdateRectSupport]]: Interface for retrieving invalid rectangular regions from window controls.

## Types

- [[TTextHinting]]: Enumeration specifying text font hinting modes.
- [[TRequireOperatorMode]]: Logical combination mode (`romAnd`, `romOr`) for querying required backend interface support.

## Routines

- [[RequireBackendSupport]]: Verifies if a bitmap's active backend supports required interface contracts, switching to a suitable backend if necessary.
- [[RestoreBackend]]: Restores a previously saved backend instance onto a bitmap target.
