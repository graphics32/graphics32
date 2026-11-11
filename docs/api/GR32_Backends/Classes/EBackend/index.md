---
layout: doc
docType: api
unit: GR32_Backends
entity: EBackend
kind: Class
declaration: "EBackend = class(Exception)"
inheritance:
  - Exception
  - EBackend
summary: "Exception class raised for errors originating within Graphics32 surface backends."
---

## Description

`EBackend` is the exception type raised when a surface backend encounters an error during initialization, memory allocation, device context creation, or GDI handle operations.

## Description

Backend implementations raise `EBackend` (or standard `Exception` instances) when platform surface allocation APIs fail, such as when Windows `CreateDIBSection`, `CreateCompatibleDC`, or `SelectObject` calls return invalid handles.
