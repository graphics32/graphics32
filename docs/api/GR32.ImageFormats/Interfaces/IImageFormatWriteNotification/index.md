---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: IImageFormatWriteNotification
kind: Interface
declaration: "IImageFormatWriteNotification = interface"
summary: "Notifies an adapter when writing operations begin and end."
---

## Description

`IImageFormatWriteNotification` provides lifecycle callbacks before and after calling `CanAssignTo` or `AssignTo`. This allows adapters (such as clipboard formats) to perform necessary preparation or cleanup tasks, such as opening and closing system clipboard handles.

[members]
