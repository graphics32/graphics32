---
layout: doc
docType: api
unit: GR32_Gamma
entity: UnregisterGammaChangeNotification
kind: Procedure
declaration: "procedure UnregisterGammaChangeNotification(Delegate: TGammaChangedProc);"
summary: "Unregisters an object method delegate from receiving global gamma change notifications."
parameters:
  - name: Delegate
    type: TGammaChangedProc
    description: "Previously registered object method delegate."
seealso:
  - "[[RegisterGammaChangeNotification]]"
  - "[[SetGamma]]"
  - "[[Set_sRGB]]"
---

## Description

`UnregisterGammaChangeNotification` removes a previously registered callback delegate (`TGammaChangedProc`) from the global gamma notification list.

## Example

```pascal
destructor TMyImageControl.Destroy;
begin
  UnregisterGammaChangeNotification(GammaChanged);
  inherited;
end;
```
