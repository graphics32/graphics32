---
layout: doc
docType: api
unit: GR32_Gamma
entity: RegisterGammaChangeNotification
kind: Procedure
declaration: "procedure RegisterGammaChangeNotification(Delegate: TGammaChangedProc);"
summary: "Registers an object method delegate to be notified whenever global gamma tables are modified."
parameters:
  - name: Delegate
    type: TGammaChangedProc
    description: "Object method delegate signature procedure of object to register for notifications."
seealso:
  - "[[UnregisterGammaChangeNotification]]"
  - "[[SetGamma]]"
  - "[[Set_sRGB]]"
---

## Description

`RegisterGammaChangeNotification` registers a callback procedure (`TGammaChangedProc`) into the global delegate list.

Whenever [[SetGamma]] or [[Set_sRGB]] is called to modify global lookup tables ([[GAMMA_ENCODING_TABLE]] or [[GAMMA_DECODING_TABLE]]), all registered delegates are invoked automatically.

::: warning Thread Safety
`RegisterGammaChangeNotification` and gamma delegate invocation are **not thread-safe**. Do not modify registered delegates or change global gamma settings concurrently across multiple execution threads.
:::

## Delegate Signature

```pascal
type
  TGammaChangedProc = procedure of object;
```

## Example

```pascal
type
  TMyImageControl = class
  private
    procedure GammaChanged;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TMyImageControl.Create;
begin
  inherited;
  RegisterGammaChangeNotification(GammaChanged);
end;

destructor TMyImageControl.Destroy;
begin
  UnregisterGammaChangeNotification(GammaChanged);
  inherited;
end;

procedure TMyImageControl.GammaChanged;
begin
  // Repaint or recalculate buffers when global gamma changes
end;
```
