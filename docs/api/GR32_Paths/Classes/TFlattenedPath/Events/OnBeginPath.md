---
layout: doc
docType: api
unit: GR32_Paths
parent: TFlattenedPath
entity: TFlattenedPath.OnBeginPath
kind: Event
declaration: "property OnBeginPath: TNotifyEvent read FOnBeginPath write FOnBeginPath;"
summary: "Occurs when a new sub-path contour begins construction."
---

## Description

`OnBeginPath` is triggered when a new sub-path contour commences building (such as upon receiving the first vertex after a call to [[MoveTo]]).

## Example

```pascal
procedure TMyClass.HandleBeginPath(Sender: TObject);
begin
  WriteLn('Sub-path construction started.');
end;

procedure TMyClass.BuildPath;
var
  Path: TFlattenedPath;
begin
  Path := TFlattenedPath.Create;
  try
    Path.OnBeginPath := HandleBeginPath;
    Path.MoveTo(10.0, 10.0);
    Path.LineTo(100.0, 10.0);
    Path.EndPath;
  finally
    Path.Free;
  end;
end;
```
