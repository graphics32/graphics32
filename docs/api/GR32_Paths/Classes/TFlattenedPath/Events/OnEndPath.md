---
layout: doc
docType: api
unit: GR32_Paths
parent: TFlattenedPath
entity: TFlattenedPath.OnEndPath
kind: Event
declaration: "property OnEndPath: TNotifyEvent read FOnEndPath write FOnEndPath;"
summary: "Occurs when a sub-path contour is completed and added to the Path array."
---

## Description

`OnEndPath` is triggered whenever a sub-path contour is finalized by [[EndPath]] and its flattened vertices are flushed into the [[Path]] list.

## Example

```pascal
procedure TMyClass.HandleEndPath(Sender: TObject);
begin
  WriteLn('Sub-path contour completed and flushed.');
end;

procedure TMyClass.BuildPath;
var
  Path: TFlattenedPath;
begin
  Path := TFlattenedPath.Create;
  try
    Path.OnEndPath := HandleEndPath;
    Path.Rectangle(FloatRect(10.0, 10.0, 50.0, 50.0));
  finally
    Path.Free;
  end;
end;
```
