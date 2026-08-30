program CreatingAndManagingBitmaps;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  GR32;

procedure CreateAndClearBitmap;
var
  Bitmap: TBitmap32;
begin
  Bitmap := TBitmap32.Create;
  try
    // Set the width and height of the bitmap.
    // Do not waste time clearing the bitmap since we do that below.
    Bitmap.SetSize(400, 300, False);

    // Clear the entire canvas with a semi-transparent blue-ish background
    // Color32 parameters: Red, Green, Blue, Alpha (0 = transparent, 255 = opaque)
    Bitmap.Clear(Color32(41, 128, 185, 255));

    // Save the result to a BMP file...
    Bitmap.SaveToFile('getting-started-bitmap.bmp');
    // ...and also save to a PNG file while we're at it
    Bitmap.SaveToFile('getting-started-bitmap.png');
  finally
    Bitmap.Free;
  end;
end;

begin
  try
    CreateAndClearBitmap;

    Writeln('Bitmap created and saved successfully to:');
    Writeln('* getting-started-bitmap.bmp');
    Writeln('* getting-started-bitmap.png');
    Writeln;
    Writeln('Press enter');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
