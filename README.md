# GR32PNG
PNG library for reading and writing of PNG images in combination with a TBitmap32 class from the GR32 library.

## Usage
The simplest way to load a PNG to a TBitmap32 instance is by using:

    LoadBitmap32FromPNG(Bitmap: TBitmap32; const Filename: string);

If the PNG file comes from a stream one can also use

    LoadBitmap32FromPNG(Bitmap: TBitmap32; Stream: TStream);

like in

    var
      MemoryStream: TMemoryStream;
    begin
      MemoryStream := TMemoryStream.Create;
      try
        MemoryStream.LoadFromFile(FileName);
        LoadBitmap32FromPNG(Bitmap, Stream);
      finally
        MemoryStream.Free;
      end;
    end;

This might be especially useful if the PNG file is embedded as resource into the executable. In this case you can easily use TResourceStream

To save the content of a TBitmap32 instance to a PNG file or stream the functions:

    procedure SaveBitmap32ToPNG(Bitmap: TBitmap32; FileName: string);
    procedure SaveBitmap32ToPNG(Bitmap: TBitmap32; Stream: TStream);

can be used.
