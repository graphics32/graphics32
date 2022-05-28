# GR32PNG
PNG library for reading and writing of PNG images in combination with a TBitmap32 class from the GR32 library.

## Usage
In order to use the library ensure that you use the unit GR32_PNG. This unit uses GR32_PortableNetworkGraphics to read and write PNG files, which makes it necessary to add the file to your project as well. Or just add the directory to the library path.

The simplest way to load a PNG to a TBitmap32 instance is by using:

    LoadBitmap32FromPNG(Bitmap: TBitmap32; const Filename: string);

You can check if the file is a valid PNG file by using

    function IsValidPNG(const Filename: string): Boolean;

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

## Advanced Usage
If you use the simple functions you won't be able to track the progress during loading. To do so you must create an instance of TPortableNetworkGraphic32 and load the PNG file manually. Before actually loading you can specify an OnProgress handler to track the progress. The code will then look like this:

      with TPortableNetworkGraphic32.Create do
      try
        OnProgress := OnProgressHandler;
        LoadFromFile(Filename);
        AssignTo(YourBitmap); // can be Image32.Bitmap for example
      finally
        Free;
      end;

Where the OnProgressHandler might looks like this:

    procedure TForm1.OnProgressHandler(Sender: TObject; Percent: Single);
    begin
      ProgressBar.Position := Round(Percent);
    end;

If you want to change some settings while saving you need to do this manually as well. The code can then look like this:

      with TPortableNetworkGraphic32.Create do
      try
        AssignTo(YourBitmap); // can be Image32.Bitmap for example
        Interlaced := imAdam7; // save as interlaced png file
        SaveToFile(Filename);
      finally
        Free;
      end;

With the current state of implementation it's not possible to alter everything without ending in an invalid PNG file. For example you could convert the image to use a pallete and then alter or delete the palette before saving. Doing so will not give you an error, but most probably the image won't be readable by other programs (or at least different to the original image).
