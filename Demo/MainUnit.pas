unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GR32_Image;

type
  TFmPngDemo = class(TForm)
    ImageDisplay: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure ImageDisplayClick(Sender: TObject);
  end;

var
  FmPngDemo: TFmPngDemo;

implementation

uses
  GR32_PNG;

{$R *.dfm}

procedure TFmPngDemo.FormCreate(Sender: TObject);
begin
 if FileExists('PNG.png') then
  with TPortableNetworkGraphic32.Create do
   try
    LoadFromFile('PNG.png');
    AssignTo(ImageDisplay.Bitmap);
   finally
    Free;
   end;

 ClientWidth := ImageDisplay.Bitmap.Width + 16;
 ClientHeight := ImageDisplay.Bitmap.Height + 16;
end;

procedure TFmPngDemo.ImageDisplayClick(Sender: TObject);
var
  PNG : TPortableNetworkGraphic32;
begin
 with TSaveDialog.Create(Self) do
  try
   Filter := 'PNG Images (*.png)|*.png';
   DefaultExt := '.png';
   if Execute then
    begin
     with TPortableNetworkGraphic32.Create do
      try
       Assign(ImageDisplay.Bitmap);
       SaveToFile(FileName);
      finally
       Free;
      end;
    end;
  finally
   Free;
  end;
end;

end.
