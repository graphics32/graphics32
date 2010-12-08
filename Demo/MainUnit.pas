unit MainUnit;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE}Windows, Messages, {$ENDIF} SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, GR32_Image;

type
  TFmPngDemo = class(TForm)
    ImageDisplay: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure ImageDisplayClick(Sender: TObject);
    procedure ImageDisplayDblClick(Sender: TObject);
  end;

var
  FmPngDemo: TFmPngDemo;

implementation

uses
  GR32_PNG, GR32_PortableNetworkGraphic;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

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
begin
 with TSaveDialog.Create(Self) do
  try
   Filter := 'PNG Images (*.png)|*.png';
   DefaultExt := '.png';
   if Execute then
    begin
     with TPortableNetworkGraphic32.Create do
      try
       CompressionFilterMethods := [aafmSub, aafmUp, aafmAverage];
       Assign(ImageDisplay.Bitmap);
       InterlaceMethod := imAdam7;
       SaveToFile(FileName);
      finally
       Free;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmPngDemo.ImageDisplayDblClick(Sender: TObject);
begin
 with TPortableNetworkGraphic32.Create do
  try
   Assign(ImageDisplay.Bitmap);
   SaveToFile('Test.png');
  finally
   Free;
  end;
end;

end.
