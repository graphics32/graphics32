unit MainUnit;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GR32_Image;

type
  TFmPngDemo = class(TForm)
    ImageDisplay: TImage32;
    Panel1: TPanel;
    ButtonLoad: TButton;
    ButtonSave: TButton;
    CheckBoxFit: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure CheckBoxFitClick(Sender: TObject);
  end;

var
  FmPngDemo: TFmPngDemo;

implementation

uses
  GR32.Examples,
  GR32,
  GR32_PNG,
  GR32_PortableNetworkGraphic;

{$R *.dfm}

procedure TFmPngDemo.ButtonLoadClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  PNG: TPortableNetworkGraphic32;
begin
  OpenDialog := TOpenDialog.Create(Self);
  try
    OpenDialog.Filter := 'PNG Images (*.png)|*.png|All files (*.*)|*.*';
    OpenDialog.DefaultExt := '.png';
    if not OpenDialog.Execute then
      exit;

    (*
    ** Load via TPortableNetworkGraphic32
    *)
    PNG := TPortableNetworkGraphic32.Create;
    try

      PNG.LoadFromFile(OpenDialog.FileName);

      ImageDisplay.Bitmap.Assign(PNG);

    finally
      PNG.Free;
    end;

    (*
    ** Note that we could just as easily have loaded directly via TBitmap32:
    **
    **   ImageDisplay.Bitmap.LoadFromFile(OpenDialog.FileName);
    **
    ** The end-result is the same.
    *)

  finally
    OpenDialog.Free;
  end;
end;

procedure TFmPngDemo.ButtonSaveClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  PNG: TPortableNetworkGraphic32;
begin
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.Filter := 'PNG Images (*.png)|*.png|All files (*.*)|*.*';
    SaveDialog.DefaultExt := '.png';
    if not SaveDialog.Execute then
      exit;

    (*
    ** Save via TPortableNetworkGraphic32
    *)
    PNG := TPortableNetworkGraphic32.Create;
    try

      PNG.AdaptiveFilterMethods := [aafmSub, aafmUp, aafmAverage];
      PNG.Assign(ImageDisplay.Bitmap);
      PNG.InterlaceMethod := imAdam7;

      PNG.SaveToFile(SaveDialog.FileName);

    finally
      PNG.Free;
    end;

    (*
    ** Note that we could also have saved directly via TBitmap32:
    **
    **   ImageDisplay.Bitmap.SaveToFile(SaveDialog.FileName);
    **
    ** but that would not have allowed us to set any PNG options.
    *)

  finally
    SaveDialog.Free;
  end;
end;

procedure TFmPngDemo.CheckBoxFitClick(Sender: TObject);
begin
  if (TCheckBox(Sender).Checked) then
  begin
    ImageDisplay.ScaleMode := smResize;
    ImageDisplay.BitmapAlign := baCenter;
  end else
  begin
    ImageDisplay.ScaleMode := smNormal;
    ImageDisplay.BitmapAlign := baTopLeft;
  end;
end;

procedure TFmPngDemo.FormCreate(Sender: TObject);
begin
{$IFNDEF FPC}
  ImageDisplay.Margins.Top := 8;
  ImageDisplay.Margins.Left := 8;
  ImageDisplay.Margins.Bottom := 8;
  ImageDisplay.Margins.Right := 8;
  ImageDisplay.AlignWithMargins := True;
{$ENDIF}

  (*
  ** Note: This is just an example of LoadBitmap32FromPNG.
  ** Since TBitmap32 supports PNG we could also have loaded the image with:
  **
  **   ImageDisplay.Bitmap.LoadFromFile(...);
  *)

  if Graphics32Examples.MediaFileExists('Dice.png') then
    LoadBitmap32FromPNG(ImageDisplay.Bitmap, Graphics32Examples.MediaFolder+'\Dice.png');
end;

end.
