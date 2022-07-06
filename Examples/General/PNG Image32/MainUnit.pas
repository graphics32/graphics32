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
  GR32_PNG, GR32_PortableNetworkGraphic;

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

    PNG := TPortableNetworkGraphic32.Create;
    try
      PNG.LoadFromFile(OpenDialog.FileName);
      PNG.AssignTo(ImageDisplay.Bitmap);
    finally
      PNG.Free;
    end;
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

    PNG := TPortableNetworkGraphic32.Create;
    try
      PNG.AdaptiveFilterMethods := [aafmSub, aafmUp, aafmAverage];
      PNG.Assign(ImageDisplay.Bitmap);
      PNG.InterlaceMethod := imAdam7;
      PNG.SaveToFile(SaveDialog.FileName);
    finally
      PNG.Free;
    end;
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

  if FileExists('..\Demo.png') then
    with TPortableNetworkGraphic32.Create do
      try
        LoadFromFile('..\Demo.png');
        AssignTo(ImageDisplay.Bitmap);
      finally
        Free;
      end;
end;

end.
