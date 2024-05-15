unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  GR32,
  GR32_Image,
  GR32_Layers,
  amEasing;

type
  TEaseOptions = set of (eoHorizontal, eoVertical);

  TEaseThread = class(TThread)
  private
    FEaseHorizontal: TEaseFunc;
    FEaseVertical: TEaseFunc;
    FCycleTime: integer;
    FUpdateDelegate: TThreadMethod;
    FHorizontalValue: Double;
    FVerticalValue: Double;
    FOptions: TEaseOptions;
  protected
    procedure Execute; override;
  public
    constructor Create(AEaseHorizontal, AEaseVertical: TEaseFunc; ACycleTime: integer; AUpdateDelegate: TThreadMethod);

    property Options: TEaseOptions read FOptions write FOptions;
    property HorizontalValue: Double read FHorizontalValue;
    property VerticalValue: Double read FVerticalValue;
  end;

type
  TFormMain = class(TForm)
    Image32: TImage32;
    PanelOptions: TPanel;
    CheckBoxAnimateHorizontal: TCheckBox;
    CheckBoxAnimateVertical: TCheckBox;
    ButtonBounce: TButton;
    procedure CheckBoxAnimateHorizontalClick(Sender: TObject);
    procedure CheckBoxAnimateVerticalClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonBounceClick(Sender: TObject);
  private
    FPlanetLayer: TBitmapLayer;
    FAlienLayer: TBitmapLayer;
    FEaseThread: TEaseThread;
  private
    procedure UpdatePosition;
  public
    constructor Create(AOwner: TCOmponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  GR32.Examples,
  GR32_Resamplers,
  GR32_Math;


constructor TFormMain.Create(AOwner: TCOmponent);
begin
  inherited;

  FPlanetLayer := TBitmapLayer(Image32.Layers.Add(TBitmapLayer));
  FPlanetLayer.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder + '\sprite4.bmp');
  FPlanetLayer.Bitmap.DrawMode := dmBlend;
  FPlanetLayer.Bitmap.CombineMode := cmMerge;
  FPlanetLayer.Bitmap.ResamplerClassName := TLinearResampler.ClassName;

  FAlienLayer := TBitmapLayer(Image32.Layers.Add(TBitmapLayer));
  FAlienLayer.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder + '\sprite1.bmp');
  FAlienLayer.Bitmap.DrawMode := dmBlend;
  FAlienLayer.Bitmap.CombineMode := cmMerge;
  FAlienLayer.Bitmap.ResamplerClassName := TLinearResampler.ClassName;

  FEaseThread := TEaseThread.Create(TEaseCircular.EaseInOut, TEaseExponential.EaseInOut, 5000, UpdatePosition);

  FEaseThread.Start;
end;

destructor TFormMain.Destroy;
begin
  FEaseThread.Terminate;
  FEaseThread.WaitFor;
  FEaseThread.Free;

  inherited;
end;

procedure TFormMain.FormResize(Sender: TObject);
var
  r: TFloatRect;
begin
  r.Left := (Image32.ClientWidth - FAlienLayer.Bitmap.Width) / 2;
  r.Right := r.Left + FAlienLayer.Bitmap.Width;
  r.Top := Image32.ClientHeight - FAlienLayer.Bitmap.Height;
  r.Bottom := r.Top + FAlienLayer.Bitmap.Height;

  FAlienLayer.Location := r;
end;

procedure TFormMain.ButtonBounceClick(Sender: TObject);
begin
  // First animate the alien being shot into "space"...
  AnimatedTween(TEaseQuartic.EaseOut, 1500,
    procedure(Value: Double; var Continue: boolean)
    var
      r: TFloatRect;
    begin
      r := FAlienLayer.Location;

      r.Top := (Image32.ClientHeight - FAlienLayer.Bitmap.Height) * (1-Value);
      r.Bottom := r.Top + FAlienLayer.Bitmap.Height;

      FAlienLayer.Location := r;
      Image32.Update;

      CheckSynchronize; // Also give thread synchronize some love
    end, 50);

  // ...and then let it fall back and bounce
  AnimatedTween(TEaseBounce.EaseOut, 2000,
    procedure(Value: Double; var Continue: boolean)
    var
      r: TFloatRect;
    begin
      r := FAlienLayer.Location;

      r.Top := (Image32.ClientHeight - FAlienLayer.Bitmap.Height) * Value;
      r.Bottom := r.Top + FAlienLayer.Bitmap.Height;

      FAlienLayer.Location := r;
      Image32.Update;

      CheckSynchronize;
    end, 50);
end;

procedure TFormMain.CheckBoxAnimateHorizontalClick(Sender: TObject);
begin
  if (TCheckBox(Sender).Checked) then
    FEaseThread.Options := FEaseThread.Options + [eoHorizontal]
  else
    FEaseThread.Options := FEaseThread.Options - [eoHorizontal];
end;

procedure TFormMain.CheckBoxAnimateVerticalClick(Sender: TObject);
begin
  if (TCheckBox(Sender).Checked) then
    FEaseThread.Options := FEaseThread.Options + [eoVertical]
  else
    FEaseThread.Options := FEaseThread.Options - [eoVertical];
end;

procedure TFormMain.UpdatePosition;
var
  r: TFloatRect;
begin
  // Called from thread to update the position of the planet layer

  // Center
  r.Left := (Image32.ClientWidth - FPlanetLayer.Bitmap.Width) / 2;
  r.Top := (Image32.ClientHeight - FPlanetLayer.Bitmap.Height) / 2;

  // Offset from center
  if (eoHorizontal in FEaseThread.Options) then
    r.Left := r.Left + (Image32.ClientWidth - FPlanetLayer.Bitmap.Width) * (FEaseThread.HorizontalValue - 0.5);

  if (eoVertical in FEaseThread.Options) then
    r.Top := r.Top + (Image32.ClientHeight - FPlanetLayer.Bitmap.Height) * (FEaseThread.VerticalValue - 0.5);

  r.Right := r.Left + FPlanetLayer.Bitmap.Width;
  r.Bottom := r.Top + FPlanetLayer.Bitmap.Height;

  FPlanetLayer.Location := r;
end;

{ TEaseThread }

constructor TEaseThread.Create(AEaseHorizontal, AEaseVertical: TEaseFunc; ACycleTime: integer; AUpdateDelegate: TThreadMethod);
begin
  inherited Create(True);
  FEaseHorizontal := AEaseHorizontal;
  FEaseVertical := AEaseVertical;
  FCycleTime := ACycleTime;
  FUpdateDelegate := AUpdateDelegate;
end;

procedure TEaseThread.Execute;
const
  VerticalScale = 3;
begin
  while (not Terminated) do
  begin
    // Use a linear ease to produce a linear value from [0..1]. Then use that value
    // to produce a vertical and a horizontal value using the specified easing
    // functions.

    AnimatedTween(TEaseLinear.Ease, FCycleTime,
      procedure(Value: Double; var Continue: boolean)
      var
        HorValue, VerValue: Double;
      begin
        // Map from [0..1] to [0..1..0]
        HorValue := Abs((Value - 0.5) * 2);
        VerValue := Abs((FMod(Value * VerticalScale, 1.0) - 0.5) * 2);

        if (eoHorizontal in FOptions) then
          FHorizontalValue := FEaseHorizontal(HorValue);

        if (eoVertical in FOptions) then
          FVerticalValue := FEaseVertical(VerValue);

        Continue := not Terminated;

        // Update the planet layer position
        if (not Terminated) then
          Queue(FUpdateDelegate);
      end, 50);
  end;
end;

end.
