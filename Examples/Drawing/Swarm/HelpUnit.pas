unit HelpUnit;

interface

uses
{$ifndef FPC}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;
{$else}
  LCLType,
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, StdCtrls, ExtCtrls;
{$endif}

type
  TFormHelp = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
  protected
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure CreateParams(var Params: TCreateParams); override;
  public
  end;

implementation

{$R *.dfm}

{ TFormHelp }

procedure TFormHelp.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST or WS_EX_NOACTIVATE;
end;

procedure TFormHelp.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Application.MainForm.OnKeyDown(Sender, Key, Shift);
end;

procedure TFormHelp.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

end.
