unit MainUnit;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is TextDemoVPR Example (based on VPR example)
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Christian-W. Budde (GR32 version 2.0 port)
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, Buttons, {$ENDIF} Messages, SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  GR32, GR32_Paths, GR32_Image, GR32_Layers;

type
  TMainForm = class(TForm)
    btnSelectFont: TButton;
    cbHinted: TCheckBox;
    FontDialog: TFontDialog;
    gbSettings: TGroupBox;
    Img: TImage32;
    lblGamma: TLabel;
    lblGammaValue: TLabel;
    PB: TPaintBox32;
    pnlControl: TPanel;
    pnlImage: TPanel;
    rgMethod: TRadioGroup;
    tbGamma: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure btnSelectFontClick(Sender: TObject);
    procedure cbHintedClick(Sender: TObject);
    procedure ImgClick(Sender: TObject);
    procedure ImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure rgMethodClick(Sender: TObject);
    procedure tbGammaChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPath: TFlattenedPath;
  public
    procedure BuildPolygonFromText;
    procedure RenderText;
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  GR32_Backends, GR32_Polygons,
  {$IFDEF FPC}
  {$IFDEF LCLWin32}
    GR32_Text_LCL_Win;
  {$ENDIF}
  {$IF defined(LCLGtk) or defined(LCLGtk2)}
    GR32_Text_LCL_GTK;
  {$IFEND}
  {$IFDEF LCLCarbon}
    GR32_Text_LCL_Carbon;
  {$ENDIF}
  {$IFDEF LCLCustomDrawn}
    GR32_Text_LCL_CustomDrawn;
  {$ENDIF}
  {$ELSE}
  GR32_Text_VCL;
  {$ENDIF}

const
  CLoremIpsum =
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin sit ' +
    'amet nulla.' + #13#10 + 'Nam turpis nisl, faucibus ut, pulvinar eget, ' +
    'porta ac, lacus. Nam ultricies' + #13#10 + 'quam sed est. Mauris ' +
    'auctor nibh ut dui. Phasellus facilisis libero sit amet urna.' + #13#10 +
    'Pellentesque non lorem. Donec aliquam, turpis in ornare placerat, ' +
    'risus justo' + #13#10 + 'rhoncus nibh, vitae commodo sem eros vitae ' +
    'massa. Donec tincidunt. Suspendisse' + #13#10 + 'potenti. Praesent ' +
    'sapien augue, fermentum in, aliquet et, vestibulum vel, neque.' + #13#10 +
    'Vivamus diam. Suspendisse commodo odio non erat. Fusce ornare, ipsum ' +
    'et luctus' + #13#10 + 'eleifend, sapien lectus placerat ante, a posuere ' +
    'nibh risus nec quam. Pellentesque' + #13#10 + 'pretium. Etiam leo urna, ' +
    'gravida eu, pellentesque eu, imperdiet in, enim. Nam nunc.' + #13#10 +
    'Quisque commodo.' + #13#10 + #13#10 +

    'In scelerisque. Mauris vitae magna. Curabitur tempor. Pellentesque ' +
    'condimentum.' + #13#10 + 'Maecenas molestie turpis sed arcu pulvinar ' +
    'malesuada. Morbi quis metus in leo' + #13#10 + 'vestibulum mollis. ' +
    'Ut libero arcu, molestie eget, tincidunt at, lobortis et,' + #13#10 +
    'libero. Duis molestie venenatis magna. Nulla non ligula. Proin est. ' +
    'Curabitur nisl.' + #13#10 + 'Nulla facilisi. Nam dolor nulla, mollis ' +
    'non, tristique eu, vestibulum eget, mi.' + #13#10 + 'Donec venenatis, ' +
    'lacus adipiscing interdum laoreet, risus odio ullamcorper turpis,' +
    #13#10 + 'at feugiat pede neque ac dui.' + #13#10 + #13#10 +

    'Nulla quis dolor eget justo ullamcorper consectetur. Mauris in ante. ' +
    'Integer placerat' + #13#10 + 'dui at orci. Pellentesque at augue. Fusce ' +
    'a turpis. Aliquam tincidunt dolor ut augue.' + #13#10 + 'Quisque ' +
    'euismod mi ultrices mi. Sed pulvinar dolor sagittis mauris. Sed iaculis ' +
    'nisl' + #13#10 + 'sed orci. Sed massa nisl, porta a, blandit vel, ' +
    'ultrices quis, neque. Curabitur' + #13#10 + 'consequat urna id pede. ' +
    'Suspendisse sed metus.';

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPath := TFlattenedPath.Create;

  SetGamma(0.88);
  Img.SetupBitmap(True, clWhite32);
  Img.Bitmap.Font.Name := 'Georgia';
  Img.Bitmap.Font.Size := 8;
  Img.Bitmap.Font.Style := [fsItalic];
  FontDialog.Font.Assign(Img.Bitmap.Font);
  BuildPolygonFromText;
  SetGamma(1);
  RenderText;
  PB.Buffer.SetSizeFrom(PB);
  PB.Buffer.Clear(clWhite32);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FPath.Free;
end;

procedure TMainForm.btnSelectFontClick(Sender: TObject);
begin
  if FontDialog.Execute then
  begin
    Img.Bitmap.Font.Assign(FontDialog.Font);
    BuildPolygonFromText;
    RenderText;
  end;
end;

procedure TMainForm.ImgClick(Sender: TObject);
begin
 if rgMethod.ItemIndex + 1 < rgMethod.Items.Count then
   rgMethod.ItemIndex := rgMethod.ItemIndex + 1
 else
   rgMethod.ItemIndex := 0;
end;

procedure TMainForm.ImgMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
const
  Delta = 16;
begin
  PB.Buffer.Draw(PB.Buffer.BoundsRect, Rect(X - Delta, Y - Delta, X + Delta,
    Y + Delta), Img.Bitmap);
  PB.Repaint;
end;

procedure TMainForm.BuildPolygonFromText;
var
  Intf: ITextToPathSupport;
begin
  if Supports(Img.Bitmap.Backend, ITextToPathSupport, Intf) then
    Intf.TextToPath(FPath, 10, 10, CLoremIpsum)
  else
    raise Exception.Create(RCStrInpropriateBackend);
end;

procedure TMainForm.RenderText;
begin
  Img.SetupBitmap(True, clWhite32);
  case rgMethod.ItemIndex of
    0: PolyPolygonFS(Img.Bitmap, FPath.Path, clBlack32, pfWinding);
    1: PolyPolygonFS_LCD(Img.Bitmap, FPath.Path, clBlack32, pfWinding);
    2: PolyPolygonFS_LCD2(Img.Bitmap, FPath.Path, clBlack32, pfWinding);
  end;
  with Img.ScreenToClient(Mouse.CursorPos) do
    ImgMouseMove(nil, [], X, Y, nil);
end;

procedure TMainForm.rgMethodClick(Sender: TObject);
begin
  RenderText;
end;

procedure TMainForm.tbGammaChange(Sender: TObject);
begin
  SetGamma(tbGamma.Position * 0.01);
  lblGammaValue.Caption := Format('(%1.2f)', [tbGamma.Position * 0.01]);
  RenderText;
end;

procedure TMainForm.cbHintedClick(Sender: TObject);
begin
  UseHinting := cbHinted.Checked;
  BuildPolygonFromText;
  RenderText;
end;

end.
