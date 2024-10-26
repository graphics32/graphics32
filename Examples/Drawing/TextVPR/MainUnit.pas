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

{$include GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, Buttons, {$ENDIF} Messages, SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  GR32, GR32_Paths, GR32_Image, GR32_Layers;

type
  TMainForm = class(TForm)
    ButtonExit: TButton;
    ButtonSelectFont: TButton;
    CheckBoxSingleLine: TCheckBox;
    CheckBoxWordbreak: TCheckBox;
    FontDialog: TFontDialog;
    GroupBoxFont: TGroupBox;
    GroupBoxLayout: TGroupBox;
    GroupBoxRendering: TGroupBox;
    Img: TImage32;
    LblFontInfo: TLabel;
    LblGamma: TLabel;
    LblGammaValue: TLabel;
    PaintBox32: TPaintBox32;
    PnlControl: TPanel;
    PnlImage: TPanel;
    PnlZoom: TPanel;
    RadioGroupHinting: TRadioGroup;
    RadioGroupHorizontalAlign: TRadioGroup;
    RadioGroupVerticalAlign: TRadioGroup;
    RadioGroupMethod: TRadioGroup;
    StatusBar: TStatusBar;
    TbrGamma: TTrackBar;
    GroupBoxGamma: TGroupBox;
    PanelLeft: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonSelectFontClick(Sender: TObject);
    procedure ImgClick(Sender: TObject);
    procedure ImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure RadioGroupHintingClick(Sender: TObject);
    procedure RadioGroupHorizontalAlignClick(Sender: TObject);
    procedure RadioGroupMethodClick(Sender: TObject);
    procedure TbrGammaChange(Sender: TObject);
  private
    FPath: TFlattenedPath;
  public
    procedure BuildPolygonFromText;
    procedure RenderText;
    procedure DisplayFontInfo;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
{$IFNDEF FPC}
  Types,
  System.UITypes,
{$ENDIF}
  GR32.Text.Types,
  GR32_Backends,
  GR32_Gamma,
  GR32_Polygons;

const
  CLoremIpsum =
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin sit ' +
    'amet nulla. Nam turpis nisl, faucibus ut, pulvinar eget, ' +
    'porta ac, lacus. Nam ultricies quam sed est. Mauris ' +
    'auctor nibh ut dui. Phasellus facilisis libero sit amet urna. ' +
    'Pellentesque non lorem. Donec aliquam, turpis in ornare placerat, ' +
    'risus justo rhoncus nibh, vitae commodo sem eros vitae ' +
    'massa. Donec tincidunt. Suspendisse potenti. Praesent ' +
    'sapien augue, fermentum in, aliquet et, vestibulum vel, neque. ' +
    'Vivamus diam. Suspendisse commodo odio non erat. Fusce ornare, ipsum ' +
    'et luctus eleifend, sapien lectus placerat ante, a posuere ' +
    'nibh risus nec quam. Pellentesque pretium. Etiam leo urna, ' +
    'gravida eu, pellentesque eu, imperdiet in, enim. Nam nunc. ' +
    'Quisque commodo.' + #10#10 +

    'In scelerisque. Mauris vitae magna. Curabitur tempor. Pellentesque ' +
    'condimentum. Maecenas molestie turpis sed arcu pulvinar ' +
    'malesuada. Morbi quis metus in leo vestibulum mollis. ' +
    'Ut libero arcu, molestie eget, tincidunt at, lobortis et,' +
    'libero. Duis molestie venenatis magna. Nulla non ligula. Proin est. ' +
    'Curabitur nisl. Nulla facilisi. Nam dolor nulla, mollis ' +
    'non, tristique eu, vestibulum eget, mi. Donec venenatis, ' +
    'lacus adipiscing interdum laoreet, risus odio ullamcorper turpis, ' +
    'at feugiat pede neque ac dui.' + #10#10 +

    'Nulla quis dolor eget justo ullamcorper consectetur. Mauris in ante. ' +
    'Integer placerat dui at orci. Pellentesque at augue. Fusce ' +
    'a turpis. Aliquam tincidunt dolor ut augue. Quisque ' +
    'euismod mi ultrices mi. Sed pulvinar dolor sagittis mauris. Sed iaculis ' +
    'nisl sed orci. Sed massa nisl, porta a, blandit vel, ' +
    'ultrices quis, neque. Curabitur consequat urna id pede. ' +
    'Suspendisse sed metus.';

procedure TMainForm.FormCreate(Sender: TObject);
var
  FontHinting: IFontHintingSupport;
begin
  FPath := TFlattenedPath.Create;

  SetGamma(TbrGamma.Position * 0.001);
  Img.SetupBitmap(True, clWhite32);
  Img.Bitmap.Font.Name := 'Georgia';
  Img.Bitmap.Font.Size := 9;
  FontDialog.Font.Assign(Img.Bitmap.Font);
  DisplayFontInfo;

  // TODO : This is a misuse of TPaintBox32; A TImage32 would have been more suitable.
  PaintBox32.Buffer.SetSizeFrom(PaintBox32);
  PaintBox32.Buffer.Clear(clWhite32);

  if Supports(Img.Bitmap.Backend, IFontHintingSupport, FontHinting) then
    RadioGroupHinting.ItemIndex := Ord(FontHinting.GetHinting)
  else
    RadioGroupHinting.Enabled := False;

{$ifndef FPC}
  Self.Padding.SetBounds(4,4,4,4);
  ButtonExit.AlignWithMargins := True;
  ButtonSelectFont.AlignWithMargins := True;
  GroupBoxFont.AlignWithMargins := True;
  GroupBoxLayout.AlignWithMargins := True;
  RadioGroupHorizontalAlign.AlignWithMargins := True;
  RadioGroupVerticalAlign.AlignWithMargins := True;
  CheckBoxSingleLine.AlignWithMargins := True;
  CheckBoxWordbreak.AlignWithMargins := True;
  GroupBoxRendering.AlignWithMargins := True;
  RadioGroupHinting.AlignWithMargins := True;
  GroupBoxGamma.AlignWithMargins := True;
  RadioGroupMethod.AlignWithMargins := True;
{$else}
  ButtonExit.BorderSpacing.Around := 4;
  ButtonSelectFont.BorderSpacing.Around := 4;
  GroupBoxFont.BorderSpacing.Around := 4;
  GroupBoxLayout.BorderSpacing.Around := 4;
  RadioGroupHorizontalAlign.BorderSpacing.Around := 4;
  RadioGroupVerticalAlign.BorderSpacing.Around := 4;
  CheckBoxSingleLine.BorderSpacing.Around := 4;
  CheckBoxWordbreak.BorderSpacing.Around := 4;
  GroupBoxRendering.BorderSpacing.Around := 4;
  RadioGroupHinting.BorderSpacing.Around := 4;
  GroupBoxGamma.BorderSpacing.Around := 4;
  RadioGroupMethod.BorderSpacing.Around := 4;
{$endif}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FPath.Free;
end;

procedure TMainForm.ButtonSelectFontClick(Sender: TObject);
begin
  if FontDialog.Execute then
  begin
    Img.Bitmap.Font.Assign(FontDialog.Font);
    BuildPolygonFromText;
    RenderText;
    DisplayFontInfo;
  end;
end;

procedure TMainForm.ImgClick(Sender: TObject);
begin
 if RadioGroupMethod.ItemIndex + 1 < RadioGroupMethod.Items.Count then
   RadioGroupMethod.ItemIndex := RadioGroupMethod.ItemIndex + 1
 else
   RadioGroupMethod.ItemIndex := 0;
end;

procedure TMainForm.ImgMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
const
  Delta = 16;
begin
  PaintBox32.Buffer.Draw(PaintBox32.Buffer.BoundsRect,
    Rect(X - Delta , Y - Delta, X + Delta, Y + Delta), Img.Bitmap);
  PaintBox32.Repaint;
end;

procedure TMainForm.BuildPolygonFromText;
var
  Intf: ITextToPathSupport;
  DestRect: TFloatRect;
  Flag: Integer;
begin
  if Supports(Img.Bitmap.Backend, ITextToPathSupport, Intf) then
  begin
    DestRect := FloatRect(Img.BoundsRect);
    GR32.InflateRect(DestRect, -10, -10);
    Flag := RadioGroupHorizontalAlign.ItemIndex;

    case RadioGroupVerticalAlign.ItemIndex of
      0: ;
      1: Flag := Flag or DT_VCENTER;
    else
      Flag := Flag or DT_BOTTOM;
    end;

    if  CheckBoxSingleLine.Checked then
      Flag := Flag or DT_SINGLELINE;

    if CheckBoxWordbreak.Checked then
      Flag := Flag or DT_WORDBREAK;

    Intf.TextToPath(FPath, DestRect, CLoremIpsum, Flag);
  end else
    raise Exception.Create(RCStrInpropriateBackend);
end;

procedure TMainForm.RenderText;
begin
  Img.SetupBitmap(True, clWhite32);
  case RadioGroupMethod.ItemIndex of
    0: PolyPolygonFS(Img.Bitmap, FPath.Path, clBlack32, pfWinding);
    1: PolyPolygonFS_LCD(Img.Bitmap, FPath.Path, clBlack32, pfWinding);
    2: PolyPolygonFS_LCD2(Img.Bitmap, FPath.Path, clBlack32, pfWinding);
  end;

  // paint the close-up of the image around the mouse cursor ...
  with Img.ScreenToClient(Mouse.CursorPos) do
    ImgMouseMove(nil, [], X, Y, nil);
end;

function FontStylesToString(FontStyles: TFontStyles): string;
var
  Styles: TFontStyles;
begin
  Styles := [fsBold, fsItalic] * FontStyles;
  if Styles = [] then
    Result := ''
  else
  if Styles = [fsBold] then
    Result := ', Bold'
  else
  if Styles = [fsItalic] then
    Result := ', Italic'
  else
    Result := ', Bold & Italic';
end;

procedure TMainForm.DisplayFontInfo;
begin
  LblFontInfo.Caption := Format('%s'#10'%d%s', [FontDialog.Font.Name, FontDialog.Font.Size, FontStylesToString(FontDialog.Font.Style)]);
end;

procedure TMainForm.RadioGroupMethodClick(Sender: TObject);
begin
  RenderText;
end;

procedure TMainForm.TbrGammaChange(Sender: TObject);
begin
  SetGamma(TbrGamma.Position * 0.001);
  LblGammaValue.Caption := Format('(%1.2f)', [TbrGamma.Position * 0.001]);
  RenderText;
end;

procedure TMainForm.RadioGroupHintingClick(Sender: TObject);
var
  FontHinting: IFontHintingSupport;
begin
  if not Supports(Img.Bitmap.Backend, IFontHintingSupport, FontHinting) then
    exit;

  case RadioGroupHinting.ItemIndex of
    0: FontHinting.SetHinting(thNone);
    1: FontHinting.SetHinting(thNoHorz);
  else
    FontHinting.SetHinting(thHinting)
  end;

  BuildPolygonFromText;
  RenderText;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  BuildPolygonFromText;
  RenderText;
end;

procedure TMainForm.RadioGroupHorizontalAlignClick(Sender: TObject);
begin
  BuildPolygonFromText;
  RenderText;
end;

procedure TMainForm.ButtonExitClick(Sender: TObject);
begin
  Close;
end;

end.
