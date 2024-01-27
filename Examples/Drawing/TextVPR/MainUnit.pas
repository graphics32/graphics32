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
    BtnExit: TButton;
    BtnSelectFont: TButton;
    CbxSingleLine: TCheckBox;
    CbxWordbreak: TCheckBox;
    FontDialog: TFontDialog;
    GBxFont: TGroupBox;
    GbxLayout: TGroupBox;
    GbxRendering: TGroupBox;
    Img: TImage32;
    LblFontInfo: TLabel;
    LblGamma: TLabel;
    LblGammaValue: TLabel;
    PaintBox32: TPaintBox32;
    PnlControl: TPanel;
    PnlImage: TPanel;
    PnlZoom: TPanel;
    RgpHinting: TRadioGroup;
    RgpHorzAlign: TRadioGroup;
    RgpVerticalAlign: TRadioGroup;
    RgxMethod: TRadioGroup;
    StatusBar: TStatusBar;
    TbrGamma: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    procedure BtnSelectFontClick(Sender: TObject);
    procedure ImgClick(Sender: TObject);
    procedure ImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure RgpHintingClick(Sender: TObject);
    procedure RgpHorzAlignClick(Sender: TObject);
    procedure RgxMethodClick(Sender: TObject);
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

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
{$IFNDEF FPC}
  Types,
  System.UITypes,
{$ENDIF}
  GR32_Backends,
  GR32_Gamma,
  GR32_Polygons,
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
begin
  FPath := TFlattenedPath.Create;

  SetGamma(TbrGamma.Position * 0.001);
  Img.SetupBitmap(True, clWhite32);
  Img.Bitmap.Font.Name := 'Georgia';
  Img.Bitmap.Font.Size := 9;
  FontDialog.Font.Assign(Img.Bitmap.Font);
  DisplayFontInfo;
  PaintBox32.Buffer.SetSizeFrom(PaintBox32);
  PaintBox32.Buffer.Clear(clWhite32);
  RgpHinting.ItemIndex := Ord(GetHinting);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FPath.Free;
end;

procedure TMainForm.BtnSelectFontClick(Sender: TObject);
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
 if RgxMethod.ItemIndex + 1 < RgxMethod.Items.Count then
   RgxMethod.ItemIndex := RgxMethod.ItemIndex + 1
 else
   RgxMethod.ItemIndex := 0;
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
    Flag := RgpHorzAlign.ItemIndex;
    case RgpVerticalAlign.ItemIndex of
      0: ;
      1: Flag := Flag or DT_VCENTER;
      else Flag := Flag or DT_BOTTOM;
    end;
    if  CbxSingleLine.Checked then
      Flag := Flag or DT_SINGLELINE;
    if  CbxWordbreak.Checked then
      Flag := Flag or DT_WORDBREAK;

    Intf.TextToPath(FPath, DestRect, CLoremIpsum, Flag);
  end else
    raise Exception.Create(RCStrInpropriateBackend);
end;

procedure TMainForm.RenderText;
begin
  Img.SetupBitmap(True, clWhite32);
  case RgxMethod.ItemIndex of
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
  else if Styles = [fsBold] then
    Result := ', Bold'
  else if Styles = [fsItalic] then
    Result := ', Italic'
  else
    Result := ', Bold && Italic';
end;

procedure TMainForm.DisplayFontInfo;
begin
  with FontDialog.Font do
    LblFontInfo.Caption :=
      Format('%s'#10'%d%s', [Name, Size, FontStylesToString(Style)]);
end;

procedure TMainForm.RgxMethodClick(Sender: TObject);
begin
  RenderText;
end;

procedure TMainForm.TbrGammaChange(Sender: TObject);
begin
  SetGamma(TbrGamma.Position * 0.001);
  LblGammaValue.Caption := Format('(%1.2f)', [TbrGamma.Position * 0.001]);
  RenderText;
end;

procedure TMainForm.RgpHintingClick(Sender: TObject);
begin
  case RgpHinting.ItemIndex of
    0: SetHinting(thNone);
    1: SetHinting(thNoHorz);
    else SetHinting(thHinting)
  end;
  BuildPolygonFromText;
  RenderText;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  BuildPolygonFromText;
  RenderText;
end;

procedure TMainForm.RgpHorzAlignClick(Sender: TObject);
begin
  BuildPolygonFromText;
  RenderText;
end;

procedure TMainForm.BtnExitClick(Sender: TObject);
begin
  Close;
end;

end.
