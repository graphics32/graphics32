unit MainUnit;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Andre Beckedorf <andre@metaexception.de>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$MODE Delphi}

uses
  LCLIntf, LResources,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GR32, GR32_Image, GR32_Resamplers, GR32_RangeBars;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image: TImage32;
    rgScaleMode: TRadioGroup;
    rgKernel: TRadioGroup;
    rgBitmapAlign: TRadioGroup;
    StaticText1: TStaticText;
    sbScale: TGaugeBar;
    procedure rgBitmapAlignClick(Sender: TObject);
    procedure sbScaleChange(Sender: TObject);
    procedure rgScaleModeClick(Sender: TObject);
    procedure rgKernelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    Time: Single;
  end;

var
  Form1: TForm1;

implementation

uses
{$IFDEF Darwin}
  FPCMacOSAll,
{$ENDIF}
  LazJPEG;

procedure TForm1.rgBitmapAlignClick(Sender: TObject);
const
  BA_CONSTS: array [0..2] of TBitmapAlign = (baTopLeft, baCenter, baTile);
begin
  Image.BitmapAlign := BA_CONSTS[rgBitmapAlign.ItemIndex];
end;

procedure TForm1.sbScaleChange(Sender: TObject);
begin
  sbScale.Update;
  Image.Scale := sbScale.Position / 100;
end;

procedure TForm1.rgScaleModeClick(Sender: TObject);
const
  SM_CONSTS: array [0..5] of TScaleMode = (smNormal, smStretch, smScale, smResize, smOptimal, smOptimalScaled);
var
  ScaleEnabled: Boolean;
begin
  Image.ScaleMode := SM_CONSTS[rgScaleMode.ItemIndex];
  ScaleEnabled := (rgScaleMode.ItemIndex = 2) or (rgScaleMode.ItemIndex = 5);
  sbScale.Enabled := ScaleEnabled;
  StaticText1.Enabled := ScaleEnabled;
end;

procedure TForm1.rgKernelClick(Sender: TObject);
const
  K_CONSTS: array [0..4] of TCustomKernelClass =
    (TBoxKernel, TLinearKernel, TSplineKernel, TLanczosKernel, TMitchellKernel);
begin
  TKernelResampler(Image.Bitmap.Resampler).Kernel := K_CONSTS[rgKernel.ItemIndex].Create;
end;

procedure TForm1.FormCreate(Sender: TObject);
{$IFDEF Darwin}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
begin
  // Under Mac OS X we need to get the location of the bundle
{$IFDEF Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
{$ENDIF}

  // On Lazarus we don't use design-time packages because they consume time to be installed
  Image := TImage32.Create(Self);
  with Image do
  begin
    Parent := Self;
    Left := 2;
    Height := 398;
    Top := 2;
    Width := 381;
    Align := alClient;
    Bitmap.ResamplerClassName := 'TNearestResampler';
    Scale := 1;
    TabOrder := 0;
    // Different platforms store resource files on different locations
    {$IFDEF Windows}
      Bitmap.LoadFromFile('..\..\..\Media\delphi.jpg');
    {$ENDIF}

    {$IFDEF UNIX}
      {$IFDEF Darwin}
        Bitmap.LoadFromFile(pathStr + '/Contents/Resources/Media/delphi.jpg');
      {$ELSE}
        Bitmap.LoadFromFile('../../../Media/delphi.jpg');
      {$ENDIF}
    {$ENDIF}
  end;

  with TKernelResampler.Create(Image.Bitmap) do
  begin
    KernelMode := kmTableNearest;
    TableSize := 16;
  end;

  sbScale := TGaugeBar.Create(Panel2);
  with sbScale do
  begin
    Parent := Panel2;
    Left := 8;
    Top := 260;
    Width := 160;
    Height := 16;
    Backgnd := bgPattern;
    BorderStyle := bsNone;
    Enabled := False;
    Max := 1000;
    Min := 25;
    ShowHandleGrip := True;
    Position := 100;
    OnChange := sbScaleChange;
  end;
end;

initialization
  {$I MainUnit.lrs}

end.
