unit GR32.Design.Misc;

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
 * The Original Code is Graphics32
 *
 * The Initial Developers of the Original Code are
 * Mattias Andersson <mattias@centaurix.com>
 * Andre Beckedorf <andre@metaexception.de>
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LazIDEIntf, PropEdits,{$ELSE}
  Windows, DesignIntf, DesignEditors, ToolsAPI,{$ENDIF}
  Classes, TypInfo, GR32_Containers;

type
  TCustomClassProperty = class(TClassProperty)
  private
    function HasSubProperties: Boolean;
  protected
    class function GetClassList: TClassList; virtual;
    procedure SetClassName(const CustomClass: string); virtual; {$IFNDEF BCB} abstract; {$ENDIF}
    function GetObject: TObject; virtual; {$IFNDEF BCB} abstract; {$ENDIF}
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

  TKernelClassProperty = class(TCustomClassProperty)
  protected
    class function GetClassList: TClassList; override;
    procedure SetClassName(const CustomClass: string); override;
    function GetObject: TObject; override;
  end;

  TResamplerClassProperty = class(TCustomClassProperty)
  protected
    class function GetClassList: TClassList; override;
    procedure SetClassName(const CustomClass: string); override;
    function GetObject: TObject; override;
  end;

implementation

{$if (not defined(FPC)) and declared(IOTAAboutBoxServices120)}

{$define BRANDING}

{$if declared(IOTASplashScreenServices280) or declared(IOTAAboutBoxServices280)}
{$define BRANDING_EX}
{$ifend}

{$ifend}

uses
{$IFNDEF FPC}
  SysUtils,
{$ENDIF}
{$if defined(BRANDING_EX)}
  Vcl.Graphics,
  Vcl.Imaging.pngimage,
{$ifend}
  GR32,
  GR32_Resamplers;

{ TCustomClassProperty }

function TCustomClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paReadOnly] +
    [paValueList, paRevertable, paVolatileSubProperties];
  if not HasSubProperties then Exclude(Result, paSubProperties);
end;

class function TCustomClassProperty.GetClassList: TClassList;
begin
  Result := nil;
end;

function TCustomClassProperty.GetValue: string;
begin
  if PropCount > 0 then
    Result := GetObject.ClassName
  else
    Result := '';
end;

procedure TCustomClassProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  L: TClassList;
begin
  L := GetClassList;
  if Assigned(L) then
    for I := 0 to L.Count - 1 do
      Proc(L.Items[I].ClassName);
end;

function TCustomClassProperty.HasSubProperties: Boolean;
begin
  if PropCount > 0 then
    Result := GetTypeData(GetObject.ClassInfo)^.PropCount > 0
  else
    Result := False;
end;

procedure TCustomClassProperty.SetValue(const Value: string);
var
  L: TClassList;
begin
  L := GetClassList;
  if Assigned(L) and Assigned(L.Find(Value)) then
    SetClassName(Value)
  else SetStrValue('');
  Modified;
end;

{$IFDEF BCB}

procedure TCustomClassProperty.SetClassName(const CustomClass: string);
begin
end;

function TCustomClassProperty.GetObject: TObject;
begin
  Result := nil;
end;
{$ENDIF}

{ TKernelClassProperty }

class function TKernelClassProperty.GetClassList: TClassList;
begin
  Result := TClassList(KernelList);
end;

function TKernelClassProperty.GetObject: TObject;
begin
  Result := TKernelResampler(GetComponent(0)).Kernel;
end;

procedure TKernelClassProperty.SetClassName(const CustomClass: string);
begin
  TKernelResampler(GetComponent(0)).KernelClassName := CustomClass;
end;

{ TResamplerClassProperty }

class function TResamplerClassProperty.GetClassList: TClassList;
begin
  Result := TClassList(ResamplerList);
end;

function TResamplerClassProperty.GetObject: TObject;
begin
  Result := TBitmap32(GetComponent(0)).Resampler;
end;

procedure TResamplerClassProperty.SetClassName(
  const CustomClass: string);
begin
  TBitmap32(GetComponent(0)).ResamplerClassName := CustomClass;
end;


//------------------------------------------------------------------------------
//
//      Branding
//
//------------------------------------------------------------------------------
{$if defined(BRANDING)}
const
  sProductName = 'Graphics32';
  sProductVersion = GR32.Graphics32Version;
  sProductCopyright = 'Copyright '#$00A9' 1997-%s'#13#10'Alex A. Denisov and the GR32 team'#13#10'All rights reserved.';
  sProductLogo = 'GR32';
  sProductLogos: array of string = ['GR32_24x24', 'GR32_48x48', 'GR32_64x64'];
  sProductLicense = 'Open Source: MPL 1.1 or LGPL 2.1 with linking exception';
  sProductSKU = ''; // Will be appended, with a space before, to the product name on the splash
  sProductRegistered = True;

type
  BrandingInfo = record
  private
{$if defined(BRANDING_EX)}
    class var FLogoBitmaps: TGraphicArray;
{$ifend}
    class var FLogoBitmap: HBITMAP;
    class function GetProductName: string; static;
    class function GetProductCopyright: string; static;
  public
    class procedure Initialize; static;
    class procedure Finalize; static;

    class property ProductName: string read GetProductName;
    class property ProductCopyright: string read GetProductCopyright;
{$if defined(BRANDING_EX)}
    class property LogoBitmaps: TGraphicArray read FLogoBitmaps;
{$ifend}
    class property LogoBitmap: HBITMAP read FLogoBitmap;
  end;

class procedure BrandingInfo.Initialize;
begin
{$if defined(BRANDING_EX)}
  SetLength(FLogoBitmaps, Length(sProductLogos));
  for var i := 0 to High(FLogoBitmaps) do
  begin
    FLogoBitmaps[i] := TPngImage.Create;
    TPngImage(FLogoBitmaps[i]).LoadFromResourceName(hInstance, sProductLogos[i]);
  end;
{$ifend}
  FLogoBitmap := LoadBitmap(hInstance, sProductLogo);
end;

class procedure BrandingInfo.Finalize;
begin
{$if defined(BRANDING_EX)}
  for var i := 0 to High(FLogoBitmaps) do
    FLogoBitmaps[i].Free;
{$ifend}
  DeleteObject(FLogoBitmap);
end;

class function BrandingInfo.GetProductName: string;
begin
  if (sProductVersion <> '') then
    Result := Format('%s %s', [sProductName, sProductVersion])
  else
    Result := sProductName;
end;

class function BrandingInfo.GetProductCopyright: string;
begin
  Result := Format(sProductCopyright, [FormatDateTime('YYYY', Now)]);
end;
{$ifend}


//------------------------------------------------------------------------------
//
//      IDE splash screen
//
//------------------------------------------------------------------------------
{$if defined(BRANDING)}
type
  SplashInfo = record
  public
    class procedure Initialize; static;
    class procedure Finalize; static;
  end;

class procedure SplashInfo.Initialize;
var
  SplashScreenServices270: IOTASplashScreenServices270;
begin
{$if defined(BRANDING_EX)}
  var SplashScreenServices280: IOTASplashScreenServices280;
  if Supports(SplashScreenServices, IOTASplashScreenServices280, SplashScreenServices280) then
  begin
    SplashScreenServices280.AddPluginBitmap(BrandingInfo.ProductName, BrandingInfo.LogoBitmaps, not sProductRegistered, sProductLicense, sProductSKU);
    exit;
  end else
{$ifend}
  if Supports(SplashScreenServices, IOTASplashScreenServices270, SplashScreenServices270) then
  begin
    // The service will delete the bitmap, so we need to hand it a copy
    var Bitmap := CopyImage(BrandingInfo.LogoBitmap, IMAGE_BITMAP, 0, 0, 0);
    SplashScreenServices270.AddPluginBitmap(BrandingInfo.ProductName, Bitmap, not sProductRegistered, sProductLicense, sProductSKU);
    exit;
  end;

  // Apparently Embarcadero doesn't understand how inherited interfaces work...
  // They don't appear to have declared support for the versioned base interfaces
  // on the implementing object. This means that it's pointless to query the
  // service for the interfaces; They only support the unversioned one.

{$if defined(BRANDING_EX)}
  SplashScreenServices.AddPluginBitmap(BrandingInfo.ProductName, BrandingInfo.LogoBitmaps, not sProductRegistered, sProductLicense, sProductSKU)
{$else}
  // The service will delete the bitmap, so we need to hand it a copy
  var Bitmap := CopyImage(BrandingInfo.LogoBitmap, IMAGE_BITMAP, 0, 0, 0);
  SplashScreenServices.AddPluginBitmap(BrandingInfo.ProductName, Bitmap, not sProductRegistered, sProductLicense, sProductSKU);
{$ifend}
end;

class procedure SplashInfo.Finalize;
begin
end;
{$ifend}


//------------------------------------------------------------------------------
//
//      IDE about box
//
//------------------------------------------------------------------------------
{$if defined(BRANDING)}
type
  AboutInfo = record
  private
    class var FAboutInfoIndex: integer;
  public
    class procedure Initialize; static;
    class procedure Finalize; static;
  end;

class procedure AboutInfo.Initialize;
var
  AboutBoxServices120: IOTAAboutBoxServices120;
begin
  FAboutInfoIndex := -1;

  var Info := Format('%s'#13#10#13#10'%s', [BrandingInfo.ProductName, BrandingInfo.ProductCopyright]);

{$if defined(BRANDING_EX)}
  var AboutBoxServices280: IOTAAboutBoxServices280;
  if Supports(BorlandIDEServices, IOTAAboutBoxServices280, AboutBoxServices280) then
  begin
    FAboutInfoIndex := AboutBoxServices280.AddPluginInfo(BrandingInfo.ProductName, Info, BrandingInfo.LogoBitmaps, not sProductRegistered, sProductLicense, sProductSKU);
    exit;
  end else
{$ifend}
  if Supports(BorlandIDEServices, IOTAAboutBoxServices120, AboutBoxServices120) then
  begin
    // The service will delete the bitmap, so we need to hand it a copy
    var Bitmap := CopyImage(BrandingInfo.LogoBitmap, IMAGE_BITMAP, 0, 0, 0);
    FAboutInfoIndex := AboutBoxServices120.AddPluginInfo(BrandingInfo.ProductName, Info, Bitmap, not sProductRegistered, sProductLicense, sProductSKU);
    exit;
  end;

{$if defined(BRANDING_EX)}
  FAboutInfoIndex := (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(BrandingInfo.ProductName, Info, BrandingInfo.LogoBitmaps, not sProductRegistered, sProductLicense, sProductSKU);
{$else}
  // The service will delete the bitmap, so we need to hand it a copy
  var Bitmap := CopyImage(BrandingInfo.LogoBitmap, IMAGE_BITMAP, 0, 0, 0);
  FAboutInfoIndex := (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(BrandingInfo.ProductName, Info, Bitmap, not sProductRegistered, sProductLicense, sProductSKU);
{$ifend}
end;

class procedure AboutInfo.Finalize;
var
  AboutBoxServices120: IOTAAboutBoxServices120;
begin
  if (FAboutInfoIndex <> -1) then
  begin
    if Supports(BorlandIDEServices, IOTAAboutBoxServices120, AboutBoxServices120) then
      AboutBoxServices120.RemovePluginInfo(FAboutInfoIndex)
    else
      (BorlandIDEServices as IOTAAboutBoxServices).RemovePluginInfo(FAboutInfoIndex);
  end;
end;
{$ifend}

//------------------------------------------------------------------------------

initialization
{$if defined(BRANDING)}
  BrandingInfo.Initialize;
  SplashInfo.Initialize;
  AboutInfo.Initialize;
{$ifend}

finalization
{$if defined(BRANDING)}
  SplashInfo.Finalize;
  AboutInfo.Finalize;
  BrandingInfo.Finalize;
{$ifend}

end.
