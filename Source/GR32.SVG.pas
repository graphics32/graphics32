unit GR32.SVG;

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
 * The Original Code is SVG Image Format support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2025-2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  SysUtils, Classes,
  GR32, GR32.SVG.Types, GR32.SVG.Tree, GR32.SVG.Renderer, GR32.SVG.Renderer.Filters;

type
  TSvgDocument = class(TPersistent)
  private
    FRoot: TSvgDocumentNode;
    function GetWidth: TSvgLength;
    function GetHeight: TSvgLength;
    function GetViewBox: TSvgViewBox;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromText(const AXmlText: string);

    procedure Draw(ATarget: TCustomBitmap32); overload;
    procedure Draw(ATarget: TCustomBitmap32; const ARect: TRect); overload;
    procedure Draw(ATarget: TCustomBitmap32; const ARect: TFloatRect); overload;

    property Root: TSvgDocumentNode read FRoot;
    property Width: TSvgLength read GetWidth;
    property Height: TSvgLength read GetHeight;
    property ViewBox: TSvgViewBox read GetViewBox;
  end;

implementation

{ TSvgDocument }

constructor TSvgDocument.Create;
begin
  inherited Create;
  FRoot := nil;
end;

destructor TSvgDocument.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSvgDocument.Clear;
begin
  FreeAndNil(FRoot);
end;

procedure TSvgDocument.LoadFromStream(AStream: TStream);
var
  utf8Text: UTF8String;
begin
  Clear;
  if AStream = nil then
    exit;

  SetLength(utf8Text, AStream.Size - AStream.Position);
  SetLength(utf8Text, AStream.Read(Pointer(utf8Text)^, Length(utf8Text)));

  FRoot := ParseSvgXml(utf8Text);
end;

procedure TSvgDocument.LoadFromFile(const AFileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try

    LoadFromStream(FileStream);

  finally
    FileStream.Free;
  end;
end;

procedure TSvgDocument.LoadFromText(const AXmlText: string);
begin
  Clear;
  if AXmlText <> '' then
    FRoot := ParseSvgXml(UTF8String(AXmlText));
end;

function TSvgDocument.GetWidth: TSvgLength;
begin
  if FRoot <> nil then
    Result := FRoot.Width
  else
    Result := TSvgLength.Create(0, suPx);
end;

function TSvgDocument.GetHeight: TSvgLength;
begin
  if FRoot <> nil then
    Result := FRoot.Height
  else
    Result := TSvgLength.Create(0, suPx);
end;

function TSvgDocument.GetViewBox: TSvgViewBox;
begin
  if FRoot <> nil then
    Result := FRoot.ViewBox
  else
  begin
    Result.IsDefined := False;
    Result.X := 0;
    Result.Y := 0;
    Result.Width := 0;
    Result.Height := 0;
  end;
end;

procedure TSvgDocument.Draw(ATarget: TCustomBitmap32);
begin
  if ATarget <> nil then
    Draw(ATarget, ATarget.BoundsRect);
end;

procedure TSvgDocument.Draw(ATarget: TCustomBitmap32; const ARect: TRect);
begin
  Draw(ATarget, FloatRect(ARect));
end;

procedure TSvgDocument.Draw(ATarget: TCustomBitmap32; const ARect: TFloatRect);
var
  Renderer: TSvgRenderer;
begin
  if (FRoot = nil) or (ATarget = nil) then
    Exit;

  Renderer := TSvgRenderer.Create(ATarget);
  try

    Renderer.RenderDocument(FRoot, ARect);

  finally
    Renderer.Free;
  end;
end;

end.
