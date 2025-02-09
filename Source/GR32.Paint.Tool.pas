unit GR32.Paint.Tool;

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
 * The Original Code is Paint tools for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander, anders@melander.dk
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2025
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$INCLUDE GR32.inc}

uses
  Classes,
  Controls,
  GR32,
  GR32.Paint.Host.API,
  GR32.Paint.Tool.API;


//------------------------------------------------------------------------------
//
//      TCustomBitmap32PaintTool
//
//------------------------------------------------------------------------------
// IBitmap32PaintTool reference implementation
//------------------------------------------------------------------------------
type
  TCustomBitmap32PaintTool = class(TInterfacedObject, IBitmap32PaintExtension, IBitmap32PaintTool)
  strict private
    FPaintHost: IBitmap32PaintHost;
    FSettingValues: ISettingValues;
    FActive: boolean;
  strict protected
    // IBitmap32PaintExtension,
    function GetIsVisible: boolean; virtual;
    function GetIsEnabled: boolean; virtual;
    function GetCaption: string; virtual;
    function GetHint: string; virtual;
    function GetDescription: string; virtual;
    function GetAttribution: string; virtual;
    procedure Clear; virtual;
    procedure Reset; virtual;
    property IsVisible: boolean read GetIsVisible;
    property IsEnabled: boolean read GetIsEnabled;
    property Caption: string read GetCaption;
    property Hint: string read GetHint;
    property Description: string read GetDescription;
    property Attribution: string read GetAttribution;

  strict protected
    // IBitmap32PaintTool
    procedure Activate(var Continue: boolean); virtual;
    procedure Deactivate; virtual;
    procedure BeginTool(var Continue: boolean); virtual;
    procedure EndTool; virtual;

    procedure MouseDown(const Context: IBitmap32PaintToolContext; Button: TMouseButton); virtual;
    procedure MouseMove(const Context: IBitmap32PaintToolContext); virtual;
    procedure MouseUp(const Context: IBitmap32PaintToolContext; Button: TMouseButton); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure BeginAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState); virtual;
    procedure ContinueAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState); virtual;
    procedure EndAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState); virtual;
    procedure CallbackAction(Buffer: TBitmap32; Data: pointer; var Result: boolean); virtual;

    function GetCursor(out Cursor: TCursor): boolean; virtual;
    function GetSnapMouse: boolean; virtual;
    property SnapMouse: boolean read GetSnapMouse;
    function GetToolFeatures: TBitmap32PaintToolFeatures; virtual;

  strict protected
    procedure SnapToSquare(const StartPos: TPoint; var Pos: TPoint);

  public
    constructor Create(const APaintHost: IBitmap32PaintHost); virtual;
    destructor Destroy; override;

    function ActiveColor(Shift: TShiftState; Invert: boolean = False): TColor32; overload;
    function ActiveColor(PrimaryColor, SecondaryColor: TColor32; Shift: TShiftState; Invert: boolean = False): TColor32; overload; virtual;

    property PaintHost: IBitmap32PaintHost read FPaintHost;
    property Active: boolean read FActive;
  end;

  TBitmap32PaintToolClass = class of TCustomBitmap32PaintTool;



//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math, SysUtils;

//------------------------------------------------------------------------------
//
//      TCustomBitmap32PaintTool
//
//------------------------------------------------------------------------------
constructor TCustomBitmap32PaintTool.Create(const APaintHost: IBitmap32PaintHost);
begin
  inherited Create;
  FPaintHost := APaintHost;
  FSettingValues := FPaintHost.GetToolSettings('');
end;

destructor TCustomBitmap32PaintTool.Destroy;
begin
  FPaintHost := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintTool.BeginTool(var Continue: boolean);
begin
  Continue := True;
  FActive := True;
end;

procedure TCustomBitmap32PaintTool.EndTool;
begin
  FActive := False;
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintTool.Activate(var Continue: boolean);
begin
end;

procedure TCustomBitmap32PaintTool.Deactivate;
begin
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintTool.ActiveColor(PrimaryColor, SecondaryColor: TColor32; Shift: TShiftState; Invert: boolean): TColor32;
begin
  if ((ssRight in Shift) xor Invert) then
    Result := SecondaryColor
  else
    Result := PrimaryColor;
end;

function TCustomBitmap32PaintTool.ActiveColor(Shift: TShiftState; Invert: boolean): TColor32;
begin
  Result := ActiveColor(PaintHost.ColorPrimary, PaintHost.ColorSecondary, Shift, Invert);
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintTool.CallbackAction(Buffer: TBitmap32;
  Data: pointer; var Result: boolean);
begin
  Result := False;
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintTool.Clear;
begin
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintTool.GetAttribution: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintTool.GetCaption: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintTool.GetHint: string;
begin
  Result := Caption;
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintTool.GetIsEnabled: boolean;
begin
  Result := True;
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintTool.GetIsVisible: boolean;
begin
  Result := True;
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintTool.GetSnapMouse: boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintTool.GetToolFeatures: TBitmap32PaintToolFeatures;
begin
  Result := [];
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintTool.KeyDown(var Key: Word; Shift: TShiftState);
begin
end;

procedure TCustomBitmap32PaintTool.KeyUp(var Key: Word; Shift: TShiftState);
begin
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintTool.GetCursor(out Cursor: TCursor): boolean;
begin
  Cursor := crCross;
  Result := True;
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintTool.GetDescription: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintTool.MouseDown(const Context: IBitmap32PaintToolContext; Button: TMouseButton);
begin
end;

procedure TCustomBitmap32PaintTool.MouseMove(const Context: IBitmap32PaintToolContext);
begin
end;

procedure TCustomBitmap32PaintTool.MouseUp(const Context: IBitmap32PaintToolContext; Button: TMouseButton);
begin
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintTool.MouseEnter;
begin
end;

procedure TCustomBitmap32PaintTool.MouseLeave;
begin
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintTool.BeginAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
begin
end;

procedure TCustomBitmap32PaintTool.ContinueAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
begin
end;

procedure TCustomBitmap32PaintTool.EndAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
begin
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintTool.Reset;
begin
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintTool.SnapToSquare(const StartPos: TPoint; var Pos: TPoint);

  function Sign(Value: integer): integer; inline;
  begin
    if (Value < 0) then
      Result := -1
    else
      Result := 1;
  end;

var
  dx, dy: integer;
begin
  dx := Pos.X-StartPos.X;
  dy := Pos.Y-StartPos.Y;

  if (Abs(dx) > Abs(dy)) then
    Pos.Y := StartPos.Y+Sign(dy)*Abs(dx)
  else
  if (Abs(dx) < Abs(dy)) then
    Pos.X := StartPos.X+Sign(dx)*Abs(dy);
end;

//------------------------------------------------------------------------------

end.
