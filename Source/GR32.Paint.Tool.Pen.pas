unit GR32.Paint.Tool.Pen;

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
  GR32.Paint.Tool,
  GR32.Paint.Tool.API;

//------------------------------------------------------------------------------
//
//      TCustomBitmap32PaintToolPen
//
//------------------------------------------------------------------------------
type
  TCustomBitmap32PaintToolPen = class abstract(TCustomBitmap32PaintTool)
  strict private
    FLastPos: TPoint;

  strict protected
    procedure DoAction(const Context: IBitmap32PaintToolContext);

  strict protected
    function GetTransparent: boolean; virtual;
    function GetToolFeatures: TBitmap32PaintToolFeatures; override;

    procedure BeginAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState); override;
    procedure ContinueAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState); override;
    procedure EndAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState); override;

    function GetCursor(out Cursor: TCursor): boolean; override;
  public
  end;


//------------------------------------------------------------------------------
//
//      TBitmap32PaintToolPen
//
//------------------------------------------------------------------------------
type
  TBitmap32PaintToolPen = class(TCustomBitmap32PaintToolPen)
  strict private
    FTransparent: boolean;

  strict protected
    function GetTransparent: boolean; override;
    function GetCaption: string; override;

  public
    constructor Create(const APaintHost: IBitmap32PaintHost); override;

    property Transparent: boolean read FTransparent write FTransparent;
  end;


resourcestring
  sBitmap32PaintToolPenCaption = 'Pencil';


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Types;

//------------------------------------------------------------------------------
//
//      TCustomBitmap32PaintToolPen
//
//------------------------------------------------------------------------------
function TCustomBitmap32PaintToolPen.GetCursor(out Cursor: TCursor): boolean;
begin
  Cursor := crCross;
  Result := True;
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintToolPen.GetToolFeatures: TBitmap32PaintToolFeatures;
begin
  Result := [];
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintToolPen.GetTransparent: boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintToolPen.DoAction(const Context: IBitmap32PaintToolContext);
begin
  // Draw line from new pos to old pos, but don't draw the end pixel as we have
  // already drawn it.
  (*
  ** []                   Replace
  ** [ssShift]            Transparent (blend)
  *)
  if (GetTransparent) then
    Context.Buffer.LineTS(Context.MouseParams.BitmapPos.X, Context.MouseParams.BitmapPos.Y, FLastPos.X, FLastPos.Y, ActiveColor(Context.MouseParams.ShiftState), False)
  else
    Context.Buffer.LineS(Context.MouseParams.BitmapPos.X, Context.MouseParams.BitmapPos.Y, FLastPos.X, FLastPos.Y, ActiveColor(Context.MouseParams.ShiftState), False);

  FLastPos := Context.MouseParams.BitmapPos;
  PaintHost.Changed(Caption);
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintToolPen.BeginAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
begin
  inherited;

  if ([ssLeft, ssRight] * Context.MouseParams.ShiftState <> []) then
  begin
    if (GetTransparent) then
      Context.Buffer.SetPixelTS(Context.MouseParams.BitmapPos.X, Context.MouseParams.BitmapPos.Y, ActiveColor(Context.MouseParams.ShiftState))
    else
      Context.Buffer.PixelS[Context.MouseParams.BitmapPos.X, Context.MouseParams.BitmapPos.Y] := ActiveColor(Context.MouseParams.ShiftState);

    // By default the Pixel setter doesn't call Changed (unless CHANGED_IN_PIXELS is defined)
    Context.Buffer.Changed(MakeRect(Context.MouseParams.BitmapPos.X, Context.MouseParams.BitmapPos.Y, Context.MouseParams.BitmapPos.X+1, Context.MouseParams.BitmapPos.Y+1));

    FLastPos := Context.MouseParams.BitmapPos;
    PaintHost.Changed(Caption);
  end else
    ToolState := tsAbort;
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintToolPen.ContinueAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
begin
  if (FLastPos = Context.MouseParams.BitmapPos) then
    Exit;

  if ([ssLeft, ssRight] * Context.MouseParams.ShiftState <> []) then
    DoAction(Context);
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintToolPen.EndAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
begin
  DoAction(Context);
end;



//------------------------------------------------------------------------------
//
//      TBitmap32PaintToolPen
//
//------------------------------------------------------------------------------
constructor TBitmap32PaintToolPen.Create(const APaintHost: IBitmap32PaintHost);
begin
  inherited;

  FTransparent := True;
end;

function TBitmap32PaintToolPen.GetCaption: string;
begin
  Result := sBitmap32PaintToolPenCaption;
end;

function TBitmap32PaintToolPen.GetTransparent: boolean;
begin
  Result := FTransparent;
end;

//------------------------------------------------------------------------------

end.
