unit GR32_TestGuiPngDisplay;

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
 * The Initial Developer of the Original Code is
 * Christian-W. Budde
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GR32, GR32_Image;

type
  TFmDisplay = class(TForm)
    BtNo: TButton;
    BtYes: TButton;
    Image32: TImage32;
    LbQuestion: TLabel;
    LbRenderer: TLabel;
    RbInternal: TRadioButton;
    RbPngImage: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure RbInternalClick(Sender: TObject);
    procedure RbPngImageClick(Sender: TObject);
  private
    FReference : TBitmap32;
    FInternal  : TBitmap32;
    procedure ClearImage32;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Reference: TBitmap32 read FReference write FReference;
    property Internal: TBitmap32 read FInternal write FInternal;
  end;

var
  FmDisplay: TFmDisplay;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

constructor TFmDisplay.Create(AOwner: TComponent);
begin
 inherited;
 FInternal := TBitmap32.Create;
 FInternal.DrawMode := dmBlend;
 FReference := TBitmap32.Create;
 FReference.DrawMode := dmBlend;
end;

destructor TFmDisplay.Destroy;
begin
 FreeAndNil(FInternal);
 FreeAndNil(FReference);
 inherited;
end;

procedure TFmDisplay.FormShow(Sender: TObject);
begin
 RbInternal.Checked := True;
 RbInternalClick(Sender);
end;

procedure TFmDisplay.ClearImage32;
const
  Colors: array [Boolean] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  R: TRect;
  I, J: Integer;
  OddY: Integer;
  TilesHorz, TilesVert: Integer;
  TileX, TileY: Integer;
  TileHeight, TileWidth: Integer;
begin
 Image32.Bitmap.Clear($FFFFFFFF);

 TileHeight := 8;
 TileWidth := 8;

 R := Image32.ClientRect;
 TilesHorz := (R.Right - R.Left) div TileWidth;
 TilesVert := (R.Bottom - R.Top) div TileHeight;
 TileY := 0;

 for J := 0 to TilesVert do
  begin
   TileX := 0;
   OddY := J and $1;
   for I := 0 to TilesHorz do
    begin
     Image32.Bitmap.FillRectS(TileX, TileY, TileX + TileWidth, TileY + TileHeight, Colors[I and $1 = OddY]);
     Inc(TileX, TileWidth);
    end;
   Inc(TileY, TileHeight);
  end
end;

procedure TFmDisplay.RbInternalClick(Sender: TObject);
begin
 with Image32 do
  begin
   ClearImage32;
   Bitmap.Draw(0, 0, FInternal);
  end;
end;

procedure TFmDisplay.RbPngImageClick(Sender: TObject);
begin
 with Image32 do
  begin
   ClearImage32;
   Bitmap.Draw(0, 0, FReference);
  end;
end;

end.
