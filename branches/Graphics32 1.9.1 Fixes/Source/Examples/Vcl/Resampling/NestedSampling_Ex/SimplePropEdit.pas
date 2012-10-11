unit SimplePropEdit;

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
 * The Original Code is Nested Sampling Example
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Andre Beckedorf <Andre@metaException.de>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF}
  Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Grids, Messages, Classes,
  Graphics, TypInfo, GR32_OrdinalMaps;

const
  WM_SELECTOBJECT = WM_USER + 2000;

type
  TWMSelectObject = packed record
    Msg: Cardinal;
    Unused: Longint;
    Obj: TPersistent;
    Result: Longint;
  end;

  PPropertyRangeEntry = ^TPropertyRangeEntry;
  TPropertyRangeEntry = record
    AClass: TClass;
    PropName: string;
    LoValue: Single;
    HiValue: Single;
  end;

  TSimplePropertyEditor = class(TCustomPanel)
  private
    FLabels: TList;
    FPropertyControls: TList;
    FPropertyRangeList: TList;
    FSelectedObject: TPersistent;
    FProps: PPropList;
    FCaption: string;
    function GetControlClass(Kind: TTypeKind): TPersistentClass;
    procedure ButtonHandler(Sender: TObject);
    procedure TrackBarHandler(Sender: TObject);
    procedure ComboBoxHandler(Sender: TObject);
    procedure StringGridEditHandler(Sender: TObject; ACol, ARow: Longint; const Value: string);
    procedure WMSelectObject(var Msg: TWMSelectObject); message WM_SELECTOBJECT;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemoveSelectedObject;
    procedure SelectObject(ObjName: string; AObject: TPersistent);
    procedure RegisterClassPropertyRange(AClass: TClass; const PropName: string;
      LoValue, HiValue: Single);
    procedure GetPropertyRange(Instance: TObject; const PropName: string;
      out LoValue, HiValue: Single);
  end;

const
  // scale trackbar min and max values for floating-point properties
  SCALE_FLOAT = 10000;

implementation

uses
  SysUtils, Math, GR32_Resamplers, GR32_LowLevel;

{ TSimplePropertyEditor }

procedure TSimplePropertyEditor.ButtonHandler(Sender: TObject);
var
  PropInfo: PPropInfo;
  Obj: TPersistent;
begin
  PropInfo := FProps^[TComponent(Sender).Tag];
  Obj := TPersistent(GetObjectProp(FSelectedObject, PropInfo.Name));
  PostMessage(Handle, WM_SELECTOBJECT, 0, Integer(Obj));
end;

procedure TSimplePropertyEditor.ComboBoxHandler(Sender: TObject);
var
  PropInfo: PPropInfo;
begin
  PropInfo := FProps[TComponent(Sender).Tag];
  with PropInfo^ do
  begin
    case PropType^.Kind of
      tkEnumeration:
        SetOrdProp(FSelectedObject, PropInfo, TComboBox(Sender).ItemIndex);
    end;
  end;
end;

constructor TSimplePropertyEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPropertyControls := TList.Create;
  FLabels := TList.Create;
  FPropertyRangeList := TList.Create;
  BorderWidth := 1;
  BorderStyle := bsNone;
  {$IFNDEF FPC}
  Ctl3D := False;
  {$ENDIF}
  Color := clWhite;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Font.Size := 7;
  Font.Name := 'Verdana';
end;

destructor TSimplePropertyEditor.Destroy;
var
  I: Integer;
begin
  if Assigned(FSelectedObject) then
    RemoveSelectedObject;
  FPropertyControls.Free;
  FLabels.Free;
  for I := 0 to FPropertyRangeList.Count - 1 do
    Dispose(PPropertyRangeEntry(FPropertyRangeList.Items[I]));
  FPropertyRangeList.Clear;
  FPropertyRangeList.Free;
  inherited;
end;

function TSimplePropertyEditor.GetControlClass(
  Kind: TTypeKind): TPersistentClass;
begin
  case Kind of
    tkInteger: Result := TEdit;
    tkClass: Result := TButton;
  else
    Result := TEdit;
  end;
end;

procedure TSimplePropertyEditor.GetPropertyRange(Instance: TObject;
  const PropName: string; out LoValue, HiValue: Single);
var
  I: Integer;
  P: PPropertyRangeEntry;
begin
  LoValue := 0;
  HiValue := 100;
  for I := 0 to FPropertyRangeList.Count - 1 do
  begin
    P := FPropertyRangeList.Items[I];
    if Instance is P.AClass then
      if P.PropName = PropName then
      begin
        LoValue := P.LoValue;
        HiValue := P.HiValue;
        Exit;
      end;
  end;
end;

procedure TSimplePropertyEditor.Paint;
begin
  inherited;
  with Canvas do
  begin
    Brush.Color := clSilver;
    Pen.Color := clWhite;
    Font.Style := [fsBold];
    Font.Size := 8;
    Font.Name := 'Tahoma';
    if FCaption <> '' then
      TextRect(Rect(0, 0, Width, 18), 6, 2, FCaption);
  end;
end;

procedure TSimplePropertyEditor.RegisterClassPropertyRange(AClass: TClass;
  const PropName: string; LoValue, HiValue: Single);
var
  P: PPropertyRangeEntry;
begin
  New(P);
  P.AClass := AClass;
  P.PropName := PropName;
  P.LoValue := LoValue;
  P.HiValue := HiValue;
  FPropertyRangeList.Add(P);
end;

procedure TSimplePropertyEditor.RemoveSelectedObject;
var
  I: Integer;
begin
  for I := 0 to FPropertyControls.Count - 1 do
  begin
    if Assigned(FPropertyControls[I]) then
      TWinControl(FPropertyControls[I]).Free;
    if Assigned(Flabels[I]) then
      TLabel(FLabels[I]).Free;
  end;
  FPropertyControls.Clear;
  FLabels.Clear;
  if Assigned(FProps) then
    FreeMem(FProps);
end;

procedure TSimplePropertyEditor.SelectObject(ObjName: string; AObject: TPersistent);
var
  I, L, K, Count, T, T1: Integer;
  Control: TWinControl;
  ALabel: TLabel;
  Map: TIntegerMap;
  LoValue, HiValue: Single;
  P: PPropInfo;
  TD: PTypeData;
  S, SelName: string;
const
  ROW_SPACE = 30;
  TOP_MARGIN = 20;
  MARGIN_CONTROLS = TOP_MARGIN + 4;
  MARGIN_LABELS = TOP_MARGIN + 8;
begin
  if Assigned(FSelectedObject) then
    RemoveSelectedObject;
  FSelectedObject := AObject;
  if not Assigned(AObject) then
  begin
    FCaption := '';
    Repaint;
    Exit;
  end;

  FCaption := ObjName + ': ' + AObject.ClassName;
  Count := GetTypeData(AObject.ClassInfo).PropCount;

  if AObject is TIntegerMap then
  begin
    Map := AObject as TIntegerMap;

    Control := TStringGrid.Create(nil);
    with TStringGrid(Control) do
    begin
      RowCount := 5;
      ColCount := 5;
      FixedCols := 0;
      FixedRows := 0;
      DefaultColWidth := 32;
      DefaultRowHeight := 16;
      Options := Options + [goEditing];

      for K := 0 to 4 do
        for L := 0 to 4 do
          Cells[K, L] := FloatToStr(Map[K, L] / 256);

      OnSetEditText := StringGridEditHandler;
    end;

    Control.Width := 168;
    Control.Height := 89;
    Control.Left := 12;
    Control.Top := MARGIN_CONTROLS + ROW_SPACE;

    Control.Parent := Self;
    FPropertyControls.Add(Control);

    ALabel := TLabel.Create(nil);
    ALabel.Caption := 'Kernel / Structuring Element:';
    ALabel.Left := 8;
    ALabel.Top := MARGIN_CONTROLS + 8;
    ALabel.Width := 84;
    ALabel.Parent := Self;
    FLabels.Add(ALabel);
  end;

  FProps := nil;
  if Count > 0 then
  begin
    GetMem(FProps, Count * SizeOf(PPropInfo));
    Count := GetPropList(AObject.ClassInfo, tkProperties, FProps);
    Self.Canvas.Brush.Color := Self.Color;

    T := MARGIN_CONTROLS;
    for I := 0 to Count - 1 do
    begin
      P := FProps[I];
      T1 := T;
      case P.PropType^.Kind of

        tkInteger, tkFloat:
          begin
            GetPropertyRange(AObject, P.Name, LoValue, HiValue);

            Control := TTrackBar.Create(nil);
            Control.Parent := Self;
            Control.Tag := I;
            FPropertyControls.Add(Control);

            with TTrackBar(Control) do
            begin
              if P.PropType^.Kind = tkInteger then
              begin
                Min := Round(LoValue);
                Max := Round(HiValue);
                Frequency := Math.Max(1, Round(HiValue - LoValue) div 20);
                Position := GetOrdProp(FSelectedObject, P)
              end
              else
              begin
                LoValue := LoValue * SCALE_FLOAT;
                HiValue := HiValue * SCALE_FLOAT;
                Min := Round(LoValue);
                Max := Round(HiValue);
                Frequency := Math.Max(1, Round((HiValue - LoValue) / 20));
                Position := Round(GetFloatProp(FSelectedObject, P) * SCALE_FLOAT);
              end;
              {$IFNDEF FPC}
              ThumbLength := 16;
              {$ENDIF}
              OnChange := TrackBarHandler;
            end;
            Control.Width := 98;
            Control.Height := 25;
            Control.Left := 88;
            Control.Top := T + 4;
          end;

        tkClass:
          begin
            Control := TButton.Create(nil);
            Control.Parent := Self;
            Control.Tag := I;
            FPropertyControls.Add(Control);

            TButton(Control).OnClick := ButtonHandler;
            TButton(Control).Caption := 'Edit';
            Control.Width := 60;
            Control.Height := 20;
            Control.Left := 90;
            Control.Top := T + 4;
            if GetObjectProp(FSelectedObject, P) = nil then
              Control.Enabled := False;
          end;

        tkEnumeration:
          begin
            Control := TComboBox.Create(nil);
            Control.Parent := Self;
            Control.Tag := I;
            FPropertyControls.Add(Control);

            TComboBox(Control).OnChange := ComboBoxHandler;
            TComboBox(Control).Style := csDropDownList;
            Control.Width := 100;
            Control.Height := 20;
            Control.Left := 90;
            Control.Top := T + 4;

            SelName := GetEnumProp(FSelectedObject, P);
            {$IFDEF FPC}
            TD := GetTypeData(P.PropType);
            {$ELSE}
            TD := GetTypeData(P.PropType^);
            {$ENDIF}

            L := 0;
            for K := TD.MinValue to TD.MaxValue do
            begin
              {$IFDEF FPC}
              S := GetEnumName(P.PropType, K);
              {$ELSE}
              S := GetEnumName(P.PropType^, K);
              {$ENDIF}
              if S = SelName then L := K;
              TComboBox(Control).AddItem(S, nil);
            end;

            TComboBox(Control).ItemIndex := L;
          end;
      else
        FPropertyControls.Add(nil);
        FLabels.Add(nil);
        Continue;
      end;

      ALabel := TLabel.Create(nil);
      ALabel.Caption := P.Name + ':';
      ALabel.Left := 0;
      ALabel.Top := T1 + 8;
      ALabel.Width := 84;
      ALabel.Alignment := taRightJustify;
      ALabel.Parent := Self;
      FLabels.Add(ALabel);

      Inc(T, ROW_SPACE);
    end;
  end;
  Repaint;
end;

procedure TSimplePropertyEditor.StringGridEditHandler(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  Weights: TIntegerMap;
  W: Real;
  Code: Integer;
begin
  Weights := FSelectedObject as TIntegerMap;
  Val(Value, W, Code);
  if Code = 0 then
    Weights[ACol, ARow] := Round(W * 256);
end;

procedure TSimplePropertyEditor.TrackBarHandler(Sender: TObject);
var
  PropInfo: PPropInfo;
begin
  PropInfo := FProps[TComponent(Sender).Tag];
  with PropInfo^ do
  begin
    case PropType^.Kind of
      tkInteger:
        SetOrdProp(FSelectedObject, PropInfo, TTrackBar(Sender).Position);
      tkFloat:
        SetFloatProp(FSelectedObject, PropInfo, TTrackBar(Sender).Position / SCALE_FLOAT);
    end;
  end;
end;

procedure TSimplePropertyEditor.WMSelectObject(var Msg: TWMSelectObject);
begin
  SelectObject('Object', Msg.Obj);
end;

end.
