unit SimplePropEdit;

interface

uses
  Windows, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Grids, Messages, Classes,
  Graphics, TypInfo, GR32_IntegerMaps;

const
  WM_SELECTOBJECT = WM_USER + 2000;

type
  TWMSelectObject = packed record
    Msg: Cardinal;
    Unused: Longint;
    Obj: TPersistent;
    Result: Longint;
  end;

  TSimplePropertyEditor = class(TCustomPanel)
  private
    FLabels: TList;
    FPropertyControls: TList;
    FSelectedObject: TPersistent;
    FProps: PPropList;
    FCaption: string;
    function GetControlClass(Kind: TTypeKind): TPersistentClass;
    procedure ButtonHandler(Sender: TObject);
    procedure TrackBarHandler(Sender: TObject);
    procedure StringGridEditHandler(Sender: TObject; ACol, ARow: Longint; const Value: string);
    procedure WMSelectObject(var Msg: TWMSelectObject); message WM_SELECTOBJECT;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemoveSelectedObject;
    procedure SelectObject(AObject: TPersistent);
  end;

implementation

uses
  GR32_Resamplers, GR32_LowLevel, SysUtils;

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

constructor TSimplePropertyEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPropertyControls := TList.Create;
  FLabels := TList.Create;
  BorderWidth := 1;
  BorderStyle := bsNone;
  Ctl3D := False;
  Color := clWhite;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Font.Size := 7;
  Font.Name := 'Verdana';
end;

destructor TSimplePropertyEditor.Destroy;
begin
  if Assigned(FSelectedObject) then
    RemoveSelectedObject;
  FPropertyControls.Free;
  FLabels.Free;
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

procedure TSimplePropertyEditor.Paint;
begin
  inherited;
  Canvas.Brush.Color := clSilver;
  Canvas.Pen.Color := clWhite;
  Canvas.Font.Style := [fsBold];
  Canvas.Font.Size := 8;
  Canvas.Font.Name := 'Tahoma';
  if FCaption <> '' then
    Canvas.TextRect(Rect(0, 0, Width, 18), 6, 2, FCaption);
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

procedure TSimplePropertyEditor.SelectObject(AObject: TPersistent);
var
  I, Count, T, T1: Integer;
  Control: TWinControl;
  ALabel: TLabel;
  Map: TIntegerMap;
  K, L: Integer;
  P: PPropInfo;
const
  ROW_SPACE = 30;
  TOP_MARGIN = 20;
  MARGIN_CONTROLS = TOP_MARGIN + 4;
  MARGIN_LABELS = TOP_MARGIN + 8;
begin
  if Assigned(FSelectedObject) then
    RemoveSelectedObject;
  FSelectedObject := AObject;
  if not Assigned(AObject) then Exit;

  FCaption := 'Object: ' + AObject.ClassName;
  Count := GetTypeData(AObject.ClassInfo).PropCount;

  if AObject is TIntegerMap then
  begin
    Map := AObject as TIntegerMap;

    Control := TStringGrid.Create(nil);
    TStringGrid(Control).RowCount := 5;
    TStringGrid(Control).ColCount := 5;
    TStringGrid(Control).FixedCols := 0;
    TStringGrid(Control).FixedRows := 0;
    TStringGrid(Control).DefaultColWidth := 32;
    TStringGrid(Control).DefaultRowHeight := 16;
    TStringGrid(Control).Options := TStringGrid(Control).Options + [goEditing];

    for K := 0 to 4 do
      for L := 0 to 4 do
        TStringGrid(Control).Cells[K, L] := FloatToStr(Map[K, L]/256);

    TStringGrid(Control).OnSetEditText := StringGridEditHandler;

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
            Control := TTrackBar.Create(nil);
            TTrackBar(Control).ThumbLength := 16;
            TTrackBar(Control).Min := 0;
            TTrackBar(Control).Max := 20;
            if P.PropType^.Kind = tkInteger then
              TTrackBar(Control).Position := GetOrdProp(FSelectedObject, P)
            else
              TTrackBar(Control).Position := Round(GetFloatProp(FSelectedObject, P));
            TTrackBar(Control).OnChange := TrackBarHandler;
            Control.Width := 98;
            Control.Height := 25;
            Control.Left := 88;
            Control.Top := T + 4; //MARGIN_CONTROLS + I * ROW_SIZE;
          end;
        tkClass:
          begin
            Control := TButton.Create(nil);
            TButton(Control).OnClick := ButtonHandler;
            TButton(Control).Caption := 'Edit';
            Control.Width := 60;
            Control.Height := 20;
            Control.Left := 90;
            Control.Top := T + 4; //MARGIN_CONTROLS + I * ROW_SIZE;
            if GetObjectProp(FSelectedObject, P) = nil then
              Control.Enabled := False;
          end;
      else
        FPropertyControls.Add(nil);
        FLabels.Add(nil);
        Continue;
      end;
      Control.Parent := Self;
      Control.Tag := I;
      FPropertyControls.Add(Control);

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
  PropInfo: PPropInfo;
  W: Real;
  Code: Integer;
begin
  PropInfo := FProps[TComponent(Sender).Tag];
  Weights := TIntegerMap(GetObjectProp(FSelectedObject, PropInfo));
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
        SetFloatProp(FSelectedObject, PropInfo, TTrackBar(Sender).Position);
    end;
  end;
end;

procedure TSimplePropertyEditor.WMSelectObject(var Msg: TWMSelectObject);
begin
  SelectObject(Msg.Obj);
end;

end.
