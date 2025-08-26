unit HexDump;

// -----------------------------------------------------------------------------
// Based on the Delphi ResXplor demo.
// The original author is CodeGear.
// -----------------------------------------------------------------------------
// Portions copyright © CodeGear
// Portions copyright © 2005 Colin Wilson
// Portions copyright © 2008-2009 Anders Melander
// -----------------------------------------------------------------------------


interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

const
  MAXDIGITS = 16;

{ THexDump }

type
  TForegroundColor = (fcAddress, fcHex, fcAnsi, fcLeftDivider, fcRightDivider);
  TBackgroundColor = (bcAddress, bcHex, bcHexOdd, bcAnsi);
  TByteSet = set of byte;
  THexDump = class(TCustomControl)
  private
    FActive: Boolean;
    FAddress: Pointer;
    FDataSize: Integer;
    FTopLine: Integer;
    FCurrentLine: Integer;
    FVisibleLines: Integer;
    FLineCount: Integer;
    FBytesPerLine: Integer;
    FItemHeight: Integer;
    FItemWidth: Integer;
    FForegroundColors: array[TForegroundColor] of TColor;
    FBackgroundColors: array[TBackgroundColor] of TColor;
    FShowCharacters: Boolean;
    FShowAddress: Boolean;
    FBorder: TBorderStyle;
    FReadOnly: boolean;
    FCurrentLinePos: Integer;
    FEditCharacters: boolean;
    FLowNibble: boolean;
    FChanged: boolean;
    FUpdateCount: integer;
    FOnChanged: TNotifyEvent;
    FAddressOffset: Integer;
    FRelativeAddress: Boolean;
    FAddressWidth: Integer;
    FAddressFormat: string;
    FLeftHexMargin: integer;
    FRightHexMargin: integer;
    FRightAddressMargin: integer;
    FLeftAnsiMargin: integer;
    FLeftDividerWidth: integer;
    FRightDividerWidth: integer;
    FVisibleCharacters: TByteSet;
    FNonprintableReplacement: Char;


    procedure CalcPaintParams;
    procedure SetTopLine(Value: Integer);
    procedure SetCurrentLine(Value: Integer);
    procedure SetForegroundColor(Index: TForegroundColor; Value: TColor);
    function GetForegroundColor(Index: TForegroundColor): TColor;
    procedure SetShowCharacters(Value: Boolean);
    procedure SetShowAddress(Value: Boolean);
    procedure SetBorder(Value: TBorderStyle);
    procedure SetAddress(Value: Pointer);
    procedure SetDataSize(Value: Integer);
    procedure AdjustScrollBars;
    function LineAddr(Index: Integer): string;
    function LineData(Index: Integer; Count: integer): string;
    function LineChars(Index: Integer; Count: integer): String;
    function ScrollIntoView: Boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMLostFocus); message CM_EXIT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSetFocus(var Message : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMChar(var Message : TWMChar); message WM_CHAR;
    procedure SetReadOnly(const Value: boolean);
    procedure SetRelativeAddress(const Value: Boolean);
    procedure SetCurrentLinePos(const Value: Integer);
    procedure SetCaretPos;
    procedure SetEditCharacters(const Value: boolean);
    procedure SetLowNibble(const Value: boolean);
    procedure SetAddressOffset(const Value: Integer);
    function GetBackgroundColor(const Index: TBackgroundColor): TColor;
    procedure SetBackgroundColor(const Index: TBackgroundColor; const Value: TColor);
    procedure SetAddressFormat(const Value: string);
    procedure SetLeftHexMargin(const Value: integer);
    procedure SetRightHexMargin(const Value: integer);
    procedure SetRightAddressMargin(const Value: integer);
    procedure SetLeftAnsiMargin(const Value: integer);
    procedure SetLeftDividerWidth(const Value: integer);
    procedure SetRightDividerWidth(const Value: integer);
    procedure SetVisibleCharacters(const Value: TByteSet);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Changed;
    function CanEdit: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property CurrentLine: Integer read FCurrentLine write SetCurrentLine;
    property CurrentLinePos : Integer read FCurrentLinePos write SetCurrentLinePos;
    property EditCharacters : boolean read FEditCharacters write SetEditCharacters;
    property Address: Pointer read FAddress write SetAddress;
    property DataSize: Integer read FDataSize write SetDataSize;
    property AddressOffset: Integer read FAddressOffset write SetAddressOffset;
    property LowNibble : boolean read FLowNibble write SetLowNibble;
    property Modified: boolean read FChanged;
    property AddressFormat: string read FAddressFormat write SetAddressFormat;
    property RightAddressMargin: integer read FRightAddressMargin write SetRightAddressMargin default 5;
    property LeftHexMargin: integer read FLeftHexMargin write SetLeftHexMargin default 5;
    property RightHexMargin: integer read FRightHexMargin write SetRightHexMargin default 5;
    property LeftAnsiMargin: integer read FLeftAnsiMargin write SetLeftAnsiMargin default 5;
    property LeftDividerWidth: integer read FLeftDividerWidth write SetLeftDividerWidth default 1;
    property RightDividerWidth: integer read FRightDividerWidth write SetRightDividerWidth default 1;
    property VisibleCharacters: TByteSet read FVisibleCharacters write SetVisibleCharacters;
    property NonprintableReplacement: Char read FNonprintableReplacement write FNonprintableReplacement;
  published
    property Align;
    property Anchors;
    property Border: TBorderStyle read FBorder write SetBorder;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property Color default clWhite;
    property BevelKind;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property Ctl3D;
    property Enabled;
    property Font;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property ReadOnly : boolean read fReadOnly write SetReadOnly;
    property ShowAddress: Boolean read FShowAddress write SetShowAddress default True;
    property RelativeAddress: Boolean read FRelativeAddress write SetRelativeAddress default True;
    property ShowCharacters: Boolean read FShowCharacters write SetShowCharacters default True;
    property AddressColor: TColor index fcAddress read GetForegroundColor write SetForegroundColor default clNavy;
    property AddressBackgroundColor: TColor index bcAddress read GetBackgroundColor write SetBackgroundColor default clWhite;
    property HexDataColor: TColor index fcHex read GetForegroundColor write SetForegroundColor default clBlack;
    property HexDataBackgroundColor: TColor index bcHex read GetBackgroundColor write SetBackgroundColor default clWhite;
    property HexDataBackgroundColorOdd: TColor index bcHexOdd read GetBackgroundColor write SetBackgroundColor default clDefault;
    property AnsiCharColor: TColor index fcAnsi read GetForegroundColor write SetForegroundColor default clBlue;
    property AnsiCharBackgroundColor: TColor index bcAnsi read GetBackgroundColor write SetBackgroundColor default clWhite;
    property LeftDividerColor: TColor index fcLeftDivider read GetForegroundColor write SetForegroundColor default clGray;
    property RightDividerColor: TColor index fcRightDivider read GetForegroundColor write SetForegroundColor default clGray;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

function CreateHexDump(AOwner: TWinControl): THexDump;

implementation

uses
  Types;

{ Form Methods }

function CreateHexDump(AOwner: TWinControl): THexDump;
begin
  Result := THexDump.Create(AOwner);
  with Result do
  begin
    Parent := AOwner;
    Font.Name := 'FixedSys';
    ShowCharacters := True;
    Align := alClient;
  end;
end;

{ THexDump }

constructor THexDump.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csFramed, csCaptureMouse, csClickEvents, csDoubleClicks];
  FBorder := bsSingle;
  Color := clWhite;
  FForegroundColors[fcAddress] := clNavy;
  FForegroundColors[fcHex] := clBlack;
  FForegroundColors[fcAnsi] := clBlue;
  FForegroundColors[fcLeftDivider] := clGray;
  FForegroundColors[fcRightDivider] := clGray;
  FBackgroundColors[bcAddress] := clSilver;
  FBackgroundColors[bcHex] := clWhite;
  FBackgroundColors[bcHexOdd] := clDefault;
  FBackgroundColors[bcAnsi] := clSilver;
  FShowAddress := True;
  FRelativeAddress := True;
  FShowCharacters := True;
  Width := 300;
  Height := 200;
  FAddressFormat := '%p';
  FRightAddressMargin := 5;
  FLeftHexMargin := 5;
  FRightHexMargin := 5;
  FLeftAnsiMargin := 5;
  FLeftDividerWidth := 1;
  FRightDividerWidth := 1;
  FVisibleCharacters := [32..127];
  NonprintableReplacement := '.';
end;

destructor THexDump.Destroy;
begin
  inherited Destroy;
end;

procedure THexDump.EndUpdate;
begin
  if (FUpdateCount = 1) and (FChanged) then
  begin
    if Assigned(OnChanged) then
      OnChanged(Self);
    FChanged := False;
    Invalidate;
  end;
  Dec(FUpdateCount);
end;

procedure THexDump.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if FBorder = bsSingle then
      Style := Style or WS_BORDER;
    Style := Style or WS_VSCROLL;
  end;
end;

{ VCL Command Messages }

procedure THexDump.CMFontChanged(var Message: TMessage);
var
  CharSize: TSize;
begin
  inherited;
  Canvas.Font := Self.Font;
  CharSize := Canvas.TextExtent('E');
  FItemHeight := CharSize.cy + 2;
  FItemWidth := CharSize.cx;
  CalcPaintParams;
  AdjustScrollBars;
end;

procedure THexDump.CMEnter;
begin
  inherited;
{  InvalidateLineMarker; }
end;

procedure THexDump.CMExit;
begin
  inherited;
{  InvalidateLineMarker; }
end;

{ Windows Messages }

procedure THexDump.WMSize(var Message: TWMSize);
var
  offset : Integer;
  obpl : Integer;
begin
  inherited;
  obpl := fBytesPerLine;
  offset := CurrentLine * FBytesPerLine + CurrentLinePos;
  CalcPaintParams;
  if (FBytesPerLine > 0) and (obpl <> FBytesPerLine) then
  begin
    FCurrentLine := offset div FBytesPerLine;
    FCurrentLinePos := offset mod FBytesPerLine;
    SetCaretPos;
  end;
  AdjustScrollBars;
end;

procedure THexDump.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
  if not ReadOnly then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure THexDump.WMVScroll(var Message: TWMVScroll);
var
  NewTopLine: Integer;
  LinesMoved: Integer;
  R: TRect;
begin
  inherited;
  NewTopLine := FTopLine;
  case Message.ScrollCode of
    SB_LINEDOWN: Inc(NewTopLine);
    SB_LINEUP: Dec(NewTopLine);
    SB_PAGEDOWN: Inc(NewTopLine, FVisibleLines - 1);
    SB_PAGEUP: Dec(NewTopLine, FVisibleLines - 1);
    SB_THUMBPOSITION, SB_THUMBTRACK: NewTopLine := Message.Pos;
  end;

  if NewTopLine < 0 then NewTopLine := 0;
  if NewTopLine >= FLineCount then
    NewTopLine := FLineCount - 1;

  if NewTopLine <> FTopLine then
  begin
    LinesMoved := FTopLine - NewTopLine;
    FTopLine := NewTopLine;
    SetScrollPos(Handle, SB_VERT, FTopLine, True);

    if Abs(LinesMoved) = 1 then
    begin
      R := Bounds(0, 0, ClientWidth, ClientHeight - FItemHeight);
      if LinesMoved = 1 then OffsetRect(R, 0, FItemHeight);

      ScrollWindow(Handle, 0, FItemHeight * LinesMoved, @R, nil);

      if LinesMoved = -1 then
      begin
        R.Top := ClientHeight - FItemHeight;
        R.Bottom := ClientHeight;
      end
      else
      begin
        R.Top := 0;
        R.Bottom := FItemHeight;
      end;

      Windows.InvalidateRect(Handle, @R, False);

    end
    else Invalidate;
  end;
  SetCaretPos
end;

{ Painting Related }

procedure THexDump.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure THexDump.CalcPaintParams;
const
  Divisor: array[boolean] of Integer = (3,4);
var
  CharsPerLine: Integer;
  Width: integer;
begin
  if FItemHeight < 1 then Exit;
  FVisibleLines := (ClientHeight div FItemHeight) + 1;
  if FShowAddress then
    FAddressWidth := Canvas.TextWidth(LineAddr(0))
  else
    FAddressWidth := 0;
  Width := ClientWidth-FLeftHexMargin-FRightHexMargin;
  if FShowAddress then
    dec(Width, FAddressWidth+FRightAddressMargin+FLeftDividerWidth);
  if FShowCharacters then
    dec(Width, FLeftAnsiMargin+FRightDividerWidth);
  CharsPerLine := Width div FItemWidth+1;
  FBytesPerLine := CharsPerLine div Divisor[FShowCharacters];
  if FBytesPerLine < 1 then
    FBytesPerLine := 1
  else if FBytesPerLine > MAXDIGITS then
    FBytesPerLine := MAXDIGITS;
  FLineCount := (DataSize div FBytesPerLine);
  if (DataSize mod FBytesPerLine <> 0) then
    Inc(FLineCount);
end;

function THexDump.CanEdit: boolean;
begin
  Result := (not ReadOnly) and (FActive);
end;

procedure THexDump.Changed;
begin
  BeginUpdate;
  FChanged := True;
  EndUpdate;
end;

procedure THexDump.AdjustScrollBars;
begin
  SetScrollRange(Handle, SB_VERT, 0, FLineCount - 1, True);
end;

function THexDump.ScrollIntoView: Boolean;
begin
  Result := False;
  if FCurrentLine < FTopLine then
  begin
    Result := True;
    SetTopLine(FCurrentLine);
  end
  else if FCurrentLine >= (FTopLine + FVisibleLines) - 1 then
  begin
    SetTopLine(FCurrentLine - (FVisibleLines - 2));
    Result := True;
  end;
end;

procedure THexDump.SetTopLine(Value: Integer);
var
  LinesMoved: Integer;
  R: TRect;
begin
  if Value <> FTopLine then
  begin
    if Value < 0 then Value := 0;
    if Value >= FLineCount then Value := FLineCount - 1;

    LinesMoved := FTopLine - Value;
    FTopLine := Value;
    SetScrollPos(Handle, SB_VERT, FTopLine, True);

    if Abs(LinesMoved) = 1 then
    begin
      R := Bounds(1, 0, ClientWidth, ClientHeight - FItemHeight);
      if LinesMoved = 1 then OffsetRect(R, 0, FItemHeight);

      ScrollWindow(Handle, 0, FItemHeight * LinesMoved, @R, nil);

      if LinesMoved = -1 then
      begin
        R.Top := ClientHeight - FItemHeight;
        R.Bottom := ClientHeight;
      end
      else
      begin
        R.Top := 0;
        R.Bottom := FItemHeight;
      end;

      InvalidateRect(Handle, @R, False);

    end
    else Invalidate;
  end;
end;

procedure THexDump.SetVisibleCharacters(const Value: TByteSet);
begin
  FVisibleCharacters := Value;
  Invalidate;
end;

procedure THexDump.SetCurrentLine(Value: Integer);
var
  R: TRect;
begin
  if Value <> FCurrentLine then
  begin
    if Value < 0 then Value := 0;
    if Value >= FLineCount then Value := FLineCount - 1;

    if (FCurrentLine >= FTopLine) and (FCurrentLine < FTopLine + FVisibleLines - 1) then
    begin
      R := Bounds(0, 0, 1, FItemHeight);
      OffsetRect(R, 0, (FCurrentLine - FTopLine) * FItemHeight);
      Windows.InvalidateRect(Handle, @R, True);
    end;
    FCurrentLine := Value;

    R := Bounds(0, 0, 1, FItemHeight);
    OffsetRect(R, 0, (FCurrentLine - FTopLine) * FItemHeight);
    Windows.InvalidateRect(Handle, @R, True);
    ScrollIntoView;
    SetCaretPos
  end;
end;

procedure THexDump.Paint;
var
  R, R2: TRect;
  I: Integer;
  ByteCnt: Integer;
  s: string;
//  sa: AnsiString;
begin
  if Focused then
    HideCaret (handle);
  try
    inherited Paint;

    R := ClientRect;

    R2 := R;
    Canvas.Brush.Style := bsSolid;

    if FShowAddress then
    begin
      Canvas.Brush.Color := FBackgroundColors[bcAddress];
      R2.Right := R2.Left+FAddressWidth+FRightAddressMargin;
      Canvas.FillRect(R2);
      if (FLeftDividerWidth > 0) then
      begin
        R2.Left := R2.Right;
        inc(R2.Right, FLeftDividerWidth);
        Canvas.Brush.Color := FForegroundColors[fcLeftDivider];
        Canvas.FillRect(R2);
      end;
      R2.Left := R2.Right;
    end;

    Canvas.Brush.Style := bsSolid;
    if FShowCharacters then
      R2.Right := R2.Left+FLeftHexMargin+FRightHexMargin+(FItemWidth*(FBytesPerLine*3-1));
    if (FBackgroundColors[bcHexOdd] <> FBackgroundColors[bcHex]) and (FBackgroundColors[bcHexOdd] <> clDefault) then
    begin
      R2.Bottom := R2.Top+FItemHeight;
      i := FTopLine;
      while (R2.Bottom < R.Bottom) do
      begin
        if (i and 1 = 0) then
          Canvas.Brush.Color := FBackgroundColors[bcHex]
        else
          Canvas.Brush.Color := FBackgroundColors[bcHexOdd];
        Canvas.FillRect(R2);
        OffsetRect(R2, 0, FItemHeight);
        inc(i);
      end;
      R2.Top := R.Top;
      R2.Bottom := R.Bottom;
    end else
    begin
      Canvas.Brush.Color := FBackgroundColors[bcHex];
      Canvas.FillRect(R2);
    end;

    if FShowCharacters then
    begin
      if (FRightDividerWidth > 0) then
      begin
        R2.Left := R2.Right;
        inc(R2.Right, FRightDividerWidth);
        Canvas.Brush.Color := FForegroundColors[fcRightDivider];
        Canvas.FillRect(R2);
      end;
      R2.Left := R2.Right;
      R2.Right := R.Right;
      Canvas.Brush.Color := FBackgroundColors[bcAnsi];
      Canvas.FillRect(R2);
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Brush.Color := Self.Color;
    R.Bottom := R.Top+FItemHeight;

    R2 := R;
    ByteCnt := FBytesPerLine;
    for I := 0 to FVisibleLines - 1 do
    begin
      R2.Bottom := R2.Top+FItemHeight;
      R2.Left := R.Left;
      if (FLineCount > 0) and (I + FTopLine < FLineCount) then
      begin
        if FShowAddress then
        begin
          R2.Right := R2.Left+FAddressWidth+FRightAddressMargin;
          Canvas.Font.Color := FForegroundColors[fcAddress];
          Canvas.Brush.Color := FBackgroundColors[bcAddress];
          s := LineAddr(I+FTopLine);
          ExtTextOut(Canvas.Handle, R2.Left, R2.Top, ETO_OPAQUE or ETO_CLIPPED, @R2, s, Length(s), nil);
          R2.Left := R2.Right+FLeftDividerWidth;
        end;
        if (I+FTopLine = FLineCount-1) and ((DataSize mod FBytesPerLine) > 0) then
          ByteCnt := DataSize mod FBytesPerLine;
//        TabbedTextOut(Canvas.Handle, R.Left, R.Top, LineData(I+FTopLine),
//          (ByteCnt*3)-1, 1, TabStop, R.Left);


        inc(R2.Left, LeftHexMargin);
        R2.Right := R2.Left+FItemWidth*(FBytesPerLine*3-1);
        Canvas.Font.Color := FForegroundColors[fcHex];
        if (FBackgroundColors[bcHexOdd] <> FBackgroundColors[bcHex]) and (FBackgroundColors[bcHexOdd] <> clDefault) and
          ((i+FTopLine) and 1 = 1) then
          Canvas.Brush.Color := FBackgroundColors[bcHexOdd]
        else
          Canvas.Brush.Color := FBackgroundColors[bcHex];
        Canvas.TextOut(R2.Left, R2.Top, LineData(I+FTopLine, ByteCnt));

        if FShowCharacters then
        begin
          R2.Left := R2.Right+FRightHexMargin+FLeftAnsiMargin;

          if (FRightDividerWidth > 0) then
            inc(R2.Left, FRightDividerWidth);
          R2.Right := R.Right;
          Canvas.Font.Color := FForegroundColors[fcAnsi];
          Canvas.Brush.Color := FBackgroundColors[bcAnsi];

          // Porting note: Here ExtTextOutA should be explicitly called,
          // the default of ExtTextOutW would apply if the call
          // had been ExtTextOut.

//          sa := LineChars(I+FTopLine, ByteCnt);
//          ExtTextOutA(Canvas.Handle, R2.Left, R2.Top, ETO_OPAQUE or ETO_CLIPPED, @R2, sa, Length(sa), nil);
          s := LineChars(I+FTopLine, ByteCnt);
          ExtTextOut(Canvas.Handle, R2.Left, R2.Top, ETO_OPAQUE or ETO_CLIPPED, @R2, s, Length(s), nil);
        end;
(*      end
      else
      begin
        Canvas.Font.Color := FForegroundColors[fcAnsi];
        Canvas.Brush.Color := FBackgroundColors[bcAnsi];
        ExtTextOut(Canvas.Handle, R2.Left+LeftHexMargin, R2.Top, ETO_OPAQUE or ETO_CLIPPED,
          @R2, nil, 0, nil);
*)
      end;
      R2.Top := R2.Bottom;
    end;
  finally
    if Focused then
      ShowCaret (handle)
  end
end;

{ Event Overrides }

procedure THexDump.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not FActive then Exit;

  case Key of
    VK_DOWN: CurrentLine := CurrentLine + 1;
    VK_UP: CurrentLine := CurrentLine - 1;
    VK_NEXT: CurrentLine := CurrentLine + FVisibleLines;
    VK_PRIOR: CurrentLine := CurrentLine - FVisibleLines;
    VK_HOME: CurrentLine := 0;
    VK_END: CurrentLine := FLineCount - 1;

    VK_LEFT : if EditCharacters or not LowNibble then
              begin
                FLowNibble := True;
                CurrentLinePos := CurrentLinePos - 1
              end
              else
                LowNibble := False;

    VK_RIGHT : if EditCharacters or LowNibble then
               begin
                 FLowNibble := False;
                 CurrentLinePos := CurrentLinePos + 1
               end
               else
                 LowNibble := True;
    VK_TAB : EditCharacters := not EditCharacters
  end;
end;

procedure THexDump.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  NewLinePos: integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not Focused then SetFocus;
  if (Button = mbLeft) and FActive then
  begin
    CurrentLine := FTopLine + (Y div FItemHeight);

    NewLinePos := 0;
    Dec(X, FLeftDividerWidth+LeftHexMargin);
    if (FShowAddress) then
      Dec(X, FAddressWidth+FRightAddressMargin);

    if (X >= 0) then
    begin
      if (X > FItemWidth*(FBytesPerLine*3-1)+FRightHexMargin+FRightDividerWidth) then
      begin
        // X is in ANSI
        Dec(X, FItemWidth*(FBytesPerLine*3-1)+FRightHexMargin+FRightDividerWidth+FLeftAnsiMargin);
        if (FShowCharacters) then
        begin
          if (X >= 0) then
          begin
            if (X < FItemWidth*FBytesPerLine) then
              NewLinePos := X div FItemWidth
            else
              NewLinePos := FBytesPerLine-1;
          end;
          FEditCharacters := True;
        end;
      end else
      begin
        // X is in Hex dump
        NewLinePos := X div (FItemWidth*3);
        FEditCharacters := False;
      end;
    end;

    FLowNibble := False;
    CurrentLinePos := NewLinePos;
  end;
end;

{ Property Set/Get Routines }

procedure THexDump.SetBackgroundColor(const Index: TBackgroundColor;
  const Value: TColor);
begin
  if FBackgroundColors[Index] <> Value then
  begin
    FBackgroundColors[Index] := Value;
    Invalidate;
  end;
end;

procedure THexDump.SetBorder(Value: TBorderStyle);
begin
  if Value <> FBorder then
  begin
    FBorder := Value;
    RecreateWnd;
  end;
end;

procedure THexDump.SetShowAddress(Value: Boolean);
begin
  if FShowAddress <> Value then
  begin
    FShowAddress := Value;
    Invalidate;
  end;
end;

procedure THexDump.SetShowCharacters(Value: Boolean);
begin
  if Value <> FShowCharacters then
  begin
    FShowCharacters := Value;
    Invalidate;
  end;
end;

procedure THexDump.SetForegroundColor(Index: TForegroundColor; Value: TColor);
begin
  if FForegroundColors[Index] <> Value then
  begin
    FForegroundColors[Index] := Value;
    Invalidate;
  end;
end;


procedure THexDump.SetRelativeAddress(const Value: Boolean);
begin
  FRelativeAddress := Value;
  Invalidate;
end;

procedure THexDump.SetRightAddressMargin(const Value: integer);
begin
  FRightAddressMargin := Value;
  Invalidate;
end;

procedure THexDump.SetRightDividerWidth(const Value: integer);
begin
  FRightDividerWidth := Value;
  Invalidate;
end;

procedure THexDump.SetRightHexMargin(const Value: integer);
begin
  FRightHexMargin := Value;
  Invalidate;
end;

function THexDump.GetBackgroundColor(const Index: TBackgroundColor): TColor;
begin
  Result := FBackgroundColors[Index];
  if (Result = clDefault) and (Index = bcHexOdd) then
    Result := FBackgroundColors[bcHex];
end;

function THexDump.GetForegroundColor(Index: TForegroundColor): TColor;
begin
  Result := FForegroundColors[Index];
end;

procedure THexDump.SetAddress(Value: Pointer);
begin
  FAddress := Value;
  FActive := (FAddress <> nil) and (FDataSize > 0);
  FCurrentLine := 0;
  FCurrentLinePos := 0;
  FTopLine := 0;
  SetScrollPos(Handle, SB_VERT, FTopLine, True);
  Invalidate;
end;

procedure THexDump.SetAddressFormat(const Value: string);
begin
  FAddressFormat := Value;
  CalcPaintParams;
  Invalidate;
end;

procedure THexDump.SetDataSize(Value: Integer);
begin
  FDataSize := Value;
  FActive := (FAddress <> nil) and (FDataSize > 0);
  FCurrentLine := 0;
  FCurrentLinePos := 0;
  FTopLine := 0;
  SetScrollPos(Handle, SB_VERT, FTopLine, True);
  CalcPaintParams;
  Invalidate;
  AdjustScrollBars;
end;

function THexDump.LineAddr(Index: Integer): string;
var
  Origin: pointer;
begin
  if (RelativeAddress) then
    Origin := pointer(AddressOffset)
  else
    Origin := Address;
  Result := Format(FAddressFormat, [pointer(PAnsiChar(Origin)+Index*FBytesPerLine)]);
end;

function THexDump.LineData(Index: Integer; Count: integer): string;

  procedure SetData(Source: PByte);
  const
    HexDigits : array[0..15] of Char = '0123456789ABCDEF';
  var
    Dest: PChar;
  begin
    Dest := PChar(Result);
    while (Count > 0) do
    begin
      Dest^ := HexDigits[Source^ SHR $04]; inc(Dest);
      Dest^ := HexDigits[Source^ AND $0F]; inc(Dest);
      inc(Dest);
      inc(Source);
      dec(Count);
    end;
  end;

begin
  Result := StringOfChar(' ', Count*3-1);

  SetData(PByte(FAddress) + Index*FBytesPerLine);
end;

function THexDump.LineChars(Index: Integer; Count: integer): String;
var
  p: PChar;
  Source: PAnsiChar;
begin
  SetLength(Result, Count);
  p := PChar(Result);
  Source := PAnsiChar(FAddress) + Index*FBytesPerLine;
  while (Count > 0) do
  begin
    if (ord(Source^) in FVisibleCharacters) then
      p^ := Char(Source^)
    else
      p^ := NonprintableReplacement;
    inc(p);
    inc(Source);
    dec(Count);
  end;
end;
  

procedure THexDump.CreateWnd;
var
  CharSize: TSize;
begin
  inherited;
  Canvas.Font := Self.Font;
  CharSize := Canvas.TextExtent('E');
  FItemHeight := CharSize.cy + 2;
  FItemWidth := CharSize.cx;
end;

procedure THexDump.SetReadOnly(const Value: boolean);
begin
  if value <> fReadOnly then
  begin
    fReadOnly := Value;
    RecreateWnd
  end
end;

procedure THexDump.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Win32Check(CreateCaret(Handle, 0, FItemWidth, FItemHeight));
  SetCaretPos;
  ShowCaret(Handle)
end;

procedure THexDump.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  HideCaret(Handle);
  DestroyCaret
end;

procedure THexDump.SetCurrentLinePos(const Value: Integer);
var
  v : Integer;
  MaxPos: integer;
begin
  if Value <> FCurrentLinePos then
  begin
    v := Value;
    while (V >= FBytesPerLine) and (CurrentLine < FLineCount - 1) do
    begin
      CurrentLine := CurrentLine + 1;
      Dec (V, FBytesPerLine)
    end;

    if (CurrentLine = FLineCount-1) then
    begin
      MaxPos := FDataSize mod FBytesPerLine;
      if (MaxPos = 0) then
        MaxPos := FBytesPerLine;
    end else
      MaxPos := FBytesPerLine;

    if V >= MaxPos then
    begin
      V := MaxPos-1;
      FLowNibble := True
    end;

    while (V < 0) and (CurrentLine > 0) do
    begin
      CurrentLine := CurrentLine - 1;
      Inc(V, FBytesPerLine)
    end;

    if V < 0 then
    begin
      V := 0;
      FLowNibble := False
    end;

    FCurrentLinePos := V;
    SetCaretPos
  end
end;

procedure THexDump.SetCaretPos;
var
  r: TRect;
  p: TPoint;
begin
  if Focused then
  begin
    r := ClientRect;
    p := r.TopLeft;
    inc(p.Y, FItemHeight * (CurrentLine - FTopLine));
    inc(p.X, FAddressWidth+FLeftHexMargin);
    if (FShowAddress) then
     inc(p.X, FRightAddressMargin+FLeftDividerWidth);
    if FEditCharacters then
      inc(p.X, FRightHexMargin+FRightDividerWidth+FLeftAnsiMargin+(FItemWidth*(FBytesPerLine*3-1)) + FItemWidth*CurrentLinePos)
    else
    begin
      inc(p.X, FItemWidth * 3 * CurrentLinePos);
      if FLowNibble then
        inc(p.X, FItemWidth)
    end;
    Windows.SetCaretPos(p.X, p.Y)
  end
end;

procedure THexDump.SetEditCharacters(const Value: boolean);
begin
  if FEditCharacters <> Value then
  begin
    FEditCharacters := Value and ShowCharacters;
    SetCaretPos
  end
end;

procedure THexDump.SetLeftAnsiMargin(const Value: integer);
begin
  FLeftAnsiMargin := Value;
  Invalidate;
end;

procedure THexDump.SetLeftDividerWidth(const Value: integer);
begin
  FLeftDividerWidth := Value;
  Invalidate;
end;

procedure THexDump.SetLeftHexMargin(const Value: integer);
begin
  FLeftHexMargin := Value;
  Invalidate;
end;

procedure THexDump.SetLowNibble(const Value: boolean);
begin
  if FLowNibble <> Value then
  begin
    FLowNibble := Value;
    SetCaretPos
  end
end;

procedure THexDump.WMChar(var Message: TWMChar);
var
  ch : AnsiChar;
  offset : Integer;
  data : byte;
  changes : boolean;
  b : byte;
begin
  inherited;

  offset := CurrentLine * FBytesPerLine + CurrentLinePos;

  if (CanEdit) and (Offset < DataSize) and (message.CharCode >= 32) and (message.CharCode <= 255) then
  begin
    ch := AnsiChar(message.CharCode);
    changes := False;
    if EditCharacters then
      changes := True
    else
      if ch in ['0'..'9', 'A'..'F', 'a'..'f'] then
      begin
        data := Byte(PAnsiChar(Address)[Offset]);
        changes := True;
        b := StrToInt('$' + ch);
        if LowNibble then
          ch := AnsiChar(data and $f0 or b)
        else
          ch := AnsiChar(data and $0f or (b shl 4));
      end;

    if changes then
    begin
      PAnsiChar(Address)[Offset] := ch;
      if EditCharacters or LowNibble then
      begin
        FLowNibble := False;
        CurrentLinePos := CurrentLinePos + 1
      end
      else
        LowNibble := True;
      Changed;
    end;
  end;
end;

procedure THexDump.SetAddressOffset(const Value: Integer);
begin
  FAddressOffset := Value;
  Invalidate;
end;

end.
