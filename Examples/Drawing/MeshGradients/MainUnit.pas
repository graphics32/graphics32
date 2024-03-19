unit MainUnit;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Graphics, 
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, SyncObjs, GR32, GR32_Image,
  GR32_ColorGradients, GR32_RangeBars;

type
  TFrmMeshGradients = class(TForm)
    BtnRecall: TButton;
    BtnStore: TButton;
    CbxColoredPolygons: TCheckBox;
    CmbBackgroundSampler: TComboBox;
    ColorDialog: TColorDialog;
    GbrPower: TGaugeBar;
    LblBackgroundSampler: TLabel;
    LblPower: TLabel;
    LblVertexColor: TLabel;
    PaintBox32: TPaintBox32;
    PnlDelaunayTriangulation: TPanel;
    PnlSampler: TPanel;
    PnlSettings: TPanel;
    PnlVertex: TPanel;
    VertexColorShape: TShape;
    procedure FormCreate(Sender: TObject);
    procedure BtnStoreClick(Sender: TObject);
    procedure BtnRecallClick(Sender: TObject);
    procedure CbxAdaptiveSuperSamplerClick(Sender: TObject);
    procedure CmbBackgroundSamplerChange(Sender: TObject);
    procedure GbrPowerChange(Sender: TObject);
    procedure PaintBox32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBox32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure SelectVertexColorClick(Sender: TObject);
    procedure VertexColorShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CbxColoredPolygonsClick(Sender: TObject);
  private
    FColorPoints: TArrayOfColor32FloatPoint;
    FClipboard: TArrayOfColor32FloatPoint;
    FSelected: Integer;
    FIdwPower: TFloat;
    procedure SetSelected(const Value: Integer);
  protected
    procedure SelectedChanged;
  public
    property Selected: Integer read FSelected write SetSelected;
  end;

var
  FrmMeshGradients: TFrmMeshGradients;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math,
  Types,
  GR32_Geometry,
  GR32_Resamplers,
  GR32_Polygons,
  GR32_VectorUtils;

procedure TFrmMeshGradients.FormCreate(Sender: TObject);
var
  Index: Integer;
begin
  SetLength(FColorPoints, 3);
  for Index := 0 to High(FColorPoints) do
  begin
    FColorPoints[Index].Point := FloatPoint(PaintBox32.Width * Random,
      PaintBox32.Height * Random);
    FColorPoints[Index].Color32 := SetAlpha(Random($FFFFFF), $FF);
  end;

  FColorPoints[0].Point := FloatPoint(274, 199);
  FColorPoints[1].Point := FloatPoint(134, 419);
  FColorPoints[2].Point := FloatPoint(46, 146);

  FSelected := -1;
  FIdwPower := 8;

  GbrPowerChange(GbrPower);
end;

procedure TFrmMeshGradients.GbrPowerChange(Sender: TObject);
begin
  FIdwPower := 15.9 * (Log2(1 + 0.0001 * GbrPower.Position)) + 0.1;
  PaintBox32.Invalidate;
end;

procedure TFrmMeshGradients.SelectVertexColorClick(Sender: TObject);
begin
  if (FSelected >= 0) then
  begin
    ColorDialog.Color := WinColor(FColorPoints[FSelected].Color32);
    if ColorDialog.Execute then
    begin
      FColorPoints[FSelected].Color32 := Color32(ColorDialog.Color);
      PaintBox32.Invalidate;
      VertexColorShape.Brush.Color := WinColor(FColorPoints[Selected].Color32);
    end;
  end;
end;

procedure TFrmMeshGradients.PaintBox32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  Selected := -1;
  for Index := 0 to High(FColorPoints) do
  begin
    if (Abs(FColorPoints[Index].Point.X - X) < 4) and
      (Abs(FColorPoints[Index].Point.Y - Y) < 4) then
    begin
      Selected := Index;
      Break;
    end;
  end;

  if (Selected >= 0) and (Button = mbRight) then
  begin
    // do not delete last point!
    if Length(FColorPoints) = 1 then
      Exit;

    if Selected < Length(FColorPoints) - 1 then
      Move(FColorPoints[Selected + 1], FColorPoints[Selected],
        (Length(FColorPoints) - Selected - 1) * SizeOf(TColor32FloatPoint));
    SetLength(FColorPoints, Length(FColorPoints) - 1);
    Selected := -1;
  end;

  if (Selected < 0) and (Button = mbLeft) then
  begin
    Selected := Length(FColorPoints);
    SetLength(FColorPoints, Length(FColorPoints) + 1);
    FColorPoints[Selected].Point := FloatPoint(X, Y);
    FColorPoints[Selected].Color32 := SetAlpha(Random($FFFFFF), $FF);
    VertexColorShape.Brush.Color := WinColor(FColorPoints[Selected].Color32);
    if ssShift in Shift then
      SelectVertexColorClick(Sender);
  end;
  PaintBox32.Invalidate;
end;

procedure TFrmMeshGradients.PaintBox32MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and (Selected >= 0) then
  begin
    FColorPoints[Selected].Point.X := X;
    FColorPoints[Selected].Point.Y := Y;
    PaintBox32.Invalidate;
  end;
end;

procedure TFrmMeshGradients.PaintBox32MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//  Selected := -1;
end;

procedure TFrmMeshGradients.PaintBox32PaintBuffer(Sender: TObject);
var
  Index: Integer;
  X, Y: Integer;
  FrameColor: TColor32;
  Renderer: TPolygonRenderer32VPR;
  Points: TArrayOfFloatPoint;
  Sampler: TCustomArbitrarySparsePointGradientSampler;
  Delaunay: TGourandShadedDelaunayTrianglesSampler;
begin
  // clear paint box
  PaintBox32.Buffer.Clear;

  case CmbBackgroundSampler.ItemIndex of
    1, 2:
      begin
        Sampler := TVoronoiSampler.Create;
        try
          Sampler.SetColorPoints(FColorPoints);

          if CmbBackgroundSampler.ItemIndex = 2 then
            with TAdaptiveSuperSampler.Create(Sampler) do
            begin
              Level := 4;
              PrepareSampling;
              with PaintBox32 do
                for Y := 0 to Height - 1 do
                  for X := 0 to Width - 1 do
                  begin
                    Buffer.Pixel[X, Y] := GetSampleInt(X, Y);
                  end;
            end
          else
          begin
            Sampler.PrepareSampling;
            with PaintBox32 do
              for Y := 0 to Height - 1 do
                for X := 0 to Width - 1 do
                begin
                  Buffer.Pixel[X, Y] := Sampler.GetSampleInt(X, Y);
                end;
          end;
        finally
          Sampler.Free;
        end;
      end;
    3, 4:
      begin
        Sampler := TInvertedDistanceWeightingSampler.Create;
        try
          if CmbBackgroundSampler.ItemIndex = 4 then
            TInvertedDistanceWeightingSampler(Sampler).Power := FIdwPower;
          Sampler.SetColorPoints(FColorPoints);
          Sampler.PrepareSampling;
          with PaintBox32 do
            for Y := 0 to Height - 1 do
              for X := 0 to Width - 1 do
                Buffer.Pixel[X, Y] := Sampler.GetSampleInt(X, Y);
        finally
          Sampler.Free;
        end;
      end;
    5:
      begin
        Sampler := TGourandShadedDelaunayTrianglesSampler.Create;
        try
          Sampler.SetColorPoints(FColorPoints);
          Sampler.PrepareSampling;
          with PaintBox32 do
            for Y := 0 to Height - 1 do
              for X := 0 to Width - 1 do
                Buffer.Pixel[X, Y] := Sampler.GetSampleInt(X, Y);
        finally
          Sampler.Free;
        end;
      end;
  end;

  SetLength(Points, Length(FColorPoints));
  for Index := 0 to High(FColorPoints) do
    Points[Index] := FColorPoints[Index].Point;

  if CbxColoredPolygons.Checked then
  begin
    Renderer := TPolygonRenderer32VPR.Create(PaintBox32.Buffer);
    try
      Delaunay := TGourandShadedDelaunayTrianglesSampler.Create;
      try
        Renderer.FillMode := pfWinding;
        Renderer.Filler := TSamplerFiller.Create(Delaunay);
        Delaunay.SetColorPoints(FColorPoints);
        Renderer.PolygonFS(Points);
      finally
        Delaunay.Free;
      end;
    finally
      Renderer.Free;
    end;
  end;

  with PaintBox32.Buffer do
    for Index := 0 to High(FColorPoints) do
      with FColorPoints[Index] do
      begin
        if Index = FSelected then
          FrameColor := clWhite32
        else
          FrameColor := clBlack32;
        FillRectS(Round(Point.X - 4), Round(Point.Y - 4), Round(Point.X + 4),
          Round(Point.Y + 4), Color32);
        FrameRectTS(Round(Point.X - 5), Round(Point.Y - 5), Round(Point.X + 5),
          Round(Point.Y + 5), FrameColor);
      end;
end;

procedure TFrmMeshGradients.SelectedChanged;
begin
  LblVertexColor.Visible := FSelected >= 0;
  VertexColorShape.Visible := FSelected >= 0;
  if FSelected >= 0 then
    VertexColorShape.Brush.Color := WinColor(FColorPoints[FSelected].Color32);
end;

procedure TFrmMeshGradients.SetSelected(const Value: Integer);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    SelectedChanged;
  end;
end;

procedure TFrmMeshGradients.VertexColorShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    SelectVertexColorClick(Sender);
end;

procedure TFrmMeshGradients.BtnRecallClick(Sender: TObject);
begin
  if Length(FColorPoints) > 0 then
  begin
    FColorPoints := Copy(FClipboard, 0, Length(FColorPoints));
    PaintBox32.Invalidate;
  end;
end;

procedure TFrmMeshGradients.BtnStoreClick(Sender: TObject);
begin
  FClipboard := Copy(FColorPoints, 0, Length(FColorPoints));
  PaintBox32.Invalidate;

  BtnRecall.Enabled := True;
end;

procedure TFrmMeshGradients.CbxAdaptiveSuperSamplerClick(Sender: TObject);
begin
  PaintBox32.Invalidate;
end;

procedure TFrmMeshGradients.CbxColoredPolygonsClick(Sender: TObject);
begin
  PaintBox32.Invalidate;
end;

procedure TFrmMeshGradients.CmbBackgroundSamplerChange(Sender: TObject);
begin
  LblPower.Visible := CmbBackgroundSampler.ItemIndex = 4;
  GbrPower.Visible := LblPower.Visible;

  PaintBox32.Invalidate;
end;

end.
