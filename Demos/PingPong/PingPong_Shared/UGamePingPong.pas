unit UGamePingPong;

interface

uses
  System.Types,
  FMX.Graphics,
  {$IFDEF VER270}System.Math.Vectors,{$ENDIF}
  UGame,
  UGame2D;

type
  TGamePingPong = class(TGame2D)
  private
    FSolidWhiteBrush: TBrush;
    FBallBrush: TBrush;
    FDeltaTime: TTime;
    FCurrTime: TTime;
    GPath: TPathData;
    PlateY: Integer;
    procedure DrawEllipse(Canvas: TCanvas; aRect: TRectF; aOpacity: Single);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DoRender(ACanvas: TCanvas; ARect: TRectF); override;
    procedure DoProgress(const deltaTime, newTime: TTime); override;
  end;

implementation

uses
  System.SysUtils, System.UITypes, FMX.Types, UPingPongTypes, uFMDrawUtils;

{ TGamePingPong }

const
  PlateColor = TAlphaColorRec.Maroon;
  BallColor = TAlphaColorRec.Red;

constructor TGamePingPong.Create;
begin
  inherited;
  GPath := TPathData.Create;
  FSolidWhiteBrush := TBrush.Create(TBrushKind.bkSolid, TAlphaColorRec.White);
  FBallBrush := TBrush.Create(TBrushKind.bkSolid, BallColor);
end;

destructor TGamePingPong.Destroy;
begin
  FSolidWhiteBrush.Free;
  FBallBrush.Free;
  GPath.Free;
  inherited;
end;

procedure TGamePingPong.DoProgress(const deltaTime, newTime: TTime);
begin
  inherited;
  FDeltaTime := deltaTime;
  FCurrTime := newTime;
end;

procedure TGamePingPong.DrawEllipse(Canvas: TCanvas; aRect: TRectF; aOpacity: Single);
begin
  // work around so ellipses don't crash on mobile
  GPath.Clear;
  GPath.AddEllipse(aRect);
  Canvas.DrawPath(GPath, aOpacity);
end;


procedure TGamePingPong.DoRender(ACanvas: TCanvas; ARect: TRectF);

  procedure DrawBall(ACanvas: TCanvas; X,Y: Double);
  var mRect: TRectF;
  begin
    mRect := RectF(X-DefaultBallRadius/2, MapHeight-Y-DefaultBallRadius/2,
      X+DefaultBallRadius/2, MapHeight-Y+DefaultBallRadius/2);
    ACanvas.Stroke.Color := BallColor;
    ACanvas.Stroke.Kind := TBrushKind.bkSolid;
    ACanvas.Stroke.Thickness := 3;
    //ACanvas.DrawEllipse(mRect, 100);
    DrawEllipse(ACanvas,mRect, 100);
  end;

const DEFAULT_OPACITY: Single = 100;

var mRect: TRectF; s: string;
  i: Integer;
  tmpX: Single;
  b: TBlock;
  poly: TPolygon;
begin
  inherited;

  ACanvas.BeginScene;
  try
    ACanvas.Fill := FSolidWhiteBrush;
    ACanvas.FillRect(ARect,0,0,[],DEFAULT_OPACITY);

    for i := 0 to Blocks.Count-1 do
    begin
      b := TBlock(Blocks[i]);

      if b.HP <= 0 then
        Continue;

      ACanvas.Fill.Color := b.Color;
      mRect := RectF(
        b.X + 1,
        MapHeight - b.Y + 1 - BlockHeight,
        b.X + BlockWidth - 1,
        MapHeight - b.Y - 1);
      ACanvas.FillRect(mRect,0,0,[],DEFAULT_OPACITY);

      if SelectedBlock = b then
      begin
        ACanvas.Stroke.Thickness := 1;
        ACanvas.Stroke.Kind := TBrushKind.bkSolid;
        ACanvas.Stroke.Color := GetReverseColor(b.Color);

        ACanvas.DrawLine(
          PointF(b.X + 1, MapHeight-(b.Y + BlockHeight - 1)),
          PointF(b.X + BlockWidth - 1, MapHeight-(b.Y + 1)),
          DEFAULT_OPACITY);

        ACanvas.DrawLine(
          PointF(b.X + 1, MapHeight-(b.Y + 1)),
          PointF(b.X + BlockWidth - 1, MapHeight-(b.Y + BlockHeight - 1)),
          DEFAULT_OPACITY);
      end;
    end;

    if not EditingMap then
    begin
      ACanvas.Fill.Color := PlateColor;
      ACanvas.Stroke.Color := PlateColor;
      ACanvas.Stroke.Kind := TBrushKind.bkSolid;
      ACanvas.Stroke.Thickness := 3;

      if PlateY=0 then
       begin
         PlateY := Trunc(MapHeight - DefaultBallRadius - 2);
       end;

      b2dPointsToPolygon(Plate.DrawPoints, poly, Plate.X);
      ACanvas.DrawLine(PointF(Plate.X-(Plate.Width/2),PlateY),PointF(Plate.X+(Plate.Width-(Plate.Width/2)),PlateY),DEFAULT_OPACITY);
      // Doesn't appear on mobile. Polygon points are not right.
      //ACanvas.DrawPolygon(poly, DEFAULT_OPACITY);

      DrawBall(ACanvas, Ball.X, Ball.Y);

      tmpX := MapWidth - 7;
      for i := 1 to Life - 1 do
      begin
        DrawBall(ACanvas, tmpX, MapHeight - DefaultBallRadius - 2);
        tmpX := tmpX - DefaultBallRadius * 2 - 3;
      end;

      if (GameState = gsNotLaunched) or (GameState = gsGameOver) or (GameState = gsGameFinished) then
      begin

        if GameState = gsNotLaunched then
          s := 'Launch the ball by left clicking.'
        else if GameState = gsGameOver then
          s := 'Game Over'
        else if GameState = gsGameFinished then
          s := 'Game Success';

        ACanvas.Fill.Color := TAlphaColors.Black;
        mRect.Create(2, 2, 160, 20);

        ACanvas.FillText(mRect, s, false, DEFAULT_OPACITY, [],
          TTextAlign.taLeading, TTextAlign.taLeading);
      end
    end;

  finally
    ACanvas.EndScene;
  end;
end;

end.
