unit uFMDrawUtils;

interface

uses
  System.Types,
  UPhysics2DTypes,
  {$IFDEF VER270}System.Math.Vectors,{$ENDIF}
  System.UITypes;

procedure b2dPointsToPolygon(points: UPhysics2DTypes.TPointsF; var polygon: TPolygon; ATranslateX: Double = 0; InvertY: boolean = True; AMapHeight: Double = 500);
function VCLToFMColor(AColor: TColor): TColor;

implementation

procedure b2dPointsToPolygon(points: UPhysics2DTypes.TPointsF; var polygon: TPolygon; ATranslateX: Double = 0; InvertY: boolean = True; AMapHeight: Double = 500);
var i, aCount: integer;
begin
  aCount := Length(points);
  SetLength(polygon, aCount);
  for i := 0 to aCount-1 do
  begin
    polygon[i].X := points[i].x + ATranslateX;
    if not InvertY then
      polygon[i].Y := points[i].y
    else
      polygon[i].Y := AMapHeight - points[i].y;
  end;
end;

function VCLToFMColor(AColor: TColor): TColor;
begin
  Result := TAlphaColor($FF000000) or TAlphaColor(AColor);
end;

end.
