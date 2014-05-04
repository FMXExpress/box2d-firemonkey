unit UConvexHull;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TConvexHull = class(TTester)
   private
      procedure Generate;
   public
      m_points: Tb2PolyVertices;
      m_auto: Boolean;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

const
   e_count = b2_maxPolygonVertices;

{ TConvexHull }

constructor TConvexHull.Create;
begin
   inherited;
   Generate;
   m_auto := False;
end;

procedure TConvexHull.Step(var settings: TSettings; timeStep: PhysicsFloat);
var
   i: Integer;
   shape: Tb2PolygonShape;
begin
   inherited;

   shape := Tb2PolygonShape.Create;
   shape.SetVertices(@m_points[0], e_count);

   DrawText('Press G to generate a new random convex hull.');
   DrawText('Press A to switch auto-generating convex hull.');
   m_debugDraw.DrawPolygon(shape.m_vertices, shape.m_count, MakeColor(0.9, 0.9, 0.9));
   shape.Free;

   for i := 0 to e_count - 1 do
   begin
      m_debugDraw.DrawPoint(m_points[i], 3.0, MakeColor(0.9, 0.5, 0.5));
      //DrawText(m_points[i] + b2Vec2(0.05f, 0.05f), "%d", i);
   end;

   if m_auto then
      Generate;
end;

procedure TConvexHull.Generate;
var
   i: Integer;
   v: TVector2;
   lowerBound, upperBound: TVector2;
begin
   lowerBound := MakeVector(-8.0, -8.0);
   upperBound := MakeVector(8.0, 8.0);

   for i := 0 to e_count - 1 do
   begin
			v.x := 10.0 * RandomFloat;
			v.y := 10.0 * RandomFloat;

			// Clamp onto a square to help create collinearities.
			// This will stress the convex hull algorithm.
			v := b2Clamp(v, lowerBound, upperBound);
			m_points[i] := v;
   end;
end;

procedure TConvexHull.Keyboard(key: Byte);
var
   f, p: TVector2;
begin
   case key of
      Ord('A'): m_auto := not m_auto;
      Ord('G'): Generate;
   end;
end;

initialization
   RegisterTestEntry('Convex Hull', TConvexHull);

end.
