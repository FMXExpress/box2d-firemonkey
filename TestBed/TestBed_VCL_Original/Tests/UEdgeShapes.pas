unit UEdgeShapes;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

const
   e_maxBodies = 256;

type
   TEdgeShapes = class(TTester)
   private
      procedure CreateShape(index: Int32);
      procedure DestroyBody;
   public
      m_bodyIndex: Int32;
      m_bodies: array[0..e_maxBodies - 1] of Tb2Body;
      m_polygons: array[0..3] of Tb2PolygonShape;
      m_circle: Tb2CircleShape;
      m_angle: PhysicsFloat;

      constructor Create; override;
      destructor Destroy; override;

      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TEdgeShapes }

constructor TEdgeShapes.Create;
var
   i: Integer;
   bd: Tb2BodyDef;
   ground: Tb2Body;
   x1, y1, x2, y2, w, b, s: PhysicsFloat;
   shape: Tb2EdgeShape;
   vertices: array[0..7] of TVector2;
begin
   inherited;
   // Ground body
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      x1 := -20.0;
      y1 := 2.0 * Cos(x1 / 10.0 * Pi);
      for i := 0 to 79 do
      begin
         x2 := x1 + 0.5;
         y2 := 2.0 * Cos(x2 / 10.0 * Pi);

         shape := Tb2EdgeShape.Create;
         shape.SetVertices(MakeVector(x1, y1), MakeVector(x2, y2));
         ground.CreateFixture(shape, 0.0);

         x1 := x2;
         y1 := y2;
      end;
   end;

   for i := 0 to 3 do
      m_polygons[i] := Tb2PolygonShape.Create;
   begin
      SetValue(vertices[0], -0.5, 0.0);
      SetValue(vertices[1], 0.5, 0.0);
      SetValue(vertices[2], 0.0, 1.5);
      m_polygons[0].SetVertices(@vertices[0], 3);
   end;

   begin
      SetValue(vertices[0], -0.1, 0.0);
      SetValue(vertices[1], 0.1, 0.0);
      SetValue(vertices[2], 0.0, 1.5);
      m_polygons[1].SetVertices(@vertices[0], 3);
   end;

   begin
      w := 1.0;
      b := w / (2.0 + Sqrt(2.0));
      s := Sqrt(2.0) * b;

      SetValue(vertices[0], 0.5 * s, 0.0);
      SetValue(vertices[1], 0.5 * w, b);
      SetValue(vertices[2], 0.5 * w, b + s);
      SetValue(vertices[3], 0.5 * s, w);
      SetValue(vertices[4], -0.5 * s, w);
      SetValue(vertices[5], -0.5 * w, b + s);
      SetValue(vertices[6], -0.5 * w, b);
      SetValue(vertices[7], -0.5 * s, 0.0);

      m_polygons[2].SetVertices(@vertices[0], 8);
   end;
   m_polygons[3].SetAsBox(0.5, 0.5);

   m_circle := Tb2CircleShape.Create;
   m_circle.m_radius := 0.5;

   m_bodyIndex := 0;
   for i := 0 to e_maxBodies - 1 do
      m_bodies[i] := nil;
   m_angle := 0.0;
end;

destructor TEdgeShapes.Destroy;
var
   i: Integer;
begin
   for i := 0 to 3 do
      m_polygons[i].Free;
   m_circle.Free;
   inherited;
end;

procedure TEdgeShapes.CreateShape(index: Int32);
var
   bd: Tb2BodyDef;
   x, y: PhysicsFloat;
   fd: Tb2FixtureDef;
begin
   if Assigned(m_bodies[m_bodyIndex]) then
   begin
      m_world.DestroyBody(m_bodies[m_bodyIndex]);
      m_bodies[m_bodyIndex] := nil;
   end;

   bd := Tb2BodyDef.Create;

   x := RandomFloat(-10.0, 10.0);
   y := RandomFloat(10.0, 20.0);
   SetValue(bd.position, x, y);
   bd.angle := RandomFloat(-Pi, Pi);
   bd.bodyType := b2_dynamicBody;

   if index = 4 then
      bd.angularDamping := 0.02;

   m_bodies[m_bodyIndex] := m_world.CreateBody(bd);

   fd := Tb2FixtureDef.Create;
   fd.friction := 0.3;
   fd.density := 20.0;
   if index < 4 then
      fd.shape := m_polygons[index]
   else
      fd.shape := m_circle;
   m_bodies[m_bodyIndex].CreateFixture(fd, True, False);
   m_bodyIndex := (m_bodyIndex + 1) mod e_maxBodies;
end;

procedure TEdgeShapes.DestroyBody;
var
   i: Integer;
begin
   for i := 0 to e_maxBodies - 1 do
     if Assigned(m_bodies[i]) then
     begin
        m_world.DestroyBody(m_bodies[i]);
        m_bodies[i] := nil;
        Exit;
     end;
end;

type
   TEdgeShapesCallback = class(Tb2RayCastCallback)
   public
      m_fixture: Tb2Fixture;
      m_point, m_normal: TVector2;

      function ReportFixture(fixture:	Tb2Fixture; const point, normal: TVector2;
         fraction: PhysicsFloat): PhysicsFloat; override;
   end;

{ TEdgeShapesCallback }

function TEdgeShapesCallback.ReportFixture(fixture: Tb2Fixture; const point,
   normal: TVector2; fraction: PhysicsFloat): PhysicsFloat;
begin
		m_fixture := fixture;
		m_point := point;
		m_normal := normal;
    Result := fraction;
end;

procedure TEdgeShapes.Step(var settings: TSettings; timeStep: PhysicsFloat);
const
   L = 25;
   point1: TVector2 = (X: 0.0; Y: 10.0);
   color1: RGBA = (0.4, 0.9, 0.4, 1.0);
   color2: RGBA = (0.8, 0.8, 0.8, 1.0);
   color3: RGBA = (0.9, 0.9, 0.4, 1.0);

var
   d, point2, head: TVector2;
   callback: TEdgeShapesCallback;
   nextstep: Boolean;
begin
   nextstep := (not settings.pause) or (settings.singleStep);
   inherited;
   DrawText('Press 1-5 to drop stuff');

   SetValue(d, L * Cos(m_angle), -L * Abs(Sin(m_angle)));
   {$IFDEF OP_OVERLOAD}
   point2 := point1 + d;
   {$ELSE}
   point2 := Add(point1, d);
   {$ENDIF}

   callback := TEdgeShapesCallback.Create;
   m_world.RayCast(callback, point1, point2);

   if Assigned(callback.m_fixture) then
   begin
      m_debugDraw.DrawPoint(callback.m_point, 5.0, color1);
      m_debugDraw.DrawSegment(point1, callback.m_point, color2);
      {$IFDEF OP_OVERLOAD}
      head := callback.m_point + 0.5 * callback.m_normal;
      {$ELSE}
      head := Add(callback.m_point, Multiply(callback.m_normal, 0.5));
      {$ENDIF}
      m_debugDraw.DrawSegment(callback.m_point, head, color3);
   end
   else
      m_debugDraw.DrawSegment(point1, point2, color2);

   if nextstep then
      m_angle := m_angle + 0.25 * Pi / 180.0;
   callback.Free;
end;

procedure TEdgeShapes.Keyboard(key: Byte);
begin
    case key of
       Ord('1')..Ord('5'): CreateShape(key - Ord('1'));
       Ord('D'): DestroyBody;
    end;
end;

initialization
   RegisterTestEntry('Edge Shapes', TEdgeShapes);

end.

