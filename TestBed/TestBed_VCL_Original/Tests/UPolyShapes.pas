unit UPolyShapes;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils, Math;

const k_maxBodies = 256;

type
   TPolyShapes = class(TTester)
   private
      procedure CreateBody(index: Int32);
      procedure DestroyBody;
   public
      m_bodyIndex: Int32;
      m_bodies: array[0..k_maxBodies - 1] of Tb2Body;
      m_polygons: array[0..3] of Tb2PolygonShape;
      m_circle: Tb2CircleShape;

      constructor Create; override;
      destructor Destroy; override;

      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

/// This tests stacking. It also shows how to use b2World::Query
/// and b2TestOverlap.

/// This callback is called by b2World::QueryAABB. We find all the fixtures
/// that overlap an AABB. Of those, we use b2TestOverlap to determine which fixtures
/// overlap a circle. Up to 4 overlapped fixtures will be highlighted with a yellow border.
const
   e_maxCount = 4;
type
   TPolyShapesCallback = class(Tb2QueryCallback)
   public
      m_circle: Tb2CircleShape;
      m_transform: Tb2Transform;
      m_debugDraw: Tb2Draw;
      m_count: Int32;

      constructor Create;
      destructor Destroy; override;
      procedure DrawFixture(fixture: Tb2Fixture);
	    /// Called for each fixture found in the query AABB.
	    /// @return false to terminate the query.
	    function ReportFixture(fixture: Tb2Fixture): Boolean; override;
   end;

constructor TPolyShapesCallback.Create;
begin
   m_circle := Tb2CircleShape.Create;
   m_count := 0;
end;

destructor TPolyShapesCallback.Destroy;
begin
   m_circle.Free;
end;

procedure TPolyShapesCallback.DrawFixture(fixture: Tb2Fixture);
const
   color: RGBA = (0.95, 0.95, 0.6, 1.0);
var
   i: Integer;
   xf: Pb2Transform;
   vertices: Tb2PolyVertices;
begin
   xf := fixture.GetBody.GetTransform;
   case fixture.GetType of
      e_circleShape:
         with Tb2CircleShape(fixture.GetShape) do
            m_debugDraw.DrawCircle(b2Mul(xf^, m_p), m_radius, color);
      e_polygonShape:
         with Tb2PolygonShape(fixture.GetShape) do
         begin
            //b2Assert(vertexCount <= b2_maxPolygonVertices);
            for i := 0 to m_count - 1 do
              vertices[i] := b2Mul(xf^, m_vertices[i]);
            m_debugDraw.DrawPolygon(vertices, m_count, color);
         end;
   end;
end;

function TPolyShapesCallback.ReportFixture(fixture: Tb2Fixture): Boolean;
begin
   if m_count = e_maxCount then
   begin
      Result := False;
      Exit;
   end;

   if b2TestOverlap(fixture.GetShape, m_circle, 0, 0, fixture.GetBody.GetTransform^, m_transform) then
   begin
      DrawFixture(fixture);
      Inc(m_count);
   end;

   Result := True;
end;

constructor TPolyShapes.Create;
var
   i: Integer;
   bd: Tb2BodyDef;
   ground: Tb2Body;
   shape: Tb2EdgeShape;
   vertices: array[0..7] of TVector2;
   w, b, s: PhysicsFloat;
begin
   inherited;
   // Ground body
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      shape := Tb2EdgeShape.Create;
      shape.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
      ground.CreateFixture(shape, 0.0);
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
   for i := 0 to k_maxBodies - 1 do
      m_bodies[i] := nil;
end;

destructor TPolyShapes.Destroy;
begin
   m_polygons[0].Free;
   m_polygons[1].Free;
   m_polygons[2].Free;
   m_polygons[3].Free;
   m_circle.Free;
   inherited;
end;

procedure TPolyShapes.Step(var settings: TSettings; timeStep: PhysicsFloat);
const
   color: RGBA = (0.4, 0.7, 0.8, 1.0);
var
   callback: TPolyShapesCallback;
   aabb: Tb2AABB;
begin
   inherited;
   callback := TPolyShapesCallback.Create;
   callback.m_circle.m_radius := 2.0;
   SetValue(callback.m_circle.m_p, 0.0, 1.1);
   {$IFDEF OP_OVERLOAD}
   callback.m_transform.SetIdentity;
   {$ELSE}
   SetIdentity(callback.m_transform);
   {$ENDIF}
   callback.m_debugDraw := m_debugDraw;

   callback.m_circle.ComputeAABB(aabb, callback.m_transform, 0);
   m_world.QueryAABB(callback, aabb);

   m_debugDraw.DrawCircle(callback.m_circle.m_p, callback.m_circle.m_radius, color);
   DrawText('Press 1-5 to drop stuff.');
   DrawText('Press ''a'' to (de)activate some bodies.');
   DrawText('Press ''d'' to destroy a body.');
   callback.Free;
end;

procedure TPolyShapes.CreateBody(index: Int32);
var
   bd: Tb2BodyDef;
   fd: Tb2FixtureDef;
begin
   if Assigned(m_bodies[m_bodyIndex]) then
   begin
      m_world.DestroyBody(m_bodies[m_bodyIndex]);
      m_bodies[m_bodyIndex] := nil;
   end;

   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;

   SetValue(bd.position, RandomFloat(-2.0, 2.0), 10.0);
   bd.angle := RandomFloat(-Pi, Pi);

   if index = 4 then
      bd.angularDamping := 0.02;

   m_bodies[m_bodyIndex] := m_world.CreateBody(bd);

   fd := Tb2FixtureDef.Create;
   fd.density := 1.0;
   fd.friction := 0.3;
   if index < 4 then
   begin
       fd.shape := m_polygons[index];
       m_bodies[m_bodyIndex].CreateFixture(fd, True, False);
   end
   else
   begin
      fd.shape := m_circle;
      m_bodies[m_bodyIndex].CreateFixture(fd, True, False);
   end;

   m_bodyIndex := (m_bodyIndex + 1) mod k_maxBodies;
end;

procedure TPolyShapes.DestroyBody;
var
   i: Integer;
begin
   for i := 0 to k_maxBodies - 1 do
      if Assigned(m_bodies[i]) then
      begin
         m_world.DestroyBody(m_bodies[i]);
         m_bodies[i] := nil;
         Exit;
      end;
end;

procedure TPolyShapes.Keyboard(key: Byte);
var
   i: Integer;
begin
   case key of
      Ord('1')..Ord('5'): CreateBody(key - Ord('1'));
      Ord('A'):
         begin
            i := 0;
            while (i < k_maxBodies) do
            begin
		        	 if Assigned(m_bodies[i]) then
                  m_bodies[i].SetActive(not m_bodies[i].IsActive);
               Inc(i, 2);
            end;
         end;
      Ord('D'): DestroyBody;
   end;
end;

initialization
   RegisterTestEntry('Poly Shapes', TPolyShapes);

end.


