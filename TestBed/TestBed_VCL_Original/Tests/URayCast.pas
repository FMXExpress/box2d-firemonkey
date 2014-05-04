unit URayCast;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

const
   e_maxBodies = 256;

type
   // This test demonstrates how to use the world ray-cast feature.
   TMode = (e_closest, e_any,	e_multiple);

   TRayCast = class(TTester)
   private
      procedure CreateBody(index: Int32);
      procedure DestroyBody;
   public
      m_bodyIndex: Int32;
      m_bodies: array[0..e_maxBodies - 1] of Tb2Body;
      m_userData: array[0..e_maxBodies - 1] of Int32;
      m_polygons: array[0..3] of Tb2PolygonShape;
      m_circle: Tb2CircleShape;

      m_angle: PhysicsFloat;
      m_mode: TMode;

      constructor Create; override;
      destructor Destroy; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

type
   // This callback finds the closest hit. Polygon 0 is filtered.
   TRayCastClosestCallback = class(Tb2RayCastCallback)
   public
      m_hit: Boolean;
      m_point, m_normal: TVector2;

      function ReportFixture(fixture:	Tb2Fixture; const point, normal: TVector2;
         fraction: PhysicsFloat): PhysicsFloat; override;
   end;

   // This callback finds any hit. Polygon 0 is filtered. For this type of query we are usually
   // just checking for obstruction, so the actual fixture and hit point are irrelevant.
   TRayCastAnyCallback = class(Tb2RayCastCallback)
   public
      m_hit: Boolean;
      m_point, m_normal: TVector2;

      function ReportFixture(fixture:	Tb2Fixture; const point, normal: TVector2;
         fraction: PhysicsFloat): PhysicsFloat; override;
   end;

const
   e_maxCount = 3;

type
   // This ray cast collects multiple hits along the ray. Polygon 0 is filtered.
   TRayCastMultipleCallback = class(Tb2RayCastCallback)
   public
      m_points, m_normals: array[0..e_maxCount - 1] of TVector2;
	    m_count: Int32;

      function ReportFixture(fixture:	Tb2Fixture; const point, normal: TVector2;
         fraction: PhysicsFloat): PhysicsFloat; override;
   end;

{ TRayCastClosestCallback }

function TRayCastClosestCallback.ReportFixture(fixture: Tb2Fixture; const point,
   normal: TVector2; fraction: PhysicsFloat): PhysicsFloat;
var
   userData: PInt32;
begin
   userData := PInt32(fixture.GetBody.UserData);
   if Assigned(userData) and (userData^ = 0) then
      // By returning -1, we instruct the calling code to ignore this fixture and
      // continue the ray-cast to the next fixture.
      Result := -1.0 // filter
   else
   begin
      m_hit := True;
      m_point := point;
      m_normal := normal;

      // By returning the current fraction, we instruct the calling code to clip the ray and
      // continue the ray-cast to the next fixture. WARNING: do not assume that fixtures
      // are reported in order. However, by clipping, we can always get the closest fixture.
      Result := fraction;
   end;
end;

{ TRayCastAnyCallback }

function TRayCastAnyCallback.ReportFixture(fixture: Tb2Fixture; const point,
   normal: TVector2; fraction: PhysicsFloat): PhysicsFloat;
var
   userData: PInt32;
begin
   userData := PInt32(fixture.GetBody.UserData);
   if Assigned(userData) and (userData^ = 0) then
      // By returning -1, we instruct the calling code to ignore this fixture
      // and continue the ray-cast to the next fixture.
      Result := -1.0 // filter
   else
   begin
      m_hit := True;
      m_point := point;
      m_normal := normal;
      // At this point we have a hit, so we know the ray is obstructed.
      // By returning 0, we instruct the calling code to terminate the ray-cast.
      Result := 0.0;
   end;
end;

{ TRayCastMultipleCallback }

// This ray cast collects multiple hits along the ray. Polygon 0 is filtered.
// The fixtures are not necessary reported in order, so we might not capture
// the closest fixture.

function TRayCastMultipleCallback.ReportFixture(fixture: Tb2Fixture;
   const point, normal: TVector2; fraction: PhysicsFloat): PhysicsFloat;
var
   userData: PInt32;
begin
   userData := PInt32(fixture.GetBody.UserData);
   if Assigned(userData) and (userData^ = 0) then
      // By returning -1, we instruct the calling code to ignore this fixture
      // and continue the ray-cast to the next fixture.
      Result := -1.0 // filter
   else
   begin
      //b2Assert(m_count < e_maxCount);
      m_points[m_count] := point;
      m_normals[m_count] := normal;
      Inc(m_count);

      if m_count = e_maxCount then
         // At this point the buffer is full.
         // By returning 0, we instruct the calling code to terminate the ray-cast.
         Result := 0.0
      else
         // By returning 1, we instruct the caller to continue without clipping the ray.
         Result := 1.0;
   end;
end;

{ TRayCast }

constructor TRayCast.Create;
var
   i: Integer;
   bd: Tb2BodyDef;
   ground: Tb2Body;
   shape: Tb2EdgeShape;
   vertices: Tb2PolyVertices;
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
   for i := 0 to e_maxBodies - 1 do
      m_bodies[i] := nil;

   m_angle := 0.0;
   m_mode := e_closest;
end;

destructor TRayCast.Destroy;
var
   i: Integer;
begin
   for i := 0 to 3 do
      m_polygons[i].Free;
   m_circle.Free;
   inherited;
end;

procedure TRayCast.CreateBody(index: Int32);
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
   SetValue(bd.position, RandomFloat(-10.0, 10.0), RandomFloat(0.0, 20.0));
   bd.angle := RandomFloat(-Pi, Pi);

   m_userData[m_bodyIndex] := index + 1;
   bd.userData := @m_userData[m_bodyIndex];

   if index = 4 then
      bd.angularDamping := 0.02;

   m_bodies[m_bodyIndex] := m_world.CreateBody(bd);
   fd := Tb2FixtureDef.Create;
   fd.friction := 0.3;
   if index < 4 then
      fd.shape := m_polygons[index]
   else
      fd.shape := m_circle;
   m_bodies[m_bodyIndex].CreateFixture(fd, True, False);

   m_bodyIndex := (m_bodyIndex + 1) mod e_maxBodies;
end;

procedure TRayCast.DestroyBody;
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

procedure TRayCast.Keyboard(key: Byte);
begin
   case key of
      Ord('1')..Ord('5'): CreateBody(key - Ord('1'));
      Ord('D'): DestroyBody;
      Ord('M'):
         if m_mode = e_closest then
            m_mode := e_any
         else if m_mode = e_any then
            m_mode := e_multiple
         else if m_mode = e_multiple then
            m_mode := e_closest;
   end;
end;

procedure TRayCast.Step(var settings: TSettings; timeStep: PhysicsFloat);
const
   L = 11.0;
   point1: TVector2 = (X: 0.0; Y: 10.0);
   color1: RGBA = (0.4, 0.9, 0.4, 1.0);
   color2: RGBA = (0.8, 0.8, 0.8, 1.0);
   color3: RGBA = (0.9, 0.9, 0.4, 1.0);
var
   i: Integer;
   d, point2, p, n: TVector2;
   callback1: TRayCastClosestCallback;
   callback2: TRayCastAnyCallback;
   callback3: TRayCastMultipleCallback;
   nextstep: Boolean;
begin
   nextstep := (not settings.pause) or (settings.singleStep);
   inherited;
   DrawText('Press 1-5 to drop stuff, M to change the mode');
   if m_mode = e_closest then
      DrawText('Ray-cast mode: any - check for obstruction')
   else if m_mode = e_any then
      DrawText('Ray-cast mode: multiple - gather multiple fixtures')
   else if m_mode = e_multiple then
      DrawText('Ray-cast mode: closest - find closest fixture along the ray');

   d.x := L * Cos(m_angle);
   d.y := L * Sin(m_angle);
   {$IFDEF OP_OVERLOAD}
   point2 := point1 + d;
   {$ELSE}
   point2 := Add(point1, d);
   {$ENDIF}

   if m_mode = e_closest then
   begin
      callback1 := TRayCastClosestCallback.Create;
      m_world.RayCast(callback1, point1, point2);

      if callback1.m_hit then
      begin
         m_debugDraw.DrawPoint(callback1.m_point, 5.0, color1);
         m_debugDraw.DrawSegment(point1, callback1.m_point, color2);
         {$IFDEF OP_OVERLOAD}
         m_debugDraw.DrawSegment(callback1.m_point, callback1.m_point +
            0.5 * callback1.m_normal, color3);
         {$ELSE}
         m_debugDraw.DrawSegment(callback1.m_point, Add(callback1.m_point,
            Multiply(callback1.m_normal, 0.5)), color3);
         {$ENDIF}
      end
      else
         m_debugDraw.DrawSegment(point1, point2, color2);
      callback1.Free;
   end
   else if m_mode = e_any then
   begin
      callback2 := TRayCastAnyCallback.Create;
      m_world.RayCast(callback2, point1, point2);

      if callback2.m_hit then
      begin
         m_debugDraw.DrawPoint(callback2.m_point, 5.0, color1);
         m_debugDraw.DrawSegment(point1, callback2.m_point, color2);
         {$IFDEF OP_OVERLOAD}
         m_debugDraw.DrawSegment(callback2.m_point, callback2.m_point +
            0.5 * callback2.m_normal, color3);
         {$ELSE}
         m_debugDraw.DrawSegment(callback2.m_point, Add(callback2.m_point,
            Multiply(callback2.m_normal, 0.5)), color3);
         {$ENDIF}
      end
      else
         m_debugDraw.DrawSegment(point1, point2, color2);
      callback2.Free;
   end
   else if m_mode = e_multiple then
   begin
      callback3 := TRayCastMultipleCallback.Create;
      m_world.RayCast(callback3, point1, point2);
      m_debugDraw.DrawSegment(point1, point2, color2);

      for i := 0 to callback3.m_count - 1 do
      begin
         p := callback3.m_points[i];
         n := callback3.m_normals[i];
         m_debugDraw.DrawPoint(p, 5.0, color1);
         m_debugDraw.DrawSegment(point1, p, color2);
         {$IFDEF OP_OVERLOAD}
         m_debugDraw.DrawSegment(p, p + 0.5 * n, color3);
         {$ELSE}
         m_debugDraw.DrawSegment(p, Add(p, Multiply(n, 0.5)), color3);
         {$ENDIF}
      end;
      callback3.Free;
   end;

   if nextstep then
      m_angle := m_angle + 0.25 * Pi / 180.0;
end;

initialization
   RegisterTestEntry('RayCast', TRayCast);

end.


