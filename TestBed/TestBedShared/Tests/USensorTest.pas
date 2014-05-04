unit USensorTest;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   uTestBed,
   UPhysics2DTypes, UPhysics2D, SysUtils;

const
   e_count = 7;

type
   TSensorTest = class(TTester)
   private
      procedure BeginContact(var contact: Tb2Contact); override;
      procedure EndContact(var contact: Tb2Contact); override;
   public
	    m_sensor: Tb2Fixture;
      m_bodies: array[0..e_count - 1] of Tb2Body;
      m_touching: array[0..e_count - 1] of Boolean;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;

implementation

{ TSensorTest }

constructor TSensorTest.Create;
var
   i: Integer;
   bd: Tb2BodyDef;
   shape: Tb2EdgeShape;
   fd: Tb2FixtureDef;
   cshape: Tb2CircleShape;
   ground: Tb2Body;
begin
   inherited;
   bd := Tb2BodyDef.Create;
   ground := m_world.CreateBody(bd);

   shape := Tb2EdgeShape.Create;
   shape.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
   ground.CreateFixture(shape, 0.0);

   cshape := Tb2CircleShape.Create;
   cshape.m_radius := 5.0;
   SetValue(cshape.m_p, 0.0, 10.0);

   fd := Tb2FixtureDef.Create;
   fd.shape := cshape;
   fd.isSensor := True;
   m_sensor := ground.CreateFixture(fd);

   cshape := Tb2CircleShape.Create;
   cshape.m_radius := 1.0;
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   for i := 0 to e_count - 1 do
   begin
      SetValue(bd.position, -10.0 + 3.0 * i, 20.0);
      bd.userData := @m_touching[i];

      m_touching[i] := False;
      m_bodies[i] := m_world.CreateBody(bd, False);
      m_bodies[i].CreateFixture(cshape, 1.0, False);
   end;
   cshape.Free;
   bd.Free;
end;

procedure TSensorTest.BeginContact(var contact: Tb2Contact);
var
   userData: PBoolean;
begin
   with contact do
   begin
      if m_fixtureA = m_sensor then
      begin
         userData := m_fixtureB.GetBody.UserData;
         if Assigned(userData) then
            userData^ := True;
      end;

      if m_fixtureB = m_sensor then
      begin
         userData := m_fixtureA.GetBody.UserData;
         if Assigned(userData) then
            userData^ := True;
      end;
   end;
end;

procedure TSensorTest.EndContact(var contact: Tb2Contact);
var
   userData: PBoolean;
begin
   with contact do
   begin
      if m_fixtureA = m_sensor then
      begin
         userData := m_fixtureB.GetBody.UserData;
         if Assigned(userData) then
            userData^ := False;
      end;

      if m_fixtureB = m_sensor then
      begin
         userData := m_fixtureA.GetBody.UserData;
         if Assigned(userData) then
            userData^ := False;
      end;
   end;
end;

procedure TSensorTest.Step(var settings: TSettings; timeStep: PhysicsFloat);
var
   i: Integer;
   body, ground: Tb2Body;
   circle: Tb2CircleShape;
   center, d: TVector2;
begin
   inherited;

   // Traverse the contact results. Apply a force on shapes that overlap the sensor.
   for i := 0 to e_count - 1 do
      with m_points[i] do
      begin
         if not m_touching[i] then
            Continue;

         body := m_bodies[i];
         ground := m_sensor.GetBody;

         circle := Tb2CircleShape(m_sensor.GetShape);
         center := ground.GetWorldPoint(circle.m_p);

         {$IFDEF OP_OVERLOAD}
         d := center - body.GetPosition;
         if d.SqrLength < FLT_EPSILON * FLT_EPSILON then
            Continue;
         d.Normalize;
         body.ApplyForce(100 * d, body.GetPosition, True);
         {$ELSE}
         d := Subtract(center, body.GetPosition);
         if SqrLength(d) < FLT_EPSILON * FLT_EPSILON then
            Continue;
         Normalize(d);
         body.ApplyForce(Multiply(d, 100), body.GetPosition, True);
         {$ENDIF}
      end;
end;

initialization
   RegisterTestEntry('Sensor Test', TSensorTest);

end.

