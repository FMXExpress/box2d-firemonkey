unit UShapeEditing;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain,
   UPhysics2DTypes,
   UPhysics2D,
   SysUtils,
   OpenGL;

type
   TShapeEditing = class(TTester)
   public
      m_body: Tb2Body;
      m_fixture1, m_fixture2: Tb2Fixture;
      m_sensor: Boolean;

      constructor Create; override;

      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TDistanceTest }

constructor TShapeEditing.Create;
var
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   bd: Tb2BodyDef;
   ground: Tb2Body;
begin
   inherited;
   bd := Tb2BodyDef.Create;
   ground := m_world.CreateBody(bd);

   edge := Tb2EdgeShape.Create;
   edge.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
   ground.CreateFixture(edge, 0.0);

   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   SetValue(bd.position, 0.0, 10.0);
   m_body := m_world.CreateBody(bd);

   shape := Tb2PolygonShape.Create;
   shape.SetAsBox(1.0, 1.0, MakeVector(0.0, 0.0), 0.0);
   m_fixture1 := m_body.CreateFixture(shape, 10.0);
   m_fixture2 := nil;

   m_sensor := False;
end;

procedure TShapeEditing.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
	 DrawText('Press C create a shape, D destroy a shape and S to switch sensor.');
   if m_sensor then
      DrawText('sensor: YES')
   else
      DrawText('sensor: NO');
end;

procedure TShapeEditing.Keyboard(key: Byte);
var
   shape: Tb2CircleShape;
begin
   inherited;
   case key of
      Ord('C'):
         if not Assigned(m_fixture2) then
         begin
            shape := Tb2CircleShape.Create;
            shape.m_radius := 3.0;
            SetValue(shape.m_p, 0.5, -4.0);
            m_fixture2 := m_body.CreateFixture(shape, 10.0);
            m_body.SetAwake(True);
         end;
      Ord('D'):
         if Assigned(m_fixture2) then
         begin
            m_body.DestroyFixture(m_fixture2);
            m_fixture2 := nil;
            m_body.SetAwake(True);
         end;
      Ord('S'):
         if Assigned(m_fixture2) then
         begin
				    m_sensor := not m_sensor;
            m_fixture2.IsSensor := m_sensor;
         end;
   end;
end;

initialization
   RegisterTestEntry('Shape Editing', TShapeEditing);

end.

