unit UTheoJansen;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TJansenWalker = class(TTester)
   private
      procedure CreateLeg(s: PhysicsFloat; const wheelAnchor: TVector2);
   public
      m_offset: TVector2;
      m_chassis, m_wheel: Tb2Body;
      m_motorJoint: Tb2RevoluteJoint;
      m_motorOn: Boolean;
      m_motorSpeed: PhysicsFloat;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TJansenWalker }

constructor TJansenWalker.Create;
const
   pivot: TVector2 = (X: 0.0; Y: 0.8);
var
   i: Integer;
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   cshape: Tb2CircleShape;
   bd: Tb2BodyDef;
   sd: Tb2FixtureDef;
   ground, body: Tb2Body;
   jd: Tb2RevoluteJointDef;
   wheelAnchor: TVector2;
begin
   inherited;
   SetValue(m_offset, 0.0, 8.0);
   m_motorSpeed := 2.0;
   m_motorOn := True;

   // Ground
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      edge := Tb2EdgeShape.Create;
      edge.SetVertices(MakeVector(-50.0, 0.0), MakeVector(50.0, 0.0));
      ground.CreateFixture(edge, 0, False, False);

      edge.SetVertices(MakeVector(-50.0, 0.0), MakeVector(-50.0, 10.0));
      ground.CreateFixture(edge, 0, False, False);

      edge.SetVertices(MakeVector(50.0, 0.0), MakeVector(50.0, 10.0));
      ground.CreateFixture(edge, 0.0);
   end;

   // Balls
   cshape := Tb2CircleShape.Create;
   cshape.m_radius := 0.25;
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   for i := 0 to 39 do
   begin
			SetValue(bd.position, -40.0 + 2.0 * i, 0.5);
			body := m_world.CreateBody(bd, False);
			body.CreateFixture(cshape, 1.0, False);
   end;
   cshape.Free;
   bd.Free;

   // Chassis
   begin
			shape := Tb2PolygonShape.Create;
			shape.SetAsBox(2.5, 1.0);

			sd := Tb2FixtureDef.Create;
			sd.density := 1.0;
			sd.shape := shape;
			sd.filter.groupIndex := -1;
      bd := Tb2BodyDef.Create;
			bd.bodyType := b2_dynamicBody;
      {$IFDEF OP_OVERLOAD}
			bd.position := pivot + m_offset;
      {$ELSE}
      bd.position := Add(pivot, m_offset);
      {$ENDIF}
			m_chassis := m_world.CreateBody(bd);
			m_chassis.CreateFixture(sd);
   end;

   begin
			cshape := Tb2CircleShape.Create;
			cshape.m_radius := 1.6;

			sd := Tb2FixtureDef.Create;
			sd.density := 1.0;
			sd.shape := cshape;
			sd.filter.groupIndex := -1;
			bd := Tb2BodyDef.Create;
			bd.bodyType := b2_dynamicBody;
      {$IFDEF OP_OVERLOAD}
			bd.position := pivot + m_offset;
      {$ELSE}
      bd.position := Add(pivot, m_offset);
      {$ENDIF}
			m_wheel := m_world.CreateBody(bd);
			m_wheel.CreateFixture(sd);
   end;

   begin
      jd := Tb2RevoluteJointDef.Create;
      {$IFDEF OP_OVERLOAD}
      jd.Initialize(m_wheel, m_chassis, pivot + m_offset);
      {$ELSE}
      jd.Initialize(m_wheel, m_chassis, Add(pivot, m_offset));
      {$ENDIF}
      jd.collideConnected := False;
      jd.motorSpeed := m_motorSpeed;
      jd.maxMotorTorque := 400.0;
      jd.enableMotor := m_motorOn;
      m_motorJoint := Tb2RevoluteJoint(m_world.CreateJoint(jd));
   end;

   {$IFDEF OP_OVERLOAD}
   wheelAnchor := pivot + MakeVector(0.0, -0.8);
   {$ELSE}
   wheelAnchor := Add(pivot, MakeVector(0.0, -0.8));
   {$ENDIF}

   CreateLeg(-1.0, wheelAnchor);
   CreateLeg(1.0, wheelAnchor);

   m_wheel.SetTransform(m_wheel.GetPosition, 120.0 * Pi / 180.0);
   CreateLeg(-1.0, wheelAnchor);
   CreateLeg(1.0, wheelAnchor);

   m_wheel.SetTransform(m_wheel.GetPosition, -120.0 * Pi / 180.0);
   CreateLeg(-1.0, wheelAnchor);
   CreateLeg(1.0, wheelAnchor);
end;

procedure TJansenWalker.CreateLeg(s: PhysicsFloat; const wheelAnchor: TVector2);
var
   p1, p2, p3, p4, p5, p6: TVector2;
   vertices: array[0..2] of TVector2;
   poly1, poly2: Tb2PolygonShape;
   fd1, fd2: Tb2FixtureDef;
   bd1, bd2: Tb2BodyDef;
   body1, body2: Tb2Body;
   djd: Tb2DistanceJointDef;
   rjd: Tb2RevoluteJointDef;
begin
   p1 := MakeVector(5.4 * s, -6.1);
   p2 := MakeVector(7.2 * s, -1.2);
   p3 := MakeVector(4.3 * s, -1.9);
   p4 := MakeVector(3.1 * s, 0.8);
   p5 := MakeVector(6.0 * s, 1.5);
   p6 := MakeVector(2.5 * s, 3.7);

   fd1 := Tb2FixtureDef.Create;
   fd2 := Tb2FixtureDef.Create;
   fd1.filter.groupIndex := -1;
   fd2.filter.groupIndex := -1;
   fd1.density := 1.0;
   fd2.density := 1.0;
   poly1 := Tb2PolygonShape.Create;
   poly2 := Tb2PolygonShape.Create;

   if s > 0.0 then
   begin
      vertices[0] := p1;
      vertices[1] := p2;
      vertices[2] := p3;
      poly1.SetVertices(@vertices[0], 3);

      vertices[0] := b2Vec2_zero;
      {$IFDEF OP_OVERLOAD}
      vertices[1] := p5 - p4;
      vertices[2] := p6 - p4;
      {$ELSE}
      vertices[1] := Subtract(p5, p4);
      vertices[2] := Subtract(p6, p4);
      {$ENDIF}
      poly2.SetVertices(@vertices[0], 3);
   end
   else
   begin
      vertices[0] := p1;
      vertices[1] := p3;
      vertices[2] := p2;
      poly1.SetVertices(@vertices[0], 3);

      vertices[0] := b2Vec2_zero;
      {$IFDEF OP_OVERLOAD}
      vertices[1] := p6 - p4;
      vertices[2] := p5 - p4;
      {$ELSE}
      vertices[1] := Subtract(p6, p4);
      vertices[2] := Subtract(p5, p4);
      {$ENDIF}
      poly2.SetVertices(@vertices[0], 3);
   end;

   fd1.shape := poly1;
   fd2.shape := poly2;

   bd1 := Tb2BodyDef.Create;
   bd1.bodyType := b2_dynamicBody;
   bd1.position := m_offset;
   bd2 := Tb2BodyDef.Create;
   bd2.bodyType := b2_dynamicBody;
   {$IFDEF OP_OVERLOAD}
   bd2.position := p4 + m_offset;
   {$ELSE}
   bd2.position := Add(p4, m_offset);
   {$ENDIF}

   bd1.angularDamping := 10.0;
   bd2.angularDamping := 10.0;

   body1 := m_world.CreateBody(bd1);
   body2 := m_world.CreateBody(bd2);

   body1.CreateFixture(fd1);
   body2.CreateFixture(fd2);

   djd := Tb2DistanceJointDef.Create;
   // Using a soft distance constraint can reduce some jitter.
   // It also makes the structure seem a bit more fluid by
   // acting like a suspension system.
   djd.dampingRatio := 0.5;
   djd.frequencyHz := 10.0;

   {$IFDEF OP_OVERLOAD}
   djd.Initialize(body1, body2, p2 + m_offset, p5 + m_offset);
   {$ELSE}
   djd.Initialize(body1, body2, Add(p2, m_offset), Add(p5, m_offset));
   {$ENDIF}
   m_world.CreateJoint(djd, False);

   {$IFDEF OP_OVERLOAD}
   djd.Initialize(body1, body2, p3 + m_offset, p4 + m_offset);
   {$ELSE}
   djd.Initialize(body1, body2, Add(p3, m_offset), Add(p4, m_offset));
   {$ENDIF}
   m_world.CreateJoint(djd, False);

   {$IFDEF OP_OVERLOAD}
   djd.Initialize(body1, m_wheel, p3 + m_offset, wheelAnchor + m_offset);
   {$ELSE}
   djd.Initialize(body1, m_wheel, Add(p3, m_offset), Add(wheelAnchor, m_offset));
   {$ENDIF}
   m_world.CreateJoint(djd, False);

   {$IFDEF OP_OVERLOAD}
   djd.Initialize(body2, m_wheel, p6 + m_offset, wheelAnchor + m_offset);
   {$ELSE}
   djd.Initialize(body2, m_wheel, Add(p6, m_offset), Add(wheelAnchor, m_offset));
   {$ENDIF}
   m_world.CreateJoint(djd);

   rjd := Tb2RevoluteJointDef.Create;
   {$IFDEF OP_OVERLOAD}
   rjd.Initialize(body2, m_chassis, p4 + m_offset);
   {$ELSE}
   rjd.Initialize(body2, m_chassis, Add(p4, m_offset));
   {$ENDIF}
   m_world.CreateJoint(rjd);
end;

procedure TJansenWalker.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
	 DrawText('Keys: left = A, brake = S, right = D, toggle motor = M');
end;

procedure TJansenWalker.Keyboard(key: Byte);
begin
   case key of
      Ord('A'): m_motorJoint.SetMotorSpeed(-m_motorSpeed);
      Ord('S'): m_motorJoint.SetMotorSpeed(0.0);
      Ord('D'): m_motorJoint.SetMotorSpeed(m_motorSpeed);
      Ord('M'): m_motorJoint.EnableMotor(not m_motorJoint.IsMotorEnabled);
   end;
end;

initialization
   RegisterTestEntry('Theo Jansen''s Walker', TJansenWalker);

end.

