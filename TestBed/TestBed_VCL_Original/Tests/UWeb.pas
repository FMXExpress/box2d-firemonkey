unit UWeb;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TWeb = class(TTester)
   public
      m_bodies: array[0..3] of Tb2Body;
      m_joints: array[0..7] of Tb2Joint;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
      procedure JointDestroyed(joint: Tb2Joint); override;
   end;

implementation

{ TWeb }

constructor TWeb.Create;
var
   ground: Tb2Body;
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   bd: Tb2BodyDef;
   jd: Tb2DistanceJointDef;
   p1, p2, d: TVector2;
begin
   inherited;
   begin
			bd := Tb2BodyDef.Create;
			ground := m_world.CreateBody(bd);

			edge := Tb2EdgeShape.Create;
			edge.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
			ground.CreateFixture(edge, 0.0);
   end;

   begin
			shape := Tb2PolygonShape.Create;
			shape.SetAsBox(0.5, 0.5);

			bd := Tb2BodyDef.Create;
			bd.bodyType := b2_dynamicBody;

      SetValue(bd.position, -5.0, 5.0);
      m_bodies[0] := m_world.CreateBody(bd, False);
      m_bodies[0].CreateFixture(shape, 0, False);

      SetValue(bd.position, 5.0, 5.0);
      m_bodies[1] := m_world.CreateBody(bd, False);
      m_bodies[1].CreateFixture(shape, 0, False);

      SetValue(bd.position, 5.0, 15.0);
      m_bodies[2] := m_world.CreateBody(bd, False);
      m_bodies[2].CreateFixture(shape, 0, False);

      SetValue(bd.position, -5.0, 15.0);
      m_bodies[3] := m_world.CreateBody(bd);
      m_bodies[3].CreateFixture(shape, 0.0);

      jd := Tb2DistanceJointDef.Create;

      jd.frequencyHz := 2.0;
      jd.dampingRatio := 0.0;

      jd.bodyA := ground;
      jd.bodyB := m_bodies[0];
      SetValue(jd.localAnchorA, -10.0, 0.0);
      SetValue(jd.localAnchorB, -0.5, -0.5);
      p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
      p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
      {$IFDEF OP_OVERLOAD}
      d := p2 - p1;
      jd.length := d.Length;
      {$ELSE}
      d := Subtract(p2, p1);
      jd.length := LengthVec(d);
      {$ENDIF}
      m_joints[0] := m_world.CreateJoint(jd, False);

      jd.bodyA := ground;
      jd.bodyB := m_bodies[1];
      SetValue(jd.localAnchorA, 10.0, 0.0);
      SetValue(jd.localAnchorB, 0.5, -0.5);
      p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
      p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
      {$IFDEF OP_OVERLOAD}
      d := p2 - p1;
      jd.length := d.Length;
      {$ELSE}
      d := Subtract(p2, p1);
      jd.length := LengthVec(d);
      {$ENDIF}
      m_joints[1] := m_world.CreateJoint(jd, False);

      jd.bodyA := ground;
      jd.bodyB := m_bodies[2];
      SetValue(jd.localAnchorA, 10.0, 20.0);
      SetValue(jd.localAnchorB, 0.5, 0.5);
      p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
      p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
      {$IFDEF OP_OVERLOAD}
      d := p2 - p1;
      jd.length := d.Length;
      {$ELSE}
      d := Subtract(p2, p1);
      jd.length := LengthVec(d);
      {$ENDIF}
      m_joints[2] := m_world.CreateJoint(jd, False);

      jd.bodyA := ground;
      jd.bodyB := m_bodies[3];
      SetValue(jd.localAnchorA, -10.0, 20.0);
      SetValue(jd.localAnchorB, -0.5, 0.5);
      p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
      p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
      {$IFDEF OP_OVERLOAD}
      d := p2 - p1;
      jd.length := d.Length;
      {$ELSE}
      d := Subtract(p2, p1);
      jd.length := LengthVec(d);
      {$ENDIF}
      m_joints[3] := m_world.CreateJoint(jd, False);

      jd.bodyA := m_bodies[0];
      jd.bodyB := m_bodies[1];
      SetValue(jd.localAnchorA, 0.5, 0.0);
      SetValue(jd.localAnchorB, -0.5, 0.0);
      p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
      p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
      {$IFDEF OP_OVERLOAD}
      d := p2 - p1;
      jd.length := d.Length;
      {$ELSE}
      d := Subtract(p2, p1);
      jd.length := LengthVec(d);
      {$ENDIF}
      m_joints[4] := m_world.CreateJoint(jd, False);

      jd.bodyA := m_bodies[1];
      jd.bodyB := m_bodies[2];
      SetValue(jd.localAnchorA, 0.0, 0.5);
      SetValue(jd.localAnchorB, 0.0, -0.5);
      p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
      p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
      {$IFDEF OP_OVERLOAD}
      d := p2 - p1;
      jd.length := d.Length;
      {$ELSE}
      d := Subtract(p2, p1);
      jd.length := LengthVec(d);
      {$ENDIF}
      m_joints[5] := m_world.CreateJoint(jd, False);

      jd.bodyA := m_bodies[2];
      jd.bodyB := m_bodies[3];
      SetValue(jd.localAnchorA, -0.5, 0.0);
      SetValue(jd.localAnchorB, 0.5, 0.0);
      p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
      p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
      {$IFDEF OP_OVERLOAD}
      d := p2 - p1;
      jd.length := d.Length;
      {$ELSE}
      d := Subtract(p2, p1);
      jd.length := LengthVec(d);
      {$ENDIF}
      m_joints[6] := m_world.CreateJoint(jd, False);

      jd.bodyA := m_bodies[3];
      jd.bodyB := m_bodies[0];
      SetValue(jd.localAnchorA, 0.0, -0.5);
      SetValue(jd.localAnchorB, 0.0, 0.5);
      p1 := jd.bodyA.GetWorldPoint(jd.localAnchorA);
      p2 := jd.bodyB.GetWorldPoint(jd.localAnchorB);
      {$IFDEF OP_OVERLOAD}
      d := p2 - p1;
      jd.length := d.Length;
      {$ELSE}
      d := Subtract(p2, p1);
      jd.length := LengthVec(d);
      {$ENDIF}
      m_joints[7] := m_world.CreateJoint(jd);
   end;
end;

procedure TWeb.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
	 DrawText('This demonstrates a soft distance joint.');
	 DrawText('Press: (b) to delete a body, (j) to delete a joint');
end;

procedure TWeb.Keyboard(key: Byte);
var
   i: Integer;
begin
   case key of
      66{B}:
         for i := 0 to 3 do
           if Assigned(m_bodies[i]) then
           begin
              m_world.DestroyBody(m_bodies[i]);
              m_bodies[i] := nil;
              Break;
           end;
      74{J}:
         for i := 0 to 7 do
           if Assigned(m_joints[i]) then
           begin
              m_world.DestroyJoint(m_joints[i]);
              m_joints[i] := nil;
              Break;
           end;
   end;
end;

procedure TWeb.JointDestroyed(joint: Tb2Joint);
var
   i: Integer;
begin
   for i := 0 to 7 do
     if m_joints[i] = joint then
     begin
        m_joints[i] := nil;
        Break;
     end;
end;

initialization
   RegisterTestEntry('Web', TWeb);

end.

