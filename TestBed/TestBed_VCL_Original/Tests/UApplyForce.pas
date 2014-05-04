unit UApplyForce;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TApplyForce = class(TTester)
   public
      m_body: Tb2Body;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TApplyForce }

constructor TApplyForce.Create;
const
   k_restitution = 0.4;
   gravity = 10;
var
   i: Integer;
   ground, body: Tb2Body;
   bd: Tb2BodyDef;
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   poly1, poly2: Tb2PolygonShape;
   sd, sd1, sd2, fd: Tb2FixtureDef;
   xf1, xf2: Tb2Transform;
   vertices: array[0..2] of TVector2;
   mass, inertia, radius: PhysicsFloat;
   jd: Tb2FrictionJointDef;
begin
   inherited;
   m_world.SetGravity(b2Vec2_Zero);
   UpdateGravityText;

   begin
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 0.0, 20.0);
      ground := m_world.CreateBody(bd);

      edge := Tb2EdgeShape.Create;
      sd := Tb2FixtureDef.Create;

      sd.shape := edge;
      sd.density := 0.0;
      sd.restitution := k_restitution;

      // Left vertical
      edge.SetVertices(MakeVector(-20.0, -20.0), MakeVector(-20.0, 20.0));
      ground.CreateFixture(sd, False, False, False); // do not free sd, do not free shape, do not update mass

      // Right vertical
      edge.SetVertices(MakeVector(20.0, -20.0), MakeVector(20.0, 20.0));
      ground.CreateFixture(sd, False, False, False);

      // Top horizontal
      edge.SetVertices(MakeVector(-20.0, 20.0), MakeVector(20.0, 20.0));
      ground.CreateFixture(sd, False, False, False);

      // Bottom horizontal
      edge.SetVertices(MakeVector(-20.0, -20.0), MakeVector(20.0, -20.0));
      ground.CreateFixture(sd); // free sd and shape
   end;

   begin
      {$IFDEF OP_OVERLOAD}
      xf1.q.SetAngle(0.3524 * Pi);
      xf1.p := xf1.q.GetXAxis;
      {$ELSE}
      SetAngle(xf1.q, 0.3524 * Pi);
      xf1.p := GetXAxis(xf1.q);
      {$ENDIF}

      vertices[0] := b2Mul(xf1, MakeVector(-1.0, 0.0));
      vertices[1] := b2Mul(xf1, MakeVector(1.0, 0.0));
      vertices[2] := b2Mul(xf1, MakeVector(0.0, 0.5));

      poly1 := Tb2PolygonShape.Create;
      poly1.SetVertices(@vertices[0], 3);

      sd1 := Tb2FixtureDef.Create;
      sd1.shape := poly1;
      sd1.density := 4.0;

      {$IFDEF OP_OVERLOAD}
      xf2.q.SetAngle(-0.3524 * Pi);
      xf2.p := -xf2.q.GetXAxis;
      {$ELSE}
      SetAngle(xf2.q, -0.3524 * Pi);
      xf2.p := Negative(GetXAxis(xf2.q));
      {$ENDIF}

      vertices[0] := b2Mul(xf2, MakeVector(-1.0, 0.0));
      vertices[1] := b2Mul(xf2, MakeVector(1.0, 0.0));
      vertices[2] := b2Mul(xf2, MakeVector(0.0, 0.5));

      poly2 := Tb2PolygonShape.Create;
      poly2.SetVertices(@vertices[0], 3);

      sd2 := Tb2FixtureDef.Create;
      sd2.shape := poly2;
      sd2.density := 2.0;

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      bd.angularDamping := 5.0;
      bd.linearDamping := 0.1;

      SetValue(bd.position, 0.0, 2.0);
      bd.angle := Pi;
      bd.allowSleep := False;
      m_body := m_world.CreateBody(bd);
      m_body.CreateFixture(sd1, True, True, False);
      m_body.CreateFixture(sd2);
   end;

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.5, 0.5);

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.density := 1.0;
      fd.friction := 0.3;

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;

      jd := Tb2FrictionJointDef.Create;
      jd.localAnchorA := b2Vec2_Zero;
      jd.localAnchorB := b2Vec2_Zero;
      jd.bodyA := ground;
      jd.collideConnected := True;

      for i := 0 to 9 do
      begin
         SetValue(bd.position, 0.0, 5.0 + 1.54 * i);
         body := m_world.CreateBody(bd, False);
         body.CreateFixture(fd, False, False);

         inertia := body.GetInertia;
         mass := body.GetMass;

         // For a circle: I := 0.5 * m * r * r ==> r := sqrt(2 * I / m)
         radius := Sqrt(2.0 * inertia / mass);

         jd.bodyB := body;
         jd.maxForce := mass * gravity;
         jd.maxTorque := mass * radius * gravity;
         m_world.CreateJoint(jd, False);
      end;
      shape.Free;
      jd.Free;
      bd.Free;
      fd.Free;
   end;
end;

procedure TApplyForce.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Use W\A\D to control the plane.');
end;

procedure TApplyForce.Keyboard(key: Byte);
var
   f, p: TVector2;
begin
   case key of
      Ord('W'):
         begin
            f := m_body.GetWorldVector(MakeVector(0.0, -70.0));
            p := m_body.GetWorldPoint(MakeVector(0.0, 2.0));
            m_body.ApplyForce(f, p);
         end;
      Ord('A'): m_body.ApplyTorque(50.0);
      Ord('D'): m_body.ApplyTorque(-50.0);
   end;
end;

initialization
   RegisterTestEntry('Apply Force', TApplyForce);

end.

