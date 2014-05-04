unit UElasticBody;

interface
{$I ..\..\..\Source\Physics2D.inc}

uses
   UMain,
   UPhysics2DTypes,
   UPhysics2D,
   SysUtils;

type
   TElasticBody = class(TTester)
   private
      procedure Process;
      procedure AddSpringForce(bA: Tb2Body; const localA: TVector2;
         bB: Tb2Body; const localB: TVector2; const k, friction, desiredDist: Float);
   public
      bodies: array[0..63] of Tb2Body;
      m_ground: Tb2Body;
      m_elev: Tb2Body;
      m_joint_elev: Tb2PrismaticJoint;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: Float); override;
   end;

implementation

{ TElasticBody }

constructor TElasticBody.Create;
const
   startpoint: TVector2 = (X: 30.0; Y: 20.0);
var
   i, j: Integer;
   sd: Tb2PolygonDef;
   cd: Tb2CircleDef;
   bd: Tb2BodyDef;
   g, body, rightmotor, hammerleft, hammerright, pusher: Tb2Body;
   jr: Tb2RevoluteJointDef;
   jd: Tb2DistanceJointDef;
   jp: Tb2PrismaticJointDef;
begin
   inherited;
   /// Bottom static body
   begin
      sd := Tb2PolygonDef.Create;
      sd.SetAsBox(50.0, 2.0);
      sd.friction := 0.1;
      sd.restitution := 0.1;
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, -1.0, -7.5);
      m_ground := m_world.CreateBody(bd);
      m_ground.CreateShape(sd);
   end;
   /// Upper static body
   begin
      sd := Tb2PolygonDef.Create;
      sd.SetAsBox(20.0, 0.50, MakeVector(0.0, 0.0), 0.047 * Pi);
      sd.friction := 0.01;
      sd.restitution := 0.001;
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, -20.0, 93.0);
      g := m_world.CreateBody(bd);
      g.CreateShape(sd, False);
      sd.SetAsBox(15.0, 0.50, MakeVector(-15.0, 12.5), 0.0);
      g.CreateShape(sd, False);
      sd.SetAsBox(20.0, 0.5, MakeVector(0.0, -25.0), -0.5);
      g.CreateShape(sd);
   end;
   /// Left channel left wall
   begin
      sd := Tb2PolygonDef.Create;
      sd.SetAsBox(0.7, 55.0);
      sd.friction := 0.1;
      sd.restitution := 0.1;
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, -49.3, 50.0);
      g := m_world.CreateBody(bd);
      g.CreateShape(sd);
   end;
   /// Right wall
   begin
      sd := Tb2PolygonDef.Create;
      sd.SetAsBox(0.7, 55.0);
      sd.friction := 0.1;
      sd.restitution := 0.1;
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 45.0, 50.0);
      g := m_world.CreateBody(bd);
      g.CreateShape(sd);
   end;
   /// Left channel right upper wall
   begin
      sd := Tb2PolygonDef.Create;
      sd.SetAsBox(0.5, 20.0);
      sd.friction := 0.05;
      sd.restitution := 0.01;
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, -42.0, 70.0);
      bd.angle := -0.03 * Pi;
      g := m_world.CreateBody(bd);
      g.CreateShape(sd);
   end;
   /// Left channel right lower wall
   begin
      sd := Tb2PolygonDef.Create;
      sd.SetAsBox(0.50, 23.0);
      sd.friction := 0.05;
      sd.restitution := 0.01;
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, -44.0, 27.0);
      g := m_world.CreateBody(bd, False);
      g.CreateShape(sd, False);
      /// Bottom motors
      cd := Tb2CircleDef.Create;
      cd.radius := 3.0;
      cd.density := 15.0;
      cd.friction := 1.0;
      cd.restitution := 0.2;
      /// 1.
      SetValue(bd.position, -40.0, 2.5);
      body := m_world.CreateBody(bd, False);
      body.CreateShape(cd, False);
      body.SetMassFromShapes;

      jr := Tb2RevoluteJointDef.Create;
      {$IFDEF OP_OVERLOAD}
      jr.Initialize(g, body, body.GetWorldCenter + MakeVector(0.0, 1.0));
      {$ELSE}
      jr.Initialize(g, body, Add(body.GetWorldCenter, MakeVector(0.0, 1.0)));
      {$ENDIF}
      jr.maxMotorTorque := 30000.0;
      jr.enableMotor := True;
      jr.motorSpeed := 20.0;
      m_world.CreateJoint(jr, False);

      /// 1. left down
      SetValue(bd.position, -46.0, -2.5);
      cd.radius := 1.5;
      jr.motorSpeed := -20.0;
      body := m_world.CreateBody(bd, False);
      body.CreateShape(cd, False);
      sd.SetAsBox(2.0, 0.50);
      body.CreateShape(sd, False);
      body.SetMassFromShapes;
      jr.Initialize(g, body, body.GetWorldCenter);
      m_world.CreateJoint(jr, False);

      /// 2.
      cd.radius := 3.0;
      jr.motorSpeed := 20.0;
      SetValue(bd.position, -32.0, 2.5);
      body := m_world.CreateBody(bd, False);
      body.CreateShape(cd, False);
      body.SetMassFromShapes;
      {$IFDEF OP_OVERLOAD}
      jr.Initialize(g, body, body.GetWorldCenter + MakeVector(0.0, 1.0));
      {$ELSE}
      jr.Initialize(g, body, Add(body.GetWorldCenter, MakeVector(0.0, 1.0)));
      {$ENDIF}
      m_world.CreateJoint(jr, False);

      /// 3.
      jr.motorSpeed := 20.0;
      SetValue(bd.position, -24.0, 1.5);
      body := m_world.CreateBody(bd, False);
      body.CreateShape(cd, False);
      body.SetMassFromShapes;
      {$IFDEF OP_OVERLOAD}
      jr.Initialize(g, body, body.GetWorldCenter + MakeVector(0.0, 1.0));
      {$ELSE}
      jr.Initialize(g, body, Add(body.GetWorldCenter, MakeVector(0.0, 1.0)));
      {$ENDIF}
      m_world.CreateJoint(jr, False);

      /// 4.
      SetValue(bd.position, -16.0, 0.8);
      body := m_world.CreateBody(bd, False);
      body.CreateShape(cd, False);
      body.SetMassFromShapes;
      {$IFDEF OP_OVERLOAD}
      jr.Initialize(g, body, body.GetWorldCenter + MakeVector(0.0, 1.0));
      {$ELSE}
      jr.Initialize(g, body, Add(body.GetWorldCenter, MakeVector(0.0, 1.0)));
      {$ENDIF}
      m_world.CreateJoint(jr, False);

      /// 5.
      SetValue(bd.position, -8.0, 0.5);
      body := m_world.CreateBody(bd, False);
      body.CreateShape(cd, False);
      body.SetMassFromShapes;
      {$IFDEF OP_OVERLOAD}
      jr.Initialize(g, body, body.GetWorldCenter + MakeVector(0.0, 1.0));
      {$ELSE}
      jr.Initialize(g, body, Add(body.GetWorldCenter, MakeVector(0.0, 1.0)));
      {$ENDIF}
      m_world.CreateJoint(jr, False);

      /// 6.
      SetValue(bd.position, 0.0, 0.1);
      body := m_world.CreateBody(bd, False);
      body.CreateShape(cd, False);
      body.SetMassFromShapes;
      {$IFDEF OP_OVERLOAD}
      jr.Initialize(g, body, body.GetWorldCenter + MakeVector(0.0, 1.0));
      {$ELSE}
      jr.Initialize(g, body, Add(body.GetWorldCenter, MakeVector(0.0, 1.0)));
      {$ENDIF}
      m_world.CreateJoint(jr, False);

      /// 7.
      SetValue(bd.position, 8.0, -0.5);
      body := m_world.CreateBody(bd, False);
      body.CreateShape(cd, False);
      sd.SetAsBox(3.7, 0.5);
      body.CreateShape(sd, False);
      body.SetMassFromShapes;
      {$IFDEF OP_OVERLOAD}
      jr.Initialize(g, body, body.GetWorldCenter + MakeVector(0.0, 1.0));
      {$ELSE}
      jr.Initialize(g, body, Add(body.GetWorldCenter, MakeVector(0.0, 1.0)));
      {$ENDIF}
      m_world.CreateJoint(jr, False);

      /// 8. right rotator
      sd.SetAsBox(5.0, 0.5);
      sd.density := 2.0;
      SetValue(bd.position, 18.0, 1.0);
      rightmotor := m_world.CreateBody(bd, False);
      rightmotor.CreateShape(sd, False);
      sd.SetAsBox(4.5, 0.5, b2Vec2_Zero, Pi / 3.0);
      rightmotor.CreateShape(sd, False);
      sd.SetAsBox(4.5, 0.5, b2Vec2_Zero, Pi * 2.0 / 3.0);
      rightmotor.CreateShape(sd, False);
      cd.radius := 4.2;
      rightmotor.CreateShape(cd, False);
      rightmotor.SetMassFromShapes;
      jr.Initialize(g, rightmotor, rightmotor.GetWorldCenter);
      jr.maxMotorTorque := 70000.0;
      jr.motorSpeed := -4.0;
      m_world.CreateJoint(jr, False);

      /// 9. left rotator
      sd.SetAsBox(8.5, 0.5);
      sd.density := 2.0;
      SetValue(bd.position, -34.0, 17.0);
      body := m_world.CreateBody(bd, False);
      body.CreateShape(sd, False);
      sd.SetAsBox(8.5, 0.5, MakeVector(0.0, 0.0), Pi / 2);
      body.CreateShape(sd, False);
      cd.radius := 7.0;
      cd.friction := 0.9;
      body.CreateShape(cd);
      body.SetMassFromShapes;
      jr.Initialize(g, body, body.GetWorldCenter);
      jr.maxMotorTorque := 100000.0;
      jr.motorSpeed := -5.0;
      m_world.CreateJoint(jr);

      /// big compressor
      sd.SetAsBox(3.0, 4.0);
      sd.density := 10.0;
      SetValue(bd.position, -16.0, 17.0);
      hammerleft := m_world.CreateBody(bd, False);
      hammerleft.CreateShape(sd, False);
      hammerleft.SetMassFromShapes;
      jd := Tb2DistanceJointDef.Create;
      {$IFDEF OP_OVERLOAD}
      jd.Initialize(body, hammerleft, body.GetWorldCenter + MakeVector(0.0, 6.0), hammerleft.GetWorldCenter);
      {$ELSE}
      jd.Initialize(body, hammerleft, Add(body.GetWorldCenter, MakeVector(0.0, 6.0)), hammerleft.GetWorldCenter);
      {$ENDIF}
      m_world.CreateJoint(jd, False);

      SetValue(bd.position, 4.0, 17.0);
      hammerright := m_world.CreateBody(bd, False);
      hammerright.CreateShape(sd, False);
      hammerright.SetMassFromShapes;
      {$IFDEF OP_OVERLOAD}
      jd.Initialize(body, hammerright, body.GetWorldCenter - MakeVector(0.0, 6.0), hammerright.GetWorldCenter);
      {$ELSE}
      jd.Initialize(body, hammerright, Subtract(body.GetWorldCenter, MakeVector(0.0, 6.0)), hammerright.GetWorldCenter);
      {$ENDIF}
      m_world.CreateJoint(jd, False);

      /// pusher
      sd.SetAsBox(6.0, 0.75);
      SetValue(bd.position, -21.0, 9.0);
      pusher := m_world.CreateBody(bd);
      pusher.CreateShape(sd, False);
      sd.SetAsBox(2.0, 1.5, MakeVector(-5.0, 0.0), 0.0);
      pusher.SetMassFromShapes;
      pusher.CreateShape(sd);
      {$IFDEF OP_OVERLOAD}
      jd.Initialize(rightmotor, pusher, rightmotor.GetWorldCenter + MakeVector(-8.0, 0.0),
         pusher.GetWorldCenter + MakeVector(5.0, 0.0));
      {$ELSE}
      jd.Initialize(rightmotor, pusher, Add(rightmotor.GetWorldCenter, MakeVector(-8.0, 0.0)),
         Add(pusher.GetWorldCenter, MakeVector(5.0, 0.0)));
      {$ENDIF}
      m_world.CreateJoint(jd);
   end;

   /// Static bodies above motors
   begin
      sd := Tb2PolygonDef.Create;
      cd := Tb2CircleDef.Create;
      sd.SetAsBox(9.0, 0.5);
      sd.friction := 0.05;
      sd.restitution := 0.01;
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, -15.5, 12.0);
      bd.angle := 0.0;
      g := m_world.CreateBody(bd);
      g.CreateShape(sd, False);

      sd.SetAsBox(8.0, 0.5, MakeVector(23.0, 0.0), 0.0);
      g.CreateShape(sd, False);

      /// compressor statics
      sd.SetAsBox(7.0, 0.5, MakeVector(-2.0, 9.0), 0.0);
      g.CreateShape(sd, False);
      sd.SetAsBox(9.0, 0.5, MakeVector(22.0, 9.0), 0.0);
      g.CreateShape(sd, False);

      sd.SetAsBox(19.0, 0.5, MakeVector(-9.0, 15.0), -0.05);
      g.CreateShape(sd, False);
      sd.SetAsBox(4.7, 0.5, MakeVector(15.0, 11.5), -0.5);
      g.CreateShape(sd, False);

      /// below compressor
      sd.SetAsBox(26.0, 0.3, MakeVector(17.0, -4.4), -0.02);
      g.CreateShape(sd);
      cd.radius := 1.0;
      cd.friction := 1.0;
      cd.localPosition := MakeVector(29.0, -6.0);
      g.CreateShape(cd, False);
      cd.radius := 0.7;
      cd.localPosition := MakeVector(-2.0, -4.5);
      g.CreateShape(cd);
   end;

   /// Elevator
   begin
      bd := Tb2BodyDef.Create;
      cd := Tb2CircleDef.Create;
      sd := Tb2PolygonDef.Create;

      SetValue(bd.position, 40.0, 4.0);
      m_elev := m_world.CreateBody(bd, False);

      sd.SetAsBox(0.5, 2.5, MakeVector(3.0, -3.0), 0.0);
      sd.density := 1.0;
      sd.friction := 0.01;
      m_elev.CreateShape(sd, False);
      sd.SetAsBox(7.0, 0.5, MakeVector(-3.5, -5.5), 0.0);
      m_elev.CreateShape(sd, False);
      sd.SetAsBox(0.5, 2.5, MakeVector(-11.0, -3.5), 0.0);
      m_elev.CreateShape(sd, False);
      m_elev.SetMassFromShapes;

      jp := Tb2PrismaticJointDef.Create;
      jp.Initialize(m_ground, m_elev, bd.position, MakeVector(0.0, 1.0));
      jp.lowerTranslation := 0.0;
      jp.upperTranslation := 100.0;
      jp.enableLimit := True;
      jp.enableMotor := True;
      jp.maxMotorForce := 10000.0;
      jp.motorSpeed := 0.0;
      m_joint_elev := Tb2PrismaticJoint(m_world.CreateJoint(jp));

      /// Korb
      sd.SetAsBox(2.3, 0.5, MakeVector(1.0, 0.0), 0.0);
      sd.density := 0.5;
      SetValue(bd.position, 29.0, 6.5);
      body := m_world.CreateBody(bd, False);
      body.CreateShape(sd, False);
      sd.SetAsBox(2.5, 0.5, MakeVector(3.0, -2.0), Pi / 2.0);
      body.CreateShape(sd, False);
      sd.SetAsBox(4.6, 0.5, MakeVector(7.8, -4.0), 0.0);
      body.CreateShape(sd, False);
      sd.SetAsBox(0.5, 4.5, MakeVector(12.0, 0.0), 0.0);
      body.CreateShape(sd, False);

      sd.SetAsBox(0.5, 0.5, MakeVector(13.0, 4.0), 0.0);
      body.CreateShape(sd, False);

      cd.radius := 0.7;
      cd.density := 1.0;
      cd.friction := 0.01;
      cd.localPosition := MakeVector(0.0, 0.0);
      body.CreateShape(cd);
      body.SetMassFromShapes;

      jr := Tb2RevoluteJointDef.Create;
      jr.Initialize(m_elev, body, bd.position);
      jr.enableLimit := True;
      jr.lowerAngle := -0.2;
      jr.upperAngle := Pi * 1.1;
      jr.collideConnected := True;
      m_world.CreateJoint(jr);

      /// upper body exit
      sd.SetAsBox(14.0, 0.5, MakeVector(-3.5, -10.0), 0.0);
      SetValue(bd.position, 17.5, 96.0);
      body := m_world.CreateBody(bd);
      body.CreateShape(sd);
   end;

   /// "Elastic body" 64 bodies - something like a lin. elastic compound
   /// connected via dynamic forces (springs)
   begin
      sd := Tb2PolygonDef.Create;
      bd := Tb2BodyDef.Create;
      sd.SetAsBox(0.55, 0.55);
      sd.density := 1.5;
      sd.friction := 0.01;
      sd.filter.groupIndex := -1;
      bd.isBullet := False;
      bd.allowSleep := false;
      for i := 0 to 7 do
         for j := 0 to 7 do
         begin
            SetValue(bd.position, j * 1.02, 2.51 + 1.02 * i);
            {$IFDEF OP_OVERLOAD}
            bd.position.AddBy(startpoint);
            {$ELSE}
            AddBy(bd.position, startpoint);
            {$ENDIF}
            body := m_world.CreateBody(bd, False);
            bodies[8 * i + j] := body;
            body.CreateShape(sd, False);
            body.SetMassFromShapes;
         end;
      bd.Free;
      sd.Free;
   end;
end;

procedure TElasticBody.Step(var settings: TSettings; timeStep: Float);
begin
   settings.customedStep := True; // Use customed step method
   m_pointCount := 0;
   if not settings.realTime then
      timeStep := DefaultStep;

   if settings.pause then
   begin
      m_RemainTime := 0.0;
      if settings.singleStep then
      begin
         settings.singleStep := False;
         Inc(m_frameCount);
      end
      else
         timeStep := 0.0;
      DrawText('****PAUSED****');
   end
   else
      Inc(m_frameCount);

   if settings.realTime then // Make sure that every frame is processed using a time step pf 1/60s.
   begin
      timeStep := timeStep + m_RemainTime;
      while timeStep > DefaultStep do
      begin
         Process;
         timeStep := timeStep - DefaultStep;
      end;
      m_RemainTime := timeStep;
   end
   else
      Process;
   m_world.DrawDebugData;
   m_world.Validate;

   if Assigned(m_bomb) and m_bomb.IsFrozen then
   begin
      m_world.DestroyBody(m_bomb);
      m_bomb := nil;
   end;
   inherited; // Draw other objects, but do not Step
end;

procedure TElasticBody.Process;
const
   spring = 500.0;
   damp = 5.0;
   down: TVector2 = (X: 0.0; Y: - 0.5);
   up: TVector2 = (X: 0.0; Y: 0.5);
   right: TVector2 = (X: 0.5; Y: 0.0);
   left: TVector2 = (X: -0.5; Y: 0.0);
   drdist = 1.4142135;
   drdist2 = 2 * drdist;
var
   i, j: Integer;
   ind, indr, indd, inddr, inddl: Int32;
   p1, p2, e: TVector2;
begin
   m_world.Step(DefaultStep, 10, False);
   for i := 0 to 7 do
      for j := 0 to 7 do
      begin
         ind := i * 8 + j;
         indr := ind + 1;
         indd := ind + 8;

         if j < 7 then
         begin
            AddSpringForce(bodies[ind], b2Vec2_Zero, bodies[indr], b2Vec2_Zero, spring, damp, 1.0);
            AddSpringForce(bodies[ind], right, bodies[indr], left, 0.5 * spring, damp, 0.0);
         end;
         if i < 7 then
         begin
            AddSpringForce(bodies[ind], b2Vec2_Zero, bodies[indd], b2Vec2_Zero, spring, damp, 1.0);
            AddSpringForce(bodies[ind], up, bodies[indd], down, 0.5 * spring, damp, 0.0);
         end;
         inddr := indd + 1;
         inddl := indd - 1;
         if (i < 7) and (j < 7) then
            AddSpringForce(bodies[ind], b2Vec2_Zero, bodies[inddr], b2Vec2_Zero, spring, damp, drdist);
         if (i < 7) and (j > 0) then
            AddSpringForce(bodies[ind], b2Vec2_Zero, bodies[inddl], b2Vec2_Zero, spring, damp, drdist);

         indr := ind + 2;
         indd := ind + 8 * 2;
         if j < 6 then
            AddSpringForce(bodies[ind], b2Vec2_Zero, bodies[indr], b2Vec2_Zero, spring, damp, 2.0);
         if i < 6 then
            AddSpringForce(bodies[ind], b2Vec2_Zero, bodies[indd], b2Vec2_Zero, spring, damp, 2.0);

         inddr := indd + 2;
         inddl := indd - 2;
         if (i < 6) and (j < 6) then
            AddSpringForce(bodies[ind], b2Vec2_Zero, bodies[inddr], b2Vec2_Zero, spring, damp, drdist2);
         if (i < 6) and (j > 1) then
            AddSpringForce(bodies[ind], b2Vec2_Zero, bodies[inddl], b2Vec2_Zero, spring, damp, drdist2);
      end;

   /// Check if bodies are near elevator
   /// Look if the body to lift is near the elevator
   p1 := bodies[0].GetWorldCenter;
   p2 := bodies[63].GetWorldCenter;
   /// m_elev:   elevator prism. joint
   {$IFDEF OP_OVERLOAD}
   e := m_elev.GetWorldCenter + MakeVector(0.0, 7.0);
   {$ELSE}
   e := Add(m_elev.GetWorldCenter, MakeVector(0.0, 7.0));
   {$ENDIF}
   // maybe not the best way to do it...
   // Bodies reached the elevator side
   if (p1.x > e.x) or (p2.x > e.x) then // go up
      if ((p1.y < e.y) or (p2.y < e.y)) and (m_joint_elev.GetJointTranslation <= m_joint_elev.GetLowerLimit + 1.0) then
         m_joint_elev.SetMotorSpeed(20.0);

   // go down
   if (m_joint_elev.GetJointTranslation >= m_joint_elev.GetUpperLimit - 2.0) then
      m_joint_elev.SetMotorSpeed(-15.0);
end;

procedure TElasticBody.AddSpringForce(bA: Tb2Body; const localA: TVector2;
   bB: Tb2Body; const localB: TVector2; const k, friction, desiredDist: Float);
var
   diff, vA, vB, vdiff: TVector2;
   dx, vrel, forceMag: Float;
begin
   {$IFDEF OP_OVERLOAD}
   diff := bB.GetWorldPoint(localB) - bA.GetWorldPoint(localA);
   //Find velocities of attach points
   vA := bA.GetLinearVelocity - b2Cross(bA.GetWorldVector(localA), bA.GetAngularVelocity);
   vB := bB.GetLinearVelocity - b2Cross(bB.GetWorldVector(localB), bB.GetAngularVelocity);
   vdiff := vB - vA;
   dx := diff.Normalize; //normalizes diff and puts length into dx
   {$ELSE}
   diff := Subtract(bB.GetWorldPoint(localB), bA.GetWorldPoint(localA));
   //Find velocities of attach points
   vA := Subtract(bA.GetLinearVelocity, b2Cross(bA.GetWorldVector(localA), bA.GetAngularVelocity));
   vB := Subtract(bB.GetLinearVelocity, b2Cross(bB.GetWorldVector(localB), bB.GetAngularVelocity));
   vdiff := Subtract(vB, vA);
   dx := Normalize(diff); //normalizes diff and puts length into dx
   {$ENDIF}
   vrel := vdiff.x * diff.x + vdiff.y * diff.y;
   forceMag := -k * (dx - desiredDist) - friction * vrel;
   {$IFDEF OP_OVERLOAD}
   diff := diff * forceMag;
   {$ELSE}
   diff := Multiply(diff, forceMag);
   {$ENDIF}
   bB.ApplyForce(diff, bA.GetWorldPoint(localA));
   {$IFDEF OP_OVERLOAD}
   diff := -diff;
   {$ELSE}
   diff := Negative(diff);
   {$ENDIF}
   bA.ApplyForce(diff, bB.GetWorldPoint(localB));
end;

initialization
   RegisterTestEntry('Elastic Body', TElasticBody);

end.

