unit UTimeOfImpact;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TTimeOfImpact = class(TTester)
   public
	    m_shapeA, m_shapeB: Tb2PolygonShape;

      constructor Create; override;
      destructor Destroy; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;

   TTimeOfImpact2 = class(TTester)
   public
      down, up: Tb2Body;
      bullets: array[0..2] of Tb2Body;
      bullet2: Tb2Body;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;

implementation

{ TTimeOfImpact }

constructor TTimeOfImpact.Create;
begin
   inherited;
   m_shapeA := Tb2PolygonShape.Create;
   m_shapeB := Tb2PolygonShape.Create;
	 m_shapeA.SetAsBox(25.0, 5.0);
	 m_shapeB.SetAsBox(2.5, 2.5);
   frmMain.ResetView;
   SetCanvasTranslationOffset(-200, 600);
end;

destructor TTimeOfImpact.Destroy;
begin
   m_shapeA.Free;
   m_shapeB.Free;
   inherited;
end;

procedure TTimeOfImpact.Step(var settings: TSettings; timeStep: PhysicsFloat);
const
   clVertices: RGBA = (0.9, 0.9, 0.9, 1.0);
   clVertices2: RGBA = (0.5, 0.9, 0.5, 1.0);
   clVertices3: RGBA = (0.5, 0.7, 0.9, 1.0);
   clVertices4: RGBA = (0.9, 0.5, 0.5, 1.0);
var
   i: Integer;
   input: Tb2TOIInput;
   output: Tb2TOIOutput;
   sweepA, sweepB: Tb2Sweep;
   transformA, transformB: Tb2Transform;
   vertices: Tb2PolyVertices;
begin
   SetValue(sweepA.c0, 24.0, -60.0);
   sweepA.a0 := 2.95;
   sweepA.c := sweepA.c0;
   sweepA.a := sweepA.a0;
   sweepA.localCenter := b2Vec2_Zero;

   SetValue(sweepB.c0, 53.474274, -50.252514);
   sweepB.a0 := 513.36676; // - 162.0f * b2_pi;
   SetValue(sweepB.c, 54.595478, -51.083473);
   sweepB.a := 513.62781; //  - 162.0f * b2_pi;
   sweepB.localCenter := b2Vec2_Zero;

   {$IFDEF OP_OVERLOAD}
   input.proxyA.SetShape(m_shapeA, 0);
   input.proxyB.SetShape(m_shapeB, 0);
   {$ELSE}
   SetShape(input.proxyA, m_shapeA, 0);
   SetShape(input.proxyB, m_shapeB, 0);
   {$ENDIF}
   input.sweepA := sweepA;
   input.sweepB := sweepB;
   input.tMax := 1.0;

   b2TimeOfImpact(output, input);

   DrawText(Format('toi := %g', [output.t]));
   DrawText(Format('max toi iters := %d, max root iters := %d',
      [b2_toiMaxIters, b2_toiMaxRootIters]));

   {$IFDEF OP_OVERLOAD}
   sweepA.GetTransform(transformA, 0.0);
   {$ELSE}
   GetTransform(sweepA, transformA, 0.0);
   {$ENDIF}
   for i := 0 to m_shapeA.m_count - 1 do
      vertices[i] := b2Mul(transformA, m_shapeA.m_vertices[i]);
   m_debugDraw.DrawPolygon(vertices, m_shapeA.m_count, clVertices);

   {$IFDEF OP_OVERLOAD}
   sweepB.GetTransform(transformB, 0.0);
   {$ELSE}
   GetTransform(sweepB, transformB, 0.0);
   {$ENDIF}

   //v := (sweepB.c - sweepB.c0) + b2Cross(sweepB.a - sweepB.a0, b2Mul(transformB, MakeVector(2.0, -0.1)) - sweepB.c0);

   for i := 0 to m_shapeB.m_count - 1 do
      vertices[i] := b2Mul(transformB, m_shapeB.m_vertices[i]);
   m_debugDraw.DrawPolygon(vertices, m_shapeB.m_count, clVertices2);

   {$IFDEF OP_OVERLOAD}
   sweepB.GetTransform(transformB, output.t);
   {$ELSE}
   GetTransform(sweepB, transformB, output.t);
   {$ENDIF}
   for i := 0 to m_shapeB.m_count - 1 do
      vertices[i] := b2Mul(transformB, m_shapeB.m_vertices[i]);
   m_debugDraw.DrawPolygon(vertices, m_shapeB.m_count, clVertices3);

   {$IFDEF OP_OVERLOAD}
   sweepB.GetTransform(transformB, 1.0);
   {$ELSE}
   GetTransform(sweepB, transformB, 1.0);
   {$ENDIF}
   for i := 0 to m_shapeB.m_count - 1 do
      vertices[i] := b2Mul(transformB, m_shapeB.m_vertices[i]);
   m_debugDraw.DrawPolygon(vertices, m_shapeB.m_count, clVertices4);
end;

{ TTimeOfImpact2 }

constructor TTimeOfImpact2.Create;
var
   i: Integer;
   pd: Tb2PolygonShape;
   bd: Tb2BodyDef;
   cd: Tb2CircleShape;
   fd: Tb2FixtureDef;
begin
   inherited;
   pd := Tb2PolygonShape.Create;
   bd := Tb2BodyDef.Create;
   pd.SetAsBox(10, 0.1, MakeVector(-10, -5), 0);
   SetValue(bd.position, -10, -5);
   down := m_world.CreateBody(bd, False);
   down.CreateFixture(pd, 0, False);

   pd.SetAsBox(10, 0.1, MakeVector(-10, 0), 0);
   SetValue(bd.position, -10, 0);
   up := m_world.CreateBody(bd);
   up.CreateFixture(pd, 0.0);

   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   bd.bullet := True;
   cd := Tb2CircleShape.Create;
   cd.m_radius := 0.25;

   fd := Tb2FixtureDef.Create;
   fd.shape := cd;
   fd.density := 15.0;
   fd.restitution := 1.5;
   SetValue(bd.position, -20, -3.0);
   for i := 0 to 2 do
   begin
      bullets[i] := m_world.CreateBody(bd, False);
      bullets[i].SetLinearVelocity(MakeVector(0.0, 0.0));
      bullets[i].CreateFixture(fd, False, False);
   end;
   bd.Free;
   cd.Free;
   fd.Free;

   //////////////////////////////////////
   pd := Tb2PolygonShape.Create;
   bd := Tb2BodyDef.Create;
   pd.SetAsBox(10, 5, MakeVector(10, -5), 0);
   SetValue(bd.position, 10, -5);
   down := m_world.CreateBody(bd);
   down.CreateFixture(pd, 0.0);

   bd := Tb2BodyDef.Create;
   SetValue(bd.position, 20, 20);
   bd.bullet := True;
   bd.bodyType := b2_dynamicBody;
   cd := Tb2CircleShape.Create;
   cd.m_radius := 0.25;
   bullet2 := m_world.CreateBody(bd);
   bullet2.SetLinearVelocity(MakeVector(0.0, -500.0));
   bullet2.CreateFixture(cd, 15);
end;

procedure TTimeOfImpact2.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Switch Time of Impact option and reset the scene.');
   if m_world.ContinuousPhysics then
      DrawText('TOI ON')
   else
      DrawText('TOI OFF');
end;

initialization
   RegisterTestEntry('Time of Impact', TTimeOfImpact);
   RegisterTestEntry('Time of Impact2', TTimeOfImpact2);

end.

