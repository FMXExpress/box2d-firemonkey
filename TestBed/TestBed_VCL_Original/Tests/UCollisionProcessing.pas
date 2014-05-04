unit UCollisionProcessing;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TCollisionProcessing = class(TTester)
   public
      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;

implementation

{ TCollisionProcessing }

constructor TCollisionProcessing.Create;
const
   xLo = -5;
   xHi = 5;
   yLo = 2;
   yHi = 35;
var
   ground, body1, body2, body3, body4, body5, body6: Tb2Body;
   edge: Tb2EdgeShape;
   sd, triangleShapeDef, boxShapeDef, circleShapeDef: Tb2FixtureDef;
   bd, triangleBodyDef, boxBodyDef, circleBodyDef: Tb2BodyDef;
   polygon: Tb2PolygonShape;
   circle: Tb2CircleShape;
   vertices: array[0..2] of TVector2;
begin
   inherited;
   // Ground body
   begin
      edge := Tb2EdgeShape.Create;
      edge.SetVertices(MakeVector(-50.0, 0.0), MakeVector(50.0, 0.0));

      sd := Tb2FixtureDef.Create;
      sd.shape := edge;

      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);
      ground.CreateFixture(sd);
   end;

   // Small triangle
   vertices[0] := MakeVector(-1.0, 0.0);
   vertices[1] := MakeVector(1.0, 0.0);
   vertices[2] := MakeVector(0.0, 2.0);

   polygon := Tb2PolygonShape.Create;;
   polygon.SetVertices(@vertices[0], 3);

   triangleShapeDef := Tb2FixtureDef.Create;
   triangleShapeDef.shape := polygon;
   triangleShapeDef.density := 1.0;

   triangleBodyDef := Tb2BodyDef.Create;
   triangleBodyDef.bodyType := b2_dynamicBody;
   SetValue(triangleBodyDef.position, RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

   body1 := m_world.CreateBody(triangleBodyDef, False);
   body1.CreateFixture(triangleShapeDef, False, False);

   // Large triangle (recycle definitions)
   {$IFDEF OP_OVERLOAD}
   vertices[0].MultiplyBy(2.0);
   vertices[1].MultiplyBy(2.0);
   vertices[2].MultiplyBy(2.0);
   {$ELSE}
   MultiplyBy(vertices[0], 2.0);
   MultiplyBy(vertices[1], 2.0);
   MultiplyBy(vertices[2], 2.0);
   {$ENDIF}
   polygon.SetVertices(@vertices[0], 3);

   SetValue(triangleBodyDef.position, RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

   body2 := m_world.CreateBody(triangleBodyDef);
   body2.CreateFixture(triangleShapeDef);

   // Small box
   polygon.SetAsBox(1.0, 0.5);

   boxShapeDef := Tb2FixtureDef.Create;
   boxShapeDef.shape := polygon;
   boxShapeDef.density := 1.0;

   boxBodyDef := Tb2BodyDef.Create;
   boxBodyDef.bodyType := b2_dynamicBody;
   SetValue(boxBodyDef.position, RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

   body3 := m_world.CreateBody(boxBodyDef, False);
   body3.CreateFixture(boxShapeDef, False, False);

   // Large box (recycle definitions)
   polygon.SetAsBox(2.0, 1.0);
   SetValue(boxBodyDef.position, RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

   body4 := m_world.CreateBody(boxBodyDef);
   body4.CreateFixture(boxShapeDef);

   // Small circle
   circle := Tb2CircleShape.Create;
   circle.m_radius := 1.0;

   circleShapeDef := Tb2FixtureDef.Create;
   circleShapeDef.shape := circle;
   circleShapeDef.density := 1.0;

   circleBodyDef := Tb2BodyDef.Create;
   circleBodyDef.bodyType := b2_dynamicBody;
   SetValue(circleBodyDef.position, RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

   body5 := m_world.CreateBody(circleBodyDef, False);
   body5.CreateFixture(circleShapeDef, False, False);

   // Large circle
   circle.m_radius := circle.m_radius * 2.0;
   SetValue(circleBodyDef.position, RandomFloat(xLo, xHi), RandomFloat(yLo, yHi));

   body6 := m_world.CreateBody(circleBodyDef);
   body6.CreateFixture(circleShapeDef);
end;

procedure TCollisionProcessing.Step(var settings: TSettings; timeStep: PhysicsFloat);
const
   k_maxNuke = 6;
var
   i, j: Integer;
   // We are going to destroy some bodies according to contact
   // points. We must buffer the bodies that should be destroyed
   nuke: array[0..k_maxNuke - 1] of Tb2Body;
   nukeCount: Int32;
   b: Tb2Body;
begin
   inherited;
   nukeCount := 0;

   // Traverse the contact results. Destroy bodies that are touching heavier bodies.
   for i := 0 to m_pointCount - 1 do
      with m_points[i] do
         if (fixtureA.GetBody.GetMass > 0.0) and (fixtureB.GetBody.GetMass > 0.0) then
         begin
            if fixtureB.GetBody.GetMass > fixtureA.GetBody.GetMass then
               nuke[nukeCount] := fixtureA.GetBody
            else
               nuke[nukeCount] := fixtureB.GetBody;
            Inc(nukeCount);

            if nukeCount = k_maxNuke then
               Break;
         end;

   // Sort the nuke array to group duplicates.
   for i := 0 to nukeCount - 2 do
      for j := nukeCount - 1 downto i + 1 do
         if Integer(nuke[i]) > Integer(nuke[j]) then
         begin
            b := nuke[i];
            nuke[i] := nuke[j];
            nuke[j] := b;
         end;

   // Destroy the bodies, skipping duplicates.
   i := 0;
   while (i < nukeCount) do
   begin
      b := nuke[i];
      Inc(i);
      while (i < nukeCount) and (nuke[i] = b) do
         Inc(i);

      if b <> m_bomb then
         m_world.DestroyBody(b);
   end;
end;

initialization
   RegisterTestEntry('Collision Processing', TCollisionProcessing);

end.

