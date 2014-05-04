unit UCollisionFiltering;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TCollisionFiltering = class(TTester)
   public
      constructor Create; override;
   end;

implementation
const
   // This is a test of collision filtering.
   // There is a triangle, a box, and a circle.
   // There are 6 shapes. 3 large and 3 small.
   // The 3 small ones always collide.
   // The 3 large ones never collide.
   // The boxes don't collide with triangles (except if both are small).
   k_smallGroup = 1;
   k_largeGroup = -1;

   k_defaultCategory = $0001;
   k_triangleCategory = $0002;
   k_boxCategory = $0004;
   k_circleCategory = $0008;

   k_triangleMask = $FFFF;
   k_boxMask = $FFFF xor k_triangleCategory;
   k_circleMask = $FFFF;


{ TCollisionFiltering }

constructor TCollisionFiltering.Create;
var
   edge: Tb2EdgeShape;
   polygon, p: Tb2PolygonShape;
   sd, boxShapeDef, circleShapeDef: Tb2FixtureDef;
   bd, triangleBodyDef, boxBodyDef, circleBodyDef: Tb2BodyDef;
   body, ground, body1, body2, body3, body4, body5, body6: Tb2Body;
   triangleShapeDef: Tb2FixtureDef;
   vertices: array[0..2] of TVector2;
   jd: Tb2PrismaticJointDef;
   circle: Tb2CircleShape;
begin
   inherited;
   // Ground body
   begin
      edge := Tb2EdgeShape.Create;
      edge.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));

      sd := Tb2FixtureDef.Create;
      sd.shape := edge;
      sd.friction := 0.3;

      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);
      ground.CreateFixture(sd);
   end;

   // Small triangle
   vertices[0] := MakeVector(-1.0, 0.0);
   vertices[1] := MakeVector(1.0, 0.0);
   vertices[2] := MakeVector(0.0, 2.0);
   polygon := Tb2PolygonShape.Create;
   polygon.SetVertices(@vertices[0], 3);

   triangleShapeDef := Tb2FixtureDef.Create;
   triangleShapeDef.shape := polygon;
   triangleShapeDef.density := 1.0;

   triangleShapeDef.filter.groupIndex := k_smallGroup;
   triangleShapeDef.filter.categoryBits := k_triangleCategory;
   triangleShapeDef.filter.maskBits := k_triangleMask;

   triangleBodyDef := Tb2BodyDef.Create;
   triangleBodyDef.bodyType := b2_dynamicBody;
   SetValue(triangleBodyDef.position, -5.0, 2.0);

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
   triangleShapeDef.filter.groupIndex := k_largeGroup;
   SetValue(triangleBodyDef.position, -5.0, 6.0);
   triangleBodyDef.fixedRotation := True; // look at me!

   body2 := m_world.CreateBody(triangleBodyDef);
   body2.CreateFixture(triangleShapeDef);

   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, -5.0, 10.0);
      body := m_world.CreateBody(bd);

      p := Tb2PolygonShape.Create;
      p.SetAsBox(0.5, 1.0);
      body.CreateFixture(p, 1.0);

      jd := Tb2PrismaticJointDef.Create;
      jd.bodyA := body2;
      jd.bodyB := body;
      jd.enableLimit := True;
      SetValue(jd.localAnchorA, 0.0, 4.0);
      jd.localAnchorB := b2Vec2_Zero;
      SetValue(jd.localAxisA, 0.0, 1.0);
      jd.lowerTranslation := -1.0;
      jd.upperTranslation := 1.0;

      m_world.CreateJoint(jd);
   end;

   // Small box
   polygon.SetAsBox(1.0, 0.5);
   boxShapeDef := Tb2FixtureDef.Create;
   boxShapeDef.shape := polygon;
   boxShapeDef.density := 1.0;
   boxShapeDef.restitution := 0.1;

   boxShapeDef.filter.groupIndex := k_smallGroup;
   boxShapeDef.filter.categoryBits := k_boxCategory;
   boxShapeDef.filter.maskBits := k_boxMask;

   boxBodyDef := Tb2BodyDef.Create;
   boxBodyDef.bodyType := b2_dynamicBody;
   SetValue(boxBodyDef.position, 0.0, 2.0);

   body3 := m_world.CreateBody(boxBodyDef, False);
   body3.CreateFixture(boxShapeDef, False, False);

   // Large box (recycle definitions)
   polygon.SetAsBox(2.0, 1.0);
   boxShapeDef.filter.groupIndex := k_largeGroup;
   SetValue(boxBodyDef.position, 0.0, 6.0);

   body4 := m_world.CreateBody(boxBodyDef);
   body4.CreateFixture(boxShapeDef);

   // Small circle
   circle := Tb2CircleShape.Create;
   circle.m_radius := 1.0;

   circleShapeDef := Tb2FixtureDef.Create;
   circleShapeDef.shape := circle;
   circleShapeDef.density := 1.0;

   circleShapeDef.filter.groupIndex := k_smallGroup;
   circleShapeDef.filter.categoryBits := k_circleCategory;
   circleShapeDef.filter.maskBits := k_circleMask;

   circleBodyDef := Tb2BodyDef.Create;
   circleBodyDef.bodyType := b2_dynamicBody;
   SetValue(circleBodyDef.position, 5.0, 2.0);

   body5 := m_world.CreateBody(circleBodyDef, False);
   body5.CreateFixture(circleShapeDef, False, False);

   // Large circle
   circle.m_radius := circle.m_radius * 2.0;
   circleShapeDef.filter.groupIndex := k_largeGroup;
   SetValue(circleBodyDef.position, 5.0, 6.0);

   body6 := m_world.CreateBody(circleBodyDef);
   body6.CreateFixture(circleShapeDef);
end;

initialization
   RegisterTestEntry('Collision Filtering', TCollisionFiltering);

end.
