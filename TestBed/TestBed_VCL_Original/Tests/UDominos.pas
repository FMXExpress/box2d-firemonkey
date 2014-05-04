unit UDominos;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TDominos = class(TTester)
   public
      constructor Create; override;
   end;

implementation

{ TDominos }

constructor TDominos.Create;
const
   radius = 0.2;
var
   i: Integer;
   ground, body, b1, b2, b3, b4, b5, b6, b7: Tb2Body;
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   bd: Tb2BodyDef;
   fd: Tb2FixtureDef;
   jd: Tb2RevoluteJointDef;
   anchor, d: TVector2;
   djd: Tb2DistanceJointDef;
   cshape: Tb2CircleShape;
begin
   inherited;
   begin
      edge := Tb2EdgeShape.Create;
      edge.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));

      bd := Tb2BodyDef.Create;
      b1 := m_world.CreateBody(bd);
      b1.CreateFixture(edge, 0.0);
   end;

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(6.0, 0.25);

      bd := Tb2BodyDef.Create;
      SetValue(bd.position, -1.5, 10.0);
      ground := m_world.CreateBody(bd);
      ground.CreateFixture(shape, 0.0);
   end;

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.1, 1.0);

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.density := 20.0;
      fd.friction := 0.1;

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      for i := 0 to 9 do
      begin
         SetValue(bd.position, -6.0 + 1.0 * i, 11.25);
         body := m_world.CreateBody(bd, False);
         body.CreateFixture(fd, False, False);
      end;
      fd.Free;
      bd.Free;
      shape.Free;
   end;

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(7.0, 0.25, b2Vec2_zero, 0.3);

      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 1.0, 6.0);
      ground := m_world.CreateBody(bd);
      ground.CreateFixture(shape, 0.0);
   end;

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.25, 1.5);

      bd := Tb2BodyDef.Create;
      SetValue(bd.position, -7.0, 4.0);
      b2 := m_world.CreateBody(bd);
      b2.CreateFixture(shape, 0.0);
   end;

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(6.0, 0.125);

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, -0.9, 1.0);
      bd.angle := -0.15;

      b3 := m_world.CreateBody(bd);
      b3.CreateFixture(shape, 10.0);
   end;

   jd := Tb2RevoluteJointDef.Create;
   SetValue(anchor, -2.0, 1.0);
   jd.Initialize(b1, b3, anchor);
   jd.collideConnected := True;
   m_world.CreateJoint(jd, False);

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.25, 0.25);

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, -10.0, 15.0);
      b4 := m_world.CreateBody(bd);
      b4.CreateFixture(shape, 10.0);
   end;

   SetValue(anchor, -7.0, 15.0);
   jd.Initialize(b2, b4, anchor);
   m_world.CreateJoint(jd, False);

   begin
      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 6.5, 3.0);
      b5 := m_world.CreateBody(bd);

      shape := Tb2PolygonShape.Create;
      fd := Tb2FixtureDef.Create;

      fd.shape := shape;
      fd.density := 10.0;
      fd.friction := 0.1;

      shape.SetAsBox(1.0, 0.1, MakeVector(0.0, -0.9), 0.0);
      b5.CreateFixture(fd, False, False, False);

      shape.SetAsBox(0.1, 1.0, MakeVector(-0.9, 0.0), 0.0);
      b5.CreateFixture(fd, False, False, False);

      shape.SetAsBox(0.1, 1.0, MakeVector(0.9, 0.0), 0.0);
      b5.CreateFixture(fd);
   end;

   SetValue(anchor, 6.0, 2.0);
   jd.Initialize(b1, b5, anchor);
   m_world.CreateJoint(jd, False);

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(1.0, 0.1);

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 6.5, 4.1);
      b6 := m_world.CreateBody(bd);
      b6.CreateFixture(shape, 30.0);
   end;

   SetValue(anchor, 7.5, 4.0);
   jd.Initialize(b5, b6, anchor);
   m_world.CreateJoint(jd);

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.1, 1.0);

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 7.4, 1.0);

      b7 := m_world.CreateBody(bd);
      b7.CreateFixture(shape, 10.0);
   end;

   djd := Tb2DistanceJointDef.Create;
   djd.bodyA := b3;
   djd.bodyB := b7;
   SetValue(djd.localAnchorA, 6.0, 0.0);
   SetValue(djd.localAnchorB, 0.0, -1.0);
   {$IFDEF OP_OVERLOAD}
   d := djd.bodyB.GetWorldPoint(djd.localAnchorB) - djd.bodyA.GetWorldPoint(djd.localAnchorA);
   djd.length := d.Length;
   {$ELSE}
   d := Subtract(djd.bodyB.GetWorldPoint(djd.localAnchorB), djd.bodyA.GetWorldPoint(djd.localAnchorA));
   djd.length := LengthVec(d);
   {$ENDIF}
   m_world.CreateJoint(djd);

   begin
      cshape := Tb2CircleShape.Create;
      cshape.m_radius := radius;

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      for i := 0 to 3 do
      begin
         SetValue(bd.position, 5.9 + 2.0 * radius * i, 2.4);
         body := m_world.CreateBody(bd, False);
         body.CreateFixture(cshape, 10.0, False);
      end;
      cshape.Free;
      bd.Free;
   end;
end;

initialization
   RegisterTestEntry('Dominos', TDominos);

end.
