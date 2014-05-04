unit UMobile;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   uTestBed,
   UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TMobile = class(TTester)
   public
      constructor Create; override;
      function AddNode(parent: Tb2Body; const localAnchor: TVector2; depth: Integer; offset, a: PhysicsFloat): Tb2Body;
   end;

implementation

const e_depth = 4;

{ TMobile }

constructor TMobile.Create;
var
   bd: Tb2BodyDef;
   ground, root: Tb2Body;
   a: PhysicsFloat;
   h: TVector2;
   jointDef: Tb2RevoluteJointDef;
begin
   inherited;

   // Create ground body.
   bd := Tb2BodyDef.Create;
   SetValue(bd.position, 0.0, 20.0);
   ground := m_world.CreateBody(bd);

   a := 0.5;
   h := MakeVector(0.0, a);

   root := AddNode(ground, b2Vec2_zero, 0, 3.0, a);

   jointDef := Tb2RevoluteJointDef.Create;
   jointDef.bodyA := ground;
   jointDef.bodyB := root;
   jointDef.localAnchorA := b2Vec2_Zero;
   jointDef.localAnchorB := h;
   m_world.CreateJoint(jointDef);
end;

function TMobile.AddNode(parent: Tb2Body; const localAnchor: TVector2; depth: Integer; offset, a: PhysicsFloat): Tb2Body;
var
   density: PhysicsFloat;
   h, p, a1, a2: TVector2;
   bodyDef: Tb2BodyDef;
   body, body1, body2: Tb2Body;
   shape: Tb2PolygonShape;
   jointDef: Tb2RevoluteJointDef;
begin
   density := 20.0;
   h := MakeVector(0.0, a);

   {$IFDEF OP_OVERLOAD}
   p := parent.GetPosition + localAnchor - h;
   {$ELSE}
   p := Subtract(Add(parent.GetPosition, localAnchor), h);
   {$ENDIF}

   bodyDef := Tb2BodyDef.Create;
   bodyDef.bodyType := b2_dynamicBody;
   bodyDef.position := p;
   body := m_world.CreateBody(bodyDef);

   shape := Tb2PolygonShape.Create;
   shape.SetAsBox(0.25 * a, a);
   body.CreateFixture(shape, density);

   if depth = e_depth then
   begin
      Result := body;
      Exit;
   end;

   a1 := MakeVector(offset, -a);
	 a2 := MakeVector(-offset, -a);
	 body1 := AddNode(body, a1, depth + 1, 0.5 * offset, a);
	 body2 := AddNode(body, a2, depth + 1, 0.5 * offset, a);

   jointDef := Tb2RevoluteJointDef.Create;
   jointDef.bodyA := body;
   jointDef.localAnchorB := h;

   jointDef.localAnchorA := a1;
   jointDef.bodyB := body1;
   m_world.CreateJoint(jointDef, False);

   jointDef.localAnchorA := a2;
   jointDef.bodyB := body2;
   m_world.CreateJoint(jointDef);

	 Result := body;
end;

initialization
   RegisterTestEntry('Mobile', TMobile);

end.
