unit URopeJoint;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   /// This test shows how a rope joint can be used to stabilize a chain of
   /// bodies with a heavy payload. Notice that the rope joint just prevents
   /// excessive stretching and has no other effect.
   /// By disabling the rope joint you can see that the Box2D solver has trouble
   /// supporting heavy bodies with light bodies. Try playing around with the
   /// densities, time step, and iterations to see how they affect stability.
   /// This test also shows how to use contact filtering. Filtering is configured
   /// so that the payload does not collide with the chain.

   TRope = class(TTester)
   public
    	m_ropeDef: Tb2RopeJointDef;
	    m_rope: Tb2Joint;

      constructor Create; override;
      destructor Destroy; override;

      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;


implementation

{ TRope }

constructor TRope.Create;
const N = 10;
const y = 15.0;

var
   i: Integer;
   ground, prevBody, body: Tb2Body;
   bd: Tb2BodyDef;
   eshape: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   fd: Tb2FixtureDef;
   jd: Tb2RevoluteJointDef;
   anchor: TVector2;
   extraLength: PhysicsFloat;
begin
   inherited;

   m_ropeDef := Tb2RopeJointDef.Create;

   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      eshape := Tb2EdgeShape.Create;
      eshape.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
      ground.CreateFixture(eshape, 0.0);
   end;

   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.5, 0.125);

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.density := 20.0;
      fd.friction := 0.2;
      fd.filter.categoryBits := $0001;
      fd.filter.maskBits := $FFFF and (not $0002);

      jd := Tb2RevoluteJointDef.Create;
      jd.collideConnected := False;

      SetValue(m_ropeDef.localAnchorA, 0.0, y);

      prevBody := ground;
      bd := Tb2BodyDef.Create;
      for i := 0 to N - 1 do
      begin
         bd.bodyType := b2_dynamicBody;
         SetValue(bd.position, 0.5 + 1.0 * i, y);
         if i = N - 1 then
         begin
            shape.SetAsBox(1.5, 1.5);
            fd.density := 100.0;
            fd.filter.categoryBits := $0002;
            SetValue(bd.position, 1.0 * i, y);
            bd.angularDamping := 0.4;
         end;

         body := m_world.CreateBody(bd, False);
         body.CreateFixture(fd, False, False);

         anchor.x := i;
         anchor.y := y;
         jd.Initialize(prevBody, body, anchor);
         m_world.CreateJoint(jd, False);

         prevBody := body;
      end;
      bd.Free;
      fd.Free;
      jd.Free;
      shape.Free;

      extraLength := 0.01;
      m_ropeDef.localAnchorB := b2Vec2_Zero;
      m_ropeDef.maxLength := N - 1.0 + extraLength;
      m_ropeDef.bodyB := prevBody;
   end;

   m_ropeDef.bodyA := ground;
   m_rope := m_world.CreateJoint(m_ropeDef, False);
end;

destructor TRope.Destroy;
begin
   m_ropeDef.Free;
   inherited;
end;

procedure TRope.Keyboard(key: Byte);
begin
   if key = Ord('J') then
   begin
      if Assigned(m_rope) then
      begin
			  	m_world.DestroyJoint(m_rope);
			  	m_rope := nil;
      end
      else
         m_rope := m_world.CreateJoint(m_ropeDef, False);
   end;
end;

procedure TRope.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Press (j) to toggle the rope joint.');
   if Assigned(m_rope) then
      DrawText('Rope ON')
   else
      DrawText('Rope OFF');
end;

initialization
   RegisterTestEntry('Rope Joint', TRope);

end.
