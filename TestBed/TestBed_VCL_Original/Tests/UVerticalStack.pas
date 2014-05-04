unit UVerticalStack;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

const
	 e_columnCount = 5;
	 e_rowCount = 16;

type
   TVerticalStack = class(TTester)
   public
      m_bullet: Tb2Body;
      m_bodies: array[0..e_rowCount * e_columnCount - 1] of Tb2Body;
      m_indices: array[0..e_rowCount * e_columnCount - 1] of Int32;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TVerticalStack }

constructor TVerticalStack.Create;
const
   xs: array[0..4] of PhysicsFloat = (0.0, -10.0, -5.0, 5.0, 10.0);
var
   i, j: Integer;
   edge: Tb2EdgeShape;
   shape: Tb2PolygonShape;
   bd: Tb2BodyDef;
   ground: Tb2Body;
   fd: Tb2FixtureDef;
   n: Int32;
   x: PhysicsFloat;
begin
   inherited;
   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      edge := Tb2EdgeShape.Create;
      edge.SetVertices(MakeVector(-40.0, 0.0), MakeVector(40.0, 0.0));
      ground.CreateFixture(edge, 0.0, False);

      edge.SetVertices(MakeVector(20.0, 0.0), MakeVector(20.0, 20.0));
      ground.CreateFixture(edge, 0.0);
   end;

   for j := 0 to e_columnCount - 1 do
   begin
      shape := Tb2PolygonShape.Create;
      shape.SetAsBox(0.5, 0.5);

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.density := 1.0;
      fd.friction := 0.3;

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      for i := 0 to e_rowCount - 1 do
      begin
         n := j * e_rowCount + i;
         //b2Assert(n < e_rowCount * e_columnCount);
         m_indices[n] := n;
         bd.userData := @m_indices[n];

         x := 0.0;
         //float32 x := RandomFloat(-0.02, 0.02);
         //float32 x := i % 2 == 0 ? -0.025 : 0.025;
         SetValue(bd.position, xs[j] + x, 0.752 + 1.54 * i);
         m_bodies[n] := m_world.CreateBody(bd, False);
         m_bodies[n].CreateFixture(fd, False, False);
      end;
      fd.Free;
      shape.Free;
      bd.Free;
   end;

   m_bullet := nil;
end;

procedure TVerticalStack.Step(var settings: TSettings; timeStep: PhysicsFloat);
var
   shape: Tb2CircleShape;
   fd: Tb2FixtureDef;
   bd: Tb2BodyDef;
begin
   inherited;
	 DrawText('Press: B to launch a bullet.');
   if m_stepCount = 600 then
   begin
      if Assigned(m_bullet) then
      begin
         m_world.DestroyBody(m_bullet);
         m_bullet := nil;
      end;

       shape := Tb2CircleShape.Create;
       shape.m_radius := 0.25;

       fd := Tb2FixtureDef.Create;
       fd.shape := shape;
       fd.density := 20.0;
       fd.restitution := 0.05;

       bd := Tb2BodyDef.Create;
       bd.bodyType := b2_dynamicBody;
       bd.bullet := True;
       SetValue(bd.position, -31.0, 5.0);

       m_bullet := m_world.CreateBody(bd);
       m_bullet.CreateFixture(fd);

       m_bullet.SetLinearVelocity(MakeVector(400.0, 0.0));
   end;
end;

procedure TVerticalStack.Keyboard(key: Byte);
var
   shape: Tb2CircleShape;
   fd: Tb2FixtureDef;
   bd: Tb2BodyDef;
begin
   if key = Ord('B') then
   begin
			if Assigned(m_bullet) then
			begin
				 m_world.DestroyBody(m_bullet);
				 m_bullet := nil;
			end;

      shape := Tb2CircleShape.Create;
      shape.m_radius := 0.25;

      fd := Tb2FixtureDef.Create;
      fd.shape := shape;
      fd.density := 20.0;
      fd.restitution := 0.05;

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      bd.bullet := True;
      SetValue(bd.position, -31.0, 5.0);

      m_bullet := m_world.CreateBody(bd);
      m_bullet.CreateFixture(fd);

      m_bullet.SetLinearVelocity(MakeVector(400.0, 0.0));
   end;
end;

initialization
   RegisterTestEntry('Vertical Stack', TVerticalStack);

end.

