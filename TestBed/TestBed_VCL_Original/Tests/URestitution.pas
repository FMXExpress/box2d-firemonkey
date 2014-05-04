unit URestitution;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   Windows, UMain, UPhysics2DTypes, UPhysics2D, SysUtils;

type
   TRestitution = class(TTester)
   private
      k_restitution: PhysicsFloat;
   public
      body: Tb2Body;
      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TCCD }

constructor TRestitution.Create;
var
   bd: Tb2BodyDef;
   fd: Tb2FixtureDef;
   sd, sd_bottom, sd_left, sd_right: Tb2PolygonShape;
begin
   inherited;
   k_restitution := 1.0;
   begin
      bd := Tb2BodyDef.Create;
      SetValue(bd.position, 0.0, 20.0);
      body := m_world.CreateBody(bd);

      fd := Tb2FixtureDef.Create;
      fd.restitution := k_restitution;
      sd := Tb2PolygonShape.Create;
      fd.shape := sd;

      sd.SetAsBox(0.1, 10.0, MakeVector(-10.0, 0.0), 0.0);
      body.CreateFixture(fd, False, False, False);

      sd.SetAsBox(0.1, 10.0, MakeVector(10.0, 0.0), 0.0);
      body.CreateFixture(fd, False, False, False);

      sd.SetAsBox(0.1, 10.0, MakeVector(0.0, -10.0), 0.5 * Pi);
      body.CreateFixture(fd, False, False, False);

      sd.SetAsBox(0.1, 10.0, MakeVector(0.0, 10.0), -0.5 * Pi);
      body.CreateFixture(fd);
   end;
   begin
      sd_bottom := Tb2PolygonShape.Create;
      sd_bottom.SetAsBox(1.5, 0.15);

      sd_left := Tb2PolygonShape.Create;
      sd_left.SetAsBox(0.15, 2.7, MakeVector(-1.45, 2.35), 0.2);

      sd_right := Tb2PolygonShape.Create;
      sd_right.SetAsBox(0.15, 2.7, MakeVector(1.45, 2.35), -0.2);

      bd := Tb2BodyDef.Create;
      bd.bodyType := b2_dynamicBody;
      SetValue(bd.position, 0.0, 15.0);
      body := m_world.CreateBody(bd);
      body.CreateFixture(sd_bottom, 4.0, True, False);
      body.CreateFixture(sd_left, 4.0, True, False);
      body.CreateFixture(sd_right, 4.0, True);
   end;
end;

procedure TRestitution.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText(Format('Use +/- to control restitution. Present %f', [k_restitution]));
end;

procedure TRestitution.Keyboard(key: Byte);
var
   s: Tb2Fixture;
begin
   case key of
      187{+}:
         if k_restitution < 2.0 then
            k_restitution := k_restitution + 0.1;
      189{-}:
         if k_restitution > 0.0 then
            k_restitution := k_restitution - 0.1;
   else
      Exit;
   end;
   s := body.GetFixtureList;
   while Assigned(s) do
   begin
      s.Restitution := k_restitution;
      s := s.GetNext;
   end;
end;

initialization
   RegisterTestEntry('Restitution Test', TRestitution);

end.

