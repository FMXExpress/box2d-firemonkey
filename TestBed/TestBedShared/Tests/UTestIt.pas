unit UTestIt;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
  uTestBed,
  UPhysics2DTypes, UPhysics2D, SysUtils, Math;

type

   TTestIt = class(TTester)
   private
      time: PhysicsFloat;
      procedure CreateRandomObject;
   public
      wave: Boolean;
      density: PhysicsFloat;
      m_polygons: array[0..2] of Tb2PolygonShape;

      constructor Create; override;
      destructor Destroy; override;
      procedure Keyboard(key: Byte); override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;

implementation

{ TTestIt }

constructor TTestIt.Create;
var
   i: Integer;
   bd: Tb2BodyDef;
   ground: Tb2Body;
   shape: Tb2EdgeShape;
   vertices: Tb2PolyVertices;
   w, b, s: PhysicsFloat;
begin
   inherited;
   m_world.SetGravity(b2Vec2_Zero);
   UpdateGravityText;

   begin
      bd := Tb2BodyDef.Create;
      ground := m_world.CreateBody(bd);

      shape := Tb2EdgeShape.Create;

      // Floor
      shape.SetVertices(MakeVector(-20.0, -10.0), MakeVector(20.0, -10.0));
      ground.CreateFixture(shape, 0.0, False, False);

      // Left wall
      shape.SetVertices(MakeVector(-20.0, -10.0), MakeVector(-20.0, 15.0));
      ground.CreateFixture(shape, 0.0, False, False);

      // Right wall
      shape.SetVertices(MakeVector(20.0, -10.0), MakeVector(20.0, 15.0));
      ground.CreateFixture(shape, 0.0, False, False);

      // Roof
      shape.SetVertices(MakeVector(-20.0, 15.0), MakeVector(20.0, 15.0));
      ground.CreateFixture(shape, 0.0);
   end;

   shape := Tb2EdgeShape.Create;
   shape.SetVertices(MakeVector(16, 8), MakeVector(-3, -4));
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_staticBody;
   m_world.CreateBody(bd).CreateFixture(shape, 0.0);

   for i := 0 to 2 do
      m_polygons[i] := Tb2PolygonShape.Create;
   begin
      SetValue(vertices[0], -0.5, 0.0);
      SetValue(vertices[1], 0.5, 0.0);
      SetValue(vertices[2], 0.0, 1.5);
      m_polygons[0].SetVertices(@vertices[0], 3);
   end;

   begin
      SetValue(vertices[0], -1.2, 0.0);
      SetValue(vertices[1], 1.2, 0.0);
      SetValue(vertices[2], 0.0, 1.5);
      m_polygons[1].SetVertices(@vertices[0], 3);
   end;

   begin
      w := 1.0;
      b := w / (2.0 + Sqrt(2.0));
      s := Sqrt(2.0) * b;

      SetValue(vertices[0], 0.5 * s, 0.0);
      SetValue(vertices[1], 0.5 * w, b);
      SetValue(vertices[2], 0.5 * w, b + s);
      SetValue(vertices[3], 0.5 * s, w);
      SetValue(vertices[4], -0.5 * s, w);
      SetValue(vertices[5], -0.5 * w, b + s);
      SetValue(vertices[6], -0.5 * w, b);
      SetValue(vertices[7], -0.5 * s, 0.0);

      m_polygons[2].SetVertices(@vertices[0], 8);
   end;

  // CreateRandomObject;

   time := 0;
   wave := True;
   density := 0.6;
end;

destructor TTestIt.Destroy;
begin
   m_polygons[0].Free;
   m_polygons[1].Free;
   m_polygons[2].Free;
   inherited;
end;

procedure TTestIt.CreateRandomObject;
var
   idx: Int32;
   bodyDef: Tb2BodyDef;
   shape: Tb2PolygonShape;
   cshape: Tb2CircleShape;
   fd: Tb2FixtureDef;
   body: Tb2Body;
begin
   idx := RandomRange(0, 4);
   case idx of
      0..2:
         begin
            bodyDef := Tb2BodyDef.Create;
            bodyDef.bodyType := b2_dynamicBody;
            fd := Tb2FixtureDef.Create;
            fd.shape := m_polygons[idx];
            fd.density := density;
            // Override the default friction.
            fd.friction := 0.3;
            fd.restitution := 0.1;
            SetValue(bodyDef.position, RandomFloat(-18, 18), RandomFloat(4, 13));
            bodyDef.angle := RandomFloat * Pi;
            body := m_world.CreateBody(bodyDef);
            body.CreateFixture(fd, True, False);
         end;
      3:
         begin
            bodyDef := Tb2BodyDef.Create;
            bodyDef.bodyType := b2_dynamicBody;
            shape := Tb2PolygonShape.Create;
            fd := Tb2FixtureDef.Create;
            fd.shape := shape;
            fd.density := density;
            // Override the default friction.
            fd.friction := 0.3;
            fd.restitution := 0.1;
            shape.SetAsBox(RandomFloat(0.5, 1.6), RandomFloat(0.5, 1.6));
            SetValue(bodyDef.position, RandomFloat(-18, 18), RandomFloat(4, 13));
            bodyDef.angle := RandomFloat * Pi;
            body := m_world.CreateBody(bodyDef);
            body.CreateFixture(fd);
         end;
      4:
         begin
            bodyDef := Tb2BodyDef.Create;
            bodyDef.bodyType := b2_dynamicBody;
            cshape := Tb2CircleShape.Create;
            fd := Tb2FixtureDef.Create;
            fd.shape := cshape;
            fd.density := density;
            // Override the default friction.
            fd.friction := 0.3;
            fd.restitution := 0.1;
            cshape.m_radius := RandomFloat(0.6, 1.3);
            SetValue(bodyDef.position, RandomFloat(-18, 18), RandomFloat(4, 13));
            body := m_world.CreateBody(bodyDef);
            body.CreateFixture(fd);
         end;
   end;
end;

procedure TTestIt.Keyboard(key: Byte);
begin
   case key of
      Ord('C'): CreateRandomObject;
      187{+}:
         if density < 3.0 then
            density := density + 0.1;
      189{-}:
         if density > 0.100001 then
            density := density - 0.1;
   end;
end;

procedure TTestIt.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin


   inherited;
   DrawText('Press +/- to change density of new objects.');
   DrawText('Press ''C'' to create an object. ''W'' to toggle wave.');

end;

initialization
   RegisterTestEntry('A Test', TTestIt);
end.

