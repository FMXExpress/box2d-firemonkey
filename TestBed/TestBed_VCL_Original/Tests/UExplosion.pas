unit UExplosion;

interface
{$I ..\..\Physics2D\Physics2D.inc}

{$IFDEF CONTROLLERS}
uses
   UMain, UPhysics2DTypes, UPhysics2D, Classes, SysUtils, UPhysics2DControllers, UPhysics2DHelper;

type
   TExplosion = class(TTester)
   private
      procedure Recreate;
   public
      multiple: Single;
      bound: Boolean;
      ec: Tb2ExplosionController;
      boxes: TList;

      constructor Create; override;
      destructor Destroy; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;
{$ENDIF}
implementation
{$IFDEF CONTROLLERS}
const
   baseEnergy = 50000; // In fact you needn't set energy. The controller will pick a proper value.

{ TExplosion }

constructor TExplosion.Create;
begin
   inherited;
   boxes := TList.Create;
   ec := Tb2ExplosionController.Create;
   m_world.AddController(ec);
   multiple := 1.0;
   bound := True;
   Recreate;
end;

destructor TExplosion.Destroy;
begin
   ec.Free;
   boxes.Free;
   inherited;
end;

procedure TExplosion.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Pyramid demo is also explodable.');
   DrawText('After explosion, gravity is turned on.');
   DrawText('Press ''T'' to trigger explosion. ''R'' to reset scene after on try. ''B'' to toggle bound box.');
   DrawText(Format('Press +/- to change explosion energy. Current multiple: %f', [multiple]));
end;

procedure TExplosion.Keyboard(key: Byte);
const
   g: TVector2 = (X: 0; Y: -10);
begin
   inherited;
   case key of
      187{+}:
         begin
            if multiple < 150 then
               multiple := multiple * 1.1;
            Recreate;
            ec.Energy := baseEnergy * multiple;
         end;
      189{-}:
         begin
            multiple := multiple / 1.1;
            Recreate;
            ec.Energy := baseEnergy * multiple;
         end;
      Ord('T'):
         begin
            if not ec.Exploded then
            begin
               m_world.SetGravity(g);
               UpdateGravityText;
               ec.Trigger;
            end;
         end;
      Ord('R'):
         begin
            Recreate;
            ec.Energy := baseEnergy * multiple;
         end;
      Ord('B'):
         begin
            bound := not bound;
            Recreate;
            ec.Energy := baseEnergy * multiple;
         end;
   end;
end;

procedure TExplosion.Recreate;
var
   i, j: Integer;
   bd: Tb2BodyDef;
   sd: Tb2PolygonShape;
   body: Tb2Body;
   x, y: PhysicsFloat;
begin
   if Assigned(ec) then
   begin
      ec.Reset;
      ec.Energy := baseEnergy;
      for i := 0 to boxes.Count - 1 do
         m_world.DestroyBody(Tb2Body(boxes[i]));
   end;

   boxes.Clear;
   bd := Tb2BodyDef.Create;
   bd.bodyType := b2_dynamicBody;
   sd := Tb2PolygonShape.Create;
   sd.SetAsBox(1, 1);

   y := 10;
   for i := 0 to 10 do
   begin
      x := -10;
      for j := 0 to 10 do
      begin
         x := x + 2;
         SetValue(bd.position, x, y);
         body := m_world.CreateBody(bd, False);
         body.CreateFixture(sd, 1, False);
         ec.AddBody(body);
         boxes.Add(body);
      end;
      y := y - 2;
   end;
   sd.Free;
   bd.Free;

   if bound then
   begin
      body := BuildStaticBoundBox(100, 100, 0, 0, m_world);
      boxes.Add(body);
   end;

   m_world.SetGravity(b2Vec2_Zero);
   UpdateGravityText;
end;

initialization
   RegisterTestEntry('Explosion', TExplosion);
{$ENDIF}
end.

