unit UTiles;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils, Math;

type
   TTiles = class(TTester)
   public
      m_fixtureCount: Int32;
      m_createTime: Float64;

      constructor Create; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
   end;

implementation
const
   e_count = 20;

{ TTiles }

constructor TTiles.Create;
const
   a = 0.5;
   N = 200;
   M = 10;
   deltaX: TVector2 = (X: 0.5625; Y: 1.25);
   deltaY: TVector2 = (X: 1.125; Y: 0.0);
var
   i, j: Integer;
   bd: Tb2BodyDef;
   ground, body: Tb2Body;
   shape: Tb2PolygonShape;
   position, x, y: TVector2;
begin
   inherited;
   m_fixtureCount := 0;
   bd := Tb2BodyDef.Create;
   bd.position.y := -a;
   ground := m_world.CreateBody(bd);

   shape := Tb2PolygonShape.Create;
   position.y := 0.0;
   for j := 0 to M - 1 do
   begin
      position.x := -N * a;
      for i := 0 to N - 1 do
      begin
         shape.SetAsBox(a, a, position, 0.0);
         ground.CreateFixture(shape, 0.0, False);
         Inc(m_fixtureCount);
         position.x := position.x + 2.0 * a;
      end;
      position.y := position.y - 2.0 * a;
   end;
   shape.Free;

   shape := Tb2PolygonShape.Create;
   shape.SetAsBox(a, a);
   bd := Tb2BodyDef.Create;

   x := MakeVector(-7.0, 0.75);
   for i := 0 to e_count - 1 do
   begin
      y := x;
      for j := 0 to e_count - 1 do
      begin
         bd.bodyType := b2_dynamicBody;
         bd.position := y;

//         if (i = 0) and (j = 0) then
//						bd.allowSleep = false
//         else
//						bd.allowSleep = true;

         body := m_world.CreateBody(bd, False);
         body.CreateFixture(shape, 5.0, False);
         Inc(m_fixtureCount);
         {$IFDEF OP_OVERLOAD}
         y.AddBy(deltaY);
         {$ELSE}
         AddBy(y, deltaY);
         {$ENDIF}
      end;
      {$IFDEF OP_OVERLOAD}
      x.AddBy(deltaX);
      {$ELSE}
      AddBy(x, deltaX);
      {$ENDIF}
   end;
   bd.Free;
   shape.Free;

   {$IFDEF COMPUTE_PHYSICS_TIME}
   m_createTime := GetRawReferenceTime;
   {$ENDIF}
end;

procedure TTiles.Step(var settings: TSettings; timeStep: PhysicsFloat);
var
   cm: Tb2ContactManager;
   height, leafCount, minimumNodeCount: Int32;
   minimumHeight: PhysicsFloat;
begin
   inherited;
   cm := m_world.GetContactManager;
   height := cm.m_broadPhase.GetTreeHeight;
   leafCount := cm.m_broadPhase.GetProxyCount;
   minimumNodeCount := 2 * leafCount - 1;
   minimumHeight := Ceil(Ln(minimumNodeCount) / Ln(2.0));
   DrawText('Test of dynamic tree performance in worse case scenario.');
   DrawText('I know this is slow. I hope to address this in a future update.');
   DrawText(Format('dynamic tree height := %d, min := %d', [height, Trunc(minimumHeight)]));
   {$IFDEF COMPUTE_PHYSICS_TIME}
   DrawText(Format('create time = %6.2f ms, fixture count = %d', [m_createTime, m_fixtureCount]));
   {$ENDIF}
end;

initialization
   RegisterTestEntry('Tiles(stress test)', TTiles);

end.

