unit UDynamicTreeTest;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, SysUtils, Math;

const
   e_actorCount = 128;

type
   PActor = ^TActor;
   TActor = record
      aabb: Tb2AABB;
      fraction: PhysicsFloat;
      overlap: Boolean;
      proxyId: Int32;
   end;

   TDynamicTreeTest = class(TTester)
   private
      m_worldExtent, m_proxyExtent: PhysicsFloat;
      m_tree: Tb2DynamicTree;
      m_queryAABB: Tb2AABB;
      m_rayCastInput: Tb2RayCastInput;
      m_rayCastOutput: Tb2RayCastOutput;
      m_rayActor: PActor;
      m_actors: array[0..e_actorCount - 1] of TActor;
      m_stepCount: Int32;
      m_automated: Boolean;

      procedure GetRandomAABB(var aabb: Tb2AABB);
      procedure MoveAABB(var aabb: Tb2AABB);
      procedure CreateProxy;
      procedure DestroyProxy;
      procedure MoveProxy;
      procedure Action;
      procedure Query;
      procedure RayCast;

   public
      constructor Create; override;
      destructor Destroy; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;

      function QueryCallback(proxyId: Int32): Boolean; override;
      function RayCastCallback(const input: Tb2RayCastInput; proxyId: Int32): PhysicsFloat; override;
   end;

implementation

{ TDynamicTreeTest }

constructor TDynamicTreeTest.Create;
var
   i: Integer;
begin
   inherited;
   m_tree := Tb2DynamicTree.Create;
   m_worldExtent := 15.0;
   m_proxyExtent := 0.5;

   for i := 0 to e_actorCount - 1 do
      with m_actors[i] do
      begin
         GetRandomAABB(aabb);
         proxyId := m_tree.CreateProxy(aabb, @m_actors[i]);
      end;

   m_stepCount := 0;

   SetValue(m_queryAABB.lowerBound, -3.0, -4.0 + m_worldExtent);
   SetValue(m_queryAABB.upperBound, 5.0, 6.0 + m_worldExtent);
   SetValue(m_rayCastInput.p1, -5.0, 5.0 + m_worldExtent);
   SetValue(m_rayCastInput.p2, 7.0, -4.0 + m_worldExtent);
   //m_rayCastInput.p1.Set(0.0, 2.0 + m_worldExtent);
   //m_rayCastInput.p2.Set(0.0, -2.0 + m_worldExtent);
   m_rayCastInput.maxFraction := 1.0;
   m_automated := False;
end;

destructor TDynamicTreeTest.Destroy;
begin
   m_tree.Free;
   inherited;
end;

procedure TDynamicTreeTest.GetRandomAABB(var aabb: Tb2AABB);
var
   w: TVector2;
begin
   SetValue(w, 2.0 * m_proxyExtent, 2.0 * m_proxyExtent);
   //aabb.lowerBound.x := -m_proxyExtent;
   //aabb.lowerBound.y := -m_proxyExtent + m_worldExtent;
   aabb.lowerBound.x := RandomFloat(-m_worldExtent, m_worldExtent);
   aabb.lowerBound.y := RandomFloat(0.0, 2.0 * m_worldExtent);
   {$IFDEF OP_OVERLOAD}
   aabb.upperBound := aabb.lowerBound + w;
   {$ELSE}
   aabb.upperBound := Add(aabb.lowerBound, w);
   {$ENDIF}
end;

procedure TDynamicTreeTest.MoveAABB(var aabb: Tb2AABB);
var
   d, c0, min, max, c: TVector2;
begin
   d.x := RandomFloat(-0.5, 0.5);
   d.y := RandomFloat(-0.5, 0.5);
   //d.x := 2.0;
   //d.y := 0.0;
   {$IFDEF OP_OVERLOAD}
   aabb.lowerBound.AddBy(d);
   aabb.upperBound.AddBy(d);
   {$ELSE}
   AddBy(aabb.lowerBound, d);
   AddBy(aabb.upperBound, d);
   {$ENDIF}

   c0 := b2MiddlePoint(aabb.lowerBound, aabb.upperBound);
   SetValue(min, -m_worldExtent, 0.0);
   SetValue(max, m_worldExtent, 2.0 * m_worldExtent);
   c := b2Clamp(c0, min, max);
   {$IFDEF OP_OVERLOAD}
   aabb.lowerBound.AddBy(c - c0);
   aabb.upperBound.AddBy(c - c0);
   {$ELSE}
   AddBy(aabb.lowerBound, Subtract(c, c0));
   AddBy(aabb.upperBound, Subtract(c, c0));
   {$ENDIF}
end;

procedure TDynamicTreeTest.CreateProxy;
var
   i: Integer;
begin
   for i := 0 to e_actorCount - 1 do
      with m_actors[RandomRange(0, e_actorCount - 1)] do
         if proxyId = b2_nullNode then
         begin
            GetRandomAABB(aabb);
            proxyId := m_tree.CreateProxy(aabb, @m_actors[i]);
            Exit;
         end;
end;

procedure TDynamicTreeTest.DestroyProxy;
var
   i: Integer;
begin
   for i := 0 to e_actorCount - 1 do
      with m_actors[RandomRange(0, e_actorCount - 1)] do
         if proxyId = b2_nullNode then
         begin
            m_tree.DestroyProxy(proxyId);
            proxyId := b2_nullNode;
            Exit;
         end;
end;

procedure TDynamicTreeTest.MoveProxy;
var
   i: Integer;
   aabb0: Tb2AABB;
   displacement: TVector2;
begin
   for i := 0 to e_actorCount - 1 do
   begin
      with m_actors[RandomRange(0, e_actorCount - 1)] do
      begin
         if proxyId = b2_nullNode then
            Continue;

         aabb0 := aabb;
         MoveAABB(aabb);
         {$IFDEF OP_OVERLOAD}
         displacement := aabb.GetCenter - aabb0.GetCenter;
         {$ELSE}
         displacement := Subtract(GetCenter(aabb), GetCenter(aabb0));
         {$ENDIF}
         m_tree.MoveProxy(proxyId, aabb, displacement);
         Exit;
      end;
   end;
end;

procedure TDynamicTreeTest.Action;
begin
   case RandomRange(0, 19) of
      0: CreateProxy;
      1: DestroyProxy;
   else
      MoveProxy;
   end;
end;

procedure TDynamicTreeTest.Query;
var
   i: Integer;
   //overlap: Boolean;
begin
   m_tree.Query(Self, m_queryAABB);

   for i := 0 to e_actorCount - 1 do
   begin
      if m_actors[i].proxyId = b2_nullNode then
         Continue;

      //overlap := b2TestOverlap(m_queryAABB, m_actors[i].aabb);
      //B2_NOT_USED(overlap);
      //b2Assert(overlap == m_actors[i].overlap);
   end;
end;

function TDynamicTreeTest.QueryCallback(proxyId: Int32): Boolean;
var
   actor: PActor;
begin
   actor := PActor(m_tree.GetUserData(proxyId));
   actor^.overlap := b2TestOverlap(m_queryAABB, actor^.aabb);
   Result := True;
end;

function TDynamicTreeTest.RayCastCallback(const input: Tb2RayCastInput; proxyId: Int32): PhysicsFloat;
var
   actor: PActor;
   output: Tb2RayCastOutput;
begin
   actor := PActor(m_tree.GetUserData(proxyId));

   {$IFDEF OP_OVERLOAD}
   if actor^.aabb.RayCast(output, input) then
   {$ELSE}
   if UPhysics2D.RayCast(actor^.aabb, output, input) then
   {$ENDIF}
   begin
      m_rayCastOutput := output;
      m_rayActor := actor;
      m_rayActor^.fraction := output.fraction;
      Result := output.fraction;
      Exit;
   end;

   Result := input.maxFraction;
end;

procedure TDynamicTreeTest.RayCast;
var
   i: Integer;
   input: Tb2RayCastInput;
   output, bruteOutput: Tb2RayCastOutput;
   bruteActor: PActor;
begin
   m_rayActor := nil;
   input := m_rayCastInput;

   // Ray cast against the dynamic tree.
   m_tree.RayCast(Self, input);

   // Brute force ray cast.
   bruteActor := nil;
   for i := 0 to e_actorCount - 1 do
   begin
      if m_actors[i].proxyId = b2_nullNode then
         Continue;

      {$IFDEF OP_OVERLOAD}
      if m_actors[i].aabb.RayCast(output, input) then // hit
      {$ELSE}
      if UPhysics2D.RayCast(m_actors[i].aabb, output, input) then // hit
      {$ENDIF}
      begin
         bruteActor := @m_actors[i];
         bruteOutput := output;
         input.maxFraction := output.fraction;
      end;
   end;

   if Assigned(bruteActor) then
      //b2Assert(bruteOutput.fraction == m_rayCastOutput.fraction);
end;

procedure TDynamicTreeTest.Step(var settings: TSettings; timeStep: PhysicsFloat);
const
   c_color: RGBA = (0.9, 0.9, 0.9, 1.0);
   c_color2: RGBA = (0.7, 0.7, 0.7, 1.0);
   c_color3: RGBA = (0.2, 0.9, 0.2, 1.0);
   c_color4: RGBA = (0.9, 0.2, 0.2, 1.0);
   c_color5: RGBA = (0.2, 0.2, 0.9, 1.0);

var
   i: Integer;
   actionCount, height: Int32;
   actor: PActor;
   color: RGBA;
begin
   //B2_NOT_USED(settings);

   m_rayActor := nil;
   for i := 0 to e_actorCount - 1 do
   begin
      m_actors[i].fraction := 1.0;
      m_actors[i].overlap := False;
   end;

   if m_automated then
   begin
      actionCount := b2Max(1, e_actorCount shr 2);
      for i := 0 to actionCount - 1 do
         Action;
   end;

   Query;
   RayCast;

   for i := 0 to e_actorCount - 1 do
   begin
      actor := @m_actors[i];
      if actor^.proxyId = b2_nullNode then
        Continue;

      color := c_color;
      if (actor = m_rayActor) and actor^.overlap then
      begin
         color[1] := 0.6;
         color[2] := 0.6;
      end
      else if actor = m_rayActor then
      begin
         color[0] := 0.6;
         color[2] := 0.6;
      end
      else if actor^.overlap then
      begin
         color[0] := 0.6;
         color[1] := 0.6;
      end;
      m_debugDraw.DrawAABB(actor^.aabb, color);
   end;

   m_debugDraw.DrawAABB(m_queryAABB, c_color2);
   m_debugDraw.DrawSegment(m_rayCastInput.p1, m_rayCastInput.p2, c_color2);

   m_debugDraw.DrawPoint(m_rayCastInput.p1, 6.0, c_color3);
   m_debugDraw.DrawPoint(m_rayCastInput.p2, 6.0, c_color4);

   if Assigned(m_rayActor) then
      {$IFDEF OP_OVERLOAD}
      m_debugDraw.DrawPoint(m_rayCastInput.p1 + m_rayActor.fraction *
         (m_rayCastInput.p2 - m_rayCastInput.p1), 6.0, c_color5);
      {$ELSE}
      m_debugDraw.DrawPoint(Add(m_rayCastInput.p1, Multiply(Subtract(m_rayCastInput.p2,
         m_rayCastInput.p1), m_rayActor.fraction)), 6.0, c_color5);
      {$ENDIF}

   height := m_tree.GetHeight;
   DrawText(Format('dynamic tree height = %d', [height]));
   Inc(m_stepCount);
end;

procedure TDynamicTreeTest.Keyboard(key: Byte);
begin
   case key of
      Ord('A'): m_automated := not m_automated;
      Ord('C'): CreateProxy;
      Ord('D'): DestroyProxy;
      Ord('M'): MoveProxy;
   end;
end;

initialization
   RegisterTestEntry('Dynamic Tree Test', TDynamicTreeTest);
end.

