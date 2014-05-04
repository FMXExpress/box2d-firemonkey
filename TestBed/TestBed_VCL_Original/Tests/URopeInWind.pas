unit URopeInWind;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, UPhysics2DHelper, UPhysics2DControllers,
   Math, Classes, SysUtils;

{$IFDEF CONTROLLERS}
type
   TRopeInWind = class(TTester)
   private
      procedure CreateRopes;
   public
      wind: TVector2;
      ropebodies: TList;
      maxseg: Single;
      wc: Tb2WindController;
      grounds: array[0..2] of Tb2Body;

      constructor Create; override;
      destructor Destroy; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;
{$ENDIF}

implementation
{$IFDEF CONTROLLERS}
const // Feature points for ropes, and you may modify them as you wish.
   Ropes: array[0..2] of array[0..4] of TPointF = (
   ((x: -3; y: 5), (x: 0; y: 4), (x: 3; y: 3), (x: 6; y: 4), (x: 9; y: 5)),
   ((x: -3; y: 20), (x: 0; y: 19), (x: 3; y: 18), (x: 6; y: 19), (x: 9; y: 20)),
   ((x: -3; y: 35), (x: 0; y: 34), (x: 3; y: 33), (x: 6; y: 34), (x: 9; y: 35))
   );

// wind controller ask the user for wind-force before every step. Modify this as you wish.
function WindCallback: TVector2;
begin
   Result := TRopeInWind(Test).wind;
end;

{ TRopeInWind }

constructor TRopeInWind.Create;
var
   i: Integer;
   bd: Tb2BodyDef;
   shape: Tb2EdgeShape;
begin
   inherited;
   ropebodies := TList.Create;
   maxseg := 1.6;
   wind := MakeVector(100, 0);

   wc := Tb2WindController.Create;
   wc.Callback := WindCallback;
   m_world.AddController(wc);

   begin
      bd := Tb2BodyDef.Create;
      shape := Tb2EdgeShape.Create;
      for i := 0 to 2 do
      begin
         shape.SetVertices(MakeVector(Ropes[i][0].x, Ropes[i][0].y - 0.1),
            MakeVector(Ropes[i][0].x, Ropes[i][0].y + 0.1));
         grounds[i] := m_world.CreateBody(bd, False);
         grounds[i].CreateFixture(shape, 0.0, False);
      end;
      bd.Free;
      shape.Free;
   end;

   CreateRopes;
end;

destructor TRopeInWind.Destroy;
begin
   ropebodies.Free;
   wc.Free;
   inherited;
end;

procedure TRopeInWind.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Turn on "Center of Masses" and use +/- to control rope smooth.');
   DrawText('Use ASDW to control wind direction and magnitude.');
   DrawText(Format('Wind force: (X: %f, Y: %f)', [wind.x, wind.y]));
   NextLine;
   DrawText('Note that too smooth or too large wind force may break the rope.');
end;

procedure TRopeInWind.Keyboard(key: Byte);
begin
   inherited;
   case key of
      187{+}:
         if maxseg < 4.0 then
         begin
            maxseg := maxseg + 0.2;
            CreateRopes;
         end;
      189{-}:
         if maxseg > 0.6 then
         begin
            maxseg := maxseg - 0.2;
            CreateRopes;
         end;
      Ord('A'): if wind.x > -100 then wind.x := wind.x - 20;
      Ord('S'): if wind.y > -100 then wind.y := wind.y - 20;
      Ord('D'): if wind.x < 100 then wind.x := wind.x + 20;
      Ord('W'): if wind.y < 100 then wind.y := wind.y + 20;
   end;
end;

procedure TRopeInWind.CreateRopes;
var
   i: Integer;
begin
   if Assigned(wc) then
   begin
      wc.Clear;
      for i := 0 to ropebodies.Count - 1 do
         m_world.DestroyBody(Tb2Body(ropebodies[i]));
   end;

   ropebodies.Clear;
   for i := 0 to 2 do
      BuildRope(@Ropes[i][0], 5, m_world, ropebodies, grounds[i], nil, 0.1, maxseg);
   for i := 0 to ropebodies.Count - 1 do
      wc.AddBody(Tb2Body(ropebodies[i])); // Add bodies to wind controller
end;

initialization
   RegisterTestEntry('Rope In Wind', TRopeInWind);
{$ENDIF}
end.

