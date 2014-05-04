unit UTerrainBox;

interface
{$I ..\..\Physics2D\Physics2D.inc}

uses
   UMain, UPhysics2DTypes, UPhysics2D, UPhysics2DHelper, Math;

type
   TTerrainBox = class(TTester)
   private
      angle: Single;
      procedure CreateBox(Rotation: Single);
      procedure CreateRandomObject;
   public
      box: Tb2Body;
      m_polygons: array[0..2] of Tb2PolygonShape;
      constructor Create; override;
      destructor Destroy; override;
      procedure Step(var settings: TSettings; timeStep: PhysicsFloat); override;
      procedure Keyboard(key: Byte); override;
   end;

implementation

{ TTerrainBox }

constructor TTerrainBox.Create;
var
   i: Integer;
   vertices: Tb2PolyVertices;
   w, b, s: PhysicsFloat;
begin
   inherited;
   CreateBox(0); // Build terrain

   // Initialize shapes
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

   for i := 0 to 5 do
      CreateRandomObject;
end;

destructor TTerrainBox.Destroy;
begin
   m_polygons[0].Free;
   m_polygons[1].Free;
   m_polygons[2].Free;
   inherited;
end;

procedure TTerrainBox.Step(var settings: TSettings; timeStep: PhysicsFloat);
begin
   inherited;
   DrawText('Press ''R'' to rotate box.');
   DrawText('Press ''C'' to create an object.');
end;

procedure TTerrainBox.CreateBox(Rotation: Single);
type
   TWallPoints = array[0..3] of array[0..4] of TPointF;
const
   // Wall feature points
   Walls: TWallPoints = (
   ((x: -20; y: -15), (x: -10; y: -17), (x: 0; y: -18), (x: 10; y: -12), (x: 20; y: -15)),
   ((x: -20; y: -15), (x: -18; y: -11), (x: -23; y: 0), (x: -24; y: 7.5), (x: -20; y: 15)),
   ((x: -20; y: 15), (x: -10; y: 12), (x: 0; y: 18), (x: 10; y: 15), (x: 20; y: 18)),
   ((x: 20; y: -15), (x: 23; y: -7.5), (x: 17; y: 0), (x: 23; y: 7.5), (x: 20; y: 18))
   );
var
   i, j: Integer;
   bd: Tb2BodyDef;
   pts: TWallPoints;
   s, c: Single;
begin
   angle := Rotation;
   if Assigned(box) then
      m_world.DestroyBody(box);
   // Rotate terrain festure points
   pts := Walls;
   s := Sin(Rotation);
   c := Cos(Rotation);
   for i := 0 to 3 do
      for j := 0 to 4 do
         with pts[i, j] do
         begin
            x := c * Walls[i, j].x - s * Walls[i, j].y;
            y := c * Walls[i, j].y + s * Walls[i, j].x;
         end;

   bd := Tb2BodyDef.Create;
   box := m_world.CreateBody(bd);
   for i := 0 to 3 do
      BuildEdgeShapeCurve(@pts[i][0], 5, box, 0.1);
end;

procedure TTerrainBox.CreateRandomObject;
var
   idx: Int32;
   bodyDef: Tb2BodyDef;
   shape: Tb2PolygonShape;
   cshape: Tb2CircleShape;
   fd: Tb2FixtureDef;
   body: Tb2Body;
begin
   idx := RandomRange(0, 6);
   case idx of
      0..2:
         begin
            bodyDef := Tb2BodyDef.Create;
            bodyDef.bodyType := b2_dynamicBody;
            fd := Tb2FixtureDef.Create;
            fd.shape := m_polygons[idx];
            fd.friction := 0.3;
            fd.density := 20.0;
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
            fd.friction := 0.3;
            fd.density := 20.0;
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
            fd.friction := 0.3;
            fd.density := 20.0;
            cshape.m_radius := RandomFloat(0.6, 1.5);
            SetValue(bodyDef.position, RandomFloat(-18, 18), RandomFloat(4, 13));
            body := m_world.CreateBody(bodyDef);
            body.CreateFixture(fd);
         end;
      5..6:
         begin
            bodyDef := Tb2BodyDef.Create;
            bodyDef.bodyType := b2_dynamicBody;
            shape := BuildHexagonShape(RandomFloat(0.5, 1.5));
            fd := Tb2FixtureDef.Create;
            fd.shape := shape;
            fd.friction := 0.3;
            fd.density := 20.0;
            SetValue(bodyDef.position, RandomFloat(-18, 18), RandomFloat(4, 13));
            body := m_world.CreateBody(bodyDef);
            body.CreateFixture(fd);
         end;
   end;
end;

procedure TTerrainBox.Keyboard(key: Byte);
begin
   inherited;
   case key of
      Ord('C'): CreateRandomObject;
      Ord('R'): CreateBox(angle + Pi / 6);
   end;
end;

initialization
   RegisterTestEntry('Terrain Box', TTerrainBox);

end.

