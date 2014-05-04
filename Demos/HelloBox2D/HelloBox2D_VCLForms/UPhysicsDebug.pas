unit UPhysicsDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UPhysics2D, UPhysics2DTypes, StdCtrls, ExtCtrls;

type
  TfrmDebug = class(TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDebug: TfrmDebug;

implementation
{$R *.dfm}

var
   world: Tb2World;
   groundBody: Tb2Body;

procedure TfrmDebug.FormCreate(Sender: TObject);
var
   gravity: TVector2;
   groundDef, bodyDef: Tb2BodyDef;
   groundShapeDef, shapeDef: Tb2PolygonShape;
   fd: Tb2FixtureDef;
   body: Tb2Body;
   timeStep: PhysicsFloat;
   viterations, piterations: Int32;
   i: Integer;
   position: TVector2;
   angle: PhysicsFloat;
begin
   //ReportMemoryLeaksOnShutdown := True;
   SetValue(gravity, 0, -10);
   world := Tb2World.Create(gravity);

   groundDef := Tb2BodyDef.Create;
   SetValue(groundDef.position, 0, -10);
   groundBody := world.CreateBody(groundDef);

   groundShapeDef := Tb2PolygonShape.Create;
   groundShapeDef.SetAsBox(50.0, 10.0);

   // Add the ground shape to the ground body.
   groundBody.CreateFixture(groundShapeDef, 0.0);

   // Define the dynamic body. We set its position and call the body factory.
   bodyDef := Tb2BodyDef.Create;
   bodyDef.bodyType := b2_dynamicBody;
   SetValue(bodyDef.position, 0.0, 4.0);
   body := world.CreateBody(bodyDef);

   // Define another box shape for our dynamic body.
   shapeDef := Tb2PolygonShape.Create;
   shapeDef.SetAsBox(1.0, 1.0);

   fd := Tb2FixtureDef.Create;
   fd.shape := shapeDef;
   // Set the box density to be non-zero, so it will be dynamic.
   fd.density := 1.0;

   // Override the default friction.
   fd.friction := 0.3;

   // Add the shape to the body.
   body.CreateFixture(fd);

   // Prepare for simulation. Typically we use a time step of 1/60 of a
   // second (60Hz) and 10 iterations. This provides a high quality simulation
   // in most game scenarios.
   timeStep := 1.0 / 60.0;
	 viterations := 6;
	 piterations := 2;

   ListBox1.Items.BeginUpdate;
   for i := 0 to 59 do
   begin
      // Instruct the world to perform a single step of simulation.
      // It is generally best to keep the time step and iterations fixed.
      world.Step(timeStep, viterations, piterations);

      // Now print the position and angle of the body.
      position := body.GetPosition;
      angle := body.GetAngle;

      ListBox1.Items.Add(Format('%d  x: %f  y: %f  a: %f', [i, position.x, position.y, angle]));
   end;
   ListBox1.Items.EndUpdate;

   world.Free;
end;

end.
