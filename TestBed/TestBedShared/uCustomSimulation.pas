unit uCustomSimulation;

interface

uses
  uTestBed;

type
  TSimulationTime = record
    SimulationTime: double;
  end;

  TSimulationUserInput = record
    KeyboardKey: byte;
  end;

  TCustomSimulationSettings = record
    SettingsRec: TSettings;
  end;

  TCustomSimulation = class(TTester)
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create; override;
    destructor Destroy; override;
    procedure Step(var settings: TCustomSimulationSettings; timeStep: TSimulationTime); virtual;
    procedure UserInput(aUserInput: TSimulationUserInput); virtual;
  end;

  TCustomSimulationClass = class of TCustomSimulation;

procedure RegisterSimulationEntry(AName: string; ASimulationClass: TCustomSimulationClass);

implementation

procedure RegisterSimulationEntry(AName: string; ASimulationClass: TCustomSimulationClass);
begin
  RegisterTestEntry(AName, ASimulationClass);
end;

{ TCustomSimulation }

class constructor TCustomSimulation.Create;
begin
  inherited;

  // initialize members of the class that are global to all actual siumulation instances
end;

constructor TCustomSimulation.Create;
begin
  inherited;

end;

class destructor TCustomSimulation.Destroy;
begin

  inherited;
end;

destructor TCustomSimulation.Destroy;
begin

  inherited;
end;

procedure TCustomSimulation.Step(var settings: TCustomSimulationSettings;
  timeStep: TSimulationTime);
begin
  inherited Step(settings.SettingsRec, timeStep.SimulationTime);
end;

procedure TCustomSimulation.UserInput(aUserInput: TSimulationUserInput);
begin

end;

end.
