unit UGame2D;

interface

uses
  UGame,
  System.Types,
  FMX.Graphics;


type
  TGame2D = class(TGame)
  public
    procedure DoRender(ACanvas: TCanvas; ARect: TRectF); virtual; abstract;
  end;

implementation

end.
