unit uFormDumpM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo;

type
  TFormDump = class(TForm)
    MemoDump: TMemo;
    ToolBar1: TToolBar;
    BackBTN: TButton;
    procedure BackBTNClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDump: TFormDump;

implementation

{$R *.fmx}

uses uFormDBox2DTestBedM;

procedure TFormDump.BackBTNClick(Sender: TObject);
begin
FormDBox2DTestBedM.Show;
FormDump.Close;
end;

end.
