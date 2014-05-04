unit uFormDump;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo;

type
  TFormDump = class(TForm)
    MemoDump: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDump: TFormDump;

implementation

{$R *.fmx}

end.
