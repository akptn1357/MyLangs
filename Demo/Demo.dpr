program Demo;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {frmDilDemoX};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDilDemo, frmDilDemo);
  Application.Run;
end.
