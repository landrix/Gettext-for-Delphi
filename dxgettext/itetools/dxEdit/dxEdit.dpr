program dxEdit;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  LangUtils in '..\common\LangUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
