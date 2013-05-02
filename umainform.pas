unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, StdCtrls, uCtrlForm, uModel;

type

  { TMainForm }
  TMainForm = class(TForm)
    SettingsButton: TButton;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;


var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.SettingsButtonClick(Sender: TObject);
  begin
    if SettingsButton.Enabled then
      begin
      SettingsButton.Enabled := False;
      CtrlForm.Show;
      end;
  end;

procedure TMainForm.FormCreate(Sender: TObject);
  begin
    CtrlForm.percent.asSubject.addObserver(Self);
  end;

end.

