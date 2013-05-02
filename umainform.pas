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
    volumeMeter: TProgressBar;
    procedure SettingsButtonClick(Sender: TObject);
  public
    procedure OnSubjectChange(var msg: TPercentMessage); message kSubjectChanged;
    procedure OnCtrlFormClose(Sender: TObject; var CloseAction: TCloseAction);
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
       // it's okay to add the observer multiple times, so we'll do it here.
      CtrlForm.percent.asSubject.addObserver(Self);
      CtrlForm.OnClose := @OnCtrlFormClose;
      SettingsButton.Enabled := False;
      CtrlForm.Show;
      end;
  end;

procedure TMainForm.OnCtrlFormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    CloseAction := caHide;
    SettingsButton.Enabled := True;
  end;

procedure TMainForm.OnSubjectChange(var msg: TPercentMessage);
  begin
    volumeMeter.Position := msg.data
  end;

end.

