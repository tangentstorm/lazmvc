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
    procedure FormCreate(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
  public
    procedure OnSubjectChange(var msg: TMessage); message kSubjectChanged;
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

procedure TMainForm.OnSubjectChange(var msg: TMessage);
  begin
    volumeMeter.Position := (msg.Data as TPercentModel).value;
  end;

procedure TMainForm.FormCreate(Sender: TObject);
  begin
    CtrlForm.percent.addObserver(Self);
  end;

end.

