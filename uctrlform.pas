unit uCtrlForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, uModel;

type

  { TCtrlForm }

  TCtrlForm = class(TForm)
    percent: TPercentModel;
    DoneButton: TButton;
    TrackBar1: TTrackBar;
    procedure DoneButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  CtrlForm: TCtrlForm;

implementation

{$R *.lfm}

{ TCtrlForm }

procedure TCtrlForm.TrackBar1Change(Sender: TObject);
  begin
    percent.Value := TrackBar1.Position;
  end;

procedure TCtrlForm.FormCreate(Sender: TObject);
  begin
    percent := TPercentModel.Create(5);
  end;

procedure TCtrlForm.DoneButtonClick(Sender: TObject);
  begin
    Close;
  end;

end.

