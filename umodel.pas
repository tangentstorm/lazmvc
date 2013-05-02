unit uModel;

{$mode objfpc}{$H+}{$M+}

interface

uses Classes, SysUtils, contnrs;

{%region -- Protocol --------------------------------------------------------- }
type
  { IObservable is the main interface that clients care about. }
  IObservable = interface
    procedure AddObserver(obj: TObject);
    procedure removeObserver(obj: TObject);
  end;

  { Notifications are done through message methods and TMessage }
    TMessage = record
      id   : string;
      data : IObservable;
    end;

  { here is the message to listen for }
  const
    kSubjectChanged = 'SUBJECT_CHANGED';

{%endregion}

{%region -- TModel and GModel : Observable Base Classes ---------------------- }
type

  { TModel is just a normal object that's observable through composition. }
  TModel = class(TObject, IObservable)
  public
    constructor Create;
  private
    fAsSubject : IObservable;
    procedure Notify;
  public
    property asSubject: IObservable read fAsSubject implements IObservable;
  end;

  { GModel is a generic Model that notifies when its .value is changed. }
  generic GModel<t> = class(TModel)
  private
    _value : t;
    procedure SetValue( val : t );
  public
    constructor Create( val : t );
    property value : t read _value write SetValue;
  end;
{%endregion}

{%region -- Example : TPercentModel -------------------------------------------}
TPercent = 0 .. 100;
TPercentModel = specialize GModel<TPercent>;
{%endregion}


implementation

{%region _TSubject}
type
  _TSubject = class(TInterfacedObject, IObservable)
  public
    constructor Create(subj : TObject);
    destructor Destroy; override;
  protected
    fObservers: TObjectList;
    fSubject  : TObject;
    procedure AddObserver(obj: TObject);
    procedure RemoveObserver(obj: TObject);
    procedure Notify( msgid : Cardinal );
  end;

constructor _TSubject.Create( subj : TObject );
  begin
    fSubject := subj;
    fObservers := TObjectList.Create;
  end;

procedure _TSubject.AddObserver(obj: TObject);
  begin
    fObservers.Add(obj);
  end;

procedure _TSubject.RemoveObserver(obj : TObject);
  begin
    fObservers.Extract(obj);
  end;

procedure _TSubject.Notify( msgid : cardinal );
  var ob : Pointer; msg : TMessage;
  begin
    msg.id := kSubjectChanged; msg.data := self;
    for ob in fObservers do TObject(ob).DispatchStr( msg );
  end;

destructor _TSubject.Destroy;
  begin
    fObservers.Free;
  end;
{%endregion}

{%region TModel}
constructor TModel.Create;
  begin
    inherited Create;
    fAsSubject := _TSubject.Create( self )
  end;

procedure TModel.notify;
  begin
    (fAsSubject as _TSubject).Notify( 1 );
  end;
{%endregion}

{%region GModel}
constructor GModel.Create( val : t );
  begin
    inherited Create;
    _value := val;
  end;

procedure GModel.SetValue( val : t );
  begin
    _value := val;
    notify
  end;
{%endregion}

end.
