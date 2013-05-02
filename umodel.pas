unit uModel;

{$mode objfpc}{$H+}{$M+}

interface

uses Classes, SysUtils, contnrs;

const kChanged = 'CHANGED';

type

  { IObservable is the main interface that clients care about. }
  IObservable = interface
    procedure AddObserver(obj: TObject);
    procedure removeObserver(obj: TObject);
  end;

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

  { For example, here is a model based an a "percent" type }
  TPercent = 0 .. 100;
  TPercentModel = specialize GModel<TPercent>;

implementation

{%region private types}
type
  TMessage = record
    id   : string;
    data : TObject;
  end;
  _TSubject = class(TInterfacedObject, IObservable)
  protected
    fObservers: TObjectList;
    fSubject  : TObject;
    constructor Create(subj : TObject);
    procedure AddObserver(obj: TObject);
    procedure RemoveObserver(obj: TObject);
    procedure Notify( msgid : Cardinal );
    destructor Destroy;
  end;
{%endregion}

{%region _TSubject}
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
    msg.id := kChanged; msg.data := self;
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
    _TSubject(fAsSubject).Notify( 1 );
  end;
{%endregion}

{%region GModel}
constructor GModel.Create( val : t );
  begin
    _value := val;
  end;

procedure GModel.SetValue( val : t );
  begin
    _value := val;
    notify
  end;
{%endregion}

end.
