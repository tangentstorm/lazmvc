{ unit that implements the observer pattern with generics  }
unit uModel;

{$mode objfpc}{$H+}{$M+}

interface

uses Classes, SysUtils, contnrs;

{Copyright (c) 2013 Michal J Wallace

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.}

{%region -- Protocol --------------------------------------------------------- }
type
  { IObservable is the main interface that clients care about. }
  IObservable = interface
    procedure AddObserver(obj: TObject);
    procedure removeObserver(obj: TObject);
  end;

  { Notifications are done through message methods and TMessage }
  generic GMessage<t> = record
    id   : string[16];
    data : t;
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
    procedure Notify( var msg );
  public
    property asSubject: IObservable read fAsSubject implements IObservable;
  end;

  { GModel is a generic Model that notifies when its .value is changed. }
  generic GModel<t> = class(TModel)
  private
    _value : t;
    procedure SetValue( val : t );
    type TTMessage = specialize GMessage<t>;
  public
    constructor Create( val : t );
    property value : t read _value write SetValue;
  end;
{%endregion}

{%region -- Example : TPercentModel -------------------------------------------}
TPercent = 0 .. 100;
TPercentModel   = specialize GModel<TPercent>;
TPercentMessage = specialize GMessage<TPercent>;
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
    procedure Notify( var msg );
  end;

constructor _TSubject.Create( subj : TObject );
  begin
    fSubject := subj;
    fObservers := TObjectList.Create;
  end;

procedure _TSubject.AddObserver(obj: TObject);
  begin
    if fObservers.IndexOf(obj) = -1 then fObservers.Add(obj);
    writeln('there are now ', fObservers.Count, ' observers');
  end;

procedure _TSubject.RemoveObserver(obj : TObject);
  begin
    fObservers.Extract(obj);
  end;

procedure _TSubject.Notify( var msg );
  var ob : Pointer;
  begin
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

procedure TModel.notify( var msg );
  begin
    (fAsSubject as _TSubject).Notify( msg );
  end;
{%endregion}

{%region GModel}
constructor GModel.Create( val : t );
  begin
    inherited Create;
    _value := val;
  end;

procedure GModel.SetValue( val : t );
  var msg : TTMessage;
  begin
    _value := val;
    msg.id := kSubjectChanged;
    msg.data := self.value;
    notify( msg )
  end;
{%endregion}

end.
