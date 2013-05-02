demo of the MVC and observer patterns in lazarus

This is a generic implementation of the Observer design pattern for free pascal.

The `uModel` unit provides a class, `TModel`, that you can use as an interface
delegate to make any TObject into an observable subject. (You can also just
subclass it.)

It also provides `GModel<t>`, a genericized version of TModel that contains a
property, `value : T`, which notifies observers when that value is is changed.

The notification is done through `message` methods, which means any `TObject`
can act as an observer and receive callbacks.

The rest of the repository is a Lazarus project that demonstrates how to use
observers to implement the model-view-controller pattern.

The main form provides the view, which is just a little progress bar turned on
its side to act like a volume meter.

Clicking the "settings" button opens the Controller, which in this case is just
a separate form that has a slider control.

Because the main form registers itself as an Observer of the model that the
controller is managing, moving the slider on the controller causes the main
form's view to update in real time.

Anyway, it was kind of fun to make and demonstrates some of the more interesting
features of ObjFPC, so I thought I'd share. :)


*NOTE:* This is a rather simple demo, meant to illustrate the basic
MVC concepts and experiment with various features of free pascal.

FPC ships with a much more advanced observer implementation, =fpobserver=

* http://svn.freepascal.org/svn/fpc/trunk/packages/fcl-base/src/fpobserver.pp



