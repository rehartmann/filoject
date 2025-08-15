# Filoject

Filoject is a dependency injection framework for Ada, loosely inspired by frameworks
like Spring, CDI, and Dagger. It uses a custom pragma, a custom aspect, and generated code.

## Example

Package declaration greeters.ads:
```
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Greeters is

   type Message is new Unbounded_String;

   type Greeter is interface;

   function Greet (Self : Greeter) return Message is abstract;

   type Greeter_Access is access Greeter'Class;

   type Default_Greeter is new Greeter with record
      M : Message;
   end record;

   procedure Initialize (G : out Default_Greeter)
     with Inject => True;

   pragma Bind_To_Implementation (Greeter, Default_Greeter);

   overriding
   function Greet (Self : Default_Greeter) return Message;

   type Client is tagged private;

   type Client_Access is access all Client'Class;

   procedure Greet (C : Client);

   pragma Bind_To_Implementation (Client);

private

   type Client is tagged record
      G : Greeter_Access;
   end record;

end Greeters;
```

Package body greeters.adb:
```
with Ada.Text_IO; use Ada.Text_IO;

package body Greeters is

   overriding function Greet (Self : Default_Greeter) return Message is
   begin
      return Self.M;
   end Greet;

   procedure Initialize (G : out Default_Greeter) is
   begin
      G.M := To_Unbounded_String ("Hi there!");
   end Initialize;

   procedure Greet (C : Client) is
   begin
      Put_Line (To_String (C.G.Greet));
   end Greet;

end Greeters;
```

Main program greet.adb:

```
with Filoject.Provisioning;
with Greeters; use Greeters;

procedure Greet is
   function Get is new Filoject.Get (Client, Client_Access);

   C : constant Client_Access := Get (Filoject.Get_Application_Context);
begin
   C.Greet;
end Greet;
```

The instantiated generic procedure `Get` creates objects of type `Default_Greeter` and `Client`,
injects a pointer to the `Default_Greeter` object into the `Client` object,
and returns a pointer to the `Client` object.

Generating the code for instantiation and dependency injection:

```
fjgen -d generated src/greeters.ads
```

After building the main program, running the program prints the output:

```
$ bin/greet
Hi there!
```

## Bindings

A binding maps a (often abstract) tagged package-level source type to a non-abstract derived package-level
target type (the implementation type).

Bindings are specified using the pragma `Bind_To_Implementation`:

```
type S is abstract tagged null record;

type T is new S with record
   ...
end record;

pragma Bind_To_Implementation (S, T)

```

Source and target type can be the same type. In this case, the Binding pragma may take only one argument. 

A pointer to an object of an implementation type can be dynamically obtained by instantiating
the generic function `Get` for a source type and an access type of that source type
and calling `Get` with a context reference as an argument.

Filoject will then determine a target type. If an object of that target type is already present
in the context, it will be returned.

Otherwise Filoject will instantiate and initialize the target type, including instantiating and initializing dependent types if necessary, and return a pointer to the object instantiated.

If no target type is found or the target type is ambiguous, a Resolution_Error will be raised.

Bindings can be created programmatically using `Bind` and retrieved using `Get_Binding`.

However, `Bind` can only be used to add bindings (or change existing bindings) to already existing
implementations because generated code is required for the creation of objects.

## Contexts and scopes

Filoject provides two scopes: `Application` and `Dynamic`.

The default context is the application context. A reference to the application context
can be obtained using the function `Get_Application_Context`.
The application context has the scope `Application`.

Other contexts can be dynamically created using `Create_Context` and destroyed using `Destroy_Context`.
These contexts have the scope `Dynamic`.

An object which belongs to a dynamic context cannot be injected into an object which has application scope because this would result in a dangling pointer when the dynamic context is destroyed.

The scope of types managed by Filoject and the scope of contexts can be obtained using the `Get_Scope` functions.

By default, a type has scope `Application`. The scope of a type can be changed to `Dynamic` by calling
`Set_Scope`. This allows an object to be inserted into a context with scope `Dynamic`.

## Component injection

A component eligible for component injection must meet the following criteria:

* It is a component of a tagged type which is the target of a binding.
* The type of the component is a class-wide access type which designates a tagged type.

## Initalizer procedures

An initializer procedure is a package-level procedure which meets the following criteria:

* It has a least one parameter.
* The type of the first parameter is an access type of a tagged type which is the target type or subtype
  of a target type of a binding and defined in the same package as the procedure.
* The mode of the first parameter is `in out` or `out`.
* The types of the following parameters are access types of tagged types which are the source of a binding.
* It is is marked using an `Inject` aspect with a value of `True`.

A type can have more than one initializer. The initializers are called after the components have
been injected (if the mananged type has any components eligible for component injection).
They are called in an unspecified order.

## API

The library package Filoject has the following declaration:

```
package Filoject is

   type Context_Ref is private;

   type Scope is (Application, Dynamic);

   generic
      type T is abstract tagged private;
      type T_Access is access T'Class;
   function Get (Context : Context_Ref) return T_Access;

   function Get_Application_Context return Context_Ref;

   procedure Clear_Application_Context;

   procedure Bind (Source : Ada.Tags.Tag;
                   Implementation : Ada.Tags.Tag);

   function Get_Binding (T : Ada.Tags.Tag)
                         return Ada.Tags.Tag;

   function Get_Scope (T : Ada.Tags.Tag)
                       return Scope;

   procedure Set_Scope (T : Ada.Tags.Tag;
                        S : Scope);

   function Get_Scope (Context : Context_Ref)
                       return Scope;

   function Create_Context return Context_Ref;

   procedure Destroy_Context (Context : in out Context_Ref);

   Resolution_Exception : exception;

private

   ...

end Filoject;
```

## The code generator fjgen

Dependency injection by Filoject requires code generated by the code generator fjgen
which is found in the `filoject_gen` directory.

Fjgen reads .ads files and generates the following files:

* `filoject-provisioning.ads`  and `filoject-provisioning.adb`
* `<package>-filoject_initializers.ads` and `<package>-filoject_initializers.adb` for
  each .ads file read by the generator.

These must be compiled and linked to the application. The application must contain a `with Filoject.Provisioning;` statement to make the generated code available.

Fjgen uses libadalang to analyse the .ads files.

### Synopsis

`fjgen [-h] [--version] [-v] [-d dir] [file]...`

### Options

-h  
  Displays help and exit

--version  
  Display version and exit

-v  
  Display information about managed types, bindings, access types, and initializers.

-d dir  
  Specifies the destination directory for generated files
