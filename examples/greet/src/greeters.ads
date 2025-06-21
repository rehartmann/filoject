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
