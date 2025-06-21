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
