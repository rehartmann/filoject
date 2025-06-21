with Filoject.Provisioning;
with Greeters; use Greeters;

procedure Greet is
   function Get is new Filoject.Get (Client, Client_Access);

   C : constant Client_Access := Get (Filoject.Get_Application_Context);
begin
   C.Greet;
end Greet;
