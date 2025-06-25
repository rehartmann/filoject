with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Tags;
with System;

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

   use Ada.Tags;

   type Managed_Object is record
      Obj : System.Address;
      Scoped : Scope;
   end record;

   function Hash (T : Tag) return Ada.Containers.Hash_Type;

   package Managed_Object_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Tag,
        Element_Type    => Managed_Object,
        Hash            => Hash,
        Equivalent_Keys => "=");
   use Managed_Object_Hashed_Maps;

   protected type Managed_Object_Map is
      procedure Get (T : Tag;
                     Obj : out Managed_Object;
                     Found : out Boolean);
      procedure Put (T : Tag;
                     Obj : Managed_Object);
      procedure Clear;
   private
      Objects : Managed_Object_Hashed_Maps.Map
        := Managed_Object_Hashed_Maps.Empty_Map;
   end Managed_Object_Map;

   type Context is record
      Managed_Objects : Managed_Object_Map;
      Object_Scope : Scope;
   end record;

   type Context_Ref is access all Context;

   type Provider is interface;

   type Provider_Access is access all Provider'Class;

   function New_Instance (Self : Provider;
                          T : Ada.Tags.Tag)
                          return System.Address
                          is abstract;

   procedure Initialize (Self : Provider;
                         T : Ada.Tags.Tag;
                         Addr : System.Address;
                         Context : Context_Ref)
                         is abstract;

   Provider_Implementation : Provider_Access;

   package Tag_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Ada.Tags.Tag,
        Element_Type    => Ada.Tags.Tag,
        Hash            => Hash,
        Equivalent_Keys => "=");
   use Tag_Hashed_Maps;

   Bindings : Tag_Hashed_Maps.Map;

   package Tag_Scope_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Ada.Tags.Tag,
        Element_Type    => Scope,
        Hash            => Hash,
        Equivalent_Keys => "=");

   Tag_Scopes : Tag_Scope_Maps.Map;

end Filoject;
