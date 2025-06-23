with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Unchecked_Conversion;

package body Filoject is

   Application_Context : aliased Context;

   function Hash (T : Ada.Tags.Tag) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Ada.Tags.Expanded_Name (T));
   end Hash;

   function Get_Application_Context return Context_Ref is
   begin
      return Application_Context'Access;
   end Get_Application_Context;

   procedure Clear_Application_Context is
   begin
      Application_Context.Managed_Objects.Clear;
   end Clear_Application_Context;

   protected body Managed_Object_Map is
      procedure Get (T : Tag;
                     Obj : out Managed_Object;
                     Found : out Boolean) is
         C : constant Managed_Object_Hashed_Maps.Cursor := Objects.Find (T);
      begin
         if C = Managed_Object_Hashed_Maps.No_Element then
            Found := False;
         else
            Obj := Objects (C);
            Found := True;
         end if;
      end Get;

      procedure Put (T : Tag;
                     Obj : Managed_Object) is
      begin
         Objects.Include (T, Obj);
      end Put;

      procedure Clear is
      begin
         Objects.Clear;
      end Clear;
   end Managed_Object_Map;

   function Find_Implementation (T : Ada.Tags.Tag)
                                 return Ada.Tags.Tag is
      Result : Ada.Tags.Tag;
   begin
      Result := Filoject.Get_Binding (T);
      if Result /= Ada.Tags.No_Tag then
         return Result;
      end if;
      raise Resolution_Exception with "no implementation for type "
           & Ada.Tags.Expanded_Name (T) & " found";
   end Find_Implementation;

   function Is_Implementation (T : Ada.Tags.Tag)
                               return Boolean is
   begin
      for C in Bindings.Iterate loop
         if Bindings (C) = T then
            return True;
         end if;
      end loop;
      return False;
   end Is_Implementation;

   function Get_Scope (T : Ada.Tags.Tag)
                       return Scope is
   begin
      if not Is_Implementation (T) then
         raise Constraint_Error with "type " & Ada.Tags.Expanded_Name (T)
           & " is not an implementation";
      end if;
      return Application;
   end Get_Scope;

   function Get_Scope (Context : Context_Ref)
                       return Scope is
   begin
      return Context.Object_Scope;
   end Get_Scope;

   function Get (Context : Context_Ref) return T_Access is
      Addr : System.Address;
      Implementation : Ada.Tags.Tag;
      Object_Scope : Scope;
      Obj : Managed_Object;
      Found : Boolean;

      function To_T_Access is
      new Ada.Unchecked_Conversion (Source => System.Address,
                                    Target => T_Access);
   begin
      Implementation := Find_Implementation (T'Tag);
      Object_Scope := Get_Scope (Implementation);
      if Object_Scope = Application
        and then Get_Scope (Context) /= Application
      then
         raise Constraint_Error
           with "cannot refer to object in application scope from non-application context";
      end if;
      Context.Managed_Objects.Get (Implementation,
                                   Obj,
                                   Found);
      if Found then
         return To_T_Access (Obj.Obj);
      end if;
      Addr := Provider_Implementation.New_Instance (Implementation);

      --  Insert the new (uninitialized) instance into the map so if there
      --  is a cycle in the dependency graph the following Initialize call can
      --  find the instance and will not go into infinite recursion
      Context.Managed_Objects.Put (T => Implementation,
                                   Obj => (Obj => Addr,
                                           Scoped => Object_Scope));
      Provider_Implementation.Initialize (Implementation, Addr, Context);
      return To_T_Access (Addr);
   end Get;

   procedure Bind (Source : Ada.Tags.Tag;
                   Implementation : Ada.Tags.Tag) is
   begin
      if Is_Abstract (Implementation) then
         raise Resolution_Exception
           with Ada.Tags.Expanded_Name (Implementation) & " is abstract";
      end if;
      if not Ada.Tags.Is_Descendant_At_Same_Level (Implementation, Source) then
         raise Resolution_Exception
           with Ada.Tags.Expanded_Name (Implementation) & " cannot implement "
           & Ada.Tags.Expanded_Name (Source);
      end if;
      if not Is_Implementation (Implementation) then
         raise Resolution_Exception
           with "objects of type " & Ada.Tags.Expanded_Name (Implementation)
           & " cannot be created.";
      end if;
      Bindings.Insert (Source, Implementation);
   end Bind;

   function Get_Binding (T : Ada.Tags.Tag)
                         return Ada.Tags.Tag is
      C : constant Tag_Hashed_Maps.Cursor := Bindings.Find (T);
   begin
      if C = Tag_Hashed_Maps.No_Element then
         return Ada.Tags.No_Tag;
      end if;
      return Bindings (C);
   end Get_Binding;

   function Create_Context return Context_Ref is
      Result : constant Context_Ref := new Context;
   begin
      Result.Object_Scope := Dynamic;
      return Result;
   end Create_Context;

   procedure Destroy_Context (Context : in out Context_Ref) is
   begin
      null;
   end Destroy_Context;

end Filoject;
