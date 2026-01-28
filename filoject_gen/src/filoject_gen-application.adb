with Ada.Containers;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with GNAT.Command_Line;
with Filoject_Gen.Parsing;
with Filoject_Gen.Generation;

procedure Filoject_Gen.Application is

   package WWTIO renames Ada.Wide_Wide_Text_IO;
   package CL renames GNAT.Command_Line;

   use Ada.Containers;
   use Ada.Text_IO;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Wide_Wide_Unbounded;
   use Access_Type_Maps;
   use Type_Maps;

   procedure Put (T : Record_Type) is
   begin
      Put ("type ");
      WWTIO.Put (To_Wide_Wide_String (T.Name));
      if T.Parent_Name = Null_Unbounded_Wide_Wide_String then
         Put_Line (" is record");
      else
         Put (" is new ");
         WWTIO.Put (To_Wide_Wide_String (T.Parent_Name));
         Put_Line (" with record");
      end if;
      for C of T.Components loop
         Put ("   ");
         WWTIO.Put (To_Wide_Wide_String (C.Name));
         Put (" : ");
         WWTIO.Put (To_Wide_Wide_String (C.Component_Type));
         Put (";");
         if C.Inject then
            Put (" Injected");
         end if;
         Put_Line ("");
      end loop;
   end Put;

   pragma Unreferenced (Put);

   procedure Report_Error (Message : String;
                           Location : Source_Location) is
   begin
      Put_Line (Standard_Error,
                To_String (Location.File_Name) & ':' & Location.Line'Image & ':'
                & Message);
   end Report_Error;

   procedure Analyze (Tagged_Types : in out Type_Maps.Map;
                      Access_Types : Access_Type_Maps.Map;
                      Bindings : in out Binding_Array;
                      Initializers : in out Proc_Vectors.Vector;
                      Use_Clauses : Use_Vectors.Vector;
                      Success : out Boolean) is

      function Is_Subtype (Name1 : Unbounded_Wide_Wide_String;
                           Name2 : Unbounded_Wide_Wide_String)
                           return Boolean
      is
         N : Unbounded_Wide_Wide_String;
         T : Record_Type;
      begin
         N := Name1;
         while N /= "" and then N /= Name2 loop
            if not Tagged_Types.Contains (N) then
               return False;
            end if;
            T := Tagged_Types (N);
            N := T.Parent_Name;
         end loop;
         return N = Name2;
      end Is_Subtype;

      function Is_Binding_Target (Name : Unbounded_Wide_Wide_String)
                                  return Boolean is
      begin
         for B of Bindings loop
            if B.Implementation = Name
              or else Is_Subtype (B.Implementation, Name)
            then
               return True;
            end if;
         end loop;
         return False;
      end Is_Binding_Target;

      function Is_Binding_Source (Type_Name : Unbounded_Wide_Wide_String)
                                  return Boolean is
      begin
         for B of Bindings loop
            if B.Source = Type_Name then
               return True;
            end if;
         end loop;
         return False;
      end Is_Binding_Source;

      function Find_Package_Of_Type (Type_Name : Unbounded_Wide_Wide_String)
                                     return Unbounded_Wide_Wide_String is
      begin
         for U of Use_Clauses loop
            if Tagged_Types.Contains (U.Use_Package & '.' & Type_Name)
              or else Access_Types.Contains (U.Use_Package & '.' & Type_Name)
            then
               return U.Use_Package;
            end if;
         end loop;
         return Null_Unbounded_Wide_Wide_String;
      end Find_Package_Of_Type;

      function Is_Valid_Initializer (Initializer : in out Proc)
                                     return Boolean is
         Package_Name : Unbounded_Wide_Wide_String;
      begin
         if not Is_Expanded_Name (Initializer.Params.First_Element.Param_Type)
         then
            Package_Name := Find_Package_Of_Type (Initializer.Params.First_Element
                                                  .Param_Type);
            if Package_Name /= "" then
               Initializer.Params (Initializer.Params.First_Index)
                 .Param_Type := Package_Name & '.'
                 & Initializer.Params.First_Element.Param_Type;
            else
               Initializer.Params (Initializer.Params.First_Index)
                 .Param_Type := To_Package_Name (Initializer.Name) & '.'
                 & Initializer.Params.First_Element.Param_Type;
            end if;
         end if;
         --  The initializer must belong to same package as the type
         if To_Package_Name (Initializer.Params.First_Element.Param_Type)
           /= To_Package_Name (Initializer.Name)
         or else not Is_Binding_Target (Initializer.Params.First_Element.Param_Type)
         then
            return False;
         end if;
         if Initializer.Params.Length > 1 then
            for I in Initializer.Params.First_Index + 1 .. Initializer.Params.Last_Index loop
               if not Is_Expanded_Name (Initializer.Params (I).Param_Type) then
                  Package_Name := Find_Package_Of_Type (Initializer.Params (I).Param_Type);
                  if Package_Name /= "" then
                     Initializer.Params (I).Param_Type :=
                       Package_Name & '.' & Initializer.Params (I).Param_Type;
                  else
                     Initializer.Params (I).Param_Type :=
                       To_Package_Name (Initializer.Name) & '.' & Initializer.Params (I).Param_Type;
                  end if;
               end if;
               if not Is_Binding_Source (Initializer.Params (I).Param_Type) then
                  return False;
               end if;
            end loop;
         end if;
         return True;
      end Is_Valid_Initializer;

      AC : Access_Type_Maps.Cursor;
      Type_Name, Package_Name : Unbounded_Wide_Wide_String;
      I : Positive;
      T : Record_Type;
   begin
      for I in Bindings'Range loop
         if not Is_Expanded_Name (Bindings (I).Source) then
            Package_Name := Find_Package_Of_Type (Bindings (I).Source);
            if Package_Name /= "" then
               Bindings (I).Source := Package_Name & '.' & Bindings (I).Source;
            else
               Bindings (I).Source := Bindings (I).Package_Name & '.'
                 & Bindings (I).Source;
            end if;
         end if;
         if not Tagged_Types.Contains (Bindings (I).Source) then
            Report_Error ("warning: type " & To_String (Bindings (I).Source)
                          & " does not appear to be a valid binding source",
                          Bindings (I).Location);
         end if;
         if not Is_Expanded_Name (Bindings (I).Implementation) then
            Package_Name := Find_Package_Of_Type (Bindings (I).Implementation);
            if Package_Name /= "" then
               Bindings (I).Implementation := Package_Name & '.'
                 & Bindings (I).Implementation;
            else
               Bindings (I).Implementation := Bindings (I).Package_Name & '.'
                 & Bindings (I).Implementation;
            end if;
         end if;
         if Tagged_Types.Contains (Bindings (I).Implementation) then
            T := Tagged_Types (Bindings (I).Implementation);
            if T.Abstract_Type then
               Report_Error ("error: implementation type must not be abstract",
                             Bindings (I).Location);
               Success := False;
               return;
            end if;
         else
            Report_Error ("warning: type " & To_String (Bindings (I).Implementation)
                          & " does not appear to be a valid binding target",
                          Bindings (I).Location);
         end if;
      end loop;

      for I in Bindings'Range loop
         for J in I + 1 .. Bindings'Last loop
            if Bindings (I).Source = Bindings (J).Source
            then
               Report_Error ("error: binding is ambiguous",
                             Bindings (I).Location);
               Success := False;
               return;
            end if;
         end loop;
      end loop;

      for T of Tagged_Types loop
         if T.Parent_Name /= ""
           and then not Is_Expanded_Name (T.Parent_Name)
         then
            Package_Name := Find_Package_Of_Type (T.Parent_Name);
            if Package_Name /= "" then
               T.Parent_Name := Package_Name & '.'
                 & T.Parent_Name;
            else
               T.Parent_Name := To_Package_Name (T.Name) & '.'
                 & T.Parent_Name;
            end if;
         end if;

         --  Determine injected components
         for C of T.Components loop
            Type_Name := C.Component_Type;
            if not Is_Expanded_Name (Type_Name) then
               Package_Name := Find_Package_Of_Type (Type_Name);
               if Package_Name /= "" then
                  Type_Name := Package_Name & '.' & Type_Name;
               else
                  Type_Name := To_Package_Name (T.Name) & '.' & Type_Name;
               end if;
            end if;
            AC := Access_Types.Find (Type_Name);
            if AC = Access_Type_Maps.No_Element then
               Report_Error ("error: the type of "
                             & To_String (T.Name) & "." & To_String (C.Name)
                             & " is not injectable",
                             C.Location);
               Success := False;
               return;
            end if;
            C := (Name => C.Name,
                  Component_Type => Type_Name,
                  Inject => True,
                  Target_Type => Element (AC).Target_Name,
                  Location => C.Location);
         end loop;
      end loop;

      --  Remove all initializers where the first argument is not the target
      --  of a binding and the remaining parameters are not source of a binding
      I := 1;
      while I <= Initializers.Last_Index loop
         if Is_Valid_Initializer (Initializers (I)) then
            I := I + 1;
         else
            Report_Error ("warning: " & To_String (Initializers (I).Name)
                          & " is not a valid initializer",
                         Initializers (I).Location);
            Initializers.Delete (I);
         end if;
      end loop;
      Success := True;
   end Analyze;

   procedure Put_Help is
   begin
      Put_Line ("Usage: drgen [-h] [--version] [-v] [-d dir] [file]...");
   end Put_Help;

   procedure Put_Error (S : String) is
   begin
      Put_Line (Standard_Error, S);
   end Put_Error;

   Managed_Types : Type_Maps.Map;
   Access_Types : Access_Type_Maps.Map;
   Binding_Vector : Binding_Vectors.Vector;
   Initializers : Proc_Vectors.Vector;
   Use_Clauses : Use_Vectors.Vector;
   File_Name : Unbounded_String;
   Dest_Dir : Unbounded_String;
   Verbose : Boolean := False;
   Success : Boolean;

begin
   loop
      case CL.Getopt ("h v -version d=") is    -- 2
         when 'h' =>
            Put_Help;
            return;
         when 'v' =>
            Verbose := True;
         when 'd' =>
            Dest_Dir := To_Unbounded_String (CL.Parameter);
            if Element (Dest_Dir, Length (Dest_Dir)) /= '/' then
               Append (Dest_Dir, "/");
            end if;
         when '-' =>
            if CL.Full_Switch = "-version" then
               Put_Line (Application_Name & ' ' & Version);
               return;
            end if;
         when others =>
            exit;
      end case;
   end loop;

   File_Name := To_Unbounded_String (CL.Get_Argument);
   if File_Name = "" then
      Put_Help;
      return;
   end if;

   while File_Name /= "" loop
      Filoject_Gen.Parsing.Parse (File_Name => To_String (File_Name),
                               Concrete_Types => Managed_Types,
                               Access_Types => Access_Types,
                               Bindings => Binding_Vector,
                               Initializers => Initializers,
                               Use_Clauses => Use_Clauses,
                               Report_Error => Put_Error'Access,
                               Success => Success);
      if not Success then
         Put_Line (Standard_Error, "Error parsing file " & To_String (File_Name) & '.');
         Put_Line (Standard_Error, "No code generated.");
         return;
      end if;
      File_Name := To_Unbounded_String (CL.Get_Argument);
   end loop;

   declare
      Bindings : Binding_Array (1 .. Natural (Binding_Vector.Length));
      First : Boolean;
   begin
      for I in Bindings'Range loop
         Bindings (I) := Binding_Vector (I);
      end loop;

      if Verbose then
         Put_Line ("Manged types found:");
         for T of Managed_Types loop
            Put ("  ");
            WWTIO.Put_Line (To_Wide_Wide_String (T.Name));
         end loop;
         Put_Line ("Bindings found:");
         for B of Bindings loop
            Put ("  ");
            WWTIO.Put (To_Wide_Wide_String (B.Source));
            Put (" => ");
            WWTIO.Put_Line (To_Wide_Wide_String (B.Implementation));
         end loop;
         Put_Line ("Access types found:");
         for T of Access_Types loop
            Put ("  ");
            WWTIO.Put (To_Wide_Wide_String (T.Name));
            Put (" => ");
            WWTIO.Put_Line (To_Wide_Wide_String (T.Target_Name));
         end loop;
         Put_Line ("Initializers found:");
         for IP of Initializers loop
            Put ("  ");
            WWTIO.Put (To_Wide_Wide_String (IP.Name & " ("));
            First := True;
            for P of IP.Params loop
               if First then
                  First := False;
               else
                  Put ("; ");
               end if;
               WWTIO.Put (To_Wide_Wide_String (P.Name));
               Put (" : ");
               WWTIO.Put (To_Wide_Wide_String (P.Param_Type));
               if P.Class_Wide then
                  WWTIO.Put ("'Class");
               end if;
            end loop;
            Put_Line (")");
         end loop;
      end if;
      Analyze (Tagged_Types => Managed_Types,
               Access_Types => Access_Types,
               Bindings => Bindings,
               Initializers => Initializers,
               Use_Clauses => Use_Clauses,
               Success => Success);
      if not Success or else Managed_Types.Is_Empty
      then
         Put_Line (Standard_Error, "No code generated.");
         return;
      end if;
      Filoject_Gen.Generation.Generate (To_String (Dest_Dir),
                                     Managed_Types,
                                     Bindings,
                                     Initializers);
   end;
exception
   when E : CL.Invalid_Switch =>
      Put_Line (Standard_Error, Exception_Message (E));
end Filoject_Gen.Application;
