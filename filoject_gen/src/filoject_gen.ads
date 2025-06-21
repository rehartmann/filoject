with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;

package Filoject_Gen is
   pragma Preelaborate (Filoject_Gen);

   use Ada.Strings.Unbounded;
   use Ada.Strings.Wide_Wide_Unbounded;

   Application_Name : constant String;
   Library_Name : constant String;
   Version : constant String;

   type Component (Inject : Boolean := False) is record
      Name : Unbounded_Wide_Wide_String;
      Component_Type : Unbounded_Wide_Wide_String;
      case Inject is
         when False =>
            null;
         when True =>
            Target_Type : Unbounded_Wide_Wide_String;
      end case;
   end record;

   package Component_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Component);

   type Record_Type is record
      Name : Unbounded_Wide_Wide_String;
      Parent_Name : Unbounded_Wide_Wide_String;
      Components : Component_Vectors.Vector;
      Abstract_Type : Boolean;
      Managed : Boolean;
   end record;

   package Type_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Unbounded_Wide_Wide_String,
        Element_Type    => Record_Type,
        Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
        Equivalent_Keys => "=");

   package Name_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Unbounded_Wide_Wide_String);

   type Access_Type is record
      Name : Unbounded_Wide_Wide_String;
      Target_Name : Unbounded_Wide_Wide_String;
   end record;

   package Access_Type_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Unbounded_Wide_Wide_String,
        Element_Type    => Access_Type,
        Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
        Equivalent_Keys => "=");

   type Param is record
      Name : Unbounded_Wide_Wide_String;
      Param_Type : Unbounded_Wide_Wide_String;
      Class_Wide : Boolean;
   end record;

   package Param_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Param);

   type Source_Location is record
      File_Name : Unbounded_String;
      Line : Natural;
   end record;

   type Proc is record
      Name : Unbounded_Wide_Wide_String;
      Params : Param_Vectors.Vector;
      Location : Source_Location;
   end record;

   type Binding is record
      Source : Unbounded_Wide_Wide_String;
      Implementation : Unbounded_Wide_Wide_String;
      Package_Name : Unbounded_Wide_Wide_String;
      Location : Source_Location;
   end record;

   type Binding_Array is array (Positive range <>) of Binding;

   package Binding_Vectors is new
     Ada.Containers.Vectors
       (Index_Type      => Positive,
        Element_Type    => Binding);

   package Proc_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Proc);

   type Use_Clause is record
      Use_Package : Unbounded_Wide_Wide_String;
      By_Package : Unbounded_Wide_Wide_String;
   end record;

   package Use_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Use_Clause);

   function To_String (S : Unbounded_Wide_Wide_String)
                       return String;

   function To_Mixed_Case (Source : Wide_Wide_String)
                           return Wide_Wide_String;

   function To_Mixed_Case (Source : Unbounded_Wide_Wide_String)
                           return Wide_Wide_String;

   function Is_Expanded_Name (Source : Unbounded_Wide_Wide_String)
                              return Boolean;

   function To_Package_Name (Name : Unbounded_Wide_Wide_String)
                             return Wide_Wide_String;

private
   Application_Name : constant String := "fjgen";
   Library_Name : constant String := "filoject";
   Version : constant String := "0.1.0-dev";

end Filoject_Gen;
