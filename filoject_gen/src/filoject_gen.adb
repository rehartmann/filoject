with Ada.Characters.Conversions;
with Ada.Wide_Wide_Characters.Handling;

package body Filoject_Gen is

   use Ada.Wide_Wide_Characters.Handling;

   package CC renames Ada.Characters.Conversions;

   function To_String (S : Unbounded_Wide_Wide_String) return String is
   begin
      return CC.To_String (To_Wide_Wide_String (S));
   end To_String;

   function To_Mixed_Case (Source : Wide_Wide_String)
                           return Wide_Wide_String is
      Result : Wide_Wide_String := Source;
   begin
      for I in Result'First + 1 .. Result'Last loop
         if Is_Letter (Result (I)) and then Result (I - 1) /= '_'
           and then Result (I - 1) /= '.'
         then
            Result (I) := To_Lower (Result (I));
         end if;
      end loop;
      return Result;
   end To_Mixed_Case;

   function To_Mixed_Case (Source : Unbounded_Wide_Wide_String)
                           return Wide_Wide_String is
   begin
      return To_Mixed_Case (To_Wide_Wide_String (Source));
   end To_Mixed_Case;

   function Is_Expanded_Name (Source : Unbounded_Wide_Wide_String)
                              return Boolean is
   begin
      return Index (Source, ".", 1) > 0;
   end Is_Expanded_Name;

   function To_Package_Name (Name : Unbounded_Wide_Wide_String)
                              return Wide_Wide_String is
      I : constant Natural := Ada.Strings.Wide_Wide_Unbounded.Index
        (Name,
         ".",
         Length (Name),
         Ada.Strings.Backward);
   begin
      if I <= 1 then
         return "";
      end if;
      return Ada.Strings.Wide_Wide_Unbounded.Slice (Name, 1, I - 1);
   end To_Package_Name;

end Filoject_Gen;
