with Ada.Strings.Wide_Wide_Fixed;
with Ada.Wide_Wide_Characters.Handling;
with Langkit_Support.Slocs;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;

package body Filoject_Gen.Parsing is

   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   package Slocs renames Langkit_Support.Slocs;

   use Ada.Wide_Wide_Characters.Handling;
   use Ada.Strings.Wide_Wide_Fixed;
   use type LALCO.Ada_Node_Kind_Type;

   procedure Parse (File_Name : String;
                    Concrete_Types : in out Type_Maps.Map;
                    Access_Types : in out Access_Type_Maps.Map;
                    Bindings : in out Binding_Vectors.Vector;
                    Initializers : in out Proc_Vectors.Vector;
                    Use_Clauses : in out Use_Vectors.Vector;
                    Report_Error : access procedure (S : String);
                    Success : out Boolean) is

      Package_Name : Unbounded_Wide_Wide_String;

      function To_Expanded_Name (Source : Wide_Wide_String)
                                 return Wide_Wide_String is
      begin
         if Index (Source, ".", Source'First) > 0 then
            return To_Upper (Source);
         end if;
         return To_Wide_Wide_String (Package_Name) & '.' & To_Upper (Source);
      end To_Expanded_Name;

      procedure Add_Components (Components : Ada_Node_List;
                                T : in out Record_Type) is
      begin
         for N of Components loop
            if N.Kind = LALCO.Ada_Component_Decl then
               case N.As_Component_Decl.F_Component_Def.F_Type_Expr.Kind is
                  when LALCO.Ada_Subtype_Indication =>
                     T.Components.Append
                       ((Name => To_Unbounded_Wide_Wide_String
                         (N.As_Component_Decl.F_Ids
                              .As_Defining_Name_List.Defining_Name_List_Element (1)
                              .F_Name.Text),
                         Component_Type => To_Unbounded_Wide_Wide_String
                           (To_Upper (N.As_Component_Decl.F_Component_Def
                            .F_Type_Expr.As_Subtype_Indication.Text)),
                         Inject => False));
                  when others =>
                     null;
               end case;
            end if;
         end loop;
      end Add_Components;

      procedure Add_Params (Subp : LAL.Subp_Spec;
                            Target : in out Proc) is
      begin
         Target.Params.Clear;
         for P of Subp.F_Subp_Params.F_Params loop
            if P.F_Type_Expr.As_Subtype_Indication.F_Name.Kind = LALCO.Ada_Attribute_Ref
              and then To_Upper (P.F_Type_Expr.As_Subtype_Indication.F_Name
                                 .As_Attribute_Ref.F_Attribute.Text) = "CLASS"
            then
               Target.Params.Append
                 ((Name => To_Unbounded_Wide_Wide_String (P.F_Ids.Text),
                   Param_Type => To_Unbounded_Wide_Wide_String
                     (To_Upper (P.F_Type_Expr.As_Subtype_Indication.F_Name
                      .As_Attribute_Ref.F_Prefix.Text)),
                   Class_Wide => True));
            else
               Target.Params.Append
                 ((Name => To_Unbounded_Wide_Wide_String (P.F_Ids.Text),
                   Param_Type => To_Unbounded_Wide_Wide_String
                     (To_Upper (P.F_Type_Expr.Text)),
                   Class_Wide => False));
            end if;
         end loop;
      end Add_Params;

      function Has_Inject (Assocs : LAL.Aspect_Assoc_List)
                           return Boolean is
      begin
         for C of Assocs loop
            if To_Upper (C.As_Aspect_Assoc.F_Id.Text) = "INJECT"
              and then To_Upper (C.As_Aspect_Assoc.F_Expr.Text) = "TRUE"
            then
               return True;
            end if;
         end loop;
         return False;
      end Has_Inject;

      function Is_Initializer_Signature (Subp : LAL.Subp_Spec)
                                         return Boolean is
         Param_Specs : Param_Spec_List;
      begin
         if Subp.F_Subp_Kind.Kind /= LALCO.Ada_Subp_Kind_Procedure then
            return False;
         end if;
         if Subp.F_Subp_Params.F_Params.Children'Length = 0 then
            return False;
         end if;
         Param_Specs := Subp.F_Subp_Params.F_Params;
         for P of Param_Specs loop
            if P.F_Type_Expr.Kind /= LALCO.Ada_Subtype_Indication then
               return False;
            end if;
         end loop;
         case Param_Specs.Child (Param_Specs.Param_Spec_List_First)
           .As_Param_Spec.F_Mode.Kind is
            when LALCO.Ada_Mode_In_Out | LALCO.Ada_Mode_Out =>
               null;
            when others =>
               return False;
         end case;
         return True;
      end Is_Initializer_Signature;

      procedure Process_Bind (Node : LAL.Ada_Node'Class) is
      begin
         if Node.As_Pragma_Node.F_Args.Kind /= LALCO.Ada_Base_Assoc_List then
            return;
         end if;
         declare
            Children : constant Ada_Node_Array := Node.As_Pragma_Node.F_Args.As_Base_Assoc_List.Children;
            B : Binding;
         begin
            if Children'Length = 0 or else Children'Length > 2 then
               Report_Error (File_Name
                             & ':' & Slocs.Line_Number'Image (Node.Sloc_Range.Start_Line)
                             & ':' & Slocs.Column_Number'Image (Node.Sloc_Range.Start_Column)
                             & ": warning: too many arguments to Bind_To_Implementation");
            end if;
            B.Implementation := To_Unbounded_Wide_Wide_String (To_Upper (Children (Children'Last).Text));
            B.Package_Name := Package_Name;
            B.Location.File_Name := To_Unbounded_String (File_Name);
            B.Location.Line := Natural (Node.Sloc_Range.Start_Line);
            case Children'Length is
               when 1 =>
                  B.Source := B.Implementation;
                  Bindings.Append (B);
               when 2 =>
                  B.Source := To_Unbounded_Wide_Wide_String
                    (To_Upper (Children (Children'First).Text));
                  Bindings.Append (B);
               when others =>
                  null;
            end case;
         end;
      end Process_Bind;

      procedure Process_Use_Clause (Children : Ada_Node_Array) is
      begin
         for I in Children'First .. Children'Last loop
            Use_Clauses.Append
              ((By_Package => Null_Unbounded_Wide_Wide_String,
                Use_Package => To_Unbounded_Wide_Wide_String
                  (To_Upper (Children (I).Text))));
         end loop;
      end Process_Use_Clause;

      function Process_Node (Node : LAL.Ada_Node'Class)
                             return LALCO.Visit_Status
      is
         Type_Decl : LAL.Concrete_Type_Decl;
         T : Record_Type;
         Acc_T : Access_Type;
         P : Proc;
         Type_Found : Boolean := False;
      begin
         case Node.Kind is
            when LALCO.Ada_Use_Package_Clause =>
               Process_Use_Clause (Node.As_Use_Package_Clause.F_Packages.Children);
            when LALCO.Ada_Package_Decl =>
               --  Ignore nested packages
               if Package_Name = "" then
                  Package_Name := To_Unbounded_Wide_Wide_String
                    (To_Upper (Node.As_Package_Decl.F_Package_Name.As_Defining_Name.Text));
                  return LALCO.Into;
               end if;
            when LALCO.Ada_Concrete_Type_Decl =>
               Type_Decl := Node.As_Concrete_Type_Decl;
               case Type_Decl.F_Type_Def.Kind is
               when LALCO.Ada_Derived_Type_Def =>
                  T.Name := Package_Name & "." & To_Upper
                    (Type_Decl.F_Name.As_Defining_Name.F_Name.Text);
                  T.Parent_Name := To_Unbounded_Wide_Wide_String (To_Upper
                    (Type_Decl.F_Type_Def.As_Derived_Type_Def
                     .F_Subtype_Indication.F_Name.Text));
                  Type_Found := True;
                  T.Abstract_Type := Type_Decl.F_Type_Def.As_Derived_Type_Def.F_Has_Abstract.Kind
                       = LALCO.Ada_Abstract_Present;
                  if not Type_Decl.F_Type_Def.As_Derived_Type_Def.F_Record_Extension
                      .Is_Null
                      and then Type_Decl.F_Type_Def.As_Derived_Type_Def
                        .F_Record_Extension.Kind = LALCO.Ada_Record_Def
                  then
                     Add_Components (Type_Decl.F_Type_Def.As_Derived_Type_Def
                                     .F_Record_Extension.F_Components.F_Components,
                                     T);
                  end if;
               when LALCO.Ada_Record_Type_Def =>
                  T.Name := Package_Name & '.' & To_Upper
                    (Type_Decl.F_Name.As_Defining_Name.Text);
                  if Type_Decl.F_Type_Def.As_Record_Type_Def.F_Has_Tagged.Kind = LALCO.Ada_Tagged_Present
                    and then (not Type_Decl.F_Type_Def.As_Record_Type_Def.F_Record_Def.Is_Null)
                  then
                     T.Abstract_Type := Type_Decl.F_Type_Def.As_Record_Type_Def.F_Has_Abstract.Kind
                       = LALCO.Ada_Abstract_Present;
                     if Type_Decl.F_Type_Def.As_Record_Type_Def.F_Record_Def
                       .Kind = LALCO.Ada_Record_Def
                     then
                        Add_Components (Type_Decl.F_Type_Def.As_Record_Type_Def.F_Record_Def
                                        .As_Record_Def.F_Components.F_Components,
                                        T);
                     end if;
                     Type_Found := True;
                  end if;
               when LALCO.Ada_Interface_Type_Def =>
                  T.Name := Package_Name & '.' & To_Upper
                    (Type_Decl.F_Name.As_Defining_Name.Text);
                  T.Abstract_Type := True;
                  Type_Found := True;
               when LALCO.Ada_Type_Access_Def =>
                  --  Target must be a class wide type
                  if Type_Decl.F_Type_Def.As_Type_Access_Def
                    .F_Subtype_Indication.F_Name.Kind = LALCO.Ada_Attribute_Ref
                    and then To_Upper (Type_Decl.F_Type_Def.As_Type_Access_Def
                      .F_Subtype_Indication.F_Name.As_Attribute_Ref.F_Attribute
                        .As_Identifier.Text) = "CLASS"
                  then
                     Acc_T.Name := To_Unbounded_Wide_Wide_String (To_Expanded_Name
                       (Type_Decl.F_Name.As_Defining_Name.F_Name.As_Identifier
                        .Text));
                     Acc_T.Target_Name := To_Unbounded_Wide_Wide_String (To_Expanded_Name
                       (Type_Decl.F_Type_Def.As_Type_Access_Def
                        .F_Subtype_Indication.F_Name.As_Attribute_Ref.F_Prefix
                        .As_Identifier.Text));
                     Access_Types.Include (Acc_T.Name, Acc_T);
                  end if;
               when others =>
                  null;  --  Ignore
               end case;
               if Type_Found then
                  if Concrete_Types.Contains (T.Name) then
                     Concrete_Types.Replace (T.Name, T);
                  else
                     Concrete_Types.Include (T.Name, T);
                  end if;
               end if;
            when LALCO.Ada_Subp_Decl =>
               if not Node.As_Subp_Decl.F_Aspects.Is_Null
                 and then Has_Inject (Node.As_Subp_Decl.F_Aspects.F_Aspect_Assocs)
               then
                  if Is_Initializer_Signature (Node.As_Subp_Decl.F_Subp_Spec)
                  then
                     P.Name := Package_Name & "." & To_Unbounded_Wide_Wide_String
                       (Node.As_Subp_Decl.F_Subp_Spec.F_Subp_Name.Text);
                     P.Location.File_Name := To_Unbounded_String (File_Name);
                     P.Location.Line := Natural (Node.Sloc_Range.Start_Line);
                     Add_Params (Node.As_Subp_Decl.F_Subp_Spec, P);
                     Initializers.Append (P);
                  else
                     Report_Error (File_Name & ':' & Slocs.Line_Number'Image (Node.Sloc_Range.Start_Line)
                                   & ':' & Slocs.Column_Number'Image (Node.Sloc_Range.Start_Column)
                                   & ": " & To_String (P.Name) & " is not a valid initializer.");
                     Success := False;
                     return LALCO.Stop;
                  end if;
               end if;
            when LALCO.Ada_Pragma_Node =>
               if To_Upper (Node.As_Pragma_Node.F_Id.Text) = "BIND_TO_IMPLEMENTATION" then
                  Process_Bind (Node);
               end if;
            when others =>
               return LALCO.Into;
         end case;
         return LALCO.Over;
      end Process_Node;

      Ctx  : constant Analysis_Context := Create_Context;
      Unit : constant Analysis_Unit := Ctx.Get_From_File (File_Name);

   begin
      --  Unit.Root.Print;
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Report_Error (Unit.Format_GNU_Diagnostic (D));
         end loop;
         Success := False;
         return;
      end if;
      Unit.Root.Traverse (Process_Node'Access);
      for U of Use_Clauses loop
         U.By_Package := Package_Name;
      end loop;
      Success := True;
   end Parse;

end Filoject_Gen.Parsing;
