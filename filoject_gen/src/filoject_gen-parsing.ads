package Filoject_Gen.Parsing is

   procedure Parse (File_Name : String;
                    Concrete_Types : in out Type_Maps.Map;
                    Access_Types : in out Access_Type_Maps.Map;
                    Bindings : in out Binding_Vectors.Vector;
                    Initializers : in out Proc_Vectors.Vector;
                    Use_Clauses : in out Use_Vectors.Vector;
                    Report_Error : access procedure (S : String);
                    Success : out Boolean);

end Filoject_Gen.Parsing;
