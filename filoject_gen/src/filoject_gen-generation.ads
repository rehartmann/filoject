package Filoject_Gen.Generation is

   procedure Generate (Dest_Dir : String;
                       Managed_Types : Type_Maps.Map;
                       Bindings : Binding_Array;
                       Initializers : Proc_Vectors.Vector);

end Filoject_Gen.Generation;
