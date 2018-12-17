pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__main.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E073 : Short_Integer; pragma Import (Ada, E073, "system__os_lib_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exception_table_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__io_exceptions_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__strings_E");
   E038 : Short_Integer; pragma Import (Ada, E038, "ada__containers_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exceptions_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__soft_links__initialize_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__strings__maps_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "ada__strings__maps__constants_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "interfaces__c_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "system__object_reader_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "system__dwarf_lines_E");
   E037 : Short_Integer; pragma Import (Ada, E037, "system__traceback__symbolic_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "ada__tags_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "ada__streams_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "system__file_control_block_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "system__finalization_root_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "ada__finalization_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "system__file_io_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "ada__streams__stream_io_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "system__storage_pools_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "system__finalization_masters_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "system__storage_pools__subpools_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "ada__strings__unbounded_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "ada__text_io_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "system__pool_global_E");
   E160 : Short_Integer; pragma Import (Ada, E160, "avtas__lmcp__object_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "afrl__cmasi__object_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "afrl__cmasi__abstractgeometry_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "afrl__cmasi__abstractzone_E");
   E203 : Short_Integer; pragma Import (Ada, E203, "afrl__cmasi__keepinzone_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "afrl__cmasi__keyvaluepair_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "afrl__cmasi__location3d_E");
   E185 : Short_Integer; pragma Import (Ada, E185, "afrl__cmasi__circle_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "afrl__cmasi__payloadconfiguration_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "afrl__cmasi__cameraconfiguration_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "afrl__cmasi__entityconfiguration_E");
   E201 : Short_Integer; pragma Import (Ada, E201, "afrl__cmasi__gimbalconfiguration_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E201 := E201 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "afrl__cmasi__gimbalconfiguration__finalize_spec");
      begin
         F1;
      end;
      E197 := E197 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "afrl__cmasi__entityconfiguration__finalize_spec");
      begin
         F2;
      end;
      E181 := E181 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "afrl__cmasi__cameraconfiguration__finalize_spec");
      begin
         F3;
      end;
      E183 := E183 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "afrl__cmasi__payloadconfiguration__finalize_spec");
      begin
         F4;
      end;
      E185 := E185 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "afrl__cmasi__circle__finalize_spec");
      begin
         F5;
      end;
      E195 := E195 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "afrl__cmasi__location3d__finalize_spec");
      begin
         F6;
      end;
      E199 := E199 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "afrl__cmasi__keyvaluepair__finalize_spec");
      begin
         F7;
      end;
      E203 := E203 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "afrl__cmasi__keepinzone__finalize_spec");
      begin
         F8;
      end;
      E167 := E167 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "afrl__cmasi__abstractzone__finalize_spec");
      begin
         F9;
      end;
      E163 := E163 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "afrl__cmasi__abstractgeometry__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "afrl__cmasi__object__finalize_spec");
      begin
         E165 := E165 - 1;
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "avtas__lmcp__object__finalize_spec");
      begin
         E160 := E160 - 1;
         F12;
      end;
      E171 := E171 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "system__pool_global__finalize_spec");
      begin
         F13;
      end;
      E099 := E099 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "ada__text_io__finalize_spec");
      begin
         F14;
      end;
      E135 := E135 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "ada__strings__unbounded__finalize_spec");
      begin
         F15;
      end;
      E143 := E143 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__storage_pools__subpools__finalize_spec");
      begin
         F16;
      end;
      E145 := E145 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "system__finalization_masters__finalize_spec");
      begin
         F17;
      end;
      E177 := E177 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "ada__streams__stream_io__finalize_spec");
      begin
         F18;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "system__file_io__finalize_body");
      begin
         E111 := E111 - 1;
         F19;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E023 := E023 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E068 := E068 + 1;
      Ada.Strings'Elab_Spec;
      E053 := E053 + 1;
      Ada.Containers'Elab_Spec;
      E038 := E038 + 1;
      System.Exceptions'Elab_Spec;
      E025 := E025 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E019 := E019 + 1;
      E011 := E011 + 1;
      System.Os_Lib'Elab_Body;
      E073 := E073 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E059 := E059 + 1;
      Interfaces.C'Elab_Spec;
      E055 := E055 + 1;
      E043 := E043 + 1;
      System.Object_Reader'Elab_Spec;
      System.Dwarf_Lines'Elab_Spec;
      E048 := E048 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E037 := E037 + 1;
      E079 := E079 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E103 := E103 + 1;
      Ada.Streams'Elab_Spec;
      E101 := E101 + 1;
      System.File_Control_Block'Elab_Spec;
      E115 := E115 + 1;
      System.Finalization_Root'Elab_Spec;
      E114 := E114 + 1;
      Ada.Finalization'Elab_Spec;
      E112 := E112 + 1;
      System.File_Io'Elab_Body;
      E111 := E111 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E177 := E177 + 1;
      System.Storage_Pools'Elab_Spec;
      E149 := E149 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E145 := E145 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E143 := E143 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E135 := E135 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E099 := E099 + 1;
      System.Pool_Global'Elab_Spec;
      E171 := E171 + 1;
      avtas.lmcp.object'elab_spec;
      E160 := E160 + 1;
      afrl.cmasi.object'elab_spec;
      E165 := E165 + 1;
      afrl.cmasi.abstractgeometry'elab_spec;
      E163 := E163 + 1;
      afrl.cmasi.abstractzone'elab_spec;
      E167 := E167 + 1;
      afrl.cmasi.keepinzone'elab_spec;
      E203 := E203 + 1;
      afrl.cmasi.keyvaluepair'elab_spec;
      E199 := E199 + 1;
      afrl.cmasi.location3d'elab_spec;
      E195 := E195 + 1;
      afrl.cmasi.circle'elab_spec;
      E185 := E185 + 1;
      afrl.cmasi.payloadconfiguration'elab_spec;
      E183 := E183 + 1;
      afrl.cmasi.cameraconfiguration'elab_spec;
      E181 := E181 + 1;
      afrl.cmasi.entityconfiguration'elab_spec;
      E197 := E197 + 1;
      afrl.cmasi.gimbalconfiguration'elab_spec;
      E201 := E201 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_main");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/avtas.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/avtas-lmcp.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/avtas-lmcp-types.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/avtas-lmcp-object.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-enumerations.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-object.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-abstractgeometry.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-abstractzone.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-keepinzone.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-keyvaluepair.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-location3d.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-circle.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-payloadconfiguration.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-cameraconfiguration.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-entityconfiguration.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/afrl-cmasi-gimbalconfiguration.o
   --   /home/snail/Documents/LmcpExperiments/Packages/obj/main.o
   --   -L/home/snail/Documents/LmcpExperiments/Packages/obj/
   --   -L/home/snail/Documents/LmcpExperiments/Packages/obj/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/6.4.1/adalib/
   --   -static
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end ada_main;
