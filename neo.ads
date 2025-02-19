
--
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/.             .\&%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/.                         .(%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#                                  .%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                                        .#%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&                                              ,%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%.                                                 .%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#                                                      ,&%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%,                                                         #%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&                                                            .%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#                                                              ,%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&                                                                .%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%,                                                                 # 
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&                                                                  .
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#                              ,(&&%).                              
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                           .%%%%%%%%%%%.                           
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          )%%%%%%%%%%%%%(                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                              N  E  O    E  N  G  I  N  E    
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%/                          %%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&                          
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&   
--                                                                                                                                                          
--                                                            Copyright (C) 202X Justin Squirek                                                             
--                                                                                                                                                          
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published  by the Free Software      
-- Foundation, either version 3 of the License, or (at your option) any later version.                                                                      
--                                                                                                                                                          
-- Neo is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a   
-- particular purpose. See the GNU General Public License for more details.                                                                                 
--                                                                                                                                                          

with GNAT.Compiler_Version;
with GNAT.Traceback.Symbolic;            use GNAT.Traceback.Symbolic;
with GNAT.Traceback;                     use GNAT.Traceback;
with Interfaces;                         use Interfaces;
with Interfaces.C;                       use Interfaces.C;
with System;                             use System;
with System.Machine_Code;                use System.Machine_Code;
with System.Multiprocessors;             use System.Multiprocessors;
with System.Storage_Pools;               use System.Storage_Pools;
with System.Storage_Elements;            use System.Storage_Elements;
with Ada.Containers;                     use Ada.Containers;
with Ada.Directories;                    use Ada.Directories;
with Ada.Dynamic_Priorities;             use Ada.Dynamic_Priorities;
with Ada.Numerics;                       use Ada.Numerics;
with Ada.Exceptions;                     use Ada.Exceptions;
with Ada.Finalization;                   use Ada.Finalization;
with Ada.Command_Line;                   use Ada.Command_Line;
with Ada.Calendar;                       use Ada.Calendar;
with Ada.Calendar.Formatting;            use Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;             use Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1;
with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Characters.Handling;            use Ada.Characters.Handling;
with Ada.Wide_Characters.Handling;       use Ada.Wide_Characters.Handling;
with Ada.Task_Identification;            use Ada.Task_Identification;
pragma Warnings (Off);
with Ada.Strings.Superbounded;           use Ada.Strings.Superbounded;
with Ada.Strings.Wide_Superbounded;      use Ada.Strings.Wide_Superbounded;
with Ada.Strings.Wide_Wide_Superbounded; use Ada.Strings.Wide_Wide_Superbounded;
pragma Warnings (On);
with Ada.Strings;                        use Ada.Strings;
with Ada.Strings.Fixed;                  use Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;             use Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Wide_Fixed;        use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;         use Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;    use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Streams;                        use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Wide_Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
--with Is_Debugging;

-- Basic data types and utilities
package Neo is

  function Is_Debugging return Boolean is (False);

  -----------------
  -- Information --
  -----------------

  NAME_ID : constant Wide_String := "Neo";
  VERSION : constant Wide_String := "0.1.0";

  package GNAT_Info is new GNAT.Compiler_Version;

  -------------
  -- Renames --
  -------------

  package Ada_IO    renames Ada.Wide_Text_IO;
  package Stream_IO renames Ada.Streams.Stream_IO;
  package Latin_32  renames Ada.Characters.Wide_Wide_Latin_1;

  generic procedure Unchecked_Deallocation renames Ada.Unchecked_Deallocation;
  generic function  Unchecked_Conversion   renames Ada.Unchecked_Conversion;

  -----------
  -- Types --
  -----------

  -- Characters
  subtype Char_8_C  is Char;
  subtype Char_8    is Character;
  subtype Char_16_C is WChar_T;
  subtype Char_16   is Wide_Character;
  subtype Char_32   is Wide_Wide_Character;

  -- Strings
  subtype Str_8_C        is Char_Array;
  subtype Str_8          is String;
  subtype Str_32_Unbound is Unbounded_Wide_Wide_String;
  subtype Str_16_Unbound is Unbounded_Wide_String;
  subtype Str_8_Unbound  is Unbounded_String;
  subtype Str_16_C       is WChar_Array;
  subtype Str_16         is Wide_String;
  subtype Str_C          is Str_16_C;
  subtype Str_32         is Wide_Wide_String;
  subtype Str_32_Super   is Wide_Wide_Superbounded.Super_String;
  subtype Str_16_Super   is Wide_Superbounded.Super_String;
  subtype Str_8_Super    is Superbounded.Super_String;

  -- Integers
  type Int_Ptr               is mod MEMORY_SIZE;
  type Int_8_Percent         is range 1..100; for Int_8_Percent'Size use 8;
  type Int_2_Unsigned        is mod 2 ** 2;
  type Int_4_Unsigned        is mod 2 ** 4;
  subtype Int_8_Unsigned     is Unsigned_8;
  subtype Int_8_Unsigned_C   is Interfaces.C.Unsigned_Char;
  subtype Int_8_Signed       is Short_Short_Integer;
  subtype Int_8_Signed_C     is Interfaces.C.Char;
  subtype Int_8_Natural      is Int_8_Unsigned;
  subtype Int_8_Positive     is Int_8_Unsigned range 1..Int_8_Unsigned'Last;
  subtype Int_16_Unsigned    is Unsigned_16;
  subtype Int_16_Unsigned_C  is Interfaces.C.Unsigned_Short;
  subtype Int_16_Signed      is Short_Integer;
  subtype Int_16_Signed_C    is Interfaces.C.Short;
  subtype Int_16_Natural     is Int_16_Unsigned;
  subtype Int_16_Positive    is Int_8_Unsigned range 1..Int_8_Unsigned'Last;
  subtype Int_32_Unsigned    is Unsigned_32;
  subtype Int_32_Unsigned_C  is Interfaces.C.Unsigned;
  subtype Int_32_Signed      is Integer;
  subtype Int_32_Signed_C    is Interfaces.C.Int;
  subtype Int_32_Natural     is Natural;
  subtype Int_32_Positive    is Positive;
  subtype Int_64_Unsigned    is Unsigned_64;
  subtype Int_64_Unsigned_C  is Int_64_Unsigned;
  subtype Int_64_Signed      is Long_Long_Integer;
  subtype Int_64_Signed_C    is Int_64_Signed;
  subtype Int_64_Natural     is Int_64_Unsigned;
  subtype Int_64_Positive    is Int_64_Signed range 1..Int_64_Signed'Last;
  subtype Int_128_Unsigned   is Unsigned_128;
  subtype Int_128_Unsigned_C is Int_128_Unsigned;
  subtype Int_128_Signed     is Long_Long_Long_Integer;
  subtype Int_128_Signed_C   is Int_128_Signed;
  subtype Int_128_Natural    is Int_128_Unsigned;
  subtype Int_128_Positive   is Int_128_Signed range 1..Int_128_Signed'Last;
  subtype Int_Size_C         is Interfaces.C.Size_T;
  subtype Int_Resolution     is Int_32_Signed range 256..32_389;
  subtype Int_Number_Base    is Positive range 2..64 with Predicate => Int_Number_Base in 2..36 | 57 | 58 | 64;

  -- Floating point reals
  subtype Real_16           is Short_Float;
  subtype Real_16_Natural   is Real_16 range 0.0..Real_16'Last;
  subtype Real_16_Positive  is Real_16 range 1.0..Real_16'Last;
  subtype Real_16_Percent   is Real_16 range 0.0..100.0;
  subtype Real_16_Degree    is Real_16 range 1.0..360.0;
  subtype Real_32           is Float;
  subtype Real_32_C         is Interfaces.C.C_Float;
  subtype Real_32_Natural   is Real_32 range 0.0..Real_32'Last;
  subtype Real_32_Positive  is Real_32 range 1.0..Real_32'Last;
  subtype Real_32_Percent   is Real_32 range 0.0..100.0;
  subtype Real_32_Degree    is Real_32 range 1.0..360.0;
  subtype Real_64           is Long_Float;
  subtype Real_64_C         is Interfaces.C.Double;
  subtype Real_64_Natural   is Real_64 range 0.0..Real_64'Last;
  subtype Real_64_Positive  is Real_64 range 1.0..Real_64'Last;
  subtype Real_64_Percent   is Real_64 range 0.0..100.0;
  subtype Real_64_Degree    is Real_64 range 1.0..360.0;
  subtype Real_128          is Long_Long_Float;
  subtype Real_128_Natural  is Real_128 range 0.0..Real_128'Last;
  subtype Real_128_Positive is Real_128 range 1.0..Real_128'Last;
  subtype Real_128_Percent  is Real_128 range 0.0..100.0;
  subtype Real_128_Degree   is Real_128 range 1.0..360.0;

  -- Stream types
  subtype Array_Stream is Stream_Element_Array;

  -- Access types
  subtype Ptr                is Address;
  type Ptr_Ptr               is access all Address;
  type Ptr_Str_32            is access all Str_32;
  type Ptr_Str_16            is access all Str_16;
  type Ptr_Str_8_C           is access all Str_8_C;
  type Ptr_Str_8             is access all Str_8;
  type Ptr_Str_16_C          is access all Str_16_C;
  type Ptr_Int_32_Signed     is access all Int_32_Signed;
  type Ptr_Int_32_Positive   is access all Int_32_Positive;
  type Ptr_Int_Ptr           is access all Int_Ptr;
  type Ptr_Int_64_Unsigned_C is access all Int_64_Unsigned_C;
  type Ptr_Int_32_Unsigned_C is access all Int_32_Unsigned_C;
  type Ptr_Int_32_Unsigned   is access all Int_32_Unsigned;
  type Ptr_Int_32_Signed_C   is access all Int_32_Signed_C;
  type Ptr_Int_16_Unsigned_C is access all Int_16_Unsigned_C;
  type Ptr_Int_8_Unsigned_C  is access all Int_8_Unsigned_C;
  type Ptr_Real_32_C         is access all Real_32_C;
  type Ptr_Char_8_C          is access all Char_8_C;
  type Ptr_Char_16_C         is access all Char_16_C;
  type Ptr_Const_Char_8_C    is access constant Char_8_C;
  type Ptr_Const_Char_16_C   is access constant Char_16_C;
  type Ptr_Const_Str_16_C    is access constant Str_16_C;
  type Ptr_Procedure         is access procedure;
  NULL_PTR : Ptr renames NULL_ADDRESS;

  -- Abbreviations for convience
  subtype Bool               is Boolean;
  subtype Char               is Char_16;
  subtype Stream             is Stream_Element;
  subtype Nibble             is Int_2_Unsigned;
  subtype Half_Byte          is Int_4_Unsigned;
  subtype Byte               is Int_8_Unsigned;
  subtype Int                is Int_32_Signed;
  subtype Int_C              is Int_32_Signed_C;
  subtype Real               is Real_32;
  subtype Real_C             is Real_32_C;
  subtype Real_Percent       is Real_64_Percent;
  subtype Real_Degree        is Real_64_Degree;
  subtype Int_64             is Int_64_Signed;
  subtype Int_64_C           is Int_64_Signed_C;
  subtype Int_16             is Int_16_Signed;
  subtype Int_16_C           is Int_16_Signed_C;
  subtype Int_8              is Int_8_Signed;
  subtype Int_8_C            is Int_8_Signed_C;
  subtype Int_Unsigned       is Int_32_Unsigned;
  subtype Int_Unsigned_C     is Int_32_Unsigned_C;
  subtype Str                is Str_16;
  subtype Str_Unbound        is Str_16_Unbound;
  subtype Ptr_Int_Unsigned_C is Ptr_Int_32_Unsigned_C;
  subtype Ptr_Real_C         is Ptr_Real_32_C;

  -- Unchecked conversions
  function To_Ptr_Int_16_Unsigned_C is new Unchecked_Conversion (Ptr,                   Ptr_Int_16_Unsigned_C);
  function To_Ptr_Int_16_Unsigned_C is new Unchecked_Conversion (Int_Ptr,               Ptr_Int_16_Unsigned_C);
  function To_Ptr_Int_32_Unsigned   is new Unchecked_Conversion (Ptr,                   Ptr_Int_32_Unsigned);
  function To_Ptr                   is new Unchecked_Conversion (Ptr_Const_Char_16_C,   Ptr);
  function To_Ptr                   is new Unchecked_Conversion (Int_Ptr,               Ptr);
  function To_Ptr_Char_8_C          is new Unchecked_Conversion (Ptr,                   Ptr_Char_8_C);
  function To_Ptr_Char_16_C         is new Unchecked_Conversion (Ptr,                   Ptr_Char_16_C);
  function To_Ptr_Const_Char_16_C   is new Unchecked_Conversion (Ptr,                   Ptr_Const_Char_16_C);
  function To_Ptr_Const_Char_16_C   is new Unchecked_Conversion (Int_Ptr,               Ptr_Const_Char_16_C);
  function To_Int_64_Unsigned       is new Unchecked_Conversion (Real_64,               Int_64_Unsigned);
  function To_Int_32_Unsigned_C     is new Unchecked_Conversion (Int_32_Signed_C,       Int_Unsigned_C);
  function To_Int_32_Unsigned       is new Unchecked_Conversion (Int_32_Signed_C,       Int_Unsigned);
  function To_Int_32_Unsigned       is new Unchecked_Conversion (Real_32,               Int_Unsigned);
  function To_Int_32_Signed_C       is new Unchecked_Conversion (Int_Unsigned_C,        Int_32_Signed_C);
  function To_Int_32_Signed         is new Unchecked_Conversion (Int_Unsigned,          Int_32_Signed);
  function To_Int_16_Signed         is new Unchecked_Conversion (Int_16_Unsigned,       Int_16_Signed);
  function To_Int_16_Signed_C       is new Unchecked_Conversion (Int_16_Unsigned,       Int_16_Signed_C);
  function To_Int_16_Unsigned       is new Unchecked_Conversion (Char_16_C,             Int_16_Unsigned);
  function To_Int_Ptr               is new Unchecked_Conversion (Ptr_Int_16_Unsigned_C, Int_Ptr);
  function To_Int_Ptr               is new Unchecked_Conversion (Ptr_Const_Char_16_C,   Int_Ptr);
  function To_Int_Ptr               is new Unchecked_Conversion (Ptr_Char_8_C,          Int_Ptr);
  function To_Int_Ptr               is new Unchecked_Conversion (Ptr,                   Int_Ptr);
  function To_Real_32               is new Unchecked_Conversion (Int_Unsigned,          Real_32);

  -----------
  -- Sizes --
  -----------

  kB : constant Int_Ptr := 1024;
  MB : constant Int_Ptr := kB ** 2;
  GB : constant Int_Ptr := MB ** 2;
  TB : constant Int_Ptr := GB ** 2;

  ---------------
  -- Debugging --
  ---------------

  -- Hook for OS level error information
  type Ptr_Last_Error is access function return Int;
  procedure Set_Last_Error (Val : Ptr_Last_Error);
  function Get_Last_Error return Int;

  -- Assertion to check imported C function calls. They raise a Program_Error if the value is null or 0
  procedure Assert (Val : Bool);
  procedure Assert (Val : Ptr);
  procedure Assert (Val : Int_Ptr);
  procedure Assert (Val : Int_C);
  procedure Assert (Val : Int_Unsigned_C);
  procedure Assert (Val : Int_16_Unsigned_C);


  procedure Debug_Assert (Val : Bool);
  procedure Debug_Assert (Val : Ptr);
  procedure Debug_Assert (Val : Int_Ptr);
  procedure Debug_Assert (Val : Int_C);
  procedure Debug_Assert (Val : Int_Unsigned_C);
  procedure Debug_Assert (Val : Int_16_Unsigned_C);

  -- Ignore procedures swallow the result of C functions that return useless results
  procedure Ignore (Val : Bool)              is null;
  procedure Ignore (Val : Ptr)               is null;
  procedure Ignore (Val : Int_Ptr)           is null;
  procedure Ignore (Val : Int_C)             is null;
  procedure Ignore (Val : Int_Unsigned_C)    is null;
  procedure Ignore (Val : Int_16_Unsigned_C) is null;

  ------------
  -- Status --
  ------------

  -- A mutex or task-safe flag, however you want to look at it...
  protected type Safe_Status with Lock_Free is
      procedure Occupied (Val : Bool);
      function Occupied return Bool;
    private
      Status : Bool := False;
    end;

  -------------
  -- Counter --
  -------------

  protected type Safe_Counter with Lock_Free is
      function Get return Int;
      procedure Set (Val : Int);
      procedure Increment (Amount : Int);
      procedure Increment;
      procedure Decrement (Amount : Int);
      procedure Decrement;
    private
      Count : Int := 0;
    end;

  ----------
  -- Safe --
  ----------

  generic
    type Safe_T is private;
    Initial : Safe_T;
  package Safe is
      protected type T is
          function Get return Safe_T;
          procedure Set (Val : Safe_T);
        private
          Data : Safe_T := Initial;
        end;
    end;

  generic
    type Safe_T is (<>);
    Initial : Safe_T;
  package Safe_Discrete is
      protected type T with Lock_Free is
          function Get return Safe_T;
          procedure Set (Val : Safe_T);
        private
          Data : Safe_T := Initial;
        end;
    end;

  generic
    type Safe_T is digits <>;
    Initial : Safe_T;
  package Safe_Digits is
      protected type T with Lock_Free is
          function Get return Safe_T;
          procedure Set (Val : Safe_T);
        private
          Data : Safe_T := Initial;
        end;
    end;

  -----------
  -- Timer --
  -----------

  type Timer_State is record
      Start      : Time;
      Last       : Duration := 0.0;
      Is_Stopped : Bool     := False;
    end record;
  function Get_Start_Time return Time;
  function Get_Duration   (Timer :        Timer_State) return Duration;
  procedure Start         (Timer : in out Timer_State);
  procedure Stop          (Timer : in out Timer_State);

  ------------
  -- Colors --
  ------------

  type Color_State is record Red, Green, Blue : Byte := 16#FF#; end record;
  type Transparent_Color_State is record
      Base  : Color_State := (others => <>);
      Alpha : Byte        := 16#FF#;
    end record;
  COLOR_RED          : constant Color_State := (16#FF#, 16#00#, 16#00#);
  COLOR_TAN          : constant Color_State := (16#D2#, 16#B4#, 16#8C#);
  COLOR_BLUE         : constant Color_State := (16#00#, 16#00#, 16#FF#);
  COLOR_PINK         : constant Color_State := (16#FF#, 16#C0#, 16#CB#);
  COLOR_AQUA         : constant Color_State := (16#00#, 16#FF#, 16#FF#);
  COLOR_GRAY         : constant Color_State := (16#80#, 16#80#, 16#80#);
  COLOR_CYAN         : constant Color_State := (16#00#, 16#FF#, 16#FF#);
  COLOR_TEAL         : constant Color_State := (16#00#, 16#80#, 16#80#);
  COLOR_LIME         : constant Color_State := (16#BF#, 16#FF#, 16#00#);
  COLOR_PUCE         : constant Color_State := (16#CC#, 16#88#, 16#99#);
  COLOR_PLUM         : constant Color_State := (16#84#, 16#31#, 16#79#);
  COLOR_MAUVE        : constant Color_State := (16#E0#, 16#B0#, 16#FF#);
  COLOR_BLACK        : constant Color_State := (16#00#, 16#00#, 16#00#);
  COLOR_WHITE        : constant Color_State := (16#FF#, 16#FF#, 16#FF#);
  COLOR_GREEN        : constant Color_State := (16#00#, 16#FF#, 16#00#);
  COLOR_KHAKI        : constant Color_State := (16#C3#, 16#B0#, 16#91#);
  COLOR_IVORY        : constant Color_State := (16#FF#, 16#FF#, 16#F0#);
  COLOR_BEIGE        : constant Color_State := (16#F5#, 16#F5#, 16#DC#);
  COLOR_WHEAT        : constant Color_State := (16#F5#, 16#DE#, 16#B3#);
  COLOR_CORAL        : constant Color_State := (16#FF#, 16#7F#, 16#50#);
  COLOR_OLIVE        : constant Color_State := (16#80#, 16#80#, 16#00#);
  COLOR_SILVER       : constant Color_State := (16#C0#, 16#C0#, 16#C0#);
  COLOR_YELLOW       : constant Color_State := (16#FF#, 16#FF#, 16#00#);
  COLOR_ORANGE       : constant Color_State := (16#FF#, 16#A5#, 16#00#);
  COLOR_VIOLET       : constant Color_State := (16#EE#, 16#82#, 16#EE#);
  COLOR_PURPLE       : constant Color_State := (16#80#, 16#00#, 16#80#);
  COLOR_SALMON       : constant Color_State := (16#FA#, 16#80#, 16#72#);
  COLOR_INDIGO       : constant Color_State := (16#4B#, 16#00#, 16#82#);
  COLOR_MAROON       : constant Color_State := (16#80#, 16#00#, 16#00#);
  COLOR_GOLDEN       : constant Color_State := (16#FF#, 16#D7#, 16#00#);
  COLOR_MAGENTA      : constant Color_State := (16#FF#, 16#00#, 16#FF#);
  COLOR_FUCHSIA      : constant Color_State := (16#FF#, 16#77#, 16#FF#);
  COLOR_CRIMSON      : constant Color_State := (16#DC#, 16#14#, 16#3C#);
  COLOR_LAVENDER     : constant Color_State := (16#B5#, 16#7E#, 16#DC#);
  COLOR_SKY_BLUE     : constant Color_State := (16#87#, 16#CE#, 16#EB#);
  COLOR_CHARCOAL     : constant Color_State := (16#46#, 16#46#, 16#46#);
  COLOR_HOT_PINK     : constant Color_State := (16#FC#, 16#0F#, 16#C0#);
  COLOR_NAVY_BLUE    : constant Color_State := (16#00#, 16#00#, 16#80#);
  COLOR_GOLDEN_ROD   : constant Color_State := (16#DA#, 16#A5#, 16#20#);
  COLOR_DARK_GRAY    : constant Color_State := (16#63#, 16#63#, 16#63#);
  COLOR_LIGHT_BLUE   : constant Color_State := (16#AD#, 16#D8#, 16#E6#);
  COLOR_ROYAL_BLUE   : constant Color_State := (16#08#, 16#4C#, 16#9E#);
  COLOR_AQUAMARINE   : constant Color_State := (16#7F#, 16#FF#, 16#D4#);
  COLOR_CHARTREUSE   : constant Color_State := (16#7F#, 16#FF#, 16#00#);
  COLOR_FOREST_GREEN : constant Color_State := (16#22#, 16#8B#, 16#22#);

  -------------
  -- Strings --
  -------------

  -- URL/web encoding
  function Percent_Encode (Item : Str) return Str;

  -- Paths
  function Path_Separator return Char is ((if Index (Current_Directory, "\") = 0 then '/' else '\')); -- Windows uses backslashes :\
  function S return Char renames Path_Separator; -- For convience
  PATH_MAX : constant Positive := 260;

  -- UTF routines
  -- if (To_Byte (C) and 16#80#) = 16#00# then
  --   if To_Byte (C) < 32 or To_Byte (C) >= 127 then raise Invalid_Character; end if;
  --   Result := Char_32'Val (Char_8'Pos (Text (C)));
  -- elsif (To_Byte (C) and 16#E0#) = 16#C0# then Result := Char_32'Val (Shift_Left (To_Byte (C)     and 16#1F#,  6)
  --                                                                  or            (To_Byte (C + 1) and 16#3F#)); C := C + 2;
  -- elsif (To_Byte (C) and 16#F0#) = 16#E0# then Result := Char_32'Val (Shift_Left (To_Byte (C)     and 16#0F#, 12)
  --                                                                  or Shift_Left (To_Byte (C + 1) and 16#3F#,  6)
  --                                                                  or            (To_Byte (C + 2) and 16#3F#)); C := C + 2;
  -- elsif (To_Byte (C) and 16#F8#) = 16#F0# then Result := Char_32'Val (Shift_Left (To_Byte (C)     and 16#07#, 18)
  --                                                                  or Shift_Left (To_Byte (C + 1) and 16#3F#, 12)
  --                                                                  or Shift_Left (To_Byte (C + 2) and 16#3F#,  6)
  --                                                                  or            (To_Byte (C + 3) and 16#3F#)); C := C + 3;
  Invalid_UTF_Literal : Exception;
  function To_UTF_16      (Item : Str_32) return Str_16;
  function To_UTF_8       (Item : Str_32) return Str_8;
  function UTF_To_Char_32 (Item : Str_8)  return Char_32;

  -- String conversions
  function To_Str_8          (Item : Str_32)              return Str_8;
  function To_Str_8          (Item : Ptr_Char_8_C)        return Str_8;
  function To_Str_16         (Item : Str_32)              return Str_16;
  function To_Str_16         (Item : Str_8)               return Str_16;
  function To_Str_16         (Item : Str_16_C)            return Str_16;
  function To_Str_16         (Item : Ptr_Const_Char_16_C) return Str_16;
  function To_Str_32         (Item : Str_8)               return Str_32;
  function To_Str_32         (Item : Str_16)              return Str_32;
  function To_Char_8         (Item : Char_16)             return Char_8;
  function To_Char_8         (Item : Char_32)             return Char_8;
  function To_Char_16        (Item : Char_32)             return Char_16;
  function To_Char_8_C       (Item : Char_8)              return Char_8_C       is (Char_8_C'Val  (Char_8'Pos    (Item)));
  function To_Char_8         (Item : Char_8_C)            return Char_8         is (Char_8'Val    (Char_8_C'Pos  (Item)));
  function To_Char_16_C      (Item : Char_8_C)            return Char_16_C      is (Char_16_C'Val (Char_8_C'Pos  (item)));
  function To_Char_16_C      (Item : Char_8)              return Char_16_C      is (Char_16_C'Val (Char_8'Pos    (item)));
  function To_Char_16        (Item : Char_16_C)           return Char_16        is (Char_16'Val   (Char_16_C'Pos (Item)));
  function To_Char_16        (Item : Char_8_C)            return Char_16        is (Char_16'Val   (Char_8_C'Pos  (Item)));
  function To_Char_16        (Item : Char_8)              return Char_16        is (Char_16'Val   (Char_8'Pos    (Item)));
  function To_Char_8_C       (Item : Char_16_C)           return Char_8_C       is (To_Char_8_C   (To_Char_8     (To_Char_16 (Item))));
  function To_Char_8_C       (Item : Char_16)             return Char_8_C       is (To_Char_8_C   (To_Char_8     (Item)));
  function To_Char_16_C      (Item : Char_16)             return Char_16_C      is (Char_16_C'Val (Char_16'Pos   (item)));
  function To_Char_8         (Item : Char_16_C)           return Char_8         is (To_Char_8     (To_Char_16    (Item)));
  function To_Char_32        (Item : Char_8)              return Char_32        is (Char_32'Val   (Char_8'Pos    (Item)));
  function To_Char_32        (Item : Char_16)             return Char_32        is (Char_32'Val   (Char_16'Pos   (Item)));
  function To_Str_8          (Item : Str_16)              return Str_8          is (To_String (Item));
  function To_Str_8          (Item : Str_8_Super)         return Str_8          renames Super_To_String;
  function To_Str_16         (Item : Str_16_Super)        return Str_16         renames Super_To_String;
  function To_Str_8          (Item : Str_8_Unbound)       return Str_8          renames To_String;
  function To_Str_8_Unbound  (Item : Str_8)               return Str_8_Unbound  renames To_Unbounded_String;
  function To_Str_8_Super    (Item : Str_8; L : Natural)  return Str_8_Super    is (To_Super_String (Item, L));
  function To_Str_16         (Item : Str_16_Unbound)      return Str_16         renames To_Wide_String;
  function To_Str_16_Unbound (Item : Str_16)              return Str_16_Unbound renames To_Unbounded_Wide_String;
  function To_Str_32_Unbound (Item : Str_32)              return Str_32_Unbound renames To_Unbounded_Wide_Wide_String;
  function To_Str_32_Unbound (Item : Str_8)               return Str_32_Unbound is (To_Str_32_Unbound (To_Str_32 (Item)));
  function To_Str_32_Unbound (Item : Str_16)              return Str_32_Unbound is (To_Str_32_Unbound (To_Str_32 (Item)));
  function To_Str_32         (Item : Str_32_Unbound)      return Str_32         renames To_Wide_Wide_String;
  function To_Str_8          (Item : Str_32_Unbound)      return Str_8          is (To_Str_8  (To_Str_32 (Item)));
  function To_Str_16         (Item : Str_32_Unbound)      return Str_16         is (To_Str_16 (To_Str_32 (Item)));
  function To_Str_16_Super   (Item : Str_16; L : Natural) return Str_16_Super   is (To_Super_String (Item, L));
  function To_Str_8          (Item : Str_8_C)             return Str_8          is (To_Ada (Item, True));
  function To_Str_8_C        (Item : Str_8)               return Str_8_C        is (To_C   (Item, True));
  function To_Str_8_C        (Item : Str_16)              return Str_8_C        is (To_Str_8_C (To_Str_8 (Item)));
  function To_Str_8_C        (Item : Ptr_Char_8_C)        return Str_8_C        is (To_Str_8_C (To_Str_8 (Item)));
  function To_Str_16         (Item : Char_16)             return Str_16         is ("" & Item);
  function To_Str_16         (Item : Str_8_C)             return Str_16         is (To_Str_16 (To_Str_8 (Item)));
  function To_Str_16         (Item : Ptr_Char_8_C)        return Str_16         is (To_Str_16 (To_Str_8 (Item)));
  function To_Str_16         (Item : Char_8)              return Str_16         is (To_Str_16 ("" & Item));
  function To_Str_16_Unbound (Item : Str_8)               return Str_16_Unbound is (To_Str_16_Unbound (To_Str_16 (Item)));
  function To_Str_16_Unbound (Item : Char_16)             return Str_16_Unbound is (To_Str_16_Unbound ("" & Item));
  function To_Str_16_Unbound (Item : Str_8_C)             return Str_16_Unbound is (To_Str_16_Unbound (To_Str_16 (Item)));
  function To_Str_16_Unbound (Item : Str_8_Super)         return Str_16_Unbound is (To_Str_16_Unbound (Super_To_String (Item)));
  function To_Str_16_Unbound (Item : Str_32_Unbound)      return Str_16_Unbound is (To_Str_16_Unbound (To_Str_16 (Item)));
  function To_Str_16_C       (Item : Str_16)              return Str_16_C       is (To_C (Item & To_Char_16 (Interfaces.C.NUL)));
  function To_Str_16_C       (Item : Str_16_Unbound)      return Str_16_C       is (To_Str_16_C (To_Str_16 (Item)));
  function To_Ptr_Char_8_C   (Item : in out Str_8_C)      return Ptr_Char_8_C   is (Item (Item'First)'Unchecked_Access);
  function To_Ptr_Char_16_C  (Item : in out Str_16_C)     return Ptr_Char_16_C  is (Item (Item'First)'Unchecked_Access);
  function To_Str_Unbound    (Item : Str_16)              return Str_16_Unbound renames To_Str_16_Unbound;
  function To_Str_Unbound    (Item : Str_8_C)             return Str_16_Unbound renames To_Str_16_Unbound;
  function To_Str_Unbound    (Item : Str_8)               return Str_16_Unbound renames To_Str_16_Unbound;
  function To_Str_Unbound    (Item : Char_16)             return Str_16_Unbound renames To_Str_16_Unbound;
  function To_Str_Unbound    (Item : Str_8_Super)         return Str_16_Unbound renames To_Str_16_Unbound;
  function To_Str_C          (Item : Str_16)              return Str_16_C       renames To_Str_16_C;
  function To_Str_C          (Item : Str_16_Unbound)      return Str_16_C       renames To_Str_16_C;
  function To_Str            (Item : Str_8)               return Str_16         renames To_Str_16;
  function To_Str            (Item : Str_16_C)            return Str_16         renames To_Str_16;
  function To_Str            (Item : Ptr_Const_Char_16_C) return Str_16         renames To_Str_16;
  function To_Str            (Item : Char_16)             return Str_16         renames To_Str_16;
  function To_Str            (Item : Str_8_C)             return Str_16         renames To_Str_16;
  function To_Str            (Item : Ptr_Char_8_C)        return Str_16         renames To_Str_16;
  function To_Str            (Item : Char_8)              return Str_16         renames To_Str_16;
  function To_Str            (Item : Str_16_Unbound)      return Str_16         renames To_Str_16;
  function To_Str            (Item : Str_16_Super)        return Str_16         is (To_Str_16 (Item));
  function To_Str_Unbound    (Item : Str_16_Super)        return Str_Unbound    is (To_Str_Unbound (To_Str (Item)));
  function SC                (Item : Str_16_C)            return Str_16         renames To_Str_16;
  function C                 (Item : in out Str_16_C)     return Ptr_Char_16_C  renames To_Ptr_Char_16_C;
  function C                 (Item : in out Str_8_C)      return Ptr_Char_8_C   renames To_Ptr_Char_8_C;
  function P                 (Item : Str_8;  L : Natural) return Str_8_Super    renames To_Str_8_Super;
  function P                 (Item : Str_16; L : Natural) return Str_16_Super   renames To_Str_16_Super;
  function S                 (Item : Str_16_Unbound)      return Str_16         renames To_Str_16;
  function S                 (Val  : Str_16_Super)        return Str            renames To_Str;
  function U                 (Val  : Str_16_Super)        return Str_Unbound    renames To_Str_Unbound;
  function U                 (Item : Str_16)              return Str_16_Unbound renames To_Str_16_Unbound;
  function W                 (Item : Str_32)              return Str_32_Unbound renames To_Str_32_Unbound;

  -- Deallocation routines
  procedure Free is new Unchecked_Deallocation (Str_8,  Ptr_Str_8);
  procedure Free is new Unchecked_Deallocation (Str_16, Ptr_Str_16);
  procedure Free is new Unchecked_Deallocation (Str_32, Ptr_Str_32);

  generic
    type Num_T is mod <>;
  function Generic_To_Str_Int (Item : Num_T; Base : Positive; Do_Pad_Zeros : Bool := True) return Str; -- For hex or binary strings
  generic
    type Num_T is digits <>;
  function Generic_To_Str_Real (Item : Num_T) return Str;

  -- Based integer number image routines
  function Image_128      (Item : Int_128_Unsigned; Base : Int_Number_Base := 16) return Str_8;
  function Wide_Image_128 (Item : Int_128_Unsigned; Base : Int_Number_Base := 16) return Str   is (To_Str (Image_128 (Item, Base)));
  function Image_64       (Item : Int_64_Unsigned;  Base : Int_Number_Base := 16) return Str_8 is (Image_128 (Int_128_Unsigned (Item), Base));
  function Wide_Image_64  (Item : Int_64_Unsigned;  Base : Int_Number_Base := 16) return Str   is (To_Str (Image_64 (Item, Base)));
  function Image_32       (Item : Int_32_Unsigned;  Base : Int_Number_Base := 16) return Str_8 is (Image_128 (Int_128_Unsigned (Item), Base));
  function Wide_Image_32  (Item : Int_32_Unsigned;  Base : Int_Number_Base := 16) return Str   is (To_Str (Image_32 (Item, Base)));
  function Image_16       (Item : Int_16_Unsigned;  Base : Int_Number_Base := 16) return Str_8 is (Image_128 (Int_128_Unsigned (Item), Base));
  function Wide_Image_16  (Item : Int_16_Unsigned;  Base : Int_Number_Base := 16) return Str   is (To_Str (Image_16 (Item, Base)));
  function Image          (Item : Int_32_Unsigned;  Base : Int_Number_Base := 16) return Str_8 renames Image_32;
  function Wide_Image     (Item : Int_32_Unsigned;  Base : Int_Number_Base := 16) return Str   renames Wide_Image_32;

  -- Friendly image routines for reals
  function Image_128      (Item : Real_128) return Str_8;
  function Wide_Image_128 (Item : Real_128) return Str   is (To_Str (Image_128 (Item)));
  function Image_64       (Item : Real_64)  return Str_8 is (Image_128 (Real_128 (Item)));
  function Wide_Image_64  (Item : Real_64)  return Str   is (To_Str (Image_64 (Item)));
  function Image_32       (Item : Real_32)  return Str_8 is (Image_128 (Real_128 (Item)));
  function Wide_Image_32  (Item : Real_32)  return Str   is (To_Str (Image_32 (Item)));
  function Image          (Item : Real_64)  return Str_8 renames Image_64;
  function Wide_Image     (Item : Real_64)  return Str   renames Wide_Image_64;

  generic
    Max_Length : Positive;
  package Bounded_Str_8_Super is
    subtype T is Str_8_Super (Max_Length);
    NULL_STR : constant T := To_Super_String ("", Max_Length);
    type Array_T is array (Positive range <>) of T;
    function To_Str_8_Super (Val : Str_8) return T is (To_Super_String (Val, Max_Length)); -- !!!
    function S              (Val : Str_8) return T renames To_Str_8_Super;
  end;

  generic
    Max_Length : Positive;
  package Bounded_Str_16_Super is
    subtype T is Str_16_Super (Max_Length);
    NULL_STR : constant T := To_Super_String ("", Max_Length);
    type Array_T is array (Positive range <>) of T;
    function To_Str_16_Super (Val : Str) return T is (To_Super_String (Val, Max_Length)); -- !!!
    function S               (Val : Str) return T renames To_Str_16_Super;
  end;

  generic
    Max_Length : Positive;
  package Bounded_Str_32_Super is
    subtype T is Str_32_Super (Max_Length);
    NULL_STR : constant T := To_Super_String ("", Max_Length);
    type Array_T is array (Positive range <>) of T;
    function To_Str_32_Super (Val : Str_32) return T is (To_Super_String (Val, Max_Length)); -- !!!
    function S               (Val : Str_32) return T renames To_Str_32_Super;
  end;

  -- Super strings
  package Super_64_Str   is new Bounded_Str_16_Super (64);
  package Super_128_Str  is new Bounded_Str_16_Super (128);
  package Super_256_Str  is new Bounded_Str_16_Super (256);
  package Super_512_Str  is new Bounded_Str_16_Super (512);
  package Super_1024_Str is new Bounded_Str_16_Super (1024);

  -- String contants
  NULL_CHAR_8         :          Char_8         renames ASCII.NUL;
  NULL_CHAR_8_C       :          Char_8_C       renames Interfaces.C.NUL;
  NULL_CHAR_16        : constant Char_16        := Char_16'Val   (Char_8'Pos   (NULL_CHAR_8));
  NULL_CHAR_16_C      : constant Char_16_C      := Char_16_C'Val (Char_8_C'Pos (NULL_CHAR_8_C));
  NULL_CHAR_32        : constant Char_32        := Char_32'Val   (Char_8'Pos  (NULL_CHAR_8));
  NULL_STR_8          : constant Str_8          := "";
  NULL_STR_8_C        : constant Str_8_C        := To_Str_8_C  (NULL_STR_8);
  NULL_STR_8_UNBOUND  :          Str_8_Unbound  renames NULL_UNBOUNDED_STRING;
  NULL_STR_16         : constant Str_16         := "";
  NULL_STR_16_C       : constant Str_C          := To_Str_16_C (NULL_STR_16);
  NULL_STR_16_UNBOUND :          Str_16_Unbound renames NULL_UNBOUNDED_WIDE_STRING;
  NULL_STR_32         :          Str_32         := "";
  NULL_STR_32_UNBOUND :          Str_32_Unbound renames NULL_UNBOUNDED_WIDE_WIDE_STRING;
  TOFU_8              : constant Char_8         := Char_8'Val (127);
  EOL_8               : constant Str_8          := CR & LF;
  TAB_8               : constant Str_8          := "" & HT;
  TOFU_16             : constant Char_16        := Char_16'Val (Char_8'Pos (TOFU_8));
  EOL_16              : constant Str_16         := To_Char_16 (CR) & To_Char_16 (LF);
  TAB_16              : constant Str_16         := To_Char_16 (HT) & "";
  TOFU_32             : constant Char_32        := Char_32'Val (Char_8'Pos (TOFU_8));
  EOL_32              : constant Str_32         := To_Char_32 (CR) & To_Char_32 (LF);
  TAB_32              : constant Str_32         := To_Char_32 (HT) & "";
  TOFU                :          Char_16        renames TOFU_16;
  EOL                 :          Str_16         renames EOL_16;
  TAB                 :          Str_16         renames TAB_16;
  NULL_CHAR           :          Char_16        renames NULL_CHAR_16;
  NULL_CHAR_C         :          Char_16_C      renames NULL_CHAR_16_C;
  NULL_STR            :          Str_16         renames NULL_STR_16;
  NULL_STR_C          :          Str_C          renames NULL_STR_16_C;
  NULL_STR_UNBOUND    :          Str_16_Unbound renames NULL_STR_16_UNBOUND;
end;
