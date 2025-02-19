
--                                                                                                                                                          --
--                                                                   N E O  E N G I N E                                                                     --
--                                                                                                                                                          --
--                                                            Copyright (C) 202X Justin Squirek                                                             --
--                                                                                                                                                          --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published  by the Free Software      --
-- Foundation, either version 3 of the License, or (at your option) any later version.                                                                      --
--                                                                                                                                                          --
-- Neo is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a   --
-- particular purpose. See the GNU General Public License for more details.                                                                                 --
--                                                                                                                                                          --

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
procedure Neo.SIMD is

  -- Needs -ffinite-math-only or -ffast-math
  pragma Warnings (Off, "should usually be Volatile"); -- Ignore "should be volatile" warnings - we want things to be optimized

  type Vector_2D is record X, Y       : Real := 0.0; end record;
  type Vector_3D is record X, Y, Z    : Real := 0.0; end record;
  type Vector_4D is record X, Y, Z, W : Real := 0.0; end record with Alignment => 16;
  type Matrix_2D is record
      XX, XY,
      YX, YY : Real_32 := 0.0;
    end record with Alignment => 8;
  type Matrix_3D is record -- Listing 1.3
      XX, XY, XZ,
      YX, YY, YZ, 
      ZX, ZY, ZZ : Real_32 := 0.0;
    end record with Alignment => 4;
  type Matrix_4D is record
      XX, XY, XZ, XW,
      YX, YY, YZ, YW,
      ZX, ZY, ZZ, ZW,
      WX, WY, WZ, WW : Real_32 := 0.0;
    end record with Alignment => 32;
  type Transform_4D is record -- Listing 2.9
      XX, YX, ZX, WX,
      XY, YY, ZY, WY,
      XZ, YZ, ZZ, WZ : Real_32 := 0.0;
      -- 0.0, 0.0, 0.0, 1.0
    end record with Alignment => 16;
  type Point_2D is new Vector_2D;
  type Point_3D is new Vector_3D;
  type Plane_4D is new Vector_4D; -- Listing 3.4

  E : constant Str_8 := EOL_8; -- Abbreviation for convenience

  NOT_A_NUMBER      : constant Real := To_Real_32 (16#7FC0_0000#);
  INFINITY          : constant Real := To_Real_32 (16#7F80_0000#);
  NEGATIVE_INFINITY : constant Real := To_Real_32 (16#FF80_0000#);
  ONE               : constant Real := 1.0;
  ZERO              : constant Real := 0.0;
  THREE             : constant Real := 3.0;
  NEGATIVE_ONE      : constant Real := -1.0;
  NEGATIVE_HALF     : constant Real := -0.5;
  NEGATIVE_ZERO     : constant Real := -0.0;
  FIRST_REAL        : constant Real := Real'First;
  SIGN_MASK         : constant Int_Unsigned := 16#7FFF_FFFF#;

  -- Table for calculating trigonometry functions
  TABLE_SCALE_FACTOR : constant Int_Unsigned := 16#4222_F983#; -- Scaling factor for indexing
  TRIGONOMETRY_TABLE : array (1..256, 1..2) of Int_Unsigned :=
    ((16#3F800000#, 16#00000000#), (16#3F7FEC43#, 16#3CC90AB0#), (16#3F7FB10F#, 16#3D48FB30#), (16#3F7F4E6D#, 16#3D96A905#), (16#3F7EC46D#, 16#3DC8BD36#),
     (16#3F7E1324#, 16#3DFAB273#), (16#3F7D3AAC#, 16#3E164083#), (16#3F7C3B28#, 16#3E2F10A3#), (16#3F7B14BE#, 16#3E47C5C2#), (16#3F79C79D#, 16#3E605C13#),
     (16#3F7853F8#, 16#3E78CFCD#), (16#3F76BA07#, 16#3E888E94#), (16#3F74FA0B#, 16#3E94A031#), (16#3F731447#, 16#3EA09AE5#), (16#3F710908#, 16#3EAC7CD4#),
     (16#3F6ED89E#, 16#3EB8442A#), (16#3F6C835E#, 16#3EC3EF16#), (16#3F6A09A6#, 16#3ECF7BCB#), (16#3F676BD8#, 16#3EDAE880#), (16#3F64AA59#, 16#3EE63375#),
     (16#3F61C597#, 16#3EF15AEA#), (16#3F5EBE05#, 16#3EFC5D28#), (16#3F5B941A#, 16#3F039C3D#), (16#3F584853#, 16#3F08F59B#), (16#3F54DB31#, 16#3F0E39DA#),
     (16#3F514D3D#, 16#3F13682B#), (16#3F4D9F02#, 16#3F187FC0#), (16#3F49D112#, 16#3F1D7FD2#), (16#3F45E403#, 16#3F22679A#), (16#3F41D870#, 16#3F273656#),
     (16#3F3DAEF9#, 16#3F2BEB4A#), (16#3F396842#, 16#3F3085BB#), (16#3F3504F3#, 16#3F3504F3#), (16#3F3085BA#, 16#3F396842#), (16#3F2BEB49#, 16#3F3DAEFA#),
     (16#3F273655#, 16#3F41D871#), (16#3F226799#, 16#3F45E403#), (16#3F1D7FD1#, 16#3F49D112#), (16#3F187FC0#, 16#3F4D9F02#), (16#3F13682A#, 16#3F514D3D#),
     (16#3F0E39D9#, 16#3F54DB32#), (16#3F08F59B#, 16#3F584853#), (16#3F039C3C#, 16#3F5B941B#), (16#3EFC5D27#, 16#3F5EBE05#), (16#3EF15AE7#, 16#3F61C598#),
     (16#3EE63374#, 16#3F64AA59#), (16#3EDAE881#, 16#3F676BD8#), (16#3ECF7BC9#, 16#3F6A09A7#), (16#3EC3EF15#, 16#3F6C835E#), (16#3EB84427#, 16#3F6ED89E#),
     (16#3EAC7CD3#, 16#3F710908#), (16#3EA09AE2#, 16#3F731448#), (16#3E94A030#, 16#3F74FA0B#), (16#3E888E93#, 16#3F76BA07#), (16#3E78CFC8#, 16#3F7853F8#),
     (16#3E605C12#, 16#3F79C79D#), (16#3E47C5BC#, 16#3F7B14BF#), (16#3E2F10A0#, 16#3F7C3B28#), (16#3E164085#, 16#3F7D3AAC#), (16#3DFAB26C#, 16#3F7E1324#),
     (16#3DC8BD35#, 16#3F7EC46D#), (16#3D96A8FB#, 16#3F7F4E6D#), (16#3D48FB29#, 16#3F7FB10F#), (16#3CC90A7E#, 16#3F7FEC43#), (16#00000000#, 16#3F800000#),
     (16#BCC90A7E#, 16#3F7FEC43#), (16#BD48FB29#, 16#3F7FB10F#), (16#BD96A8FB#, 16#3F7F4E6D#), (16#BDC8BD35#, 16#3F7EC46D#), (16#BDFAB26C#, 16#3F7E1324#),
     (16#BE164085#, 16#3F7D3AAC#), (16#BE2F10A0#, 16#3F7C3B28#), (16#BE47C5BC#, 16#3F7B14BF#), (16#BE605C12#, 16#3F79C79D#), (16#BE78CFC8#, 16#3F7853F8#),
     (16#BE888E93#, 16#3F76BA07#), (16#BE94A030#, 16#3F74FA0B#), (16#BEA09AE2#, 16#3F731448#), (16#BEAC7CD3#, 16#3F710908#), (16#BEB84427#, 16#3F6ED89E#),
     (16#BEC3EF15#, 16#3F6C835E#), (16#BECF7BC9#, 16#3F6A09A7#), (16#BEDAE881#, 16#3F676BD8#), (16#BEE63374#, 16#3F64AA59#), (16#BEF15AE7#, 16#3F61C598#),
     (16#BEFC5D27#, 16#3F5EBE05#), (16#BF039C3C#, 16#3F5B941B#), (16#BF08F59B#, 16#3F584853#), (16#BF0E39D9#, 16#3F54DB32#), (16#BF13682A#, 16#3F514D3D#),
     (16#BF187FC0#, 16#3F4D9F02#), (16#BF1D7FD1#, 16#3F49D112#), (16#BF226799#, 16#3F45E403#), (16#BF273655#, 16#3F41D871#), (16#BF2BEB49#, 16#3F3DAEFA#),
     (16#BF3085BA#, 16#3F396842#), (16#BF3504F3#, 16#3F3504F3#), (16#BF396842#, 16#3F3085BB#), (16#BF3DAEF9#, 16#3F2BEB4A#), (16#BF41D870#, 16#3F273656#),
     (16#BF45E403#, 16#3F22679A#), (16#BF49D112#, 16#3F1D7FD2#), (16#BF4D9F02#, 16#3F187FC0#), (16#BF514D3D#, 16#3F13682B#), (16#BF54DB31#, 16#3F0E39DA#),
     (16#BF584853#, 16#3F08F59B#), (16#BF5B941A#, 16#3F039C3D#), (16#BF5EBE05#, 16#3EFC5D28#), (16#BF61C597#, 16#3EF15AEA#), (16#BF64AA59#, 16#3EE63375#),
     (16#BF676BD8#, 16#3EDAE880#), (16#BF6A09A6#, 16#3ECF7BCB#), (16#BF6C835E#, 16#3EC3EF16#), (16#BF6ED89E#, 16#3EB8442A#), (16#BF710908#, 16#3EAC7CD4#),
     (16#BF731447#, 16#3EA09AE5#), (16#BF74FA0B#, 16#3E94A031#), (16#BF76BA07#, 16#3E888E94#), (16#BF7853F8#, 16#3E78CFCD#), (16#BF79C79D#, 16#3E605C13#),
     (16#BF7B14BE#, 16#3E47C5C2#), (16#BF7C3B28#, 16#3E2F10A3#), (16#BF7D3AAC#, 16#3E164083#), (16#BF7E1324#, 16#3DFAB273#), (16#BF7EC46D#, 16#3DC8BD36#),
     (16#BF7F4E6D#, 16#3D96A905#), (16#BF7FB10F#, 16#3D48FB30#), (16#BF7FEC43#, 16#3CC90AB0#), (16#BF800000#, 16#00000000#), (16#BF7FEC43#, 16#BCC90AB0#),
     (16#BF7FB10F#, 16#BD48FB30#), (16#BF7F4E6D#, 16#BD96A905#), (16#BF7EC46D#, 16#BDC8BD36#), (16#BF7E1324#, 16#BDFAB273#), (16#BF7D3AAC#, 16#BE164083#),
     (16#BF7C3B28#, 16#BE2F10A3#), (16#BF7B14BE#, 16#BE47C5C2#), (16#BF79C79D#, 16#BE605C13#), (16#BF7853F8#, 16#BE78CFCD#), (16#BF76BA07#, 16#BE888E94#),
     (16#BF74FA0B#, 16#BE94A031#), (16#BF731447#, 16#BEA09AE5#), (16#BF710908#, 16#BEAC7CD4#), (16#BF6ED89E#, 16#BEB8442A#), (16#BF6C835E#, 16#BEC3EF16#),
     (16#BF6A09A6#, 16#BECF7BCB#), (16#BF676BD8#, 16#BEDAE880#), (16#BF64AA59#, 16#BEE63375#), (16#BF61C597#, 16#BEF15AEA#), (16#BF5EBE05#, 16#BEFC5D28#),
     (16#BF5B941A#, 16#BF039C3D#), (16#BF584853#, 16#BF08F59B#), (16#BF54DB31#, 16#BF0E39DA#), (16#BF514D3D#, 16#BF13682B#), (16#BF4D9F02#, 16#BF187FC0#),
     (16#BF49D112#, 16#BF1D7FD2#), (16#BF45E403#, 16#BF22679A#), (16#BF41D870#, 16#BF273656#), (16#BF3DAEF9#, 16#BF2BEB4A#), (16#BF396842#, 16#BF3085BB#),
     (16#BF3504F3#, 16#BF3504F3#), (16#BF3085BA#, 16#BF396842#), (16#BF2BEB49#, 16#BF3DAEFA#), (16#BF273655#, 16#BF41D871#), (16#BF226799#, 16#BF45E403#),
     (16#BF1D7FD1#, 16#BF49D112#), (16#BF187FC0#, 16#BF4D9F02#), (16#BF13682A#, 16#BF514D3D#), (16#BF0E39D9#, 16#BF54DB32#), (16#BF08F59B#, 16#BF584853#),
     (16#BF039C3C#, 16#BF5B941B#), (16#BEFC5D27#, 16#BF5EBE05#), (16#BEF15AE7#, 16#BF61C598#), (16#BEE63374#, 16#BF64AA59#), (16#BEDAE881#, 16#BF676BD8#),
     (16#BECF7BC9#, 16#BF6A09A7#), (16#BEC3EF15#, 16#BF6C835E#), (16#BEB84427#, 16#BF6ED89E#), (16#BEAC7CD3#, 16#BF710908#), (16#BEA09AE2#, 16#BF731448#),
     (16#BE94A030#, 16#BF74FA0B#), (16#BE888E93#, 16#BF76BA07#), (16#BE78CFC8#, 16#BF7853F8#), (16#BE605C12#, 16#BF79C79D#), (16#BE47C5BC#, 16#BF7B14BF#),
     (16#BE2F10A0#, 16#BF7C3B28#), (16#BE164085#, 16#BF7D3AAC#), (16#BDFAB26C#, 16#BF7E1324#), (16#BDC8BD35#, 16#BF7EC46D#), (16#BD96A8FB#, 16#BF7F4E6D#),
     (16#BD48FB29#, 16#BF7FB10F#), (16#BCC90A7E#, 16#BF7FEC43#), (16#00000000#, 16#BF800000#), (16#3CC90A7E#, 16#BF7FEC43#), (16#3D48FB29#, 16#BF7FB10F#),
     (16#3D96A8FB#, 16#BF7F4E6D#), (16#3DC8BD35#, 16#BF7EC46D#), (16#3DFAB26C#, 16#BF7E1324#), (16#3E164085#, 16#BF7D3AAC#), (16#3E2F10A0#, 16#BF7C3B28#),
     (16#3E47C5BC#, 16#BF7B14BF#), (16#3E605C12#, 16#BF79C79D#), (16#3E78CFC8#, 16#BF7853F8#), (16#3E888E93#, 16#BF76BA07#), (16#3E94A030#, 16#BF74FA0B#),
     (16#3EA09AE2#, 16#BF731448#), (16#3EAC7CD3#, 16#BF710908#), (16#3EB84427#, 16#BF6ED89E#), (16#3EC3EF15#, 16#BF6C835E#), (16#3ECF7BC9#, 16#BF6A09A7#),
     (16#3EDAE881#, 16#BF676BD8#), (16#3EE63374#, 16#BF64AA59#), (16#3EF15AE7#, 16#BF61C598#), (16#3EFC5D27#, 16#BF5EBE05#), (16#3F039C3C#, 16#BF5B941B#),
     (16#3F08F59B#, 16#BF584853#), (16#3F0E39D9#, 16#BF54DB32#), (16#3F13682A#, 16#BF514D3D#), (16#3F187FC0#, 16#BF4D9F02#), (16#3F1D7FD1#, 16#BF49D112#),
     (16#3F226799#, 16#BF45E403#), (16#3F273655#, 16#BF41D871#), (16#3F2BEB49#, 16#BF3DAEFA#), (16#3F3085BA#, 16#BF396842#), (16#3F3504F3#, 16#BF3504F3#),
     (16#3F396842#, 16#BF3085BB#), (16#3F3DAEF9#, 16#BF2BEB4A#), (16#3F41D870#, 16#BF273656#), (16#3F45E403#, 16#BF22679A#), (16#3F49D112#, 16#BF1D7FD2#),
     (16#3F4D9F02#, 16#BF187FC0#), (16#3F514D3D#, 16#BF13682B#), (16#3F54DB31#, 16#BF0E39DA#), (16#3F584853#, 16#BF08F59B#), (16#3F5B941A#, 16#BF039C3D#),
     (16#3F5EBE05#, 16#BEFC5D28#), (16#3F61C597#, 16#BEF15AEA#), (16#3F64AA59#, 16#BEE63375#), (16#3F676BD8#, 16#BEDAE880#), (16#3F6A09A6#, 16#BECF7BCB#),
     (16#3F6C835E#, 16#BEC3EF16#), (16#3F6ED89E#, 16#BEB8442A#), (16#3F710908#, 16#BEAC7CD4#), (16#3F731447#, 16#BEA09AE5#), (16#3F74FA0B#, 16#BE94A031#),
     (16#3F76BA07#, 16#BE888E94#), (16#3F7853F8#, 16#BE78CFCD#), (16#3F79C79D#, 16#BE605C13#), (16#3F7B14BE#, 16#BE47C5C2#), (16#3F7C3B28#, 16#BE2F10A3#),
     (16#3F7D3AAC#, 16#BE164083#), (16#3F7E1324#, 16#BDFAB273#), (16#3F7EC46D#, 16#BDC8BD36#), (16#3F7F4E6D#, 16#BD96A905#), (16#3F7FB10F#, 16#BD48FB30#),
     (16#3F7FEC43#, 16#BCC90AB0#)) with Alignment => 64;

  -----------------
  -- Conversions --
  -----------------

  function Truncate_Convert (S : Real) return Int is
    Result : aliased Int;
    begin
      Asm (Clobber  => "xmm0, eax, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", S'Address)),
           Template => " vmovss       (%1), %%xmm0 " & E & --   xmm0 ← V
                       " vcvttss2si %%xmm0,  %%eax " & E & --    eax ← Convert from Real to Int with truncation
                       " movl        %%eax,   (%0) ");     -- Result ← eax
      return Result;
    end;

  -------------
  -- Min Max --
  -------------

  generic
    VCMD : String; -- "vmin" or "vmax"
  function Min_Max_4D (V : Vector_4D) return Real;
  function Min_Max_4D (V : Vector_4D) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "xmm0, xmm1, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", V'Address)),
           Template => " vmovaps    (%1), %%xmm0         " & E & --   xmm0 ← V
                       " vmovhlps %%xmm0, %%xmm0, %%xmm1 " & E & --   xmm1 ← High packed floats of xmm0
                       VCMD & "ps %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← VCMD (xmm0, xmm1)
                       " vpshufd   $0x55, %%xmm0, %%xmm1 " & E & --   xmm1 ← Shuffle xmm0 with control 0x55
                       VCMD & "ss %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← VCMD (xmm0, xmm1)
                       " vmovss   %%xmm0,   (%0)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    VCMD : String; -- "vmin" or "vmax"
  function Min_Max_3D (V : Vector_3D) return Real;
  function Min_Max_3D (V : Vector_3D) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "xmm0, xmm1, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", V'Address)),
           Template => " vmovups    (%1), %%xmm0         " & E & --   xmm0 ← V
                       " vshufps   $0x93, %%xmm0, %%xmm1 " & E & --   xmm1 ← Shuffle xmm0 to xmm1: (?, X, Y, Z)
                       VCMD & "ps %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← VCMD (X, Z), VCMD (Y, ?)
                       VCMD & "ps %%xmm0, %%xmm0, %%xmm0 " & E & --   xmm0 ← VCMD (X, Y, Z, ?)
                       " vmovss   %%xmm0,   (%0)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    VCMD : String; -- "vmin" or "vmax"
  function Min_Max_2D (V : Vector_2D) return Real;
  function Min_Max_2D (V : Vector_2D) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "xmm0, xmm1, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", V'Address)),
           Template => "vmovups     (%1), %%xmm0         " & E & --   xmm0 ← V
                       "vshufps    $0x4E, %%xmm0, %%xmm1 " & E & --   xmm1 ← Shuffle xmm0: (Y, X)
                       VCMD & "ps %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← VCMD (Y, X)
                       "vmovss    %%xmm0,   (%0)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
  function Clamp_T (V : T; Low, High : Real) return T;
  function Clamp_T (V : T; Low, High : Real) return T is
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");   
    Result : aliased T;
    begin
      Asm (Clobber  => "xmm0, xmm1, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", V'Address),
                        Ptr'Asm_Input ("r", Low'Address),
                        Ptr'Asm_Input ("r", High'Address)),
           Template => VMOVPS & "   (%1), %%xmm0         " & E & --   xmm0 ← V
                       " vmovaps    (%2), %%xmm1         " & E & --   xmm1 ← Low
                       " vmaxps   %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← Max (xmm1)
                       " vmovaps    (%3), %%xmm1         " & E & --   xmm1 ← High
                       " vminps   %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← Min (xmm1)
                       VMOVPS & " %%xmm0,   (%0)         ");     -- Result ← xmm0
      return Result;
    end;

  function Max is new Min_Max_4D ("vmax");
  function Max is new Min_Max_3D ("vmax");
  function Max is new Min_Max_2D ("vmax");

  function Min is new Min_Max_4D ("vmin");
  function Min is new Min_Max_3D ("vmin");
  function Min is new Min_Max_2D ("vmin");

  function Clamp is new Clamp_T (Vector_4D, Aligned => False);
  function Clamp is new Clamp_T (Vector_3D, Aligned => False);
  function Clamp is new Clamp_T (Vector_2D);

  ------------
  -- Normal --
  ------------

  generic
    type T is private;
    VDPPS_Flag : String;
    Aligned    : Bool := True;
  function Normalize_T (V : T) return T;
  function Normalize_T (V : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm2, memory",
           Inputs   => (Ptr'Asm_Input ("r", V'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "              (%1), %%xmm0                 " & E & --   xmm0 ← V
                       " vdpps   " & VDPPS_Flag & ", %%xmm0, %%xmm0, %%xmm2 " & E & --   xmm2 ← xmm0 · xmm0
                       " vsqrtss             %%xmm2, %%xmm2, %%xmm2         " & E & --   xmm2 ← Sqrt (xmm2)
                       " vshufps              $0x00, %%xmm2, %%xmm2, %%xmm2 " & E & --   xmm2 ← (xmm2.X, ...)
                       " vrcpps              %%xmm2, %%xmm2                 " & E & --   xmm2 ← Reciprocal of xmm2
                       " vmulps              %%xmm2, %%xmm0, %%xmm0         " & E & --   xmm0 ← xmm0 * xmm2
                       VMOVPS & "            %%xmm0, (%0)                   ");     -- Result ← xmm0
      return Result;
    end;

  function Normalize is new Normalize_T (Vector_2D, "$0x33", Aligned => False);
  function Normalize is new Normalize_T (Vector_3D, "$0x77", Aligned => False);
  function Normalize is new Normalize_T (Vector_4D, "$0xFF");

  ----------------
  -- Arithmetic --
  ----------------

  generic
    type T is private;
    Aligned : Bool := True;
  function Sum_T (V : T) return Real;
  function Sum_T (V : T) return Real is
    Result : aliased Real;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm1, memory",
           Inputs   => (Ptr'Asm_Input ("r", V'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "  (%0), %%xmm0         " & E & --   xmm0 ← V
                       " vhaddps %%xmm0, %%xmm0, %%xmm1 " & E & --   xmm1 ← (X + Y, Z + 0, X + Y, Z + 0)
                       " vhaddps %%xmm1, %%xmm1, %%xmm0 " & E & --   xmm0 ← (X + Y + Z, ...)
                       " vmovss  %%xmm0,   (%1)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
  function Add_Vector_T (Left, Right : T) return T;
  function Add_Vector_T (Left, Right : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, memory",
           Inputs   => (Ptr'Asm_Input ("r", Left'Address),
                        Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "   (%1), %%xmm0         " & E & --   xmm0 ← Right
                       " vaddps     (%0), %%xmm0, %%xmm0 " & E & --   xmm0 ← Left + xmm0
                       VMOVPS & " %%xmm0,   (%2)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
  function Add_Scalar_T (Left : T; Right : Real) return T;
  function Add_Scalar_T (Left : T; Right : Real) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, memory",
           Inputs   => (Ptr'Asm_Input ("r", Left'Address),
                        Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "   (%1), %%xmm0         " & E & --   xmm0 ← Right
                       " shufps    $0x00, %%xmm1, %%xmm1 " & E & --   xmm1 ← Broadcasted Right
                       " vaddps     (%0), %%xmm0, %%xmm0 " & E & --   xmm0 ← Left + xmm0
                       VMOVPS & " %%xmm0,   (%2)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
  function Subtract_Vector_T (Left, Right : T) return T;
  function Subtract_Vector_T (Left, Right : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, memory",
           Inputs   => (Ptr'Asm_Input ("r", Left'Address),
                        Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "   (%1), %%xmm0         " & E & --   xmm0 ← Right
                       " vsubps     (%0), %%xmm0, %%xmm0 " & E & --   xmm0 ← Left - xmm0
                       VMOVPS & " %%xmm0,   (%2)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
  function Subtract_Scalar_T (Left : T; Right : Real) return T;
  function Subtract_Scalar_T (Left : T; Right : Real) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, memory",
           Inputs   => (Ptr'Asm_Input ("r", Left'Address),
                        Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "   (%1), %%xmm0         " & E & --   xmm0 ← Right
                       " shufps    $0x00, %%xmm1, %%xmm1 " & E & --   xmm1 ← Broadcasted Right
                       " vsubps     (%0), %%xmm0, %%xmm0 " & E & --   xmm0 ← Left - xmm0
                       VMOVPS & " %%xmm0,   (%2)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
  function Negate_T (Right : T) return T;
  function Negate_T (Right : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, memory",
           Inputs   => (Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", NEGATIVE_ZERO'Address)),
           Template => " vbroadcastss   (%2), %%xmm0         " & E & --   xmm0 ← Broadcasted NEGATIVE_ZERO
                       " vxorps         (%0), %%xmm0, %%xmm0 " & E & --   xmm0 ← Right xor NEGATIVE_ZERO
                       VMOVPS & "     %%xmm0,   (%1)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
  function Divide_Vector_T (Left, Right : T) return T;
  function Divide_Vector_T (Left, Right : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", Left'Address),
                        Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "   (%0), %%xmm0         " & E & --   xmm0 ← Left
                       VMOVPS & "   (%1), %%xmm1         " & E & --   xmm1 ← Right
                       " vdivps   %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm0 / xmm1
                       VMOVPS & " %%xmm0,   (%2)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
  function Divide_Scalar_T (Left : T; Right : Real) return T;
  function Divide_Scalar_T (Left : T; Right : Real) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", Left'Address),
                        Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "   (%0), %%xmm0         " & E & --   xmm0 ← Left
                       VMOVPS & "   (%1), %%xmm1         " & E & --   xmm1 ← Right
                       " shufps    $0x00, %%xmm1, %%xmm1 " & E & --   xmm1 ← Shuffle xmm1
                       " vdivps   %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm0 / xmm1
                       VMOVPS & " %%xmm0,   (%2)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
  function Abs_T (Right : T) return T;
  function Abs_T (Right : T) return T is
    VMOVPS     : constant String                       := (if Aligned then "vmovaps" else "vmovups");   
    SIGN_MASKS : constant array (1..4) of Int_Unsigned := (others => SIGN_MASK); 
    Result     : aliased T;
    begin
      Asm (Clobber  => "xmm0, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", SIGN_MASKS (1)'Address)),
           Template => VMOVPS & "   (%1), %%xmm0         " & E & --   xmm0 ← V
                       "vandps      (%2), %%xmm0, %%xmm0 " & E & --   xmm0 ← Clear sign bits by using an and op with xmm0 and SIGN_MASKS
                       VMOVPS & " %%xmm0, (%0)           ");     -- Result ← xmm0
      return Result;
    end;

  function "+" is new Add_Vector_T (Vector_2D);
  function "+" is new Add_Vector_T (Vector_3D, Aligned => False);
  function "+" is new Add_Vector_T (Vector_4D);

  function "+" is new Add_Scalar_T (Vector_2D);
  function "+" is new Add_Scalar_T (Vector_3D, Aligned => False);
  function "+" is new Add_Scalar_T (Vector_4D);

  function "-" is new Subtract_Vector_T (Vector_2D);
  function "-" is new Subtract_Vector_T (Vector_3D, Aligned => False);
  function "-" is new Subtract_Vector_T (Vector_4D);

  function "-" is new Subtract_Scalar_T (Vector_2D);
  function "-" is new Subtract_Scalar_T (Vector_3D, Aligned => False);
  function "-" is new Subtract_Scalar_T (Vector_4D);

  function "/" is new Divide_Vector_T (Vector_2D);
  function "/" is new Divide_Vector_T (Vector_3D, Aligned => False);
  function "/" is new Divide_Vector_T (Vector_4D);

  function "/" is new Divide_Scalar_T (Vector_2D);
  function "/" is new Divide_Scalar_T (Vector_3D, Aligned => False);
  function "/" is new Divide_Scalar_T (Vector_4D);

  function "-" is new Negate_T (Vector_2D);
  function "-" is new Negate_T (Vector_3D, Aligned => False);
  function "-" is new Negate_T (Vector_4D);

  function "Abs" is new Abs_T (Vector_4D);
  function "Abs" is new Abs_T (Vector_3D, Aligned => False);
  function "Abs" is new Abs_T (Vector_2D);

  function Sum is new Sum_T (Vector_2D);
  function Sum is new Sum_T (Vector_3D, Aligned => False);
  function Sum is new Sum_T (Vector_4D);

  --------------
  -- Rounding --
  --------------

  generic
    type T is private;
    VROUND_Flag : String;
    Aligned     : Bool := True;
  function Round_T (V : T) return T;
  function Round_T (V : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, memory",
           Inputs   => (Ptr'Asm_Input ("r", V'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => " vroundps " & VROUND_Flag & ", (%0), %%xmm0 " & E & --   xmm0 ← Round (V, VROUND_Flag)
                       VMOVPS & "              %%xmm0, (%1)        ");      -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
  procedure Floor_Ceiling_T (V : T; Floor, Ceiling : out T);
  procedure Floor_Ceiling_T (V : T; Floor, Ceiling : out T) is
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm1, memory",
           Inputs   => (Ptr'Asm_Input ("r", V'Address),
                        Ptr'Asm_Input ("r", Floor'Address),
                        Ptr'Asm_Input ("r", Ceiling'Address)),
           Template => " vroundps     $9, (%0), %%xmm0 " & E & --    xmm0 ← Floor (V)
                       " vroundps    $10, (%0), %%xmm1 " & E & --    xmm1 ← Ceiling (V)
                       VMOVPS & " %%xmm0, (%1)         " & E & -- Floor   ← xmm0
                       VMOVPS & " %%xmm1, (%2)         ");     -- Ceiling ← xmm1
    end;

  -----------
  -- Signs --
  -----------

  generic
    type T is private;
    Value   : Real;
    Aligned : Bool := True;
  function Signs_T (V : T) return T;
  function Signs_T (V : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, memory",
           Inputs   => (Ptr'Asm_Input ("r", V'Address),
                        Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", NEGATIVE_ZERO'Address),
                        Ptr'Asm_Input ("r", Value'Address),
                        Ptr'Asm_Input ("r", NOT_A_NUMBER'Address)),
           Template => " vmovdqa        (%0), %%xmm0         " & E & --   xmm0 ← V
                       " vpbroadcastd   (%2), %%xmm1         " & E & --   xmm1 ← NEGATIVE_ZERO
                       " vpand        %%xmm1, %%xmm0, %%xmm1 " & E & --   xmm1 ← V and NEGATIVE_ZERO
                       " vpbroadcastd   (%3), %%xmm2         " & E & --   xmm2 ← Value
                       " vpor         %%xmm2, %%xmm1, %%xmm1 " & E & --   xmm1 ← Value or (V and NEGATIVE_ZERO)
                       " vpbroadcastd   (%4), %%xmm2         " & E & --   xmm2 ← NOT_A_NUMBER
                       " vpand        %%xmm2, %%xmm0, %%xmm0 " & E & --   xmm0 ← V and NOT_A_NUMBER
                       " vpxor        %%xmm2, %%xmm2, %%xmm2 " & E & --   xmm2 ← 0.0
                       " vpcmpgtd     %%xmm2, %%xmm0, %%xmm0 " & E & --   xmm0 ← if V > 0 then Mask
                       " vpand        %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← (Value or ...) and Mask
                       VMOVPS & "     %%xmm0,   (%1)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Value   : Real;
    Aligned : Bool := True;
  function Nonzero_Signs_T (V : T) return T;
  function Nonzero_Signs_T (V : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, memory",
           Inputs   => (Ptr'Asm_Input ("r", V'Address),
                        Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", NEGATIVE_ZERO'Address),
                        Ptr'Asm_Input ("r", Value'Address)),
           Template => " vbroadcastss   (%2), %%xmm0         " & E & --   xmm0 ← NEGATIVE_ZERO
                       " vandps         (%0), %%xmm0, %%xmm0 " & E & --   xmm0 ← V and NEGATIVE_ZERO
                       " vbroadcastss   (%3), %%xmm1         " & E & --   xmm1 ← ONE
                       " vorps        %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm0 or xmm1
                       VMOVPS & "     %%xmm0,   (%1)         ");     -- Result ← xmm0
      return Result;
    end;

  function Signs is new Signs_T (Vector_2D, ONE, Aligned => False);
  function Signs is new Signs_T (Vector_3D, ONE, Aligned => False);
  function Signs is new Signs_T (Vector_4D, ONE);

  function Negated_Signs is new Signs_T (Vector_2D, NEGATIVE_ONE, Aligned => False);
  function Negated_Signs is new Signs_T (Vector_3D, NEGATIVE_ONE, Aligned => False);
  function Negated_Signs is new Signs_T (Vector_4D, NEGATIVE_ONE);

  function Nonzero_Signs is new Nonzero_Signs_T (Vector_2D, ONE, Aligned => False);
  function Nonzero_Signs is new Nonzero_Signs_T (Vector_3D, ONE, Aligned => False);
  function Nonzero_Signs is new Nonzero_Signs_T (Vector_4D, ONE);

  function Nonzero_Negated_Signs is new Nonzero_Signs_T (Vector_2D, NEGATIVE_ONE, Aligned => False);
  function Nonzero_Negated_Signs is new Nonzero_Signs_T (Vector_3D, NEGATIVE_ONE, Aligned => False);
  function Nonzero_Negated_Signs is new Nonzero_Signs_T (Vector_4D, NEGATIVE_ONE);

  ------------------
  -- Trigonometry --
  ------------------

  function Sin (X : Real) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "al, eax, rax, rcx, xmm0, xmm1, memory",
           Inputs   => (Ptr'Asm_Input ("r", SIGN_MASK'Address),
                        Ptr'Asm_Input ("r", TABLE_SCALE_FACTOR'Address),
                        Ptr'Asm_Input ("r", ZERO'Address),
                        Ptr'Asm_Input ("r", TRIGONOMETRY_TABLE'Address),
                        Ptr'Asm_Input ("r", X'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => " vmovss         (%4), %%xmm0                 " & E & --   xmm0 ← X
                       " vbroadcastss   (%0), %%xmm1                 " & E & --   xmm1 ← SIGN_MASK
                       " vandps       %%xmm1, %%xmm0, %%xmm1         " & E & --   xmm1 ← X and SIGN_MASK (clear sign bit)
                       " vmulss         (%1), %%xmm1, %%xmm1         " & E & --   xmm1 ← xmm1 * TABLE_SCALE_FACTOR
                       " vcvttss2si   %%xmm1,  %%eax                 " & E & --    eax ← Int (xmm1.X)
                       " movzbl         %%al,  %%eax                 " & E & --    eax ← Zero extend al into eax
                       " leaq           (%3),  %%rcx                 " & E & --    rcx ← TRIGONOMETRY_TABLE
                       " vbroadcastss   (%2), %%xmm1                 " & E & --   xmm1 ← ZERO
                       " vmovss       4(%%rcx, %%rax, 8), %%xmm2     " & E & --   xmm2 ← Sin from table
                       " vbroadcastss   (%0), %%xmm1                 " & E & --   xmm1 ← SIGN_MASK
                       " vxorps       %%xmm3, %%xmm3, %%xmm3         " & E & --   xmm3 ← 0.0
                       " vcmpltss     %%xmm3, %%xmm0, %%xmm0         " & E & --   xmm0 ← Mask if X < 0.0
                       " vblendvps    %%xmm0, %%xmm1, %%xmm2, %%xmm0 " & E & --   xmm0 ← Select xmm1 or xmm2 based on mask
                       " vmovss       %%xmm0,   (%5)                 ");     -- Result ← xmm0 
      return Result;
    end;
  function Cos (X : Real) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "al, eax, rax, rcx, xmm0, xmm1, memory",
           Inputs   => (Ptr'Asm_Input ("r", SIGN_MASK'Address),
                        Ptr'Asm_Input ("r", TABLE_SCALE_FACTOR'Address),
                        Ptr'Asm_Input ("r", TRIGONOMETRY_TABLE'Address),
                        Ptr'Asm_Input ("r", X'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => " vmovss         (%3), %%xmm0            " & E & --   xmm0 ← X
                       " vbroadcastss   (%0), %%xmm1            " & E & --   xmm1 ← SIGN_MASK
                       " vandps       %%xmm1, %%xmm0, %%xmm0    " & E & --   xmm0 ← X and SIGN_MASK
                       " vmulss         (%1), %%xmm0, %%xmm0    " & E & --   xmm0 ← xmm0 * TABLE_SCALE_FACTOR
                       " vcvttss2si   %%xmm0, %%eax             " & E & --    eax ← Int (xmm0)
                       " movzbl         %%al, %%eax             " & E & --    eax ← Zero extend al into eax
                       " leaq           (%2), %%rcx             " & E & --    rcx ← TRIGONOMETRY_TABLE
                       " vmovss       (%%rcx, %%rax, 8), %%xmm0 " & E & --   xmm0 ← Cos from table
                       " vmovss       %%xmm0,   (%4)            ");     -- Result ← xmm0
      return Result;
    end;
  function Tan (X : Real) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "al, eax, rax, rcx, xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", SIGN_MASK'Address),
                        Ptr'Asm_Input ("r", TABLE_SCALE_FACTOR'Address),
                        Ptr'Asm_Input ("r", TRIGONOMETRY_TABLE'Address),
                        Ptr'Asm_Input ("r", ZERO'Address),
                        Ptr'Asm_Input ("r", X'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => " vmovss         (%4), %%xmm0                     " & E & --   xmm0 ← X
                       " vbroadcastss   (%0), %%xmm1                     " & E & --   xmm1 ← SIGN_MASK
                       " vandps       %%xmm1, %%xmm0, %%xmm1             " & E & --   xmm1 ← X and SIGN_MASK
                       " vmulss         (%1), %%xmm1, %%xmm1             " & E & --   xmm1 ← xmm1 * TABLE_SCALE_FACTOR
                       " vcvttss2si   %%xmm1,  %%eax                     " & E & --    eax ← Int (xmm1)
                       " movzbl         %%al,  %%eax                     " & E & --    eax ← Zero extend al into eax
                       " leaq           (%2),  %%rcx                     " & E & --    rcx ← TRIGONOMETRY_TABLE
                       " vmovss       4(%%rcx, %%rax, 8), %%xmm1         " & E & --   xmm1 ← Numerator from table
                       " vdivss        (%%rcx, %%rax, 8), %%xmm1, %%xmm1 " & E & --   xmm1 ← xmm1 / Denominator from table
                       " vbroadcastss   (%0), %%xmm2                     " & E & --   xmm1 ← SIGN_MASK
                       " vxorps       %%xmm3, %%xmm3, %%xmm3             " & E & --   xmm3 ← 0.0
                       " vcmpltss     %%xmm3, %%xmm0, %%xmm0             " & E & --   xmm0 ← Mask if X < 0.0
                       " vblendvps    %%xmm0, %%xmm2, %%xmm1, %%xmm0     " & E & --   xmm0 ← Select xmm2 or xmm1 based on mask
                       " vmovss       %%xmm0,   (%5)                     ");     -- Result ← xmm0
      return Result;
    end;
  procedure Cos_Sin (Cos_X, Sin_X : out Real; X : Real) is
    begin
      Asm (Clobber  => "al, eax, rax, rcx, xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", SIGN_MASK'Address),
                        Ptr'Asm_Input ("r", TABLE_SCALE_FACTOR'Address),
                        Ptr'Asm_Input ("r", TRIGONOMETRY_TABLE'Address),
                        Ptr'Asm_Input ("r", ZERO'Address),
                        Ptr'Asm_Input ("r", X'Address),
                        Ptr'Asm_Input ("r", Cos_X'Address),
                        Ptr'Asm_Input ("r", Sin_X'Address)),
           Template => " vmovss         (%4), %%xmm0                 " & E & --  xmm0 ← X
                       " vbroadcastss   (%0), %%xmm1                 " & E & --  xmm1 ← SIGN_MASK
                       " vandps       %%xmm1, %%xmm0, %%xmm1         " & E & --  xmm1 ← X and SIGN_MASK
                       " vmulss         (%1), %%xmm1, %%xmm1         " & E & --  xmm1 ← xmm1 * TABLE_SCALE_FACTOR
                       " vcvttss2si   %%xmm1, %%eax                  " & E & --   eax ← Int (xmm1)
                       " movzbl         %%al, %%eax                  " & E & --   eax ← Zero extend al
                       " leaq           (%2), %%rcx                  " & E & --   rcx ← TRIGONOMETRY_TABLE
                       " vmovss        (%%rcx, %%rax, 8), %%xmm1     " & E & --  xmm1 ← Cos from table
                       " movss        %%xmm1,   (%5)                 " & E & -- Cos_X ← xmm1
                       " vbroadcastss   (%3), %%xmm1                 " & E & --  xmm1 ← ZERO
                       " vmovss       4(%%rcx, %%rax, 8), %%xmm2     " & E & --  xmm2 ← Sin from table
                       " vbroadcastss   (%0), %%xmm1                 " & E & --  xmm1 ← SIGN_MASK
                       " vxorps       %%xmm3, %%xmm3, %%xmm3         " & E & --  xmm3 ← 0.0
                       " vcmpltss     %%xmm3, %%xmm0, %%xmm0         " & E & --  xmm0 ← Mask if X < 0.0
                       " vblendvps    %%xmm0, %%xmm1, %%xmm2, %%xmm0 " & E & --  xmm0 ← Select xmm1 or xmm2 based on mask
                       " movss        %%xmm0,   (%6)                 ");     -- Sin_X ← xmm0
    end;

  ----------
  -- Sqrt --
  ----------

  function Sqrt (S : Real) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", S'Address),
                        Ptr'Asm_Input ("r", FIRST_REAL'Address),
                        Ptr'Asm_Input ("r", THREE'Address),
                        Ptr'Asm_Input ("r", NEGATIVE_HALF'Address)), 
           Template => " movss         (%1), %%xmm1                 " & E & --   xmm1 ← V
                       " vxorps      %%xmm1, %%xmm1, %%xmm1         " & E & --   xmm1 ← 0
                       " vblendps        $1, %%xmm0, %%xmm1, %%xmm1 " & E & --   xmm1 ← Blend with mask
                       " vcmpltss      (%2), %%xmm1, %%xmm2         " & E & --   xmm2 ← Compare S < FIRST_REAL
                       " vrsqrtss    %%xmm1, %%xmm1, %%xmm1         " & E & --   xmm1 ← Reciprocal sqrt
                       " vmulss      %%xmm1, %%xmm1, %%xmm3         " & E & --   xmm3 ← xmm1 * xmm1
                       " vfmsub213ss   (%3), %%xmm0, %%xmm3         " & E & --   xmm3 ← (THREE * xmm0) - xmm3
                       " vmulss        (%4), %%xmm1, %%xmm1         " & E & --   xmm1 ← NEGATIVE_HALF * xmm1
                       " vmulss      %%xmm3, %%xmm1, %%xmm1         " & E & --   xmm1 ← xmm3 * xmm1
                       " vandnps     %%xmm1, %%xmm2, %%xmm1         " & E & --   xmm1 ← Masked multiply
                       " vmulss      %%xmm0, %%xmm1, %%xmm0         " & E & --   xmm0 ← xmm0 * xmm1
                       " movss       %%xmm0,   (%0)                 ");     -- Result ← xmm0
      return Result;
    end;
  generic
     type T is private;
     Aligned : Bool := True;
  function Sqrt_T (V : T) return T;
  function Sqrt_T (V : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", V'Address),
                        Ptr'Asm_Input ("r", THREE'Address),
                        Ptr'Asm_Input ("r", NEGATIVE_HALF'Address)),
           Template => VMOVPS & "       (%1), %%xmm0         " & E & --   xmm0 ← V
                       " vrsqrtps     %%xmm0, %%xmm1         " & E & --   xmm1 ← Reciprocal sqrt
                       " vmulps       %%xmm1, %%xmm1, %%xmm2 " & E & --   xmm2 ← xmm1 * xmm1
                       " vbroadcastss   (%2), %%xmm3         " & E & --   xmm3 ← THREE
                       " vfmsub231ps  %%xmm2, %%xmm0, %%xmm3 " & E & --   xmm3 ← (xmm0 - xmm2) * xmm3
                       " vbroadcastss   (%3), %%xmm2         " & E & --   xmm2 ← NEGATIVE_HALF
                       " vmulps       %%xmm2, %%xmm0, %%xmm2 " & E & --   xmm2 ← xmm2 * xmm0
                       " vmulps       %%xmm2, %%xmm1, %%xmm1 " & E & --   xmm1 ← xmm2 * xmm1
                       " vmulps       %%xmm3, %%xmm1, %%xmm1 " & E & --   xmm1 ← xmm3 * xmm1
                       " vxorps       %%xmm2, %%xmm2, %%xmm2 " & E & --   xmm2 ← (0.0, ...)
                       " vcmpneqps    %%xmm2, %%xmm0, %%xmm0 " & E & --   xmm0 ← Compare not equal
                       " vandps       %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm1 and mask
                       VMOVPS & "     %%xmm0,   (%0)         ");     -- Result ← xmm0
      return Result;
    end;
  function Inverse_Sqrt (S : Real) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", S'Address),
                        Ptr'Asm_Input ("r", FIRST_REAL'Address),
                        Ptr'Asm_Input ("r", THREE'Address),
                        Ptr'Asm_Input ("r", NEGATIVE_HALF'Address),
                        Ptr'Asm_Input ("r", INFINITY'Address)), 
           Template => " movss          (%1), %%xmm1                 " & E & --   xmm1 ← V
                       " vxorps       %%xmm1, %%xmm1, %%xmm1         " & E & --   xmm1 ← 0
                       " vblendps         $1, %%xmm0, %%xmm1, %%xmm1 " & E & --   xmm1 ← Blend with mask
                       " vcmpltss       (%2), %%xmm1, %%xmm2         " & E & --   xmm2 ← Compare S < FIRST_REAL
                       " vrsqrtss     %%xmm1, %%xmm1, %%xmm1         " & E & --   xmm1 ← Reciprocal sqrt
                       " vmulss       %%xmm1, %%xmm1, %%xmm3         " & E & --   xmm3 ← xmm1 * xmm1
                       " vfmsub213ss    (%3), %%xmm0, %%xmm3         " & E & --   xmm3 ← (THREE * xmm0) - xmm3
                       " vmulss         (%4), %%xmm1, %%xmm0         " & E & --   xmm0 ← NEGATIVE_HALF * xmm1
                       " vmulss       %%xmm3, %%xmm0, %%xmm0         " & E & --   xmm0 ← xmm3 * xmm0
                       " vandnps      %%xmm0, %%xmm2, %%xmm0         " & E & --   xmm0 ← Masked multiply
                       " vbroadcastss   (%5), %%xmm1                 " & E & --   xmm1 ← INFINITY
                       " vandps       %%xmm1, %%xmm2, %%xmm1         " & E & --   xmm1 ← INFINITY and mask
                       " vorps        %%xmm1, %%xmm0, %%xmm0         " & E & --   xmm0 ← xmm1 or xmm0
                       " movss        %%xmm0,   (%0)                 ");     -- Result ← xmm0
      return Result;
    end;
  generic
     type T is private;
     Aligned : Bool := True;
  function Inverse_Sqrt_T (V : T) return T;
  function Inverse_Sqrt_T (V : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", V'Address),
                        Ptr'Asm_Input ("r", THREE'Address),
                        Ptr'Asm_Input ("r", NEGATIVE_HALF'Address)),                     
           Template => VMOVPS & "       (%1), %%xmm0         " & E & --   xmm0 ← V
                       " vrsqrtps     %%xmm0, %%xmm1         " & E & --   xmm1 ← Reciprocal sqrt of xmm0
                       " vmulps       %%xmm1, %%xmm1, %%xmm2 " & E & --   xmm2 ← xmm1 * xmm1
                       " vbroadcastss   (%2), %%xmm3         " & E & --   xmm3 ← NEGATIVE_HALF
                       " vfmsub231ps  %%xmm2, %%xmm0, %%xmm3 " & E & --   xmm3 ← (V - xmm2) * NEGATIVE_HALF
                       " vbroadcastss   (%3), %%xmm0         " & E & --   xmm0 ← THREE
                       " vmulps       %%xmm0, %%xmm1, %%xmm0 " & E & --   xmm0 ← THREE * xmm1
                       " vmulps       %%xmm3, %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm3 * xmm0
                       VMOVPS & "     %%xmm0,   (%0)         ");     -- Result ← xmm0
      return Result;
    end;

  function Sqrt is new Sqrt_T (Vector_2D, Aligned => False);
  function Sqrt is new Sqrt_T (Vector_3D, Aligned => False);
  function Sqrt is new Sqrt_T (Vector_4D);

  function Inverse_Sqrt is new Inverse_Sqrt_T (Vector_2D, Aligned => False);
  function Inverse_Sqrt is new Inverse_Sqrt_T (Vector_3D, Aligned => False);
  function Inverse_Sqrt is new Inverse_Sqrt_T (Vector_4D);

  ---------
  -- Dot --
  ---------

  generic
    type T is private;
    VDPPS_Flag : String;
    Aligned    : Bool := True;
  function Dot_T (A, B : T) return Real;
  function Dot_T (A, B : T) return Real is
    Result : aliased Real;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, memory",
           Inputs   => (Ptr'Asm_Input ("r", A'Address),
                        Ptr'Asm_Input ("r", B'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "             (%0), %%xmm0                 " & E & --   xmm0 ← A
                       VMOVPS & "             (%1), %%xmm1                 " & E & --   xmm1 ← B
                       " vdpps  " & VDPPS_Flag & ", %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm0 · xmm1
                       " movss              %%xmm0,   (%2)                 ");     -- Result ← xmm0
      return Result;
    end;
  function Dot (Plane : Plane_4D; Point : Point_3D) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", Plane'Address),
                        Ptr'Asm_Input ("r", Point'Address),
                        Ptr'Asm_Input ("r", ONE'Address)),
           Template => " vmovaps     (%2), %%xmm0                 " & E & --   xmm0 ← Point 
                       " vinsertps    $48,   (%3), %%xmm0, %%xmm0 " & E & -- xmm0.W ← ONE
                       " vmulps      (%1), %%xmm0, %%xmm0         " & E & --   xmm0 ← Plane * xmm0
                       " vhaddps   %%xmm0, %%xmm0, %%xmm0         " & E & --   xmm0 ← Horizontal add part 1
                       " vhaddps   %%xmm0, %%xmm0, %%xmm0         " & E & --   xmm0 ← Horizontal add part 2
                       " movss     %%xmm0,   (%0)                 ");     -- Result ← xmm0
      return Result;
    end;

  function Dot is new Dot_T (Vector_2D, "$0x33", Aligned => False);
  function Dot is new Dot_T (Vector_3D, "$0x77", Aligned => False);
  function Dot is new Dot_T (Vector_4D, "$0xFF");

  ----------------
  -- Projection --
  ----------------

  generic
    type T is private;
    VDPPS_Flag : String;
    Aligned    : Bool := True;
  function Project_T (A, B : T) return T;
  function Project_T (A, B : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", A'Address),
                        Ptr'Asm_Input ("r", B'Address)),
           Template => VMOVPS & "             (%1), %%xmm0                 " & E & --   xmm0 ← A
                       VMOVPS & "             (%2), %%xmm1                 " & E & --   xmm1 ← B
                       " vdpps  " & VDPPS_Flag & ", %%xmm0, %%xmm1, %%xmm2 " & E & --   xmm2 ← xmm0 · xmm1
                       " vdpps  " & VDPPS_Flag & ", %%xmm1, %%xmm1, %%xmm3 " & E & --   xmm3 ← xmm1 · xmm1
                       " vdivss             %%xmm3, %%xmm2, %%xmm2         " & E & --   xmm2 ← xmm2 / xmm3 - Need -ffinite-math-only here!
                       " vshufps             $0x00, %%xmm2, %%xmm2, %%xmm2 " & E & --   xmm2 ← (xmm2.X, ...)
                       " vmulps             %%xmm1, %%xmm2, %%xmm0         " & E & --   xmm0 ← xmm1 * xmm2
                       VMOVPS & "           %%xmm0,   (%0)                 ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    VDPPS_Flag : String;
    Aligned    : Bool := True;
  function Reject_T (A, B : T) return T;
  function Reject_T (A, B : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", A'Address),
                        Ptr'Asm_Input ("r", B'Address)),
           Template => VMOVPS & "             (%1), %%xmm0                 " & E & --   xmm0 ← A
                       VMOVPS & "             (%2), %%xmm1                 " & E & --   xmm1 ← B
                       " vdpps  " & VDPPS_Flag & ", %%xmm0, %%xmm1, %%xmm2 " & E & --   xmm2 ← xmm0 · xmm1
                       " vdpps  " & VDPPS_Flag & ", %%xmm1, %%xmm1, %%xmm3 " & E & --   xmm3 ← xmm1 · xmm1
                       " vdivss             %%xmm3, %%xmm2, %%xmm2         " & E & --   xmm2 ← xmm2 / xmm3 - Need -ffinite-math-only here!
                       " vshufps             $0x00, %%xmm2, %%xmm2, %%xmm2 " & E & --   xmm2 ← (xmm2.X, ...)
                       " vmulps             %%xmm1, %%xmm2, %%xmm2         " & E & --   xmm2 ← xmm2 * xmm1
                       " vsubps             %%xmm2, %%xmm0, %%xmm0         " & E & --   xmm0 ← xmm0 - xmm2
                       VMOVPS & "           %%xmm0,   (%0)                 ");     -- Result ← xmm0
      return Result;
    end;

  function Project is new Project_T (Vector_2D, "$0x33", Aligned => False);
  function Project is new Project_T (Vector_3D, "$0x77", Aligned => False);
  function Project is new Project_T (Vector_4D, "$0xFF");

  function Reject is new Reject_T (Vector_2D, "$0x33", Aligned => False);
  function Reject is new Reject_T (Vector_3D, "$0x77", Aligned => False);
  function Reject is new Reject_T (Vector_4D, "$0xFF");

  -----------
  -- Cross --
  -----------

  function Cross (A, B : Vector_3D) return Vector_3D is
    Result : aliased Vector_3D;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", A'Address),
                        Ptr'Asm_Input ("r", B'Address)),
           Template => " vmovups       (%1), %%xmm0                 " & E & --   xmm0 ← A
                       " vmovups       (%2), %%xmm1                 " & E & --   xmm1 ← B
                       " vshufps       $201, %%xmm1, %%xmm1, %%xmm2 " & E & --   xmm2 ← Shuffle xmm1 with control 0x201
                       " vshufps       $201, %%xmm0, %%xmm0, %%xmm3 " & E & --   xmm3 ← Shuffle xmm0 with control 0x201
                       " vmulps      %%xmm1, %%xmm3, %%xmm1         " & E & --   xmm1 ← xmm1 * xmm3
                       " vfmsub231ps %%xmm2, %%xmm0, %%xmm1         " & E & --   xmm1 ← (xmm0 * xmm2) - xmm1
                       " vshufps       $201, %%xmm1, %%xmm1, %%xmm0 " & E & --   xmm0 ← Shuffle xmm1 with control 0x201
                       " vmovups     %%xmm0,   (%0)                 ");     -- Result ← xmm0
      return Result;
    end;

  ---------------
  -- Transform --
  ---------------

  function Transform (T : Transform_4D; V : Vector_3D) return Vector_3D is
    Result : aliased Vector_3D;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", T'Address),
                        Ptr'Asm_Input ("r", V'Address),
                        Ptr'Asm_Input ("r", Result'Address)), 
           Template => " vmovaps  (%1), %%xmm0                 " & E & -- xmm0 ← V
                       " vdpps   $0xF1,   (%0), %%xmm0, %%xmm1 " & E & -- xmm1 ← T.X
                       " vdpps   $0xF2, 16(%0), %%xmm0, %%xmm2 " & E & -- xmm2 ← T.Y
                       " vdpps   $0xF4, 32(%0), %%xmm0, %%xmm3 " & E & -- xmm3 ← T.Z

                       -- Blend xmm0 with results step by step
                       " vorps   %%xmm2, %%xmm1, %%xmm0 " & E & --   xmm0 ← Merge Row Y into xmm1 directly
                       " vorps   %%xmm3, %%xmm0, %%xmm0 " & E & --   xmm0 ← Merge Row Z result
                       " vmovaps %%xmm0,   (%2)         ");     -- Result ← xmm0
      return Result;
    end;
  function Transform (T : Transform_4D; P : Point_3D) return Point_3D is
    Result : aliased Point_3D;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, xmm4, memory",
           Inputs   => (Ptr'Asm_Input ("r", T'Address),
                        Ptr'Asm_Input ("r", P'Address),
                        Ptr'Asm_Input ("r", Result'Address)), 
           Template => " vmovaps  (%1), %%xmm0                 " & E & -- xmm0 ← V
                       " vdpps   $0xF1,   (%0), %%xmm0, %%xmm1 " & E & -- xmm1 ← T.X
                       " vdpps   $0xF2, 16(%0), %%xmm0, %%xmm2 " & E & -- xmm2 ← T.Y
                       " vdpps   $0xF4, 32(%0), %%xmm0, %%xmm3 " & E & -- xmm3 ← T.Z
                       " vdpps   $0xF8, 48(%0), %%xmm0, %%xmm4 " & E & -- xmm4 ← T.W

                       -- Merge results into xmm0
                       " vorps   %%xmm2, %%xmm1, %%xmm0 " & E & --   xmm0 ← xmm2 or xmm1
                       " vorps   %%xmm3, %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm0 or xmm3
                       " vorps   %%xmm4, %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm0 or xmm4
                       " vmovaps %%xmm0,   (%2)         ");     -- Result ← xmm0
      return Result;
    end;

  ---------------
  -- Transpose --
  ---------------

  function Transpose (M : Matrix_2D) return Matrix_2D is
    Result : aliased Matrix_2D;
    begin
      Asm (Clobber  => "xmm0, memory",
           Inputs   => (Ptr'Asm_Input ("r", M'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => " movaps   (%0), %%xmm0         " & E & --   xmm0 ← M
                       " pshufd  $0xD8, %%xmm0, %%xmm0 " & E & --   xmm0 ← Shuffle xmm0 to transpose the matrix
                       " movaps %%xmm0,   (%1)         ");     -- Result ← xmm0
      return Result;
    end;
  function Transpose (M : Matrix_3D) return Matrix_3D is
    Result              : aliased Matrix_3D;
    Permutation_Indices : aliased array (1..8) of Natural := (0, 3, 6, 1, 4, 7, 2, 5);
    begin
      Asm (Clobber  => "ymm0, xmm0, memory",
           Inputs   => (Ptr'Asm_Input ("r", M'Address),
                        Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", Permutation_Indices (1)'Address)),
           Template => " vmovups   (%2), %%ymm0         " & E & --      ymm0 ← Permutation_Indices
                       " vpermps   (%0), %%ymm0, %%ymm0 " & E & --      ymm0 ← Permute the first eight elements of M using ymm0 as the control mask
                       " vmovups %%ymm0,   (%1)         " & E & -- Result    ← ymm0
                       " vmovss  32(%0), %%xmm0         " & E & --      xmm0 ← Load the ninth element (a8) from the source matrix into xmm0
                       " vmovss  %%xmm0, 32(%1)         ");     -- Result.ZZ ← xmm0
      return Result;
    end;
  function Transpose (M : Matrix_4D) return Matrix_4D is
    Result : aliased Matrix_4D;
    begin
      Asm (Clobber  => "xmm4, xmm5, xmm6, xmm7, xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15, memory",
           Inputs   => (Ptr'Asm_Input ("r", M'Address),
                        Ptr'Asm_Input ("r", Result'Address)),

           Template => -- Load rows of M into xmm4 - xmm7
                       " vmovaps   (%0), %%xmm4 " & E & -- xmm4 ← M.X
                       " vmovaps 16(%0), %%xmm5 " & E & -- xmm5 ← M.Y
                       " vmovaps 32(%0), %%xmm6 " & E & -- xmm6 ← M.Z
                       " vmovaps 48(%0), %%xmm7 " & E & -- xmm7 ← M.W

                       -- Transpose M to get columns in xmm8 - xmm11 and Unpack lower and higher parts
                       " vunpcklps %%xmm5, %%xmm4,  %%xmm8 " & E & --  xmm8 ← Unpack low floats of X and Y
                       " vunpckhps %%xmm5, %%xmm4,  %%xmm9 " & E & --  xmm9 ← Unpack high floats of X and Y
                       " vunpcklps %%xmm7, %%xmm6, %%xmm10 " & E & -- xmm10 ← Unpack low floats of Z and W
                       " vunpckhps %%xmm7, %%xmm6, %%xmm11 " & E & -- xmm11 ← Unpack high floats of Z and W

                       -- Shuffle the unpacked results to complete the transpose
                       " vshufps $0x44, %%xmm10, %%xmm8, %%xmm12 " & E & -- xmm12 ← Column X
                       " vshufps $0xEE, %%xmm10, %%xmm8, %%xmm13 " & E & -- xmm13 ← Column Y
                       " vshufps $0x44, %%xmm11, %%xmm9, %%xmm14 " & E & -- xmm14 ← Column Z
                       " vshufps $0xEE, %%xmm11, %%xmm9, %%xmm15 " & E & -- xmm15 ← Column W

                       -- Load rows of M into xmm4 - xmm7
                       " vmovaps %%xmm12,   (%1) " & E & -- Result.X ← xmm12
                       " vmovaps %%xmm13, 16(%1) " & E & -- Result.Y ← xmm13
                       " vmovaps %%xmm14, 32(%1) " & E & -- Result.Z ← xmm14
                       " vmovaps %%xmm15, 48(%1) ");     -- Result.W ← xmm15
      return Result;
    end;

  --------------
  -- Multiply --
  --------------

  generic
    type T is private;
    Aligned : Bool := True;
  function Multiply_Vector_T (Left, Right : T) return T;
  function Multiply_Vector_T (Left, Right : T) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, memory",
           Inputs   => (Ptr'Asm_Input ("r", Left'Address),
                        Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "   (%1), %%xmm0         " & E & --   xmm0 ← Right
                       " vmulps     (%0), %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm0 * Left
                       VMOVPS & " %%xmm0,   (%2)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
  function Multiply_Scalar_T (Left : T; Right : Real) return T;
  function Multiply_Scalar_T (Left : T; Right : Real) return T is
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps" else "vmovups");
    begin
      Asm (Clobber  => "xmm0, memory",
           Inputs   => (Ptr'Asm_Input ("r", Left'Address),
                        Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "   (%1), %%xmm0         " & E & --   xmm0 ← Right
                       " shufps    $0x00, %%xmm0, %%xmm0 " & E & --   xmm0 ← (xmm0.X, ...)
                       " vmulps     (%0), %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm0 * Left
                       VMOVPS & " %%xmm0,   (%2)         ");     -- Result ← xmm0
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
    Negate  : Bool := False;
  function Multiply_Add_T (M, X, A : T) return T;
  function Multiply_Add_T (M, X, A : T) return T is -- Fused multiply add (M * X) + A
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps"      else "vmovups");
    VFADD  : constant String := (if Negate  then "vfnmadd231ps" else "vfmadd231ps");
    begin
      Asm (Clobber  => "xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", M'Address),
                        Ptr'Asm_Input ("r", X'Address),
                        Ptr'Asm_Input ("r", A'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "   (%0), %%xmm2         " & E & --   xmm2 ← M 
                       VMOVPS & "   (%1), %%xmm1         " & E & --   xmm1 ← X 
                       VMOVPS & "   (%2), %%xmm3         " & E & --   xmm3 ← A 
                       VFADD  & " %%xmm1, %%xmm2, %%xmm3 " & E & --   xmm3 ← (xmm2 * xmm1) + xmm3
                       VMOVPS & " %%xmm3,   (%3)         ");     -- Result ← xmm3
      return Result;
    end;
  generic
    type T is private;
    Aligned : Bool := True;
    Negate  : Bool := False;
  function Multiply_Subtract_T (M, X, A : T) return T;
  function Multiply_Subtract_T (M, X, A : T) return T is -- Fused multiply subtract A - (M * X)
    Result : aliased T;
    VMOVPS : constant String := (if Aligned then "vmovaps"      else "vmovups");
    VFSUB  : constant String := (if Negate  then "vfnmsub231ps" else "vfmsub231ps");
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, memory",
           Inputs   => (Ptr'Asm_Input ("r", M'Address),
                        Ptr'Asm_Input ("r", X'Address),
                        Ptr'Asm_Input ("r", A'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => VMOVPS & "   (%0), %%xmm0         " & E & --   xmm0 ← M
                       VMOVPS & "   (%1), %%xmm1         " & E & --   xmm1 ← X
                       VMOVPS & "   (%2), %%xmm2         " & E & --   xmm2 ← A
                       VFSUB  & " %%xmm1, %%xmm0, %%xmm2 " & E & --   xmm2 ← xmm2 - (xmm0 * xmm1)
                       VMOVPS & " %%xmm2,   (%3)         ");     -- Result ← xmm2
      return Result;
    end;
  function "*" (Left, Right : Matrix_2D) return Matrix_2D is
    Result : aliased Matrix_2D;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Left'Address),
                        Ptr'Asm_Input ("r", Result'Address)),
           Template => " vmovddup      (%0), %%xmm0                 " & E & --   xmm0 ← (Right.XX, Right.XX)
                       " vmovddup     8(%0), %%xmm1                 " & E & --   xmm1 ← (Right.XY, Right.XY)
                       " vmovaps       (%1), %%xmm2                 " & E & --   xmm2 ← (Left.XX, Left.XY)
                       " vmovsldup   %%xmm2, %%xmm3                 " & E & --   xmm3 ← (Left.XX, Left.XX)
                       " vmulps      %%xmm0, %%xmm3, %%xmm3         " & E & --   xmm3 ← xmm3 * xmm0
                       " vmovshdup   %%xmm2, %%xmm0                 " & E & --   xmm0 ← (Left.XY, Left.XY)
                       " vfmadd213ps %%xmm3, %%xmm1, %%xmm0         " & E & --   xmm0 ← xmm3 + xmm1 * xmm0
                       " vshufpd         $1, %%xmm0, %%xmm0, %%xmm1 " & E & --   xmm1 ← (Result.XX, Result.YX)
                       " vmovaps     %%xmm0,   (%2)                 ");     -- Result ← (Result.XX, Result.XY)
      return Result;
    end;
  function "*" (Left, Right : Matrix_3D) return Matrix_3D is
    Result : aliased Matrix_3D;
    begin
      Asm (Clobber  => "ymm0, ymm1, ymm2, ymm3, ymm4, ymm5, ymm6, ymm7, ymm8, memory",
           Inputs   => (Ptr'Asm_Input ("r", Left'Address),
                        Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Result'Address)),

           Template => -- Prefetch Right's rows
                       " prefetcht0   (%1) " & E & -- Prefetch Right.X
                       " prefetcht0 12(%1) " & E & -- Prefetch Right.Y
                       " prefetcht0 24(%1) " & E & -- Prefetch Right.Z

                       -- Initialize accumulators for all three output rows
                       " vxorps %%ymm2, %%ymm2, %%ymm2 " & E & -- ymm2 ← (0.0, ...) Accumulator for X
                       " vxorps %%ymm3, %%ymm3, %%ymm3 " & E & -- ymm3 ← (0.0, ...) Accumulator for Y
                       " vxorps %%ymm7, %%ymm7, %%ymm7 " & E & -- ymm7 ← (0.0, ...) Accumulator for Z

                       -- Compute first row
                       " vbroadcastss   (%0), %%ymm0         " & E & -- ymm0 ← (Left.XX, ...)
                       " vmovups        (%1), %%ymm1         " & E & -- ymm1 ← Right.X
                       " vfmadd231ps  %%ymm0, %%ymm1, %%ymm2 " & E & -- ymm2 ← (ymm0 * ymm1) + ymm2
                       " vbroadcastss  4(%0), %%ymm0         " & E & -- ymm0 ← (Left.XY, ...)
                       " vmovups      12(%1), %%ymm1         " & E & -- ymm1 ← Right.Y
                       " vfmadd231ps  %%ymm0, %%ymm1, %%ymm2 " & E & -- ymm2 ← (ymm0 * ymm1) + ymm2
                       " vbroadcastss  8(%0), %%ymm0         " & E & -- ymm0 ← (Left.XZ, ...)
                       " vmovups      24(%1), %%ymm1         " & E & -- ymm1 ← Right.Z
                       " vfmadd231ps  %%ymm0, %%ymm1, %%ymm2 " & E & -- ymm2 ← (ymm0 * ymm1) + ymm2

                       -- Compute second row
                       " vbroadcastss  12(%0), %%ymm0         " & E & -- ymm0 ← (Left.YX, ...)
                       " vmovups         (%1), %%ymm1         " & E & -- ymm1 ← Right.X
                       " vfmadd231ps   %%ymm0, %%ymm1, %%ymm3 " & E & -- ymm3 ← (ymm0 * ymm1) + ymm3
                       " vbroadcastss  16(%0), %%ymm0         " & E & -- ymm0 ← (Left.YY, ...)
                       " vmovups       12(%1), %%ymm1         " & E & -- ymm1 ← Right.Y
                       " vfmadd231ps   %%ymm0, %%ymm1, %%ymm3 " & E & -- ymm3 ← (ymm0 * ymm1) + ymm3
                       " vbroadcastss  20(%0), %%ymm0         " & E & -- ymm0 ← (Left.YZ, ...)
                       " vmovups       24(%1), %%ymm1         " & E & -- ymm1 ← Right.Z
                       " vfmadd231ps   %%ymm0, %%ymm1, %%ymm3 " & E & -- ymm3 ← (ymm0 * ymm1) + ymm3

                       -- Compute third row
                       " vbroadcastss 24(%0), %%ymm0         " & E & -- ymm0 ← (Left.ZX, ...)
                       " vmovups        (%1), %%ymm1         " & E & -- ymm1 ← Right.X
                       " vfmadd231ps  %%ymm0, %%ymm1, %%ymm7 " & E & -- ymm7 ← (ymm0 * ymm1) + ymm7
                       " vbroadcastss 28(%0), %%ymm0         " & E & -- ymm0 ← (Left.ZY, ...)
                       " vmovups      12(%1), %%ymm1         " & E & -- ymm1 ← Right.Y
                       " vfmadd231ps  %%ymm0, %%ymm1, %%ymm7 " & E & -- ymm7 ← (ymm0 * ymm1) + ymm7
                       " vbroadcastss 32(%0), %%ymm0         " & E & -- ymm0 ← (Left.ZZ, ...)
                       " vmovups      24(%1), %%ymm1         " & E & -- ymm1 ← Right.Z
                       " vfmadd231ps  %%ymm0, %%ymm1, %%ymm7 " & E & -- ymm7 ← (ymm0 * ymm1) + ymm7

                       -- Store the results
                       " vmovups %%ymm2,   (%2) " & E & -- Result.X ← ymm2
                       " vmovups %%ymm3, 12(%2) " & E & -- Result.Y ← ymm3
                       " vmovups %%ymm7, 24(%2) ");     -- Result.Z ← ymm7
      return Result;
    end;
  function "*" (Left, Right : Matrix_4D) return Matrix_4D is
    Result : aliased Matrix_4D;
    begin
      Asm (Clobber  => "ymm0, ymm1, ymm2, ymm3, ymm8, ymm10, ymm11, memory",
           Inputs   => (Ptr'Asm_Input ("r", Left'Address),
                        Ptr'Asm_Input ("r", Right'Address),
                        Ptr'Asm_Input ("r", Result'Address)),

           Template => -- Prefetch Right rows
                       " prefetcht0   (%1) " & E & -- Prefetch Right.X
                       " prefetcht0 16(%1) " & E & -- Prefetch Right.Y
                       " prefetcht0 32(%1) " & E & -- Prefetch Right.Z
                       " prefetcht0 48(%1) " & E & -- Prefetch Right.W

                       -- Initialize accumulators for rows 0/1
                       " vxorps  %%ymm2,  %%ymm2,  %%ymm2 " & E & --  ymm2 ← (0.0, ...) Accumulator for X
                       " vxorps  %%ymm3,  %%ymm3,  %%ymm3 " & E & --  ymm3 ← (0.0, ...) Accumulator for Y
                       " vxorps %%ymm10, %%ymm10, %%ymm10 " & E & -- ymm10 ← (0.0, ...) Accumulator for Z
                       " vxorps %%ymm11, %%ymm11, %%ymm11 " & E & -- ymm11 ← (0.0, ...) Accumulator for W

                       -- Multiply with Right.X
                       " vbroadcastss   (%0), %%ymm0          " & E & --  ymm0 ← (Left.XX, ...)
                       " vbroadcastss 16(%0), %%ymm1          " & E & --  ymm1 ← (Left.YX, ...)
                       " vmovaps        (%1), %%ymm8          " & E & --  ymm8 ← Right.X
                       " vfmadd231ps  %%ymm8, %%ymm0,  %%ymm2 " & E & --  ymm2 ← ymm2 + (ymm0 * ymm8)
                       " vfmadd231ps  %%ymm8, %%ymm1,  %%ymm3 " & E & --  ymm3 ← ymm3 + (ymm1 * ymm8)
                       " vbroadcastss 32(%0), %%ymm0          " & E & --  ymm0 ← (Left.ZX, ...)
                       " vbroadcastss 48(%0), %%ymm1          " & E & --  ymm1 ← (Left.WX, ...)
                       " vfmadd231ps  %%ymm8, %%ymm0, %%ymm10 " & E & -- ymm10 ← ymm10 + (ymm0 * Right.X)
                       " vfmadd231ps  %%ymm8, %%ymm1, %%ymm11 " & E & -- ymm11 ← ymm11 + (ymm1 * Right.X)

                       -- Multiply with Right.Y
                       " vbroadcastss  4(%0), %%ymm0          " & E & --  ymm0 ← (Left.XY, ...)
                       " vbroadcastss 20(%0), %%ymm1          " & E & --  ymm1 ← (Left.YY, ...)
                       " vmovups      16(%1), %%ymm8          " & E & --  ymm8 ← Right.Y
                       " vfmadd231ps  %%ymm8, %%ymm0,  %%ymm2 " & E & --  ymm2 ← ymm2 + (ymm0 * ymm8)
                       " vfmadd231ps  %%ymm8, %%ymm1,  %%ymm3 " & E & --  ymm3 ← ymm3 + (ymm1 * ymm8)
                       " vbroadcastss 36(%0), %%ymm0          " & E & --  ymm0 ← (Left.ZY, ...)
                       " vbroadcastss 52(%0), %%ymm1          " & E & --  ymm1 ← (Left.WY, ...)
                       " vfmadd231ps  %%ymm8, %%ymm0, %%ymm10 " & E & -- ymm10 ← ymm10 + (ymm0 * ymm8)
                       " vfmadd231ps  %%ymm8, %%ymm1, %%ymm11 " & E & -- ymm11 ← ymm11 + (ymm1 * ymm8)

                       -- Multiply with Right.Z
                       " vbroadcastss  8(%0), %%ymm0          " & E & --  ymm0 ← (Left.XZ, ...)
                       " vbroadcastss 24(%0), %%ymm1          " & E & --  ymm1 ← (Left.YZ, ...)
                       " vmovups      32(%1), %%ymm8          " & E & --  ymm8 ← Right.Z
                       " vfmadd231ps  %%ymm8, %%ymm0,  %%ymm2 " & E & --  ymm2 ← ymm2 + (ymm0 * ymm8)
                       " vfmadd231ps  %%ymm8, %%ymm1,  %%ymm3 " & E & --  ymm3 ← ymm3 + (ymm1 * ymm8)
                       " vbroadcastss 40(%0), %%ymm0          " & E & --  ymm0 ← (Left.ZZ, ...)
                       " vbroadcastss 56(%0), %%ymm1          " & E & --  ymm1 ← (Left.WZ, ...)
                       " vfmadd231ps  %%ymm8, %%ymm0, %%ymm10 " & E & -- ymm10 ← ymm10 + (ymm0 * ymm8)
                       " vfmadd231ps  %%ymm8, %%ymm1, %%ymm11 " & E & -- ymm11 ← ymm11 + (ymm1 * ymm8)

                       -- Multiply with Right.W
                       " vbroadcastss  12(%0), %%ymm0          " & E & --  ymm0 ← (Left.XW, ...)
                       " vbroadcastss  28(%0), %%ymm1          " & E & --  ymm1 ← (Left.YW, ...)
                       " vmovups       48(%1), %%ymm8          " & E & --  ymm8 ← Right.W
                       " vfmadd231ps   %%ymm8, %%ymm0,  %%ymm2 " & E & --  ymm2 ← ymm2 + (ymm0 * ymm8)
                       " vfmadd231ps   %%ymm8, %%ymm1,  %%ymm3 " & E & --  ymm3 ← ymm3 + (ymm1 * ymm8)
                       " vbroadcastss  44(%0), %%ymm0          " & E & --  ymm0 ← (Left.ZW, ...)
                       " vbroadcastss  60(%0), %%ymm1          " & E & --  ymm1 ← (Left.WW, ...)
                       " vfmadd231ps   %%ymm8, %%ymm0, %%ymm10 " & E & -- ymm10 ← ymm10 + (ymm0 * ymm8)
                       " vfmadd231ps   %%ymm8, %%ymm1, %%ymm11 " & E & -- ymm11 ← ymm11 + (ymm1 * ymm8)

                       -- Store results interleaved with computation
                       " vmovaps  %%ymm2,   (%2) " & E & -- Result.X ← ymm2
                       " vmovups  %%ymm3, 16(%2) " & E & -- Result.Y ← ymm3
                       " vmovups %%ymm10, 32(%2) " & E & -- Result.Z ← ymm10
                       " vmovups %%ymm11, 48(%2) ");     -- Result.W ← ymm11
      return Result;
    end;

  function "*" is new Multiply_Vector_T (Vector_2D, Aligned => False);
  function "*" is new Multiply_Vector_T (Vector_3D, Aligned => False);
  function "*" is new Multiply_Vector_T (Vector_4D);

  function "*" is new Multiply_Scalar_T (Vector_2D, Aligned => False);
  function "*" is new Multiply_Scalar_T (Vector_3D, Aligned => False);
  function "*" is new Multiply_Scalar_T (Vector_4D);

  function Multiply_Add is new Multiply_Add_T (Vector_2D, Aligned => False);
  function Multiply_Add is new Multiply_Add_T (Vector_3D, Aligned => False);
  function Multiply_Add is new Multiply_Add_T (Vector_4D);

  function Multiply_Subtract is new Multiply_Subtract_T (Vector_2D, Aligned => False);
  function Multiply_Subtract is new Multiply_Subtract_T (Vector_3D, Aligned => False);
  function Multiply_Subtract is new Multiply_Subtract_T (Vector_4D);
  
  function Negated_Multiply_Add is new Multiply_Add_T (Vector_2D, Negate => True, Aligned => False);
  function Negated_Multiply_Add is new Multiply_Add_T (Vector_3D, Negate => True, Aligned => False);
  function Negated_Multiply_Add is new Multiply_Add_T (Vector_4D, Negate => True);

  function Negated_Multiply_Subtract is new Multiply_Subtract_T (Vector_2D, Negate => True, Aligned => False);
  function Negated_Multiply_Subtract is new Multiply_Subtract_T (Vector_3D, Negate => True, Aligned => False);
  function Negated_Multiply_Subtract is new Multiply_Subtract_T (Vector_4D, Negate => True);

  ------------
  -- Invert --
  ------------

  function Invert (M : Matrix_2D) return Matrix_2D is
    Result : aliased Matrix_2D;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", M'Address),
                        Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", NEGATIVE_ZERO'Address)), 
           Template => " vmovaps        (%0), %%xmm0                 " & E & --   xmm0 ← M
                       " vbroadcastss   (%2), %%xmm1                 " & E & --   xmm1 ← (-0.0, ...)
                       " vxorps       %%xmm1, %%xmm0, %%xmm1         " & E & --   xmm1 ← xmm0 xor xmm1
                       " vblendps         $6, %%xmm1, %%xmm0, %%xmm1 " & E & --   xmm1 ← Blend xmm1 and xmm0 with 0110 mask
                       " vshufps         $39, %%xmm1, %%xmm1, %%xmm1 " & E & --   xmm1 ← Shuffle xmm1
                       " vshufps        $216, %%xmm0, %%xmm0, %%xmm0 " & E & --   xmm0 ← Shuffle xmm0
                       " vmulps       %%xmm1, %%xmm0, %%xmm0         " & E & --   xmm0 ← xmm0 * xmm1
                       " vshufps       $0xB1, %%xmm0, %%xmm0, %%xmm2 " & E & --   xmm2 ← Shuffle xmm0
                       " vaddps       %%xmm0, %%xmm2, %%xmm0         " & E & --   xmm0 ← xmm0 + xmm2
                       " vrcpps       %%xmm0, %%xmm2                 " & E & --   xmm2 ← 1 / xmm0 (Approx)
                       " vmulps       %%xmm2, %%xmm1, %%xmm3         " & E & --   xmm3 ← xmm2 * xmm1
                       " vfmsub213ps  %%xmm1, %%xmm3, %%xmm0         " & E & --   xmm0 ← xmm0 - (xmm3 * xmm1)
                       " vfnmadd213ps %%xmm3, %%xmm2, %%xmm0         " & E & --   xmm0 ← xmm0 - (xmm3 * xmm2)
                       " vmovaps      %%xmm0,   (%1)                 ");     -- Result ← xmm0
      return Result;
    end;
  function Invert (M : Matrix_3D) return Matrix_3D is
    Result : aliased Matrix_3D;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, ymm1, ymm2, memory",
           Inputs   => (Ptr'Asm_Input ("r", M'Address),
                        Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", ONE'Address)),

           Template => -- Load individual elements of M
                       " vmovss         (%0),  %%xmm0 " & E & --  xmm0 ← M.XX
                       " vmovss        4(%0),  %%xmm2 " & E & --  xmm2 ← M.XY
                       " vmovss        8(%0),  %%xmm1 " & E & --  xmm1 ← M.XZ
                       " vbroadcastss 12(%0),  %%xmm5 " & E & --  xmm5 ← M.YX
                       " vbroadcastss 16(%0),  %%xmm4 " & E & --  xmm4 ← M.YY
                       " vbroadcastss 20(%0),  %%xmm8 " & E & --  xmm8 ← M.YZ
                       " vbroadcastss 24(%0),  %%xmm6 " & E & --  xmm6 ← M.ZX
                       " vbroadcastss 28(%0),  %%xmm9 " & E & --  xmm9 ← M.ZY
                       " vbroadcastss 32(%0), %%xmm10 " & E & -- xmm10 ← M.ZZ

                       -- Matrix inversion calculations
                       " vmulss        %%xmm8,  %%xmm9,  %%xmm3 " & E & --  xmm3 ← YZ * ZY
                       " vbroadcastss  %%xmm3,  %%xmm3          " & E & --  xmm3 ← (xmm3.X, ...)
                       " vfmsub231ps   %%xmm4, %%xmm10,  %%xmm3 " & E & --  xmm3 ← xmm3 - (YY * ZZ)
                       " vmulss        %%xmm2, %%xmm10,  %%xmm7 " & E & --  xmm7 ← XY * ZZ
                       " vbroadcastss  %%xmm7, %%xmm11          " & E & -- xmm11 ← (xmm7
                       " vfmsub231ps   %%xmm1,  %%xmm9, %%xmm11 " & E & -- xmm11 ← xmm11 - (XZ * ZY)
                       " vmulss        %%xmm1,  %%xmm4,  %%xmm7 " & E & --  xmm7 ← XZ * YY
                       " vbroadcastss  %%xmm7, %%xmm12          " & E & -- xmm12 ← (xmm7.X, ...)
                       " vfmsub231ps   %%xmm2,  %%xmm8, %%xmm12 " & E & -- xmm12 ← xmm12 - (XY * YZ)
                       " vmulss        %%xmm5, %%xmm10,  %%xmm7 " & E & --  xmm7 ← YX * ZZ
                       " vbroadcastss  %%xmm7,  %%xmm7          " & E & --  xmm7 ← (xmm7.X, ...)
                       " vfmsub231ps   %%xmm8,  %%xmm6,  %%xmm7 " & E & --  xmm7 ← xmm7 - (YZ * ZX)
                       " vmulss        %%xmm1,  %%xmm6, %%xmm13 " & E & -- xmm13 ← XZ * ZX
                       " vbroadcastss %%xmm13, %%xmm13          " & E & -- xmm13 ← (xmm13.X, ...)
                       " vfmsub231ps  %%xmm10,  %%xmm0, %%xmm13 " & E & -- xmm13 ← xmm13 - (ZZ * XX)
                       " vunpcklps    %%xmm13, %%xmm11, %%xmm10 " & E & -- xmm10 ← (xmm13, xmm11)
                       " vmulss        %%xmm0,  %%xmm8,  %%xmm8 " & E & --  xmm8 ← XX * YZ
                       " vbroadcastss  %%xmm8,  %%xmm8          " & E & --  xmm8 ← (xmm8.X, ...)
                       " vfmsub231ps   %%xmm1,  %%xmm5,  %%xmm8 " & E & --  xmm8 ← xmm8 - (XZ * YX)
                       " vunpcklps     %%xmm8, %%xmm12,  %%xmm8 " & E & --  xmm8 ← (xmm8, xmm12)
                       " vmulss        %%xmm4,  %%xmm6, %%xmm11 " & E & -- xmm11 ← YY * ZX
                       " vbroadcastss %%xmm11, %%xmm11          " & E & -- xmm11 ← (xmm11.X, ...)
                       " vfmsub231ps   %%xmm5,  %%xmm9, %%xmm11 " & E & -- xmm11 ← xmm11 - (YX * ZY)
                       " vmulss        %%xmm0,  %%xmm9,  %%xmm9 " & E & --  xmm9 ← XX * ZY
                       " vbroadcastss  %%xmm9,  %%xmm9          " & E & --  xmm9 ← (xmm9.X, ...)
                       " vfmsub231ps   %%xmm6,  %%xmm2,  %%xmm9 " & E & --  xmm9 ← xmm9 - (ZX * XY)
                       " vmovlhps      %%xmm9, %%xmm10,  %%xmm6 " & E & --  xmm6 ← (xmm9.Low, xmm10.High)
                       " vmulss        %%xmm2,  %%xmm5,  %%xmm5 " & E & --  xmm5 ← XY * YX
                       " vbroadcastss  %%xmm5,  %%xmm5          " & E & --  xmm5 ← (xmm5.X, ...)
                       " vfmsub231ps   %%xmm4,  %%xmm0,  %%xmm5 " & E & --  xmm5 ← xmm5 - (YY * XX)
                       " vmovlhps      %%xmm5,  %%xmm8,  %%xmm4 " & E & --  xmm4 ← (xmm5.Low, xmm8.High)
                       " vmulss        %%xmm2,  %%xmm7,  %%xmm2 " & E & --  xmm2 ← XY * YZ
                       " vfmadd231ss   %%xmm1, %%xmm11,  %%xmm2 " & E & --  xmm2 ← xmm2 + (XZ * xmm11)
                       " vfmadd231ss   %%xmm0,  %%xmm3,  %%xmm2 " & E & --  xmm2 ← xmm2 + (XX * xmm3)

                       -- Load inverse determinant
                       " vmovss         (%2), %%xmm0         " & E & -- xmm0 ← 1.0
                       " vdivss       %%xmm2, %%xmm0, %%xmm0 " & E & -- xmm0 ← 1.0 / Determinant
                       " vbroadcastss %%xmm0, %%xmm0         " & E & -- xmm0 ← (xmm0.X, ...)

                       -- Finally, compute the inverse
                       " vunpcklps    %%xmm7, %%xmm3, %%xmm1         " & E & --   xmm1 ← (xmm7.Low, xmm3.Low)
                       " vmovlhps    %%xmm11, %%xmm1, %%xmm1         " & E & --   xmm1 ← (xmm11.Low, xmm1.High)
                       " vmulps       %%xmm1, %%xmm0, %%xmm1         " & E & --   xmm1 ← xmm1 * (1.0 / Determinant)
                       " vmulps       %%xmm6, %%xmm0, %%xmm2         " & E & --   xmm2 ← xmm6 * (1.0 / Determinant)
                       " vmulps       %%xmm4, %%xmm0, %%xmm0         " & E & --   xmm0 ← xmm4 * (1.0 / Determinant)
                       " vunpckhpd    %%xmm2, %%xmm1, %%xmm3         " & E & --   xmm3 ← Upper packed double
                       " vshufps        $133, %%xmm3, %%xmm2, %%xmm3 " & E & --   xmm3 ← Shuffle xmm3 and xmm2
                       " vmovlhps     %%xmm1, %%xmm2, %%xmm2         " & E & --   xmm2 ← (xmm1.Low, xmm2.High)
                       " vshufps         $98, %%xmm1, %%xmm2, %%xmm1 " & E & --   xmm1 ← Shuffle xmm1 and xmm2
                       " vinsertf128      $1, %%xmm3, %%ymm1, %%ymm1 " & E & --   ymm1 ← Insert xmm3 into ymm1
                       " vbroadcastsd %%xmm0, %%ymm2                 " & E & --   ymm2 ← (1.0 / Determinant, ...)
                       " vblendps        $36, %%ymm2, %%ymm1, %%ymm1 " & E & --   ymm1 ← Blend ymm2 and ymm1
                       " vmovups      %%ymm1,   (%1)                 " & E & -- Result ← ymm1
                       " vextractps       $2, %%xmm0, 32(%1)         ");     -- Result ← Extract specific elements
      return Result;
    end;
  function Invert (M : Matrix_4D) return Matrix_4D is
    Result : aliased Matrix_4D;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, memory",
           Inputs   => (Ptr'Asm_Input ("r", M'Address),
                        Ptr'Asm_Input ("r", Result'Address),
                        Ptr'Asm_Input ("r", ONE'Address)),

           Template => -- Load the 4 rows of M into XMM registers
                       " vmovaps   (%0), %%xmm1 " & E & -- xmm1 ← M.X
                       " vmovaps 16(%0), %%xmm2 " & E & -- xmm2 ← M.Y
                       " vmovaps 32(%0), %%xmm3 " & E & -- xmm3 ← M.Z
                       " vmovaps 48(%0), %%xmm4 " & E & -- xmm4 ← M.W
                       
                       -- Interleave rows 1 and 3 into xmm9, and rows 2 and 4 into xmm10
                       " vunpcklps  %%xmm3, %%xmm1,  %%xmm9 " & E & --  xmm9 ← Unpack low of M.Z and M.X
                       " vunpcklps  %%xmm4, %%xmm2, %%xmm10 " & E & -- xmm10 ← Unpack low of M.W and M.Y
                       " vunpcklps %%xmm10, %%xmm9,  %%xmm0 " & E & --  xmm0 ← Unpack low of xmm10 and xmm9
                       " vunpckhps  %%xmm3, %%xmm1,  %%xmm1 " & E & --  xmm1 ← Unpack high of M.Z and M.X
                       " vunpckhps  %%xmm4, %%xmm2,  %%xmm3 " & E & --  xmm3 ← Unpack high of M.W and M.Y
                       " vunpcklps  %%xmm3, %%xmm1,  %%xmm5 " & E & --  xmm5 ← Unpack low of xmm3 and xmm1
                       " vunpckhps  %%xmm3, %%xmm1,  %%xmm6 " & E & --  xmm6 ← Unpack high of xmm3 and xmm1
                       
                       -- Permute elements to prepare for matrix calculations
                       " vshufps    $0xBB, %%xmm10, %%xmm9,  %%xmm2 " & E & --  xmm2 ← Shuffle xmm10 and xmm9 with control 0xBB
                       " vshufps    $0xD8,  %%xmm2, %%xmm2,  %%xmm2 " & E & --  xmm2 ← Shuffle xmm2 with control 0xD8
                       " vshufpd       $1,  %%xmm6, %%xmm6,  %%xmm4 " & E & --  xmm4 ← Shuffle packed doublewords in xmm6 with control 1
                       " vunpcklps %%xmm1,  %%xmm3, %%xmm1          " & E & --  xmm1 ← Unpack low of xmm1 and xmm3
                       " vshufps    $0x1B,  %%xmm6, %%xmm6, %%xmm11 " & E & -- xmm11 ← Shuffle xmm6 with control 0x1B
                       
                       -- Perform first set of multiplications for cofactor preparation
                       " vmulps  %%xmm1, %%xmm11,  %%xmm1         " & E & --  xmm1 ← xmm1 * xmm11
                       " vshufpd     $1,  %%xmm1,  %%xmm1, %%xmm3 " & E & --  xmm3 ← Shuffle xmm1 with control 1
                       " vsubps  %%xmm1,  %%xmm3, %%xmm12         " & E & -- xmm12 ← xmm1 - xmm3
                       " vmulps  %%xmm0, %%xmm12,  %%xmm1         " & E & --  xmm1 ← xmm0 * xmm12
                       
                       -- Calculate determinant components
                       " vshufpd      $1,  %%xmm1,  %%xmm1,  %%xmm7 " & E & --  xmm7 ← Shuffle xmm1 with control 1
                       " vmulps   %%xmm5,  %%xmm2,  %%xmm1          " & E & --  xmm1 ← xmm5 * xmm2
                       " vshufps   $0xB1,  %%xmm1,  %%xmm1, %%xmm13 " & E & -- xmm13 ← Shuffle xmm1 with control 0xB1
                       " vshufpd      $1, %%xmm13, %%xmm13,  %%xmm3 " & E & --  xmm3 ← Shuffle xmm13 with control 1
                       " vsubps  %%xmm13,  %%xmm3,  %%xmm1          " & E & --  xmm1 ← xmm13 - xmm3
                       " vmulps   %%xmm0,  %%xmm1,  %%xmm1          " & E & --  xmm1 ← xmm0 * xmm1
                       " vshufpd      $1,  %%xmm1,  %%xmm1,  %%xmm1 " & E & --  xmm1 ← Shuffle xmm1 with control 1
                       " vmulps   %%xmm6,  %%xmm2,  %%xmm6          " & E & --  xmm6 ← xmm6 * xmm2
                       " vshufps   $0x1B,  %%xmm6,  %%xmm6,  %%xmm6 " & E & --  xmm6 ← Shuffle xmm6 with control 0x1B
                       
                       -- Combined matrix operations for cofactors
                       " vshufpd           $1,  %%xmm5, %%xmm5,  %%xmm8 " & E & --  xmm8 ← Shuffle xmm5 with control 1
                       " vshufpd           $1,  %%xmm6, %%xmm6, %%xmm14 " & E & -- xmm14 ← Shuffle xmm6 with control 1
                       " vmulps        %%xmm4,  %%xmm3, %%xmm3          " & E & --  xmm3 ← xmm4 * xmm3
                       " vfnmsub231ps  %%xmm8, %%xmm14, %%xmm3          " & E & --  xmm3 ← (xmm8 * xmm14) - xmm3
                       " vfmadd231ps  %%xmm13,  %%xmm4, %%xmm3          " & E & --  xmm3 ← (xmm13 * xmm4) + xmm3
                       " vfmadd231ps   %%xmm8,  %%xmm6, %%xmm3          " & E & --  xmm3 ← (xmm8 * xmm6) + xmm3
                       " vfmadd231ps  %%xmm12,  %%xmm2, %%xmm3          " & E & --  xmm3 ← (xmm12 * xmm2) + xmm3
                       " vsubps        %%xmm6, %%xmm14, %%xmm6          " & E & --  xmm6 ← xmm6 - xmm14
                       " vmulps        %%xmm0,  %%xmm6, %%xmm6          " & E & --  xmm6 ← xmm0 * xmm6
                       
                       -- Final determinant processing
                       " vshufpd     $1,  %%xmm6,  %%xmm6,  %%xmm6 " & E & --  xmm6 ← Shuffle xmm6 with control 1
                       " vmulps  %%xmm0,  %%xmm2, %%xmm12          " & E & -- xmm12 ← xmm0 * xmm2
                       " vshufps  $0xB1, %%xmm12, %%xmm12, %%xmm12 " & E & -- xmm12 ← Shuffle xmm12 with control 0xB1
                       " vshufpd     $1, %%xmm12, %%xmm12, %%xmm13 " & E & -- xmm13 ← Shuffle xmm12 with control 1
                       
                       -- Cofactor matrix multiplication
                       " vunpcklps     %%xmm9, %%xmm10,  %%xmm9          " & E & --  xmm9 ← Unpack low of xmm9 and xmm10
                       " vmulps        %%xmm9, %%xmm11, %%xmm10          " & E & -- xmm10 ← xmm9 * xmm11
                       " vmulps        %%xmm8, %%xmm10, %%xmm11          " & E & -- xmm11 ← xmm8 * xmm10
                       " vshufpd           $1, %%xmm10, %%xmm10, %%xmm14 " & E & -- xmm14 ← Shuffle xmm10 with control 1
                       " vfmadd231ps   %%xmm4, %%xmm12,  %%xmm6          " & E & --  xmm6 ← (xmm4 * xmm12) + xmm6
                       " vfnmsub231ps  %%xmm2, %%xmm14,  %%xmm6          " & E & --  xmm6 ← (xmm2 * xmm14) - xmm6
                       " vfmadd231ps  %%xmm10,  %%xmm2,  %%xmm6          " & E & --  xmm6 ← (xmm2 * xmm10) + xmm6
                       " vfmadd231ps   %%xmm4, %%xmm13,  %%xmm6          " & E & --  xmm6 ← (xmm4 * xmm13) + xmm6
                       " vshufps        $0x1B,  %%xmm5,  %%xmm5,  %%xmm5 " & E & --  xmm5 ← Shuffle xmm5 with control 0x1B
                       " vmulps        %%xmm5,  %%xmm9,  %%xmm5          " & E & --  xmm5 ← xmm5 * xmm9
                       " vshufpd           $1,  %%xmm5,  %%xmm5,  %%xmm9 " & E & --  xmm9 ← Shuffle xmm5 with control 1
                                   
                       -- Combine cofactors for determinant calculations
                       " vfnmsub231ps  %%xmm4,  %%xmm9, %%xmm11          " & E & -- xmm11 ← (xmm4 * xmm9) - xmm11
                       " vfmadd231ps   %%xmm4,  %%xmm5,  %%xmm7          " & E & --  xmm7 ← (xmm4 * xmm5) + xmm7
                       " vfmadd231ps  %%xmm14,  %%xmm8,  %%xmm7          " & E & --  xmm7 ← (xmm14 * xmm8) + xmm7
                       " vaddps        %%xmm7, %%xmm11,  %%xmm4          " & E & --  xmm4 ← xmm7 + xmm11
                       " vfmadd231ps   %%xmm5,  %%xmm2,  %%xmm1          " & E & --  xmm1 ← (xmm5 * xmm2) + xmm1
                       " vfnmsub231ps %%xmm13,  %%xmm8,  %%xmm1          " & E & --  xmm1 ← (xmm8 * xmm13) - xmm1
                       " vfmadd231ps   %%xmm8, %%xmm12,  %%xmm1          " & E & --  xmm1 ← (xmm8 * xmm12) + xmm1
                       " vfmadd231ps   %%xmm2,  %%xmm9,  %%xmm1          " & E & --  xmm1 ← (xmm2 * xmm9) + xmm1
                       
                       -- Reciprocal of determinant
                       " vmulps       %%xmm0, %%xmm3, %%xmm0         " & E & -- xmm0 ← xmm0 * xmm3
                       " vshufpd          $1, %%xmm0, %%xmm0, %%xmm2 " & E & -- xmm2 ← shuffle xmm0 with control 1
                       " vaddps       %%xmm0, %%xmm2, %%xmm0         " & E & -- xmm0 ← xmm0 + xmm2
                       " vshufps       $0xB1, %%xmm0, %%xmm0, %%xmm2 " & E & -- xmm2 ← shuffle xmm0 with control 0xB1
                       " vaddps       %%xmm0, %%xmm2, %%xmm0         " & E & -- xmm0 ← xmm0 + xmm2
                       " vrcpps       %%xmm0, %%xmm2                 " & E & -- xmm2 ← reciprocal of xmm0
                       " vbroadcastss   (%2), %%xmm5                 " & E & -- xmm5 ← broadcasted ONE
                       " vfmsub231ps  %%xmm0, %%xmm2, %%xmm5         " & E & -- xmm5 ← (xmm0 * xmm2) - xmm5
                       " vfnmadd132ps %%xmm2, %%xmm2, %%xmm5         " & E & -- xmm5 ← xmm5 - (xmm2 * xmm2)
                       
                       -- Apply determinant inverse to cofactors
                       " vmulps %%xmm3, %%xmm5, %%xmm0 " & E & -- xmm0 ← xmm3 * xmm5
                       " vmulps %%xmm4, %%xmm5, %%xmm2 " & E & -- xmm2 ← xmm4 * xmm5
                       " vmulps %%xmm6, %%xmm5, %%xmm3 " & E & -- xmm3 ← xmm6 * xmm5
                       " vmulps %%xmm1, %%xmm5, %%xmm1 " & E & -- xmm1 ← xmm1 * xmm5
                       
                       -- Store the inverted matrix
                       " vmovaps %%xmm0,   (%1) " & E & -- Result.X ← xmm0
                       " vmovaps %%xmm2, 16(%1) " & E & -- Result.Y ← xmm2
                       " vmovaps %%xmm3, 32(%1) " & E & -- Result.Z ← xmm3
                       " vmovaps %%xmm1, 48(%1) ");     -- Result.W ← xmm1
      return Result;
    end;

  -----------------
  -- Determinant --
  -----------------

  function Determinant (M : Matrix_2D) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, memory",
           Inputs   => (Ptr'Asm_Input ("r", M'Address),
                        Ptr'Asm_Input ("r", Result'Address)),

           Template => -- A.XX * A.YY - A.XY * A.YX
                       " vmovss    (%0), %%xmm0         " & E & --   xmm0 ← M.XX
                       " vmovss   4(%0), %%xmm1         " & E & --   xmm1 ← M.XY
                       " vmovss   8(%0), %%xmm2         " & E & --   xmm2 ← M.YX
                       " vmovss  12(%0), %%xmm3         " & E & --   xmm3 ← M.YY
                       " vmulss  %%xmm3, %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm0 * xmm3
                       " vmulss  %%xmm2, %%xmm1, %%xmm1 " & E & --   xmm1 ← xmm1 * xmm2
                       " vsubss  %%xmm1, %%xmm0, %%xmm0 " & E & --   xmm0 ← xmm0 - xmm1
                       " vmovss  %%xmm0,   (%1)         ");     -- Result ← xmm0
      return Result;
    end;
  function Determinant (M : Matrix_3D) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, memory",
           Inputs   => (Ptr'Asm_Input ("r", M'Address),
                        Ptr'Asm_Input ("r", Result'Address)),

           Template => -- Load the three rows of the matrix into XMM registers
                       " vmovups   (%0), %%xmm0 " & E & -- xmm0 ← M.X
                       " vmovups 12(%0), %%xmm1 " & E & -- xmm1 ← M.Y
                       " vmovups 24(%0), %%xmm2 " & E & -- xmm2 ← M.Z
                       
                       -- Make a copy of M.Y into xmm3 for partial cross computation
                       " vmovaps %%xmm1, %%xmm3 " & E & -- xmm3 ← M.Y
                       
                       -- Shuffle row1 in xmm3 so that each element lines up for cross
                       " vshufps $0xC9, %%xmm3, %%xmm3, %%xmm3 " & E & -- xmm3 ← Shuffle xmm3 with control (11001001) to swap Y and Z components
                       
                       -- Similarly, copy row2 (xmm2, row Z) into xmm4 and shuffle for cross
                       " vmovaps %%xmm2, %%xmm4                 " & E & -- xmm4 ← M.Z
                       " vshufps  $0xD2, %%xmm4, %%xmm4, %%xmm4 " & E & -- xmm4 ← Shuffle xmm4 with control (11010010)
                       
                       -- Multiply partial cross components
                       " vmulps %%xmm4, %%xmm3, %%xmm3 " & E & -- xmm3 ← xmm3 * xmm4
                       
                       -- Make a copy of xmm1 (row Y) into xmm5 for another partial cross computation
                       " vmovaps %%xmm1, %%xmm5                 " & E & -- xmm5 ← M.Y
                       " vshufps  $0xD2, %%xmm5, %%xmm5, %%xmm5 " & E & -- xmm5 ← Shuffle xmm5 with control 0xD2
                       " vmovaps %%xmm2, %%xmm6                 " & E & -- xmm6 ← M.Z
                       " vshufps  $0xC9, %%xmm6, %%xmm6, %%xmm6 " & E & -- xmm6 ← Shuffle xmm6 with control 0xC9
                       " vmulps  %%xmm6, %%xmm5, %%xmm5         " & E & -- xmm5 ← xmm5 * xmm6
                       
                       -- Subtract to get the final cross product in xmm3
                       " vsubps %%xmm5, %%xmm3, %%xmm3 " & E & -- xmm3 ← xmm3 - xmm5
                       " vmulps %%xmm0, %%xmm3, %%xmm3 " & E & -- xmm3 ← xmm0 * xmm3
                       
                       -- Horizontal add to reduce xmm3’s 3 floats into one
                       " vmovhlps %%xmm3, %%xmm4         " & E & -- xmm4 ← High 2 floats of xmm3
                       " vaddps   %%xmm4, %%xmm3, %%xmm3 " & E & -- xmm3 ← xmm3 + xmm4
                       
                       -- Now we have two floats in xmm3; shuffle to add the remaining float
                       " vpshufd  $0x55, %%xmm3, %%xmm4 " & E & -- xmm4 ← Shuffle xmm3 with control 0x55 (01010101) to duplicate the second float
                       " vaddss  %%xmm4, %%xmm3, %%xmm3 " & E & -- xmm3 ← xmm3 + xmm4
                       
                       -- Store the result (lowest float)
                       " vmovss %%xmm3, (%1) "); -- Result ← xmm3
      return Result;
    end;
  function Determinant (M : Matrix_4D) return Real is
    Result : aliased Real;
    begin
      Asm (Clobber  => "xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, memory",
           Inputs   => (Ptr'Asm_Input ("r", M'Address),
                        Ptr'Asm_Input ("r", Result'Address)),

           Template => -- Load the 4 rows of M
                       " vmovaps   (%0), %%xmm1 " & E & -- xmm1 ← M.X
                       " vmovaps 16(%0), %%xmm2 " & E & -- xmm2 ← M.Y
                       " vmovaps 32(%0), %%xmm3 " & E & -- xmm3 ← M.Z
                       " vmovaps 48(%0), %%xmm4 " & E & -- xmm4 ← M.W

                       -- Interleave rows 1 and 3 into xmm9, and rows 2 and 4 into xmm10
                       " vunpcklps   %%xmm3, %%xmm1,  %%xmm9 " & E & --  xmm9 ← Unpack low of M.Z and M.X
                       " vunpcklps   %%xmm4, %%xmm2, %%xmm10 " & E & -- xmm10 ← Unpack low of M.W and M.Y
                       " vunpcklps  %%xmm10, %%xmm9,  %%xmm0 " & E & --  xmm0 ← Unpack low of xmm10 and xmm9
                       " vunpckhps   %%xmm3, %%xmm1,  %%xmm1 " & E & --  xmm1 ← Unpack high of M.Z and M.X
                       " vunpckhps   %%xmm4, %%xmm2,  %%xmm3 " & E & --  xmm3 ← Unpack high of M.W and M.Y
                       " vunpcklps   %%xmm3, %%xmm1,  %%xmm5 " & E & --  xmm5 ← Unpack low of xmm3 and xmm1
                       " vunpckhps   %%xmm3, %%xmm1,  %%xmm6 " & E & --  xmm6 ← Unpack high of xmm3 and xmm1
                       
                       -- Permute elements to prepare for matrix calculations
                       " vshufps     $187, %%xmm10, %%xmm9,  %%xmm2 " & E & --  xmm2 ← Shuffle xmm10 and xmm9 with control 187
                       " vshufps     $216,  %%xmm2, %%xmm2,  %%xmm2 " & E & --  xmm2 ← Shuffle xmm2 with control 216
                       " vshufpd       $1,  %%xmm6, %%xmm6,  %%xmm4 " & E & --  xmm4 ← Shuffle packed doublewords in xmm6 with control 1
                       " vunpcklps %%xmm1,  %%xmm3, %%xmm1          " & E & --  xmm1 ← Unpack low of xmm1 and xmm3
                       " vshufps      $27,  %%xmm6, %%xmm6, %%xmm11 " & E & -- xmm11 ← Shuffle xmm6 with control 27
                       
                       -- First set of multiplications toward partial cofactors
                       " vmulps  %%xmm1, %%xmm11,  %%xmm1         " & E & --  xmm1 ← xmm1 * xmm11
                       " vshufpd     $1,  %%xmm1,  %%xmm1, %%xmm3 " & E & --  xmm3 ← Shuffle xmm1 with control 1
                       " vsubps  %%xmm1,  %%xmm3, %%xmm12         " & E & -- xmm12 ← xmm1 - xmm3
                       " vmulps  %%xmm0, %%xmm12,  %%xmm1         " & E & --  xmm1 ← xmm0 * xmm12
                       
                       -- Some determinant components
                       " vshufpd      $1,  %%xmm1,  %%xmm1,  %%xmm7 " & E & --  xmm7 ← Shuffle xmm1 with control 1
                       " vmulps   %%xmm5,  %%xmm2,  %%xmm1          " & E & --  xmm1 ← xmm5 * xmm2
                       " vshufps    $177,  %%xmm1,  %%xmm1, %%xmm13 " & E & -- xmm13 ← Shuffle xmm1 with control 177
                       " vshufpd      $1, %%xmm13, %%xmm13,  %%xmm3 " & E & --  xmm3 ← Shuffle xmm13 with control 1
                       " vsubps  %%xmm13,  %%xmm3,  %%xmm1          " & E & --  xmm1 ← xmm13 - xmm3
                       " vmulps   %%xmm0,  %%xmm1,  %%xmm1          " & E & --  xmm1 ← xmm0 * xmm1
                       " vshufpd      $1,  %%xmm1,  %%xmm1,  %%xmm1 " & E & --  xmm1 ← Shuffle xmm1 with control 1
                       " vmulps   %%xmm6,  %%xmm2,  %%xmm6          " & E & --  xmm6 ← xmm6 * xmm2
                       " vshufps     $27,  %%xmm6,  %%xmm6,  %%xmm6 " & E & --  xmm6 ← Shuffle xmm6 with control 27

                       -- More partial cofactor multiplications (still building up determinant)
                       " vshufpd           $1,  %%xmm5, %%xmm5,   %%xmm8 " & E & --  xmm8 ← Shuffle xmm5 with control 1
                       " vshufpd           $1,  %%xmm6, %%xmm6,  %%xmm14 " & E & -- xmm14 ← Shuffle xmm6 with control 1
                       " vmulps        %%xmm4,  %%xmm3, %%xmm3           " & E & --  xmm3 ← xmm4 * xmm3
                       " vfnmsub231ps  %%xmm8, %%xmm14, %%xmm3           " & E & --  xmm3 ← (xmm8 * xmm14) - xmm3
                       " vfmadd231ps  %%xmm13,  %%xmm4, %%xmm3           " & E & --  xmm3 ← (xmm4 * xmm13) + xmm3
                       " vfmadd231ps   %%xmm8,  %%xmm6, %%xmm3           " & E & --  xmm3 ← (xmm8 * xmm6) + xmm3
                       " vfmadd231ps  %%xmm12,  %%xmm2, %%xmm3           " & E & --  xmm3 ← (xmm2 * xmm12) + xmm3
                       " vsubps        %%xmm6, %%xmm14, %%xmm6           " & E & --  xmm6 ← xmm6 - xmm14
                       " vmulps        %%xmm0,  %%xmm6, %%xmm6           " & E & --  xmm6 ← xmm0 * xmm6
                       
                       -- Final partial accumulation for the determinant
                       " vshufpd           $1,  %%xmm6,  %%xmm6,  %%xmm6 " & E & --  xmm6 ← Shuffle xmm6 with control 1
                       " vmulps        %%xmm0,  %%xmm2, %%xmm12          " & E & -- xmm12 ← xmm0 * xmm2
                       " vshufps         $177, %%xmm12, %%xmm12, %%xmm12 " & E & -- xmm12 ← Shuffle xmm12 with control 177
                       " vshufpd           $1, %%xmm12, %%xmm12, %%xmm13 " & E & -- xmm13 ← Shuffle xmm12 with control 1
                       " vunpcklps     %%xmm9, %%xmm10,  %%xmm9          " & E & --  xmm9 ← Unpack low of xmm9 and xmm10
                       " vmulps        %%xmm9, %%xmm11, %%xmm10          " & E & -- xmm10 ← xmm9 * xmm11
                       " vmulps        %%xmm8, %%xmm10, %%xmm11          " & E & -- xmm11 ← xmm8 * xmm10
                       " vshufpd           $1, %%xmm10, %%xmm10, %%xmm14 " & E & -- xmm14 ← Shuffle xmm10 with control 1
                       " vfmadd231ps   %%xmm4, %%xmm12,  %%xmm6          " & E & --  xmm6 ← (xmm4 * xmm12) + xmm6
                       " vfnmsub231ps  %%xmm2, %%xmm14,  %%xmm6          " & E & --  xmm6 ← (xmm2 * xmm14) - xmm6
                       " vfmadd231ps  %%xmm10,  %%xmm2,  %%xmm6          " & E & --  xmm6 ← (xmm10 * xmm2) + xmm6
                       " vfmadd231ps   %%xmm4, %%xmm13,  %%xmm6          " & E & --  xmm6 ← (xmm4 * xmm13) + xmm6
                       " vshufps          $27,  %%xmm5,  %%xmm5,  %%xmm5 " & E & --  xmm5 ← Shuffle xmm5 with control 27
                       " vmulps        %%xmm5,  %%xmm9,  %%xmm5          " & E & --  xmm5 ← xmm5 * xmm9
                       " vshufpd           $1,  %%xmm5,  %%xmm5,  %%xmm9 " & E & --  xmm9 ← Shuffle xmm5 with control 1
                       " vfnmsub231ps  %%xmm4,  %%xmm9, %%xmm11          " & E & -- xmm11 ← (xmm4 * xmm9) - xmm11
                       " vfmadd231ps   %%xmm4,  %%xmm5,  %%xmm7          " & E & --  xmm7 ← (xmm4 * xmm5) + xmm7
                       " vfmadd231ps  %%xmm14,  %%xmm8,  %%xmm7          " & E & --  xmm7 ← (xmm14 * xmm8) + xmm7
                       " vaddps        %%xmm7, %%xmm11,  %%xmm4          " & E & --  xmm4 ← xmm7 + xmm11
                       " vfmadd231ps   %%xmm5,  %%xmm2,  %%xmm1          " & E & --  xmm1 ← (xmm5 * xmm2) + xmm1
                       " vfnmsub231ps %%xmm13,  %%xmm8,  %%xmm1          " & E & --  xmm1 ← (xmm13 * xmm8) - xmm1
                       " vfmadd231ps   %%xmm8, %%xmm12,  %%xmm1          " & E & --  xmm1 ← (xmm8 * xmm12) + xmm1
                       " vfmadd231ps   %%xmm2,  %%xmm9,  %%xmm1          " & E & --  xmm1 ← (xmm2 * xmm9) + xmm1
                       
                       -- Now multiply and sum to get the final determinant in xmm0
                       " vmulps %%xmm0, %%xmm3, %%xmm0 " & E & -- xmm0 ← xmm0 * xmm3

                       -- Horizontal add to get a single (but broadcasted) determinant
                       " vshufpd     $1, %%xmm0, %%xmm0, %%xmm2 " & E & -- xmm2 ← Shuffle xmm0 with control 1
                       " vaddps  %%xmm0, %%xmm2, %%xmm0         " & E & -- xmm0 ← xmm0 + xmm2
                       " vshufps   $177, %%xmm0, %%xmm0, %%xmm2 " & E & -- xmm2 ← Shuffle xmm0 with control 177
                       " vaddps  %%xmm0, %%xmm2, %%xmm0         " & E & -- xmm0 ← xmm0 + xmm2
                       
                       -- At this point, xmm0 contains the determinant in all 4 lanes.
                       " vmovss %%xmm0, (%1) "); -- Result ← xmm0
      return Result;
    end;

  procedure Test is separate;
begin
  Test;
end;
