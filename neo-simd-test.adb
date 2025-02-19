
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

separate (Neo.SIMD) procedure Test is

  ---------------
  -- Utilities --
  ---------------

  -- Define Epsilon values for comparison
  Epsilon_Real : constant Float  := 1.0E-5;

  procedure Put_Separator is begin Put_Line ("----------------------------------------------------"); end;

    -- Helper Functions
  function Abs_Diff (A, B : Float) return Float is
    begin
       return Abs (A - B);
    end;

  function Equal (A, B : Float; Epsilon : Float := Epsilon_Real) return Boolean is
    begin
       return Abs_Diff (A, B) <= Epsilon;
    end;

    -- Printing Routines

  procedure Put_Line (V : Vector_2D) is
    begin
      Put ("(");
      Ada.Float_Text_IO.Put (V.X, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (V.Y, Fore => 0, Aft => 6, Exp => 0);
      Put_Line (")");
    end;

  procedure Put_Line (V : Vector_3D) is
    begin
      Put ("(");
      Ada.Float_Text_IO.Put (V.X, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (V.Y, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (V.Z, Fore => 0, Aft => 6, Exp => 0);
      Put_Line (")");
    end;

  procedure Put_Line (V : Vector_4D) is
    begin
      Put ("(");
      Ada.Float_Text_IO.Put (V.X, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (V.Y, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (V.Z, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (V.W, Fore => 0, Aft => 6, Exp => 0);
      Put_Line (")");
    end;

  procedure Put_Line (M : Matrix_2D) is
    begin
      Put_Line ("[");
      Put ("  [");
      Ada.Float_Text_IO.Put (M.XX, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.XY, Fore => 0, Aft => 6, Exp => 0);
      Put_Line ("],");
      Put ("  [");
      Ada.Float_Text_IO.Put (M.YX, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.YY, Fore => 0, Aft => 6, Exp => 0);
      Put_Line ("]");
      Put_Line ("]");
    end;

  procedure Put_Line (M : Matrix_3D) is
    begin
      Put_Line ("[");
      Put ("  [");
      Ada.Float_Text_IO.Put (M.XX, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.XY, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.XZ, Fore => 0, Aft => 6, Exp => 0);
      Put_Line ("],");
      
      Put ("  [");
      Ada.Float_Text_IO.Put (M.YX, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.YY, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.YZ, Fore => 0, Aft => 6, Exp => 0);
      Put_Line ("],");
      
      Put ("  [");
      Ada.Float_Text_IO.Put (M.ZX, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.ZY, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.ZZ, Fore => 0, Aft => 6, Exp => 0);
      Put_Line ("]");
      Put_Line ("]");
    end;

  procedure Put_Line (M : Matrix_4D) is
    begin
      Put_Line ("[");
      
      Put ("  [");
      Ada.Float_Text_IO.Put (M.XX, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.XY, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.XZ, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.XW, Fore => 0, Aft => 6, Exp => 0);
      Put_Line ("],");
      
      Put ("  [");
      Ada.Float_Text_IO.Put (M.YX, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.YY, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.YZ, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.YW, Fore => 0, Aft => 6, Exp => 0);
      Put_Line ("],");
      
      Put ("  [");
      Ada.Float_Text_IO.Put (M.ZX, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.ZY, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.ZZ, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.ZW, Fore => 0, Aft => 6, Exp => 0);
      Put_Line ("],");
      
      Put ("  [");
      Ada.Float_Text_IO.Put (M.WX, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.WY, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.WZ, Fore => 0, Aft => 6, Exp => 0);
      Put (", ");
      Ada.Float_Text_IO.Put (M.WW, Fore => 0, Aft => 6, Exp => 0);
      Put_Line ("]");
      
      Put_Line ("]");
    end;

    -- Comparison Routines

  function Equal (A, B : Vector_2D) return Boolean is
    (Equal (A.X, B.X, Epsilon_Real) and
     Equal (A.Y, B.Y, Epsilon_Real));

  function Equal (A, B : Vector_3D) return Boolean is
    begin
       return Equal (A.X, B.X, Epsilon_Real) and
              Equal (A.Y, B.Y, Epsilon_Real) and
              Equal (A.Z, B.Z, Epsilon_Real);
    end;

  function Equal (A, B : Vector_4D) return Boolean is
    begin
       return Equal (A.X, B.X, Epsilon_Real) and
              Equal (A.Y, B.Y, Epsilon_Real) and
              Equal (A.Z, B.Z, Epsilon_Real) and
              Equal (A.W, B.W, Epsilon_Real);
    end;

  function Equal (A, B : Matrix_2D) return Boolean is
    begin
       return Equal (A.XX, B.XX, Epsilon_Real) and
              Equal (A.XY, B.XY, Epsilon_Real) and
              Equal (A.YX, B.YX, Epsilon_Real) and
              Equal (A.YY, B.YY, Epsilon_Real);
    end;

  function Equal (A, B : Matrix_3D) return Boolean is
    (Equal (A.XX, B.XX, Epsilon_Real) and
     Equal (A.XY, B.XY, Epsilon_Real) and
     Equal (A.XZ, B.XZ, Epsilon_Real) and
     
     Equal (A.YX, B.YX, Epsilon_Real) and
     Equal (A.YY, B.YY, Epsilon_Real) and
     Equal (A.YZ, B.YZ, Epsilon_Real) and
     
     Equal (A.ZX, B.ZX, Epsilon_Real) and
     Equal (A.ZY, B.ZY, Epsilon_Real) and
     Equal (A.ZZ, B.ZZ, Epsilon_Real));

  function Equal (A, B : Matrix_4D) return Boolean is
    (Equal (A.XX, B.XX, Epsilon_Real) and
     Equal (A.XY, B.XY, Epsilon_Real) and
     Equal (A.XZ, B.XZ, Epsilon_Real) and
     Equal (A.XW, B.XW, Epsilon_Real) and
     
     Equal (A.YX, B.YX, Epsilon_Real) and
     Equal (A.YY, B.YY, Epsilon_Real) and
     Equal (A.YZ, B.YZ, Epsilon_Real) and
     Equal (A.YW, B.YW, Epsilon_Real) and
     
     Equal (A.ZX, B.ZX, Epsilon_Real) and
     Equal (A.ZY, B.ZY, Epsilon_Real) and
     Equal (A.ZZ, B.ZZ, Epsilon_Real) and
     Equal (A.ZW, B.ZW, Epsilon_Real) and
     
     Equal (A.WX, B.WX, Epsilon_Real) and
     Equal (A.WY, B.WY, Epsilon_Real) and
     Equal (A.WZ, B.WZ, Epsilon_Real) and
     Equal (A.WW, B.WW, Epsilon_Real));

  ----------
  -- Main --
  ----------

  Data : aliased Int_Unsigned := 0;
  begin

    ----------------------------
    -- Test Matrix_2D Multiply --
    ----------------------------

    declare
      type Test_State is record
        Left, Right, Expected : Matrix_2D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State :=

        -- Identity matrix multiplication (A * I = A)
        ((Left => (1.0, 0.0,
                   0.0, 1.0),
          Right => (1.0, 0.0,
                    0.0, 1.0),
          Expected => (1.0, 0.0,
                       0.0, 1.0 )),

        -- General matrix multiplication
        (Left => (1.0, 2.0,
                  3.0, 4.0),
         Right => (5.0, 6.0,
                   7.0, 8.0),
         Expected => (19.0, 22.0,
                      43.0, 50.0 )),

        -- Scaling matrix multiplication
        (Left => (2.0, 0.0,
                  0.0, 3.0),
         Right => (1.0, 0.0,
                   0.0, 1.0),
         Expected => (2.0, 0.0,
                      0.0, 3.0 )),

        -- Rotation matrix multiplication (90 degrees counterclockwise)
        (Left => (0.0, -1.0,
                  1.0,  0.0),
         Right => (0.0, -1.0,
                   1.0,  0.0),
         Expected => (-1.0, 0.0,
                      0.0, -1.0 )));

      Result : Matrix_2D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Matrix_2D Multiply (Test" & I'Img & "):" & TAB_8);
        Result := TESTS (I).Left * TESTS (I).Right;

        if not Equal (Result, TESTS (I).Expected) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected:");
          Put_Line (TESTS (I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;

    -----------------------------
    -- Test Matrix_3D Multiply --
    -----------------------------

    declare
    type Test_State is record Left, Right, Expected : Matrix_3D; end record;
    TESTS : constant array (Positive range <>) of Test_State :=

      -- Identity matrix multiplication (A * I = A)
     ((Left => (1.0, 0.0, 0.0,
                 0.0, 1.0, 0.0,
                 0.0, 0.0, 1.0),
       Right => (1.0, 0.0, 0.0,
                 0.0, 1.0, 0.0,
                 0.0, 0.0, 1.0),
       Expected => (1.0, 0.0, 0.0,
                    0.0, 1.0, 0.0,
                    0.0, 0.0, 1.0)),
       
      -- General matrix multiplication
      (Left => (1.0, 2.0, 3.0,
                 4.0, 5.0, 6.0,
                 7.0, 8.0, 9.0),
       Right =>  (9.0, 8.0, 7.0,
                  6.0, 5.0, 4.0,
                  3.0, 2.0, 1.0),
       Expected => (30.0, 24.0, 18.0,
                    84.0, 69.0, 54.0,
                    138.0, 114.0, 90.0)),

      -- Scaling matrix multiplication
      (Left => (2.0, 0.0, 0.0,
                 0.0, 3.0, 0.0,
                 0.0, 0.0, 4.0),
       Right => (1.0, 0.0, 0.0,
                 0.0, 1.0, 0.0,
                 0.0, 0.0, 1.0),
       Expected => (2.0, 0.0, 0.0,
                    0.0, 3.0, 0.0,
                    0.0, 0.0, 4.0)),

       -- Rotation matrix multiplication (90 degrees about Z-axis)
      (Left => (0.0, -1.0, 0.0,
                 1.0,  0.0, 0.0,
                 0.0,  0.0, 1.0),
       Right =>  (0.0, -1.0, 0.0,
                  1.0,  0.0, 0.0,
                  0.0,  0.0, 1.0),
       Expected =>  (-1.0, 0.0, 0.0,
                      0.0, -1.0, 0.0,
                      0.0,  0.0, 1.0)));

    Result : Matrix_3D;
    begin
      for I in Tests'Range loop
        Put_Separator;
        Put ("Matrix_3D Multiply (Test" & I'Img & "):" & TAB_8);
        Result := Tests (I).Left * Tests (I).Right;
        if not Equal (Result, Tests (I).Expected) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected:");
          Put_Line (Tests (I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;

    -----------------------------
    -- Test Matrix_4D Multiply --
    -----------------------------

    declare
      type Test_State is record
        Left, Right, Expected : Matrix_4D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State :=

        -- Identity matrix multiplication (I * I = I)
        ((Left => ( 1.0,  0.0,  0.0,  0.0,
                    0.0,  1.0,  0.0,  0.0,
                    0.0,  0.0,  1.0,  0.0,
                    0.0,  0.0,  0.0,  1.0 ),
          Right => ( 1.0,  0.0,  0.0,  0.0,
                     0.0,  1.0,  0.0,  0.0,
                     0.0,  0.0,  1.0,  0.0,
                     0.0,  0.0,  0.0,  1.0 ),
          Expected => ( 1.0,  0.0,  0.0,  0.0,
                        0.0,  1.0,  0.0,  0.0,
                        0.0,  0.0,  1.0,  0.0,
                        0.0,  0.0,  0.0,  1.0 )),

        -- General matrix multiplication
        (Left => ( 1.0,  2.0,  3.0,  4.0,
                   5.0,  6.0,  7.0,  8.0,
                   9.0,  10.0, 11.0, 12.0,
                   13.0, 14.0, 15.0, 16.0 ),
         Right => ( 1.0,  0.0,  0.0,  0.0,
                    0.0,  1.0,  0.0,  0.0,
                    0.0,  0.0,  1.0,  0.0,
                    0.0,  0.0,  0.0,  1.0 ),
         Expected => ( 1.0,  2.0,  3.0,  4.0,
                       5.0,  6.0,  7.0,  8.0,
                       9.0,  10.0, 11.0, 12.0,
                       13.0, 14.0, 15.0, 16.0 )),

        -- Scaling matrix multiplication
        (Left => ( 2.0,  0.0,  0.0,  0.0,
                   0.0,  2.0,  0.0,  0.0,
                   0.0,  0.0,  2.0,  0.0,
                   0.0,  0.0,  0.0,  1.0 ),
         Right => ( 1.0,  2.0,  3.0,  4.0,
                    5.0,  6.0,  7.0,  8.0,
                    9.0,  10.0, 11.0, 12.0,
                    13.0, 14.0, 15.0, 16.0 ),
         Expected => ( 2.0,  4.0,  6.0,  8.0,
                       10.0, 12.0, 14.0, 16.0,
                       18.0, 20.0, 22.0, 24.0,
                       13.0, 14.0, 15.0, 16.0 )),

        -- Translation matrix multiplication
        (Left => ( 1.0,  0.0,  0.0,  5.0,
                   0.0,  1.0,  0.0,  6.0,
                   0.0,  0.0,  1.0,  7.0,
                   0.0,  0.0,  0.0,  1.0 ),
         Right => ( 1.0,  0.0,  0.0,  0.0,
                    0.0,  1.0,  0.0,  0.0,
                    0.0,  0.0,  1.0,  0.0,
                    0.0,  0.0,  0.0,  1.0 ),
         Expected => ( 1.0,  0.0,  0.0,  5.0,
                       0.0,  1.0,  0.0,  6.0,
                       0.0,  0.0,  1.0,  7.0,
                       0.0,  0.0,  0.0,  1.0 )));

      Result : Matrix_4D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Matrix_4D Multiply (Test" & I'Img & "):" & TAB_8);
        Result := TESTS (I).Left * TESTS (I).Right;

        if not Equal (Result, TESTS (I).Expected) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected:");
          Put_Line (TESTS (I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;

    ----------------------------
    -- Test Invert Matrix_2D --
    ----------------------------

    declare
      type Test_State is record
        Input, Expected : Matrix_2D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State :=

        -- Identity matrix inversion
        ((
          Input => (
            1.0, 0.0,
            0.0, 1.0
          ),
          Expected => (
            1.0, 0.0,
            0.0, 1.0
          )
        ),

        -- Simple invertible matrix inversion
        (
          Input => (
            2.0, 1.0,
            1.0, 3.0
          ),
          Expected => (
            0.6, -0.2,
           -0.2,  0.4
          )
        ),

        -- Scaling matrix inversion
        (
          Input => (
            2.0, 0.0,
            0.0, 3.0
          ),
          Expected => (
            0.5, 0.0,
            0.0, 0.33333333
          )
        ),

        -- Rotation matrix inversion (inverse is transpose for rotation matrices)
        (
          Input => (
            0.0, -1.0,
            1.0,  0.0
          ),
          Expected => (
            0.0,  1.0,
           -1.0,  0.0
          )
        )
        );

      Result : Matrix_2D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Invert Matrix_2D (Test" & I'Img & "):" & TAB_8);
        Result := Invert (TESTS(I).Input);

        if not Equal (Result, TESTS(I).Expected) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected:");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;

    ----------------------------
    -- Test Invert Matrix_3D --
    ----------------------------

    declare
      type Test_State is record
        Input, Expected : Matrix_3D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State :=

        -- Identity matrix inversion
        ((
          Input => (
            1.0, 0.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 0.0, 1.0
          ),
          Expected => (
            1.0, 0.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 0.0, 1.0
          )
        ),

        -- Simple scaling matrix inversion
        (
          Input => (
            2.0, 0.0, 0.0,
            0.0, 3.0, 0.0,
            0.0, 0.0, 4.0
          ),
          Expected => (
            0.5,       0.0,      0.0,
            0.0, 0.33333333,      0.0,
            0.0,       0.0,    0.25
          )
        ),

        -- Rotation matrix inversion (inverse is transpose for rotation matrices)
        (
          Input => (
            0.0, -1.0, 0.0,
            1.0,  0.0, 0.0,
            0.0,  0.0, 1.0
          ),
          Expected => (
            0.0, 1.0,  0.0,
           -1.0, 0.0,  0.0,
            0.0, 0.0,  1.0
          )
        ),

        -- General invertible matrix inversion
        (
          Input => (
            1.0, 2.0, 3.0,
            0.0, 1.0, 4.0,
            5.0, 6.0, 0.0
          ),
          Expected => (
           -24.0,  18.0,   5.0,
            20.0, -15.0,  -4.0,
            -5.0,   4.0,   1.0
          )
        )
        );

      Result : Matrix_3D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Invert Matrix_3D (Test" & I'Img & "):" & TAB_8);
        Result := Invert (TESTS(I).Input);

        if not Equal (Result, TESTS(I).Expected) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected:");
          Put_Line (TESTS (I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;

    ----------------------------
    -- Test Invert Matrix_4D --
    ----------------------------

    declare
      type Test_State is record
        Input, Expected : Matrix_4D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State :=

        -- Identity matrix inversion
        ((
          Input => (
            1.0, 0.0, 0.0, 0.0,
            0.0, 1.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0,
            0.0, 0.0, 0.0, 1.0
          ),
          Expected => (
            1.0, 0.0, 0.0, 0.0,
            0.0, 1.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0,
            0.0, 0.0, 0.0, 1.0
          )
        ),

        -- Simple scaling matrix inversion
        (
          Input => (
            2.0, 0.0, 0.0, 0.0,
            0.0, 3.0, 0.0, 0.0,
            0.0, 0.0, 4.0, 0.0,
            0.0, 0.0, 0.0, 1.0
          ),
          Expected => (
            0.5,        0.0,       0.0, 0.0,
            0.0, 0.33333333,       0.0, 0.0,
            0.0,        0.0,    0.25, 0.0,
            0.0,        0.0,       0.0, 1.0
          )
        ),

        -- Translation matrix inversion
        (
          Input => (
            1.0, 0.0, 0.0, 5.0,
            0.0, 1.0, 0.0, 6.0,
            0.0, 0.0, 1.0, 7.0,
            0.0, 0.0, 0.0, 1.0
          ),
          Expected => (
            1.0, 0.0, 0.0, -5.0,
            0.0, 1.0, 0.0, -6.0,
            0.0, 0.0, 1.0, -7.0,
            0.0, 0.0, 0.0, 1.0
          )
        ),

        -- General transformation matrix inversion
        (Input => (
            1.0, 2.0, 3.0, 4.0,
            0.0, 1.0, 4.0, 5.0,
            0.0, 0.0, 1.0, 6.0,
            0.0, 0.0, 0.0, 1.0),
          Expected => (1.0, -2.0,  5.0, -24.0,
            0.0,  1.0, -4.0,  19.0,
            0.0,  0.0,  1.0,  -6.0,
            0.0,  0.0,  0.0,   1.0)
        ));

      Result : Matrix_4D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Invert Matrix_4D (Test" & I'Img & "):" & TAB_8);
        Result := Invert (TESTS(I).Input);

        if not Equal (TESTS (I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected:");
          Put_Line (TESTS (I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -----------------------------
    -- Test Determinant Matrix_2D --
    -----------------------------

    declare
      type Test_State is record
        Input    : Matrix_2D;
        Expected : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State :=

        -- Identity matrix determinant (det = 1)
        ((
          Input => (
            1.0, 0.0,
            0.0, 1.0
          ),
          Expected => 1.0
        ),

        -- Diagonal matrix determinant (det = product of diagonals)
        (
          Input => (
            2.0, 0.0,
            0.0, 3.0
          ),
          Expected => 6.0  -- 2 * 3
        ),

        -- Singular matrix determinant (det = 0)
        (
          Input => (
            1.0, 2.0,
            2.0, 4.0
          ),
          Expected => 0.0
        ),

        -- General invertible matrix determinant
        (
          Input => (
            2.0, 1.0,
            1.0, 3.0
          ),
          Expected => 5.0  -- (2*3) - (1*1) = 6 - 1 = 5
        ),

        -- Rotation matrix determinant (det = 1)
        (
          Input => (
            0.0, -1.0,
            1.0,  0.0
          ),
          Expected => 1.0
        ),

        -- Another invertible matrix determinant
        (
          Input => (
            4.0, 7.0,
            2.0, 6.0
          ),
          Expected => 10.0  -- (4*6) - (7*2) = 24 - 14 = 10
        )

      );

      Result : Real;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Determinant Matrix_2D (Test" & I'Img & "):" & TAB_8);
        Result := Determinant(TESTS(I).Input);

        if not Equal (Result, TESTS(I).Expected) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: " & TESTS(I).Expected'Img);
          Put_Line ("Actual:   " & Result'Img);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;

    --------------------------------
    -- Test Determinant Matrix_3D --
    --------------------------------

    declare
      type Test_State is record
        Input    : Matrix_3D;
        Expected : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State :=

        -- Identity matrix determinant (det = 1)
        ((
          Input => (
            1.0, 0.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 0.0, 1.0
          ),
          Expected => 1.0
        ),

        -- Diagonal matrix determinant (det = product of diagonals)
        (
          Input => (
            2.0, 0.0, 0.0,
            0.0, 3.0, 0.0,
            0.0, 0.0, 4.0
          ),
          Expected => 24.0  -- 2 * 3 * 4
        ),

        -- Singular matrix determinant (det = 0)
        (
          Input => (
            1.0, 2.0, 3.0,
            2.0, 4.0, 6.0,
            3.0, 6.0, 9.0
          ),
          Expected => 0.0
        ),

        -- General invertible matrix determinant (det = 1)
        (
          Input => (
            1.0, 2.0, 3.0,
            0.0, 1.0, 4.0,
            5.0, 6.0, 0.0
          ),
          Expected => 1.0  -- Calculated as 1*(1*0 - 4*6) - 2*(0*0 - 4*5) + 3*(0*6 - 1*5) = -24 + 40 - 15 = 1
        ),

        -- Rotation matrix determinant (det = 1)
        (
          Input => (
            0.0, -1.0, 0.0,
            1.0,  0.0, 0.0,
            0.0,  0.0, 1.0
          ),
          Expected => 1.0
        )
        );

      Result : Real;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Determinant Matrix_3D (Test" & I'Img & "):" & TAB_8);
        --Result := Determinant(TESTS(I).Input);

        if not Equal (Result, TESTS(I).Expected) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: " & TESTS(I).Expected'Img);
          Put_Line ("Actual:   " & Result'Img);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;

    --------------------------------
    -- Test Determinant Matrix_4D --
    --------------------------------

    declare
      type Test_State is record
        Input    : Matrix_4D;
        Expected : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State :=

        -- Identity matrix determinant (det = 1)
        ((
          Input => (
            1.0, 0.0, 0.0, 0.0,
            0.0, 1.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0,
            0.0, 0.0, 0.0, 1.0
          ),
          Expected => 1.0
        ),

        -- Diagonal matrix determinant (det = product of diagonals)
        (
          Input => (
            2.0, 0.0, 0.0, 0.0,
            0.0, 3.0, 0.0, 0.0,
            0.0, 0.0, 4.0, 0.0,
            0.0, 0.0, 0.0, 5.0
          ),
          Expected => 120.0  -- 2 * 3 * 4 * 5
        ),

        -- Singular matrix determinant (det = 0)
        (
          Input => (
            1.0, 2.0, 3.0, 4.0,
            2.0, 4.0, 6.0, 8.0,
            3.0, 6.0, 9.0, 12.0,
            4.0, 8.0, 12.0, 16.0
          ),
          Expected => 0.0
        ),

        -- General invertible matrix determinant
        (
          Input => (
            1.0, 2.0, 3.0, 4.0,
            5.0, 6.0, 7.0, 8.0,
            9.0, 10.0, 11.0, 12.0,
            13.0, 14.0, 15.0, 16.0
          ),
          Expected => 0.0  -- Note: This specific matrix is singular, det=0
        ),

        -- Another invertible matrix with known determinant
        (
          Input => (
            4.0, 3.0, 2.0, 1.0,
            0.0, 1.0, 0.0, 2.0,
            3.0, 0.0, 1.0, 0.0,
            1.0, 2.0, 3.0, 4.0
          ),
          Expected => -40.0  -- Calculated determinant
        )
        );

      Result : Real;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Determinant Matrix_4D (Test" & I'Img & "):" & TAB_8);
        Result := Determinant(TESTS(I).Input);

        if not Equal (Result, TESTS(I).Expected) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: " & TESTS(I).Expected'Img);
          Put_Line ("Actual:   " & Result'Img);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;

    -------------------------------
    -- Test Transpose Matrix_2D --
    -------------------------------

    declare
      type Test_State is record
        Input    : Matrix_2D;
        Expected : Matrix_2D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Identity matrix transpose (I^T = I)
        (
          Input => (
            1.0, 0.0,
            0.0, 1.0
          ),
          Expected => (
            1.0, 0.0,
            0.0, 1.0
          )
        ),

        -- General matrix transpose
        (
          Input => (
            1.0, 2.0,
            3.0, 4.0
          ),
          Expected => (
            1.0, 3.0,
            2.0, 4.0
          )
        ),

        -- Symmetric matrix transpose (M^T = M)
        (
          Input => (
            5.0, 7.0,
            7.0, 9.0
          ),
          Expected => (
            5.0, 7.0,
            7.0, 9.0
          )
        ),

        -- Rotation matrix transpose
        (
          Input => (
            0.0, -1.0,
            1.0,  0.0
          ),
          Expected => (
            0.0, 1.0,
           -1.0, 0.0
          )
        )
      );

      Result : Matrix_2D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Transpose Matrix_2D (Test " & I'Img & "):" & TAB_8);
        Result := Transpose(TESTS(I).Input);

        -- Compare each element with a tolerance
        if not Equal (TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Transpose Matrix_3D --
    -------------------------------

    declare
      type Test_State is record
        Input    : Matrix_3D;
        Expected : Matrix_3D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Identity matrix transpose (I^T = I)
        (
          Input => (
            1.0, 0.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 0.0, 1.0
          ),
          Expected => (
            1.0, 0.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 0.0, 1.0
          )
        ),

        -- General matrix transpose
        (
          Input => (
            1.0, 2.0, 3.0,
            4.0, 5.0, 6.0,
            7.0, 8.0, 9.0
          ),
          Expected => (
            1.0, 4.0, 7.0,
            2.0, 5.0, 8.0,
            3.0, 6.0, 9.0
          )
        ),

        -- Symmetric matrix transpose (M^T = M)
        (
          Input => (
            2.0, 3.0, 4.0,
            3.0, 5.0, 6.0,
            4.0, 6.0, 8.0
          ),
          Expected => (
            2.0, 3.0, 4.0,
            3.0, 5.0, 6.0,
            4.0, 6.0, 8.0
          )
        ),

        -- Rotation matrix transpose
        (
          Input => (
            0.0, -1.0, 0.0,
            1.0,  0.0, 0.0,
            0.0,  0.0, 1.0
          ),
          Expected => (
            0.0, 1.0, 0.0,
           -1.0, 0.0, 0.0,
            0.0, 0.0, 1.0
          )
        )
      );

      Result : Matrix_3D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Transpose Matrix_3D (Test " & I'Img & "):" & TAB_8);
        Result := Transpose(TESTS(I).Input);

        -- Compare each element with a tolerance
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Transpose Matrix_4D --
    -------------------------------

    declare
      type Test_State is record
        Input    : Matrix_4D;
        Expected : Matrix_4D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Identity matrix transpose (I^T = I)
        (
          Input => (
            1.0, 0.0, 0.0, 0.0,
            0.0, 1.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0,
            0.0, 0.0, 0.0, 1.0
          ),
          Expected => (
            1.0, 0.0, 0.0, 0.0,
            0.0, 1.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0,
            0.0, 0.0, 0.0, 1.0
          )
        ),

        -- General matrix transpose
        (
          Input => (
            1.0, 2.0, 3.0, 4.0,
            5.0, 6.0, 7.0, 8.0,
            9.0, 10.0, 11.0, 12.0,
            13.0, 14.0, 15.0, 16.0
          ),
          Expected => (
            1.0, 5.0, 9.0, 13.0,
            2.0, 6.0, 10.0, 14.0,
            3.0, 7.0, 11.0, 15.0,
            4.0, 8.0, 12.0, 16.0
          )
        ),

        -- Symmetric matrix transpose (M^T = M)
        (
          Input => (
            2.0, 3.0, 4.0, 5.0,
            3.0, 6.0, 7.0, 8.0,
            4.0, 7.0, 10.0, 11.0,
            5.0, 8.0, 11.0, 14.0
          ),
          Expected => (
            2.0, 3.0, 4.0, 5.0,
            3.0, 6.0, 7.0, 8.0,
            4.0, 7.0, 10.0, 11.0,
            5.0, 8.0, 11.0, 14.0
          )
        ),

        -- Rotation matrix transpose
        (
          Input => (
            0.0, -1.0, 0.0, 0.0,
            1.0,  0.0, 0.0, 0.0,
            0.0,  0.0, 1.0, 0.0,
            0.0,  0.0, 0.0, 1.0
          ),
          Expected => (
            0.0, 1.0, 0.0, 0.0,
           -1.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0,
            0.0, 0.0, 0.0, 1.0
          )
        )
      );

      Result : Matrix_4D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Transpose Matrix_4D (Test " & I'Img & "):" & TAB_8);
        Result := Transpose(TESTS(I).Input);

        -- Compare each element with a tolerance
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -----------------------------
    -- Test Cross Product Vector_3D --
    -----------------------------

    declare

        -----------------------------
        -- Test_State Record --
        -----------------------------
        type Test_State is record
            A, B     : Vector_3D;
            Expected : Vector_3D;
        end record;

        -----------------------------
        -- TESTS Array --
        -----------------------------
        TESTS : constant array (Positive range <>) of Test_State := (

            -- Standard basis vectors: i x j = k
            (
                A => (1.0, 0.0, 0.0),
                B => (0.0, 1.0, 0.0),
                Expected => (0.0, 0.0, 1.0)
            ),

            -- Standard basis vectors: j x k = i
            (
                A => (0.0, 1.0, 0.0),
                B => (0.0, 0.0, 1.0),
                Expected => (1.0, 0.0, 0.0)
            ),

            -- Standard basis vectors: k x i = j
            (
                A => (0.0, 0.0, 1.0),
                B => (1.0, 0.0, 0.0),
                Expected => (0.0, 1.0, 0.0)
            ),

            -- Parallel vectors: Cross product should be zero vector
            (
                A => (2.0, 4.0, 6.0),
                B => (4.0, 8.0, 12.0),
                Expected => (0.0, 0.0, 0.0)
            ),

            -- One zero vector: Cross product should be zero vector
            (
                A => (0.0, 0.0, 0.0),
                B => (1.0, 2.0, 3.0),
                Expected => (0.0, 0.0, 0.0)
            ),

            -- General vectors
            (
                A => (3.0, -3.0, 1.0),
                B => (4.0, 9.0, 2.0),
                Expected => ((-15.0), (-2.0), (39.0))
            ),

            -- Another set of general vectors
            (
                A => (1.0, 2.0, 3.0),
                B => (4.0, 5.0, 6.0),
                Expected => ((-3.0), (6.0), (-3.0))
            )

            -- Add more test cases as needed
        );

        Result : Vector_3D;

    begin
        for I in TESTS'Range loop
            Put_Separator;
            Put ("Cross Product Vector_3D (Test " & I'Img & "):" & TAB_8);

            -- Perform the cross product
            Result := Cross(TESTS(I).A, TESTS(I).B);

            -- Compare the result with the expected vector
            if not Equal(Result, TESTS(I).Expected) then
              Put_Line ("FAIL");
              New_Line;
              Put_Line ("Expected: ");
              Put_Line (TESTS(I).Expected);
              Put_Line ("Actual:   ");
              Put_Line (Result);
            else
              Put_Line ("PASS");
            end if;
        end loop;
    end;

    -------------------------------
    -- Test Project_T Vector_2D --
    -------------------------------

    declare
      type Test_State is record
        A        : Vector_2D;
        B        : Vector_2D;
        Expected : Vector_2D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Projecting a vector onto itself (A projected onto A = A)
        (
          A => (
            3.0, 4.0
          ),
          B => (
            3.0, 4.0
          ),
          Expected => (
            3.0, 4.0
          )
        ),

        -- Projecting a vector onto the zero vector (result should be zero vector)
        (
          A => (
            5.0, -2.0
          ),
          B => (
            0.0, 0.0
          ),
          Expected => (
            0.0, 0.0
          )
        ),

        -- Projecting a vector onto a perpendicular vector (result should be zero vector)
        (
          A => (
            1.0, 0.0
          ),
          B => (
            0.0, 1.0
          ),
          Expected => (
            0.0, 0.0
          )
        ),

        -- Projecting a vector onto a non-unit vector
        (
          A => (
            4.0, 3.0
          ),
          B => (
            2.0, 0.0
          ),
          Expected => (
            4.0, 0.0
          )
        ),

        -- Projecting the zero vector onto another vector (result should be zero vector)
        (
          A => (
            0.0, 0.0
          ),
          B => (
            1.0, 2.0
          ),
          Expected => (
            0.0, 0.0
          )
        ),

        -- Projecting a vector onto another vector with known projection
        (
          A => (
            2.0, 3.0
          ),
          B => (
            4.0, 0.0
          ),
          Expected => (
            2.0, 0.0
          )
        )
      );

      Result : Vector_2D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Project Vector_2D (Test " & I'Img & "):" & TAB_8);
        Result := Project (TESTS(I).A, TESTS(I).B);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Project_T Vector_3D --
    -------------------------------

    declare
      type Test_State is record
        A        : Vector_3D;
        B        : Vector_3D;
        Expected : Vector_3D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Projecting a vector onto itself (A projected onto A = A)
        (
          A => (
            1.0, 2.0, 3.0
          ),
          B => (
            1.0, 2.0, 3.0
          ),
          Expected => (
            1.0, 2.0, 3.0
          )
        ),

        -- Projecting a vector onto the zero vector (result should be zero vector)
        (
          A => (
            4.0, -1.0, 2.0
          ),
          B => (
            0.0, 0.0, 0.0
          ),
          Expected => (
            0.0, 0.0, 0.0
          )
        ),

        -- Projecting a vector onto a perpendicular vector (result should be zero vector)
        (
          A => (
            0.0, 1.0, 0.0
          ),
          B => (
            1.0, 0.0, 0.0
          ),
          Expected => (
            0.0, 0.0, 0.0
          )
        ),

        -- Projecting a vector onto a non-unit vector
        (
          A => (
            3.0, 4.0, 0.0
          ),
          B => (
            3.0, 0.0, 4.0
          ),
          Expected => (
            1.08, 0.0, 1.44
          )
        ),


        -- Projecting the zero vector onto another vector (result should be zero vector)
        (
          A => (
            0.0, 0.0, 0.0
          ),
          B => (
            2.0, 3.0, 6.0
          ),
          Expected => (
            0.0, 0.0, 0.0
          )
        ),

        -- Projecting a vector onto another vector with known projection
        (
          A => (
            5.0, 2.0, -1.0
          ),
          B => (
            3.0, 1.0, 2.0
          ),
          Expected => (
            3.21428, 1.071428, 2.142857
          )
        )
      );

      Result : Vector_3D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Project Vector_3D (Test " & I'Img & "):" & TAB_8);
        Result := Project(TESTS(I).A, TESTS(I).B);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Project_T Vector_4D --
    -------------------------------

    declare
      type Test_State is record
        A        : Vector_4D;
        B        : Vector_4D;
        Expected : Vector_4D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Projecting a vector onto itself (A projected onto A = A)
        (
          A => (
            1.0, 2.0, 3.0, 4.0
          ),
          B => (
            1.0, 2.0, 3.0, 4.0
          ),
          Expected => (
            1.0, 2.0, 3.0, 4.0
          )
        ),

        -- Projecting a vector onto the zero vector (result should be zero vector)
        (
          A => (
            5.0, -2.0, 1.0, 3.0
          ),
          B => (
            0.0, 0.0, 0.0, 0.0
          ),
          Expected => (
            0.0, 0.0, 0.0, 0.0
          )
        ),

        -- Projecting a vector onto a perpendicular vector (result should be zero vector)
        (
          A => (
            0.0, 1.0, 0.0, 0.0
          ),
          B => (
            1.0, 0.0, 0.0, 0.0
          ),
          Expected => (
            0.0, 0.0, 0.0, 0.0
          )
        ),

        -- Projecting a vector onto a non-unit vector
        (
          A => (
            4.0, 3.0, 0.0, 0.0
          ),
          B => (
            2.0, 0.0, 0.0, 0.0
          ),
          Expected => (
            4.0, 0.0, 0.0, 0.0
          )
        ),

        -- Projecting the zero vector onto another vector (result should be zero vector)
        (
          A => (
            0.0, 0.0, 0.0, 0.0
          ),
          B => (
            2.0, 3.0, 6.0, 1.0
          ),
          Expected => (
            0.0, 0.0, 0.0, 0.0
          )
        )
      );

      Result : Vector_4D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Project Vector_4D (Test " & I'Img & "):" & TAB_8);
        Result := Project(TESTS(I).A, TESTS(I).B);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    ------------------------------
    -- Test Reject_T Vector_2D --
    ------------------------------

    declare
      type Test_State is record
        A        : Vector_2D;
        B        : Vector_2D;
        Expected : Vector_2D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Rejecting a vector from itself (A - projection = zero vector)
        (
          A => (
            3.0, 4.0
          ),
          B => (
            3.0, 4.0
          ),
          Expected => (
            0.0, 0.0
          )
        ),

        -- Rejecting a vector from the zero vector (A - 0 = A)
        (
          A => (
            5.0, -2.0
          ),
          B => (
            0.0, 0.0
          ),
          Expected => (
            5.0, -2.0
          )
        ),

        -- Rejecting a vector from a perpendicular vector (A - 0 = A)
        (
          A => (
            1.0, 0.0
          ),
          B => (
            0.0, 1.0
          ),
          Expected => (
            1.0, 0.0
          )
        ),

        -- Rejecting a vector from a non-unit vector
        (
          A => (
            4.0, 3.0
          ),
          B => (
            2.0, 0.0
          ),
          Expected => (
            0.0, 3.0
          )
        ),

        -- Rejecting the zero vector from another vector (result should be zero vector)
        (
          A => (
            0.0, 0.0
          ),
          B => (
            1.0, 2.0
          ),
          Expected => (
            0.0, 0.0
          )
        )
      );

      Result : Vector_2D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Reject Vector_2D (Test " & I'Img & "):" & TAB_8);
        Result := Reject(TESTS(I).A, TESTS(I).B);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    ------------------------------
    -- Test Reject_T Vector_3D --
    ------------------------------

    declare
      type Test_State is record
        A        : Vector_3D;
        B        : Vector_3D;
        Expected : Vector_3D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Rejecting a vector from itself (A - projection = zero vector)
        (
          A => (
            1.0, 2.0, 3.0
          ),
          B => (
            1.0, 2.0, 3.0
          ),
          Expected => (
            0.0, 0.0, 0.0
          )
        ),

        -- Rejecting a vector from the zero vector (A - 0 = A)
        (
          A => (
            4.0, -1.0, 2.0
          ),
          B => (
            0.0, 0.0, 0.0
          ),
          Expected => (
            4.0, -1.0, 2.0
          )
        ),

        -- Rejecting a vector from a perpendicular vector (A - 0 = A)
        (
          A => (
            0.0, 1.0, 0.0
          ),
          B => (
            1.0, 0.0, 0.0
          ),
          Expected => (
            0.0, 1.0, 0.0
          )
        ),

        -- Rejecting a vector from a non-unit vector
        (
          A => (
            3.0, 4.0, 0.0
          ),
          B => (
            3.0, 0.0, 4.0
          ),
          Expected => (
            1.92, 4.0, -1.44
          )
        ),

        -- Rejecting the zero vector from another vector (result should be zero vector)
        (
          A => (
            0.0, 0.0, 0.0
          ),
          B => (
            2.0, 3.0, 6.0
          ),
          Expected => (
            0.0, 0.0, 0.0
          )
        )
      );

      Result : Vector_3D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Reject Vector_3D (Test " & I'Img & "):" & TAB_8);
        Result := Reject(TESTS(I).A, TESTS(I).B);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    ------------------------------
    -- Test Reject_T Vector_4D --
    ------------------------------

    declare
      type Test_State is record
        A        : Vector_4D;
        B        : Vector_4D;
        Expected : Vector_4D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Rejecting a vector from itself (A - projection = zero vector)
        (
          A => (
            1.0, 2.0, 3.0, 4.0
          ),
          B => (
            1.0, 2.0, 3.0, 4.0
          ),
          Expected => (
            0.0, 0.0, 0.0, 0.0
          )
        ),

        -- Rejecting a vector from the zero vector (A - 0 = A)
        (
          A => (
            5.0, -2.0, 1.0, 3.0
          ),
          B => (
            0.0, 0.0, 0.0, 0.0
          ),
          Expected => (
            5.0, -2.0, 1.0, 3.0
          )
        ),

        -- Rejecting a vector from a perpendicular vector (result should be the original vector)
        (
          A => (
            0.0, 1.0, 0.0, 0.0
          ),
          B => (
            1.0, 0.0, 0.0, 0.0
          ),
          Expected => (
            0.0, 1.0, 0.0, 0.0
          )
        ),

        -- Rejecting a vector from a non-unit vector
        (
          A => (
            4.0, 3.0, 0.0, 0.0
          ),
          B => (
            2.0, 0.0, 0.0, 0.0
          ),
          Expected => (
            0.0, 3.0, 0.0, 0.0
          )
        ),

        -- Rejecting the zero vector from another vector (result should be zero vector)
        (
          A => (
            0.0, 0.0, 0.0, 0.0
          ),
          B => (
            2.0, 3.0, 6.0, 1.0
          ),
          Expected => (
            0.0, 0.0, 0.0, 0.0
          )
        )
      );

      Result : Vector_4D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Reject Vector_4D (Test " & I'Img & "):" & TAB_8);
        Result := Reject(TESTS(I).A, TESTS(I).B);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Dot_T Vector_2D --
    -------------------------------

    declare
      type Test_State is record
        A        : Vector_2D;
        B        : Vector_2D;
        Expected : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Dot product of two positive vectors
        (
          A => (
            1.0, 2.0
          ),
          B => (
            3.0, 4.0
          ),
          Expected => 11.0  -- 1*3 + 2*4 = 3 + 8 = 11
        ),

        -- Dot product with zero vector
        (
          A => (
            0.0, 0.0
          ),
          B => (
            5.0, 6.0
          ),
          Expected => 0.0  -- 0*5 + 0*6 = 0 + 0 = 0
        ),

        -- Dot product of negative and positive components
        (
          A => (
            -1.0, 3.0
          ),
          B => (
            4.0, -2.0
          ),
          Expected => -10.0  -- (-1)*4 + 3*(-2) = -4 -6 = -10
        ),

        -- Dot product with floating-point numbers
        (
          A => (
            2.5, 4.0
          ),
          B => (
            1.5, -2.0
          ),
          Expected => -4.25  -- 2.5*1.5 + 4.0*(-2.0) = 3.75 -8 = -4.25
        )
      );

      Result : Real;
      Tolerance : constant Real := 1.0E-6;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Dot Vector_2D (Test " & I'Img & "):" & TAB_8);
        Result := Dot(TESTS(I).A, TESTS(I).B);

        -- Compare the result with the expected value using tolerance
        if Abs(Result - TESTS(I).Expected) > Tolerance then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: " & TESTS(I).Expected'Img);
          Put_Line ("Actual:   " & Result'Img);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Dot_T Vector_3D --
    -------------------------------

    declare
      type Test_State is record
        A        : Vector_3D;
        B        : Vector_3D;
        Expected : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Dot product of two positive vectors
        (
          A => (
            1.0, 2.0, 3.0
          ),
          B => (
            4.0, 5.0, 6.0
          ),
          Expected => 32.0  -- 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
        ),

        -- Dot product with zero vector
        (
          A => (
            0.0, 0.0, 0.0
          ),
          B => (
            7.0, 8.0, 9.0
          ),
          Expected => 0.0  -- 0*7 + 0*8 + 0*9 = 0 + 0 + 0 = 0
        ),

        -- Dot product of negative and positive components
        (
          A => (
            -1.0, 3.0, 2.0
          ),
          B => (
            4.0, -2.0, 1.0
          ),
          Expected => -8.0  -- (-1)*4 + 3*(-2) + 2*1 = -4 -6 +2 = -8
        ),

        -- Dot product with floating-point numbers
        (
          A => (
            2.5, -1.0, 3.0
          ),
          B => (
            1.5, 4.0, -2.0
          ),
          Expected => -6.25  -- 2.5*1.5 + (-1.0)*4.0 + 3.0*(-2.0) = 3.75 -4 -6 = -6.25
        )
      );

      Result : Real;
      Tolerance : constant Real := 1.0E-6;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Dot Vector_3D (Test " & I'Img & "):" & TAB_8);
        Result := Dot(TESTS(I).A, TESTS(I).B);

        -- Compare the result with the expected value using tolerance
        if Abs(Result - TESTS(I).Expected) > Tolerance then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: " & TESTS(I).Expected'Img);
          Put_Line ("Actual:   " & Result'Img);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Dot_T Vector_4D --
    -------------------------------

    declare
      type Test_State is record
        A        : Vector_4D;
        B        : Vector_4D;
        Expected : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Dot product of two positive vectors
        (
          A => (
            1.0, 2.0, 3.0, 4.0
          ),
          B => (
            5.0, 6.0, 7.0, 8.0
          ),
          Expected => 70.0  -- 1*5 + 2*6 + 3*7 + 4*8 = 5 + 12 + 21 + 32 = 70
        ),

        -- Dot product with zero vector
        (
          A => (
            0.0, 0.0, 0.0, 0.0
          ),
          B => (
            9.0, 10.0, 11.0, 12.0
          ),
          Expected => 0.0  -- 0*9 + 0*10 + 0*11 + 0*12 = 0 + 0 + 0 + 0 = 0
        ),

        -- Dot product of negative and positive components
        (
          A => (
            -1.0, 3.0, 2.0, -2.0
          ),
          B => (
            4.0, -2.0, 1.0, 3.0
          ),
          Expected => -14.0  -- (-1)*4 + 3*(-2) + 2*1 + (-2)*3 = -4 -6 +2 -6 = -14
        ),

        -- Dot product with floating-point numbers
        (
          A => (
            2.5, -1.0, 3.0, 4.0
          ),
          B => (
            1.5, 4.0, -2.0, 1.0
          ),
          Expected => -2.25  -- 2.5*1.5 + (-1.0)*4.0 + 3.0*(-2.0) + 4.0*1.0 = 3.75 -4 -6 +4 = -2.25
        )
      );

      Result : Real;
      Tolerance : constant Real := 1.0E-6;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Dot Vector_4D (Test " & I'Img & "):" & TAB_8);
        Result := Dot(TESTS(I).A, TESTS(I).B);

        -- Compare the result with the expected value using tolerance
        if Abs(Result - TESTS(I).Expected) > Tolerance then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: " & TESTS(I).Expected'Img);
          Put_Line ("Actual:   " & Result'Img);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Dot Plane_4D Point_3D --
    -------------------------------

    declare
      type Test_State is record
        Plane    : Plane_4D;
        Point    : Point_3D;
        Expected : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Point lies on the plane
        (
          Plane => (
            1.0, 0.0, 0.0, -5.0
          ),
          Point => (
            5.0, 0.0, 0.0
          ),
          Expected => 0.0  -- 1*5 + 0*0 + 0*0 + (-5) = 5 + 0 + 0 -5 = 0
        ),

        -- Point above the plane
        (
          Plane => (
            0.0, 1.0, 0.0, -3.0
          ),
          Point => (
            0.0, 4.0, 0.0
          ),
          Expected => 1.0  -- 0*0 +1*4 +0*0 +(-3) =0 +4 +0 -3 =1
        ),

        -- Point lies on the plane
        (
          Plane => (
            0.0, 0.0, 1.0, 2.0
          ),
          Point => (
            0.0, 0.0, -2.0
          ),
          Expected => 0.0  -- 0*0 +0*0 +1*(-2) +2 =0 +0 -2 +2 =0
        ),

        -- Point lies on the plane
        (
          Plane => (
            1.0, 2.0, 3.0, -6.0
          ),
          Point => (
            1.0, 1.0, 1.0
          ),
          Expected => 0.0  --1*1 +2*1 +3*1 + (-6) =1 +2 +3 -6 =0
        ),

        -- Point lies on the plane (different plane)
        (
          Plane => (
            -1.0, -2.0, -3.0, 6.0
          ),
          Point => (
            2.0, 2.0, 0.0
          ),
          Expected => 0.0  --(-1)*2 + (-2)*2 + (-3)*0 +6 =-2 -4 +0 +6 =0
        ),

        -- Point not on the plane
        (
          Plane => (
            2.0, -1.0, 4.0, -8.0
          ),
          Point => (
            2.0, 0.0, 0.0
          ),
          Expected => -4.0  --2*2 + (-1)*0 +4*0 +(-8) =4 +0 +0 -8 =-4
        )
      );

      Result : Real;
      Tolerance : constant Real := 1.0E-6;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Dot Plane_4D Point_3D (Test " & I'Img & "):" & TAB_8);
        Result := Dot(TESTS(I).Plane, TESTS(I).Point);

        -- Compare the result with the expected value using tolerance
        if Abs(Result - TESTS(I).Expected) > Tolerance then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: " & TESTS(I).Expected'Img);
          Put_Line ("Actual:   " & Result'Img);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Inverse_Sqrt Vector_2D --
    -------------------------------

    declare
      type Test_State is record
        Input    : Vector_2D;
        Expected : Vector_2D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Inverse Sqrt of positive vectors
        (
          Input => (
            4.0, 9.0
          ),
          Expected => (
            0.5, 0.3333333333333333
          )
        ),

        -- Inverse Sqrt with one component as 1
        (
          Input => (
            1.0, 16.0
          ),
          Expected => (
            1.0, 0.25
          )
        ),

        -- Inverse Sqrt with floating-point numbers
        (
          Input => (
            2.25, 0.25
          ),
          Expected => (
            1.0 / 1.5, 1.0 / 0.5  -- ≈ 0.6666666666666666, 2.0
          )
        ),

        -- Inverse Sqrt of unit vector
        (
          Input => (
            1.0, 1.0
          ),
          Expected => (
            1.0, 1.0
          )
        )
      );

      Result : Vector_2D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Inverse_Sqrt Vector_2D (Test " & I'Img & "):" & TAB_8);
        Result := Inverse_Sqrt(TESTS(I).Input);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;

    -------------------------------
    -- Test Inverse_Sqrt Vector_3D --
    -------------------------------

    declare
      type Test_State is record
        Input    : Vector_3D;
        Expected : Vector_3D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Inverse Sqrt of positive vectors
        (
          Input => (
            4.0, 9.0, 16.0
          ),
          Expected => (
            0.5, 0.3333333333333333, 0.25
          )
        ),

        -- Inverse Sqrt with one component as 1
        (
          Input => (
            1.0, 25.0, 36.0
          ),
          Expected => (
            1.0, 0.2, 0.16666666666666666
          )
        ),

        -- Inverse Sqrt with floating-point numbers
        (
          Input => (
            2.25, 0.25, 6.25
          ),
          Expected => (
            0.6666666666666666, 2.0, 0.4
          )
        ),

        -- Inverse Sqrt of unit vector
        (
          Input => (
            1.0, 1.0, 1.0
          ),
          Expected => (
            1.0, 1.0, 1.0
          )
        )
      );

      Result : Vector_3D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Inverse_Sqrt Vector_3D (Test " & I'Img & "):" & TAB_8);
        Result := Inverse_Sqrt(TESTS(I).Input);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;

    -------------------------------
    -- Test Inverse_Sqrt Vector_4D --
    -------------------------------

    declare
      type Test_State is record
        Input    : Vector_4D;
        Expected : Vector_4D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Inverse Sqrt of positive vectors
        (
          Input => (
            4.0, 9.0, 16.0, 25.0
          ),
          Expected => (
            0.5, 0.3333333333333333, 0.25, 0.2
          )
        ),

        -- Inverse Sqrt with one component as 1
        (
          Input => (
            1.0, 36.0, 49.0, 64.0
          ),
          Expected => (
            1.0, 0.16666666666666666, 0.14285714285714285, 0.125
          )
        ),

        -- Inverse Sqrt with floating-point numbers
        (
          Input => (
            2.25, 0.25, 6.25, 9.0
          ),
          Expected => (
            0.6666666666666666, 2.0, 0.4, 0.3333333333333333
          )
        ),

        -- Inverse Sqrt of unit vector
        (
          Input => (
            1.0, 1.0, 1.0, 1.0
          ),
          Expected => (
            1.0, 1.0, 1.0, 1.0
          )
        )
      );

      Result : Vector_4D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Inverse_Sqrt Vector_4D (Test " & I'Img & "):" & TAB_8);
        Result := Inverse_Sqrt(TESTS(I).Input);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Inverse_Sqrt Real --
    -------------------------------

    declare
      type Test_State is record
        Input    : Real;
        Expected : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Inverse Sqrt of positive real number
        (
          Input => 4.0,
          Expected => 0.5  -- 1 / sqrt(4.0) = 0.5
        ),

        -- Inverse Sqrt of 1
        (
          Input => 1.0,
          Expected => 1.0  -- 1 / sqrt(1.0) = 1.0
        ),

        -- Inverse Sqrt with floating-point number
        (
          Input => 2.25,
          Expected => 1.0 / 1.5  -- ≈ 0.6666666666666666
        ),

        -- Inverse Sqrt of a larger number
        (
          Input => 25.0,
          Expected => 0.2  -- 1 / sqrt(25.0) = 0.2
        )
      );

      Result : Real;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Inverse_Sqrt Real (Test " & I'Img & "):" & TAB_8);
        Result := Inverse_Sqrt(TESTS(I).Input);

        -- Compare the result with the expected value
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: " & TESTS(I).Expected'Img);
          Put_Line ("Actual:   " & Result'Img);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Sqrt Vector_2D --
    -------------------------------

    declare
      type Test_State is record
        Input    : Vector_2D;
        Expected : Vector_2D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Sqrt of positive vectors
        (
          Input => (
            4.0, 9.0
          ),
          Expected => (
            2.0, 3.0
          )
        ),

        -- Sqrt with one component as 1
        (
          Input => (
            1.0, 16.0
          ),
          Expected => (
            1.0, 4.0
          )
        ),

        -- Sqrt with floating-point numbers
        (
          Input => (
            2.25, 0.25
          ),
          Expected => (
            1.5, 0.5
          )
        ),

        -- Sqrt of unit vector
        (
          Input => (
            1.0, 1.0
          ),
          Expected => (
            1.0, 1.0
          )
        )
      );

      Result : Vector_2D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Sqrt Vector_2D (Test " & I'Img & "):" & TAB_8);
        Result := Sqrt(TESTS(I).Input);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Sqrt Vector_3D --
    -------------------------------

    declare
      type Test_State is record
        Input    : Vector_3D;
        Expected : Vector_3D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Sqrt of positive vectors
        (
          Input => (
            4.0, 9.0, 16.0
          ),
          Expected => (
            2.0, 3.0, 4.0
          )
        ),

        -- Sqrt with one component as 1
        (
          Input => (
            1.0, 25.0, 36.0
          ),
          Expected => (
            1.0, 5.0, 6.0
          )
        ),

        -- Sqrt with floating-point numbers
        (
          Input => (
            2.25, 0.25, 6.25
          ),
          Expected => (
            1.5, 0.5, 2.5
          )
        ),

        -- Sqrt of unit vector
        (
          Input => (
            1.0, 1.0, 1.0
          ),
          Expected => (
            1.0, 1.0, 1.0
          )
        )
      );

      Result : Vector_3D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Sqrt Vector_3D (Test " & I'Img & "):" & TAB_8);
        Result := Sqrt(TESTS(I).Input);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Sqrt Vector_4D --
    -------------------------------

    declare
      type Test_State is record
        Input    : Vector_4D;
        Expected : Vector_4D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Sqrt of positive vectors
        (
          Input => (
            4.0, 9.0, 16.0, 25.0
          ),
          Expected => (
            2.0, 3.0, 4.0, 5.0
          )
        ),

        -- Sqrt with one component as 1
        (
          Input => (
            1.0, 36.0, 49.0, 64.0
          ),
          Expected => (
            1.0, 6.0, 7.0, 8.0
          )
        ),

        -- Sqrt with floating-point numbers
        (
          Input => (
            2.25, 0.25, 6.25, 81.0
          ),
          Expected => (
            1.5, 0.5, 2.5, 9.0
          )
        ),

        -- Sqrt of unit vector
        (
          Input => (
            1.0, 1.0, 1.0, 1.0
          ),
          Expected => (
            1.0, 1.0, 1.0, 1.0
          )
        )
      );

      Result : Vector_4D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Sqrt Vector_4D (Test " & I'Img & "):" & TAB_8);
        Result := Sqrt(TESTS(I).Input);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Vector Vector_2D --
    -------------------------------

    declare
      type Test_State is record
        Left     : Vector_2D;
        Right    : Vector_2D;
        Expected : Vector_2D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Component-wise multiplication of positive vectors
        (
          Left => (
            2.0, 3.0
          ),
          Right => (
            4.0, 5.0
          ),
          Expected => (
            8.0, 15.0  -- 2*4 = 8, 3*5 = 15
          )
        ),

        -- Component-wise multiplication with zero vector
        (
          Left => (
            0.0, 7.0
          ),
          Right => (
            3.0, 0.0
          ),
          Expected => (
            0.0, 0.0  -- 0*3 = 0, 7*0 = 0
          )
        ),

        -- Component-wise multiplication with negative components
        (
          Left => (
            -1.0, 4.0
          ),
          Right => (
            5.0, -2.0
          ),
          Expected => (
            -5.0, -8.0  -- (-1)*5 = -5, 4*(-2) = -8
          )
        ),

        -- Component-wise multiplication with floating-point numbers
        (
          Left => (
            1.5, 2.5
          ),
          Right => (
            3.0, 4.0
          ),
          Expected => (
            4.5, 10.0  -- 1.5*3.0 = 4.5, 2.5*4.0 = 10.0
          )
        )
      );

      Result : Vector_2D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Vector Vector_2D (Test " & I'Img & "):" & TAB_8);
        Result := TESTS(I).Left * TESTS(I).Right;

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Vector Vector_3D --
    -------------------------------

    declare
      type Test_State is record
        Left     : Vector_3D;
        Right    : Vector_3D;
        Expected : Vector_3D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Component-wise multiplication of positive vectors
        (
          Left => (
            2.0, 3.0, 4.0
          ),
          Right => (
            5.0, 6.0, 7.0
          ),
          Expected => (
            10.0, 18.0, 28.0  -- 2*5 =10, 3*6=18, 4*7=28
          )
        ),

        -- Component-wise multiplication with zero vector
        (
          Left => (
            0.0, 8.0, 0.0
          ),
          Right => (
            3.0, 0.0, 5.0
          ),
          Expected => (
            0.0, 0.0, 0.0  -- 0*3=0, 8*0=0, 0*5=0
          )
        ),

        -- Component-wise multiplication with negative components
        (
          Left => (
            -2.0, 4.0, -6.0
          ),
          Right => (
            3.0, -2.0, 5.0
          ),
          Expected => (
            -6.0, -8.0, -30.0  -- (-2)*3=-6, 4*(-2)=-8, (-6)*5=-30
          )
        ),

        -- Component-wise multiplication with floating-point numbers
        (
          Left => (
            1.5, 2.5, 3.5
          ),
          Right => (
            4.0, 5.0, 6.0
          ),
          Expected => (
            6.0, 12.5, 21.0  -- 1.5*4=6.0, 2.5*5=12.5, 3.5*6=21.0
          )
        )
      );

      Result : Vector_3D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Vector Vector_3D (Test " & I'Img & "):" & TAB_8);
        Result := TESTS(I).Left * TESTS(I).Right;

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Vector Vector_4D --
    -------------------------------

    declare
      type Test_State is record
        Left     : Vector_4D;
        Right    : Vector_4D;
        Expected : Vector_4D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Component-wise multiplication of positive vectors
        (
          Left => (
            2.0, 3.0, 4.0, 5.0
          ),
          Right => (
            6.0, 7.0, 8.0, 9.0
          ),
          Expected => (
            12.0, 21.0, 32.0, 45.0  -- 2*6=12, 3*7=21, 4*8=32, 5*9=45
          )
        ),

        -- Component-wise multiplication with zero vector
        (
          Left => (
            0.0, 8.0, 0.0, 10.0
          ),
          Right => (
            3.0, 0.0, 5.0, 0.0
          ),
          Expected => (
            0.0, 0.0, 0.0, 0.0  -- 0*3=0, 8*0=0, 0*5=0, 10*0=0
          )
        ),

        -- Component-wise multiplication with negative components
        (
          Left => (
            -1.0, 4.0, -3.0, 2.0
          ),
          Right => (
            5.0, -2.0, 6.0, -4.0
          ),
          Expected => (
            -5.0, -8.0, -18.0, -8.0  -- (-1)*5=-5, 4*(-2)=-8, (-3)*6=-18, 2*(-4)=-8
          )
        ),

        -- Component-wise multiplication with floating-point numbers
        (
          Left => (
            1.5, 2.5, 3.5, 4.5
          ),
          Right => (
            4.0, 5.0, 6.0, 7.0
          ),
          Expected => (
            6.0, 12.5, 21.0, 31.5  -- 1.5*4=6.0, 2.5*5=12.5, 3.5*6=21.0, 4.5*7=31.5
          )
        )
      );

      Result : Vector_4D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Vector Vector_4D (Test " & I'Img & "):" & TAB_8);
        Result := TESTS(I).Left * TESTS(I).Right;

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Scalar Vector_2D --
    -------------------------------

    declare
      type Test_State is record
        Left     : Vector_2D;
        Right    : Real;
        Expected : Vector_2D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Scalar multiplication with positive scalar
        (
          Left => (
            2.0, 3.0
          ),
          Right => 4.0,
          Expected => (
            8.0, 12.0  -- 2*4=8, 3*4=12
          )
        ),

        -- Scalar multiplication with zero scalar
        (
          Left => (
            5.0, -2.0
          ),
          Right => 0.0,
          Expected => (
            0.0, 0.0  -- 5*0=0, (-2)*0=0
          )
        ),

        -- Scalar multiplication with negative scalar
        (
          Left => (
            -1.0, 4.0
          ),
          Right => -3.0,
          Expected => (
            3.0, -12.0  -- (-1)*(-3)=3, 4*(-3)=-12
          )
        ),

        -- Scalar multiplication with floating-point scalar
        (
          Left => (
            1.5, 2.5
          ),
          Right => 2.0,
          Expected => (
            3.0, 5.0  -- 1.5*2=3.0, 2.5*2=5.0
          )
        )
      );

      Result : Vector_2D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Scalar Vector_2D (Test " & I'Img & "):" & TAB_8);
        Result := TESTS(I).Left * TESTS(I).Right;

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Scalar Vector_3D --
    -------------------------------

    declare
      type Test_State is record
        Left     : Vector_3D;
        Right    : Real;
        Expected : Vector_3D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Scalar multiplication with positive scalar
        (
          Left => (
            2.0, 3.0, 4.0
          ),
          Right => 5.0,
          Expected => (
            10.0, 15.0, 20.0  -- 2*5=10, 3*5=15, 4*5=20
          )
        ),

        -- Scalar multiplication with zero scalar
        (
          Left => (
            5.0, -2.0, 7.0
          ),
          Right => 0.0,
          Expected => (
            0.0, 0.0, 0.0  -- 5*0=0, (-2)*0=0, 7*0=0
          )
        ),

        -- Scalar multiplication with negative scalar
        (
          Left => (
            -1.0, 4.0, -3.0
          ),
          Right => -2.0,
          Expected => (
            2.0, -8.0, 6.0  -- (-1)*(-2)=2, 4*(-2)=-8, (-3)*(-2)=6
          )
        ),

        -- Scalar multiplication with floating-point scalar
        (
          Left => (
            1.5, 2.5, 3.5
          ),
          Right => 2.0,
          Expected => (
            3.0, 5.0, 7.0  -- 1.5*2=3.0, 2.5*2=5.0, 3.5*2=7.0
          )
        )
      );

      Result : Vector_3D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Scalar Vector_3D (Test " & I'Img & "):" & TAB_8);
        Result := TESTS(I).Left * TESTS(I).Right;

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Scalar Vector_4D --
    -------------------------------

    declare
      type Test_State is record
        Left     : Vector_4D;
        Right    : Real;
        Expected : Vector_4D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Scalar multiplication with positive scalar
        (
          Left => (
            2.0, 3.0, 4.0, 5.0
          ),
          Right => 6.0,
          Expected => (
            12.0, 18.0, 24.0, 30.0  -- 2*6=12, 3*6=18, 4*6=24, 5*6=30
          )
        ),

        -- Scalar multiplication with zero scalar
        (
          Left => (
            5.0, -2.0, 7.0, -3.0
          ),
          Right => 0.0,
          Expected => (
            0.0, 0.0, 0.0, 0.0  -- 5*0=0, (-2)*0=0, 7*0=0, (-3)*0=0
          )
        ),

        -- Scalar multiplication with negative scalar
        (
          Left => (
            -1.0, 4.0, -3.0, 2.0
          ),
          Right => -2.0,
          Expected => (
            2.0, -8.0, 6.0, -4.0  -- (-1)*(-2)=2, 4*(-2)=-8, (-3)*(-2)=6, 2*(-2)=-4
          )
        ),

        -- Scalar multiplication with floating-point scalar
        (
          Left => (
            1.5, 2.5, 3.5, 4.5
          ),
          Right => 2.0,
          Expected => (
            3.0, 5.0, 7.0, 9.0  -- 1.5*2=3.0, 2.5*2=5.0, 3.5*2=7.0, 4.5*2=9.0
          )
        )
      );

      Result : Vector_4D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Scalar Vector_4D (Test " & I'Img & "):" & TAB_8);
        Result := TESTS(I).Left * TESTS(I).Right;

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: ");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:   ");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Add Vector_2D --
    -------------------------------

    declare
      type Test_State is record
        M        : Vector_2D;
        X        : Vector_2D;
        A        : Vector_2D;
        Expected : Vector_2D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Multiply_Add with positive vectors
        (
          M => (
            2.0, 3.0
          ),
          X => (
            4.0, 5.0
          ),
          A => (
            1.0, 1.0
          ),
          Expected => (
            2.0 * 4.0 + 1.0, 3.0 * 5.0 + 1.0  -- (8.0 + 1.0, 15.0 + 1.0) = (9.0, 16.0)
          )
        ),

        -- Multiply_Add with zero vectors
        (
          M => (
            0.0, 0.0
          ),
          X => (
            5.0, 6.0
          ),
          A => (
            7.0, 8.0
          ),
          Expected => (
            0.0 * 5.0 + 7.0, 0.0 * 6.0 + 8.0  -- (0.0 + 7.0, 0.0 + 8.0) = (7.0, 8.0)
          )
        ),

        -- Multiply_Add with negative components
        (
          M => (
            -1.0, 4.0
          ),
          X => (
            3.0, -2.0
          ),
          A => (
            5.0, -3.0
          ),
          Expected => (
            (-1.0) * 3.0 + 5.0, 4.0 * (-2.0) + (-3.0)  -- (-3.0 + 5.0, -8.0 -3.0) = (2.0, -11.0)
          )
        ),

        -- Multiply_Add with floating-point numbers
        (
          M => (
            1.5, 2.5
          ),
          X => (
            2.0, 3.0
          ),
          A => (
            0.5, 1.0
          ),
          Expected => (
            1.5 * 2.0 + 0.5, 2.5 * 3.0 + 1.0  -- (3.0 + 0.5, 7.5 + 1.0) = (3.5, 8.5)
          )
        )
      );

      Result : Vector_2D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Add Vector_2D (Test " & I'Img & "):" & TAB_8);
        Result := Multiply_Add(TESTS(I).M, TESTS(I).X, TESTS(I).A);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Add Vector_3D --
    -------------------------------

    declare
      type Test_State is record
        M        : Vector_3D;
        X        : Vector_3D;
        A        : Vector_3D;
        Expected : Vector_3D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Multiply_Add with positive vectors
        (
          M => (
            2.0, 3.0, 4.0
          ),
          X => (
            5.0, 6.0, 7.0
          ),
          A => (
            1.0, 1.0, 1.0
          ),
          Expected => (
            2.0 * 5.0 + 1.0, 3.0 * 6.0 + 1.0, 4.0 * 7.0 + 1.0  -- (11.0, 19.0, 29.0)
          )
        ),

        -- Multiply_Add with zero vectors
        (
          M => (
            0.0, 8.0, 0.0
          ),
          X => (
            3.0, 0.0, 5.0
          ),
          A => (
            7.0, 8.0, 9.0
          ),
          Expected => (
            0.0 * 3.0 + 7.0, 8.0 * 0.0 + 8.0, 0.0 * 5.0 + 9.0  -- (7.0, 8.0, 9.0)
          )
        ),

        -- Multiply_Add with negative components
        (
          M => (
            -2.0, 4.0, -6.0
          ),
          X => (
            3.0, -2.0, 5.0
          ),
          A => (
            5.0, -3.0, 7.0
          ),
          Expected => (
            (-2.0) * 3.0 + 5.0, 4.0 * (-2.0) + (-3.0), (-6.0) * 5.0 + 7.0  -- (-1.0, -11.0, -23.0)
          )
        ),

        -- Multiply_Add with floating-point numbers
        (
          M => (
            1.5, 2.5, 3.5
          ),
          X => (
            2.0, 3.0, 4.0
          ),
          A => (
            0.5, 1.0, 1.5
          ),
          Expected => (
            1.5 * 2.0 + 0.5, 2.5 * 3.0 + 1.0, 3.5 * 4.0 + 1.5  -- (4.5, 8.5, 15.5)
          )
        )
      );

      Result : Vector_3D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Add Vector_3D (Test " & I'Img & "):" & TAB_8);
        Result := Multiply_Add(TESTS(I).M, TESTS(I).X, TESTS(I).A);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Add Vector_4D --
    -------------------------------

    declare
      type Test_State is record
        M        : Vector_4D;
        X        : Vector_4D;
        A        : Vector_4D;
        Expected : Vector_4D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Multiply_Add with positive vectors
        (
          M => (
            2.0, 3.0, 4.0, 5.0
          ),
          X => (
            6.0, 7.0, 8.0, 9.0
          ),
          A => (
            1.0, 1.0, 1.0, 1.0
          ),
          Expected => (
            2.0 * 6.0 + 1.0, 3.0 * 7.0 + 1.0, 4.0 * 8.0 + 1.0, 5.0 * 9.0 + 1.0  -- (13.0, 22.0, 33.0, 46.0)
          )
        ),

        -- Multiply_Add with zero vectors
        (
          M => (
            0.0, 8.0, 0.0, 10.0
          ),
          X => (
            3.0, 0.0, 5.0, 0.0
          ),
          A => (
            7.0, 8.0, 9.0, 10.0
          ),
          Expected => (
            0.0 * 3.0 + 7.0, 8.0 * 0.0 + 8.0, 0.0 * 5.0 + 9.0, 10.0 * 0.0 + 10.0  -- (7.0, 8.0, 9.0, 10.0)
          )
        ),

        -- Multiply_Add with negative components
        (
          M => (
            -1.0, 4.0, -3.0, 2.0
          ),
          X => (
            5.0, -2.0, 6.0, -4.0
          ),
          A => (
            3.0, -5.0, 7.0, -2.0
          ),
          Expected => (
            (-1.0) * 5.0 + 3.0, 4.0 * (-2.0) + (-5.0), (-3.0) * 6.0 + 7.0, 2.0 * (-4.0) + (-2.0)  -- (-2.0, -13.0, -11.0, -10.0)
          )
        ),

        -- Multiply_Add with floating-point numbers
        (
          M => (
            1.5, 2.5, 3.5, 4.5
          ),
          X => (
            2.0, 3.0, 4.0, 5.0
          ),
          A => (
            0.5, 1.0, 1.5, 2.0
          ),
          Expected => (
            1.5 * 2.0 + 0.5, 2.5 * 3.0 + 1.0, 3.5 * 4.0 + 1.5, 4.5 * 5.0 + 2.0  -- (3.5, 8.5, 15.5, 24.5)
          )
        )
      );

      Result : Vector_4D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Add Vector_4D (Test " & I'Img & "):" & TAB_8);
        Result := Multiply_Add(TESTS(I).M, TESTS(I).X, TESTS(I).A);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Subtract Vector_2D --
    -------------------------------

    declare
      type Test_State is record
        M        : Vector_2D;
        X        : Vector_2D;
        A        : Vector_2D;
        Expected : Vector_2D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Multiply_Subtract with positive vectors
        (
          M => (
            5.0, 6.0
          ),
          X => (
            2.0, 3.0
          ),
          A => (
            1.0, 1.0
          ),
          Expected => (
            5.0 * 2.0 - 1.0, 6.0 * 3.0 - 1.0  -- (10.0 - 1.0, 18.0 - 1.0) = (9.0, 17.0) 
          )
        ),

        -- Multiply_Subtract with zero vectors
        (
          M => (
            0.0, 8.0
          ),
          X => (
            3.0, 0.0
          ),
          A => (
            7.0, 8.0
          ),
          Expected => (
            0.0 * 3.0 - 7.0, 8.0 * 0.0 - 8.0  -- (0.0 - 7.0, 0.0 - 8.0) = (-7.0, -8.0)
          )
        ),

        -- Multiply_Subtract with negative components
        (
          M => (
            -1.0, 4.0
          ),
          X => (
            3.0, -2.0
          ),
          A => (
            5.0, -3.0
          ),
          Expected => (
            (-1.0) * 3.0 - 5.0, 4.0 * (-2.0) - (-3.0)  -- (-3.0 -5.0, -8.0 +3.0) = (-8.0, -5.0)
          )
        ),

        -- Multiply_Subtract with floating-point numbers
        (
          M => (
            1.5, 2.5
          ),
          X => (
            2.0, 3.0
          ),
          A => (
            0.5, 1.0
          ),
          Expected => (
            1.5 * 2.0 - 0.5, 2.5 * 3.0 - 1.0  -- (3.0 - 0.5, 7.5 -1.0) = (2.5, 6.5)
          )
        )
      );

      Result : Vector_2D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Subtract Vector_2D (Test " & I'Img & "):" & TAB_8);
        Result := Multiply_Subtract(TESTS(I).M, TESTS(I).X, TESTS(I).A);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Subtract Vector_3D --
    -------------------------------

    declare
      type Test_State is record
        M        : Vector_3D;
        X        : Vector_3D;
        A        : Vector_3D;
        Expected : Vector_3D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Multiply_Subtract with positive vectors
        (
          M => (
            5.0, 6.0, 7.0
          ),
          X => (
            2.0, 3.0, 4.0
          ),
          A => (
            1.0, 1.0, 1.0
          ),
          Expected => (
            5.0 * 2.0 - 1.0, 6.0 * 3.0 - 1.0, 7.0 * 4.0 - 1.0  -- (9.0, 17.0, 27.0)
          )
        ),

        -- Multiply_Subtract with zero vectors
        (
          M => (
            0.0, 8.0, 0.0
          ),
          X => (
            3.0, 0.0, 5.0
          ),
          A => (
            7.0, 8.0, 9.0
          ),
          Expected => (
            0.0 * 3.0 - 7.0, 8.0 * 0.0 - 8.0, 0.0 * 5.0 - 9.0  -- (-7.0, -8.0, -9.0)
          )
        ),

        -- Multiply_Subtract with negative components
        (
          M => (
            -2.0, 4.0, -6.0
          ),
          X => (
            3.0, -2.0, 5.0
          ),
          A => (
            5.0, -3.0, 7.0
          ),
          Expected => (
            (-2.0) * 3.0 - 5.0, 4.0 * (-2.0) - (-3.0), (-6.0) * 5.0 -7.0  -- (-11.0, -5.0, -37.0)
          )
        ),

        -- Multiply_Subtract with floating-point numbers
        (
          M => (
            1.5, 2.5, 3.5
          ),
          X => (
            2.0, 3.0, 4.0
          ),
          A => (
            0.5, 1.0, 1.5
          ),
          Expected => (
            1.5 * 2.0 - 0.5, 2.5 * 3.0 - 1.0, 3.5 * 4.0 - 1.5  -- (2.5, 6.5, 13.5)
          )
        )
      );

      Result : Vector_3D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Subtract Vector_3D (Test " & I'Img & "):" & TAB_8);
        Result := Multiply_Subtract(TESTS(I).M, TESTS(I).X, TESTS(I).A);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Multiply_Subtract Vector_4D --
    -------------------------------

    declare
      type Test_State is record
        M        : Vector_4D;
        X        : Vector_4D;
        A        : Vector_4D;
        Expected : Vector_4D;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Multiply_Subtract with positive vectors
        (
          M => (
            5.0, 6.0, 7.0, 8.0
          ),
          X => (
            2.0, 3.0, 4.0, 5.0
          ),
          A => (
            1.0, 1.0, 1.0, 1.0
          ),
          Expected => (
            5.0 * 2.0 - 1.0, 6.0 * 3.0 - 1.0, 7.0 * 4.0 - 1.0, 8.0 * 5.0 - 1.0  -- (9.0, 17.0, 27.0, 39.0)
          )
        ),

        -- Multiply_Subtract with zero vectors
        (
          M => (
            0.0, 8.0, 0.0, 10.0
          ),
          X => (
            3.0, 0.0, 5.0, 0.0
          ),
          A => (
            7.0, 8.0, 9.0, 10.0
          ),
          Expected => (
            0.0 * 3.0 - 7.0, 8.0 * 0.0 - 8.0, 0.0 * 5.0 - 9.0, 10.0 * 0.0 -10.0  -- (-7.0, -8.0, -9.0, -10.0)
          )
        ),

        -- Multiply_Subtract with negative components
        (
          M => (
            -1.0, 4.0, -3.0, 2.0
          ),
          X => (
            5.0, -2.0, 6.0, -4.0
          ),
          A => (
            3.0, -5.0, 7.0, -2.0
          ),
          Expected => (
            (-1.0) * 5.0 - 3.0, 4.0 * (-2.0) - (-5.0), (-3.0) * 6.0 -7.0, 2.0 * (-4.0) - (-2.0)  -- (-8.0, -3.0, -25.0, -6.0)
          )
        ),

        -- Multiply_Subtract with floating-point numbers
        (
          M => (
            1.5, 2.5, 3.5, 4.5
          ),
          X => (
            2.0, 3.0, 4.0, 5.0
          ),
          A => (
            0.5, 1.0, 1.5, 2.0
          ),
          Expected => (
            1.5 * 2.0 - 0.5, 2.5 * 3.0 - 1.0, 3.5 * 4.0 - 1.5, 4.5 * 5.0 - 2.0  -- (2.5, 6.5, 13.5, 20.5)
          )
        )
      );

      Result : Vector_4D;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Multiply_Subtract Vector_4D (Test " & I'Img & "):" & TAB_8);
        Result := Multiply_Subtract(TESTS(I).M, TESTS(I).X, TESTS(I).A);

        -- Compare the result with the expected vector
        if not Equal(TESTS(I).Expected, Result) then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected");
          Put_Line (TESTS(I).Expected);
          Put_Line ("Actual:");
          Put_Line (Result);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Sin Real --
    -------------------------------

    declare
      type Test_State is record
        Input    : Real;
        Expected : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Sin of 0 radians
        (
          Input => 0.0,
          Expected => 0.0  -- sin(0) = 0
        ),

        -- Sin of π/6 radians (30 degrees)
        (
          Input => 3.14159265358979323846 / 6.0,  -- ≈ 0.5235987755982988
          Expected => 0.5  -- sin(π/6) = 0.5
        ),

        -- Sin of π/4 radians (45 degrees)
        (
          Input => 3.14159265358979323846 / 4.0,  -- ≈ 0.7853981633974483
          Expected => 0.7071067811865475  -- sin(π/4) ≈ 0.7071067811865475
        ),

        -- Sin of π/3 radians (60 degrees)
        (
          Input => 3.14159265358979323846 / 3.0,  -- ≈ 1.0471975511965976
          Expected => 0.8660254037844386  -- sin(π/3) ≈ 0.8660254037844386
        ),

        -- Sin of π/2 radians (90 degrees)
        (
          Input => 3.14159265358979323846 / 2.0,  -- ≈ 1.5707963267948966
          Expected => 1.0  -- sin(π/2) = 1
        ),

        -- Sin of π radians (180 degrees)
        (
          Input => 3.14159265358979323846,  -- ≈ 3.141592653589793
          Expected => 0.0  -- sin(π) = 0
        ),

        -- Sin of 3π/2 radians (270 degrees)
        (
          Input => 3.0 * 3.14159265358979323846 / 2.0,  -- ≈ 4.71238898038469
          Expected => -1.0  -- sin(3π/2) = -1
        ),

        -- Sin of 2π radians (360 degrees)
        (
          Input => 2.0 * 3.14159265358979323846,  -- ≈ 6.283185307179586
          Expected => 0.0  -- sin(2π) = 0
        ),

        -- Sin of -π/2 radians (-90 degrees)
        (
          Input => -3.14159265358979323846 / 2.0,  -- ≈ -1.5707963267948966
          Expected => -1.0  -- sin(-π/2) = -1
        ),

        -- Sin of π/4 radians (45 degrees)
        (
          Input => 3.14159265358979323846 / 4.0,  -- ≈ 0.7853981633974483
          Expected => 0.7071067811865475  -- sin(π/4) ≈ 0.7071067811865475
        )
      );

      Result : Real;
      Tolerance : constant Real := 1.0E-10;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Sin Real (Test " & I'Img & "):" & TAB_8);
        Result := Sin(TESTS(I).Input);

        -- Compare the result with the expected value using tolerance
        if Abs(Result - TESTS(I).Expected) > Tolerance then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: " & TESTS(I).Expected'Img);
          Put_Line ("Actual:   " & Result'Img);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Cos Real --
    -------------------------------

    declare
      type Test_State is record
        Input    : Real;
        Expected : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Cos of 0 radians
        (
          Input => 0.0,
          Expected => 1.0  -- cos(0) = 1
        ),

        -- Cos of π/6 radians (30 degrees)
        (
          Input => 3.14159265358979323846 / 6.0,  -- ≈ 0.5235987755982988
          Expected => 0.8660254037844387  -- cos(π/6) ≈ 0.8660254037844387
        ),

        -- Cos of π/4 radians (45 degrees)
        (
          Input => 3.14159265358979323846 / 4.0,  -- ≈ 0.7853981633974483
          Expected => 0.7071067811865476  -- cos(π/4) ≈ 0.7071067811865476
        ),

        -- Cos of π/3 radians (60 degrees)
        (
          Input => 3.14159265358979323846 / 3.0,  -- ≈ 1.0471975511965976
          Expected => 0.5  -- cos(π/3) = 0.5
        ),

        -- Cos of π/2 radians (90 degrees)
        (
          Input => 3.14159265358979323846 / 2.0,  -- ≈ 1.5707963267948966
          Expected => 0.0  -- cos(π/2) = 0
        ),

        -- Cos of π radians (180 degrees)
        (
          Input => 3.14159265358979323846,  -- ≈ 3.141592653589793
          Expected => -1.0  -- cos(π) = -1
        ),

        -- Cos of 3π/2 radians (270 degrees)
        (
          Input => 3.0 * 3.14159265358979323846 / 2.0,  -- ≈ 4.71238898038469
          Expected => 0.0  -- cos(3π/2) = 0
        ),

        -- Cos of 2π radians (360 degrees)
        (
          Input => 2.0 * 3.14159265358979323846,  -- ≈ 6.283185307179586
          Expected => 1.0  -- cos(2π) = 1
        ),

        -- Cos of -π/2 radians (-90 degrees)
        (
          Input => -3.14159265358979323846 / 2.0,  -- ≈ -1.5707963267948966
          Expected => 0.0  -- cos(-π/2) = 0
        ),

        -- Cos of π/4 radians (45 degrees)
        (
          Input => 3.14159265358979323846 / 4.0,  -- ≈ 0.7853981633974483
          Expected => 0.7071067811865476  -- cos(π/4) ≈ 0.7071067811865476
        )
      );

      Result : Real;
      Tolerance : constant Real := 1.0E-10;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Cos Real (Test " & I'Img & "):" & TAB_8);
        Result := Cos(TESTS(I).Input);

        -- Compare the result with the expected value using tolerance
        if Abs(Result - TESTS(I).Expected) > Tolerance then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: " & TESTS(I).Expected'Img);
          Put_Line ("Actual:   " & Result'Img);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;
    -------------------------------
    -- Test Tan Real --
    -------------------------------

    declare
      type Test_State is record
        Input    : Real;
        Expected : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Tan of 0 radians
        (
          Input => 0.0,
          Expected => 0.0  -- tan(0) = 0
        ),

        -- Tan of π/6 radians (30 degrees)
        (
          Input => 3.14159265358979323846 / 6.0,  -- ≈ 0.5235987755982988
          Expected => 0.5773502691896257  -- tan(π/6) ≈ 0.5773502691896257
        ),

        -- Tan of π/4 radians (45 degrees)
        (
          Input => 3.14159265358979323846 / 4.0,  -- ≈ 0.7853981633974483
          Expected => 1.0  -- tan(π/4) = 1
        ),

        -- Tan of π/3 radians (60 degrees)
        (
          Input => 3.14159265358979323846 / 3.0,  -- ≈ 1.0471975511965976
          Expected => 1.7320508075688774  -- tan(π/3) ≈ 1.7320508075688774
        ),

        -- Tan of π/2 radians (90 degrees) -- Undefined (approaches infinity)
        (
          Input => 3.14159265358979323846 / 2.0,  -- ≈ 1.5707963267948966
          Expected => INFINITY  -- Approaches infinity; using a large number
        ),

        -- Tan of π radians (180 degrees)
        (
          Input => 3.14159265358979323846,  -- ≈ 3.141592653589793
          Expected => 0.0  -- tan(π) = 0
        ),

        -- Tan of 3π/2 radians (270 degrees) -- Undefined (approaches negative infinity)
        (
          Input => 3.0 * 3.14159265358979323846 / 2.0,  -- ≈ 4.71238898038469
          Expected => NEGATIVE_INFINITY  -- Approaches negative infinity; using a large negative number
        ),

        -- Tan of 2π radians (360 degrees)
        (
          Input => 2.0 * 3.14159265358979323846,  -- ≈ 6.283185307179586
          Expected => 0.0  -- tan(2π) = 0
        ),

        -- Tan of -π/4 radians (-45 degrees)
        (
          Input => -3.14159265358979323846 / 4.0,  -- ≈ -0.7853981633974483
          Expected => -1.0  -- tan(-π/4) = -1
        ),

        -- Tan of π/8 radians (22.5 degrees)
        (
          Input => 3.14159265358979323846 / 8.0,  -- ≈ 0.39269908169872414
          Expected => 0.41421356237309503  -- tan(π/8) ≈ 0.41421356237309503
        )
      );

      Result : Real;
      Tolerance : constant Real := 1.0E-7;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put ("Tan Real (Test " & I'Img & "):" & TAB_8);
        Result := Tan(TESTS(I).Input);

        -- Compare the result with the expected value using tolerance
        if Abs(Result - TESTS(I).Expected) > Tolerance then
          Put_Line ("FAIL");
          New_Line;
          Put_Line ("Expected: " & TESTS(I).Expected'Img);
          Put_Line ("Actual:   " & Result'Img);
        else
          Put_Line ("PASS");
        end if;
      end loop;
    end;

    -------------------------------
    -- Test Cos_Sin Procedure --
    -------------------------------

    declare
      type Test_State is record
        Input    : Real;
        Expected_Cos : Real;
        Expected_Sin : Real;
      end record;

      TESTS : constant array (Positive range <>) of Test_State := (
        -- Cos and Sin of 0 radians
        (
          Input => 0.0,
          Expected_Cos => 1.0,  -- cos(0) = 1
          Expected_Sin => 0.0   -- sin(0) = 0
        ),

        -- Cos and Sin of π/6 radians (30 degrees)
        (
          Input => 3.14159265358979323846 / 6.0,  -- ≈ 0.5235987755982988
          Expected_Cos => 0.8660254037844387,  -- cos(π/6) ≈ 0.8660254037844387
          Expected_Sin => 0.5                -- sin(π/6) = 0.5
        ),

        -- Cos and Sin of π/4 radians (45 degrees)
        (
          Input => 3.14159265358979323846 / 4.0,  -- ≈ 0.7853981633974483
          Expected_Cos => 0.7071067811865476,  -- cos(π/4) ≈ 0.7071067811865476
          Expected_Sin => 0.7071067811865475   -- sin(π/4) ≈ 0.7071067811865475
        ),

        -- Cos and Sin of π/3 radians (60 degrees)
        (
          Input => 3.14159265358979323846 / 3.0,  -- ≈ 1.0471975511965976
          Expected_Cos => 0.5,                -- cos(π/3) = 0.5
          Expected_Sin => 0.8660254037844386  -- sin(π/3) ≈ 0.8660254037844386
        ),

        -- Cos and Sin of π/2 radians (90 degrees)
        (
          Input => 3.14159265358979323846 / 2.0,  -- ≈ 1.5707963267948966
          Expected_Cos => 6.123233995736766e-17, -- cos(π/2) ≈ 0
          Expected_Sin => 1.0                   -- sin(π/2) = 1
        ),

        -- Cos and Sin of π radians (180 degrees)
        (
          Input => 3.14159265358979323846,  -- ≈ 3.141592653589793
          Expected_Cos => -1.0,              -- cos(π) = -1
          Expected_Sin => 1.2246467991473532e-16  -- sin(π) ≈ 0
        ),

        -- Cos and Sin of 3π/2 radians (270 degrees)
        (
          Input => 3.0 * 3.14159265358979323846 / 2.0,  -- ≈ 4.71238898038469
          Expected_Cos => -1.8369701987210297e-16,  -- cos(3π/2) ≈ 0
          Expected_Sin => -1.0                     -- sin(3π/2) = -1
        ),

        -- Cos and Sin of 2π radians (360 degrees)
        (
          Input => 2.0 * 3.14159265358979323846,  -- ≈ 6.283185307179586
          Expected_Cos => 1.0,                   -- cos(2π) = 1
          Expected_Sin => -2.4492935982947064e-16  -- sin(2π) ≈ 0
        ),

        -- Cos and Sin of -π/2 radians (-90 degrees)
        (
          Input => -3.14159265358979323846 / 2.0,  -- ≈ -1.5707963267948966
          Expected_Cos => 6.123233995736766e-17,  -- cos(-π/2) ≈ 0
          Expected_Sin => -1.0                   -- sin(-π/2) = -1
        ),

        -- Cos and Sin of π/8 radians (22.5 degrees)
        (
          Input => 3.14159265358979323846 / 8.0,  -- ≈ 0.39269908169872414
          Expected_Cos => 0.9238795325112867,    -- cos(π/8) ≈ 0.9238795325112867
          Expected_Sin => 0.3826834323650898     -- sin(π/8) ≈ 0.3826834323650898
        )
      );

      Result_Cos : Real;
      Result_Sin : Real;
      Tolerance : constant Real := 1.0E-7;
    begin
      for I in TESTS'Range loop
        Put_Separator;
        Put_Line ("Cos_Sin Procedure (Test " & I'Img & "):" & TAB_8);
        Cos_Sin(Result_Cos, Result_Sin, TESTS(I).Input);

        -- Compare the cosine result with the expected value using tolerance
        if Abs(Result_Cos - TESTS(I).Expected_Cos) > Tolerance then
          Put_Line ("FAIL (Cos)");
          New_Line;
          Put_Line ("Expected Cos: " & TESTS(I).Expected_Cos'Img);
          Put_Line ("Actual Cos:   " & Result_Cos'Img);
        else
          Put_Line ("PASS (Cos)");
        end if;

        -- Compare the sine result with the expected value using tolerance
        if Abs(Result_Sin - TESTS(I).Expected_Sin) > Tolerance then
          Put_Line ("FAIL (Sin)");
          New_Line;
          Put_Line ("Expected Sin: " & TESTS(I).Expected_Sin'Img);
          Put_Line ("Actual Sin:   " & Result_Sin'Img);
        else
          Put_Line ("PASS (Sin)");
        end if;
      end loop;
    end;

  end;
