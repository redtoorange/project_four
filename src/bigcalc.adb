with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with StackPkg;
with BigNumPkg.Signed; use BigNumPkg.Signed;

procedure bigcalc is
   OP_Chars : constant String := "+-*pPq";
   type Operator is (ADD, SUB, MUL, PRINT, POP, QUIT, NOP);
   package Operator_IO is new Enumeration_IO (Operator);

   type Input_Type is (NUM, OP, OTHER);

   package BigNumStack is new StackPkg (100, Signed_BigNum);
   use BigNumStack;

   -- Grab two operands off the stack, returning True if two were obtained,
   --  False otherwise.
   function Get_Operands
     (a, B :    out Signed_BigNum;
      s    : in out BigNumStack.Stack) return Boolean
   is
      success : Boolean := True;
   begin
      -- Grab Operand A
      if not isEmpty (s) then
         a := top (s);
         pop (s);
      else
         success := False;
      end if;

      -- Grab Operand B
      if success and then not isEmpty (s) then
         B := top (s);
         pop (s);
      else
         -- No second Operand, push A back on
         push (a, s);
         success := False;
      end if;

      return success;
   end Get_Operands;

   procedure Math_Operator (op : in Operator; s : in out BigNumStack.Stack) is
      a, b : Signed_BigNum;
   begin

      case op is
         when SUB =>
            if Get_Operands (a, b, s) then
               a := a + b;  -- This should be a minus sign
               push (a, s);
            end if;
         when ADD =>
            if Get_Operands (a, b, s) then
               a := a + b;
               push (a, s);
            end if;
         when MUL =>
            if Get_Operands (a, b, s) then
               a := a * b;
               push (a, s);
            end if;
         when others =>
            Operator_IO.Put (op);
            Put_Line (" is not a math operator!");
      end case;
   end Math_Operator;

   procedure Apply_Operator (op : in Operator; s : in out BigNumStack.Stack) is
   begin
      case op is
         when SUB | ADD | MUL =>
            Math_Operator (op, s);
         when PRINT =>
            Put (top (s));
            New_Line;
         when POP =>
            pop (s);
         when QUIT =>
            null;
         when NOP =>
            null;
      end case;
   end Apply_Operator;

   procedure Get (op : out Operator) is
      c : Character;
   begin
      Get (c);

      case c is
         when '-' =>
            op := SUB;
         when '+' =>
            op := ADD;
         when '*' =>
            op := MUL;
         when 'p' =>
            op := PRINT;
         when 'P' =>
            op := POP;
         when 'q' =>
            op := QUIT;
         when others =>
            op := NOP;
      end case;
   end Get;

   function Is_Operator (c : in Character) return Boolean is
   begin
      for k of OP_Chars loop
         if k = c then
            return True;
         end if;
      end loop;

      return False;
   end Is_Operator;

   function Is_Number (c : in Character) return Boolean is
   begin
      return
        (Character'Pos (c) >= Character'Pos ('0')
         and then Character'Pos (c) <= Character'Pos ('9'))
        or else c = '_';
   end Is_Number;

   function Parse_Char (c : in Character) return Input_Type is
      result : Input_Type := OTHER;
   begin
      if Is_Number (c) then
         result := NUM;
      elsif Is_Operator (c) then
         result := OP;
      end if;

      return result;
   end Parse_Char;

--     numStack : BigNumStack.Stack;
begin
   --     declare
   --        --line : String := Get_Line;
   --        ch    : Character;
   --        nl    : Boolean := False;
   --        bn    : Signed_BigNum;
   --        op    : Operator;
   --        trash : Character;
   --     begin
   --        Main_Loop :
   --        loop
   --           Look_Ahead (Item => ch, End_Of_Line => nl);
   --
   --           if not nl then
   --              if ch = 'q' then
   --                 Put_Line ("Quitting");
   --                 exit Main_Loop;
   --              elsif Is_Operator (ch) then
   --                 Get (op);
   --                 Put ("Found an Operator <");
   --                 Operator_IO.Put (op);
   --                 Put_Line (">");
   --
   --                 Apply_Operator (op, numStack);
   --              elsif Is_Number (ch) then
   --                 Get (bn);
   --                 Put ("Found a Number <");
   --                 Put (bn);
   --                 Put_Line (">");
   --                 push (bn, numStack);
   --              else
   --                 Get (trash);
   --                 if trash /= ' ' then
   --                    Put ("Found something else <");
   --                    Put (trash);
   --                    Put_Line (">");
   --                 end if;
   --
   --              end if;
   --           elsif nl then
   --              Skip_Line;
   --           end if;
   --        end loop Main_Loop;
   --
   --        declare
   --           index : Integer := 0;
   --        begin
   --           while not isEmpty (numStack) loop
   --              declare
   --                 current : Signed_BigNum := top (numStack);
   --              begin
   --                 BigNumStack.pop (numStack);
   --                 Put (index, 0);
   --                 Put (": <");
   --                 Put (current);
   --                 Put_Line (">");
   --                 index := index + 1;
   --              end;
   --
   --           end loop;
   --        end;
   declare
      a, b, c : Signed_BigNum;
      result  : Boolean;
   begin
      a := BigNumPkg.Signed.Minus_One;
      b := BigNumPkg.Signed.Minus_One;

      if a < b then
         Put (a);
         Put (" < ");
         Put (b);
      else
         Put (a);
         Put (" !< ");
         Put (b);
      end if;

      New_Line;

      if a > b then
         Put (a);
         Put (" > ");
         Put (b);
      else
         Put (a);
         Put (" !> ");
         Put (b);
      end if;

      New_Line;

      if a <= b then
         Put (a);
         Put (" <= ");
         Put (b);
      else
         Put (a);
         Put (" !<= ");
         Put (b);
      end if;

      New_Line;

      if a >= b then
         Put (a);
         Put (" >= ");
         Put (b);
      else
         Put (a);
         Put (" !>= ");
         Put (b);
      end if;

      New_Line;

      if a = b then
         Put (a);
         Put (" = ");
         Put (b);
      else
         Put (a);
         Put (" != ");
         Put (b);
      end if;

      New_Line;

   end;

end bigcalc;
