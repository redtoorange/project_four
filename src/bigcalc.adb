with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with StackPkg;
with BigNumPkg.Signed; use BigNumPkg.Signed;

procedure bigcalc is
   Error_Message : constant String := "Error: Stack is empty";
   OP_Chars      : constant String := "+-*pPq";
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
               a := b - a;
               push (a, s);
            else
               Put_Line (Error_Message);
            end if;
         when ADD =>
            if Get_Operands (a, b, s) then
               a := a + b;
               push (a, s);
            else
               Put_Line (Error_Message);
            end if;
         when MUL =>
            if Get_Operands (a, b, s) then
               a := a * b;
               push (a, s);
            else
               Put_Line (Error_Message);
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
            if not isEmpty (s) then
               Put (top (s));
               New_Line;
            else
               Put_Line (Error_Message);
            end if;

         when POP =>
            if not isEmpty (s) then
               pop (s);
            else
               Put_Line (Error_Message);
            end if;
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

   procedure Print_Stack (s : in out BigNumStack.Stack) is
      index   : Integer := 0;
      current : Signed_BigNum;
   begin
      while not isEmpty (s) loop
         current := top (s);
         pop (s);
         Put (index, 0);
         Put (": <");
         Put (current);
         Put_Line (">");
         index := index + 1;
      end loop;
   end Print_Stack;

   procedure Input_Loop (s : in out BigNumStack.Stack) is
      letter : Character;
      nl     : Boolean;
      bn     : Signed_BigNum;
      op     : Operator;
   begin
      loop
         Look_Ahead (letter, nl);
         exit when letter = 'q';

         if not nl then
            if Is_Operator (letter) then
               Get (op);
               Apply_Operator (op, s);
            elsif Is_Number (letter) then
               Get (bn);
               push (bn, s);
            else
               Get (letter);
               if letter /= ' ' then
                  Put ("Unknown input <");
                  Put (letter);
                  Put_Line ("> skipping...");
               end if;
            end if;
         elsif nl then
            Skip_Line;
         end if;
      end loop;
   end Input_Loop;

   numStack : BigNumStack.Stack;
begin
   Input_Loop (numStack);
   Print_Stack (numStack);
end bigcalc;
