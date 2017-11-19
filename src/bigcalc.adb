with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with StackPkg;
with BigNumPkg.Signed;    use BigNumPkg.Signed;

procedure bigcalc is
   Error_Message : constant String := "Error: Stack is empty";
   OP_Chars      : constant String := "+-*pPqe";

   type Operator is (ADD, SUB, MUL, PRINT, POP, EMPTY);
   Operator_Exception : exception;

   package Signed_BigNum_Stack is new StackPkg (100, Signed_BigNum);
   use Signed_BigNum_Stack;

   ----------------------------------------------------------
   -- Purpose: Performs multiplication by repeated addition
   -- Parameters: x, y: values to multiply
   -- Precondition: x <= y
   -- Postcondition: Returns product of x and y
   ----------------------------------------------------------
   procedure Print_Stack (s : in out Stack) is
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

   ----------------------------------------------------------
   -- Purpose: Pop two operands from the Stack and store in A
   --    and B, if the pop fails, the Stack remains unchanged.
   -- Parameters: a, b: Operand A and B from the Stack
   --                s: Stack to pull operands from
   ----------------------------------------------------------
   procedure Get_Operands (a, B : out Signed_BigNum; s : in out Stack) is

   begin
      -- Grab Operand A
      if not isEmpty (s) then
         a := top (s);
         pop (s);
      else
         raise Stack_Empty;
      end if;

      -- Grab Operand B
      if not isEmpty (s) then
         B := top (s);
         pop (s);
      else
         push (a, s);    -- No second Operand, push A back on
         raise Stack_Empty;
      end if;

   end Get_Operands;

   ----------------------------------------------------------
   -- Purpose: Performs multiplication by repeated addition
   -- Parameters: x, y: values to multiply
   -- Precondition: x <= y
   -- Postcondition: Returns product of x and y
   ----------------------------------------------------------
   procedure Math_Operator (op : in Operator; s : in out Stack) is
      a, b : Signed_BigNum;
   begin
      Get_Operands (a, b, s);

      if op = ADD then
         a := a + b;
      elsif op = SUB then
         a := b - a;
      elsif op = MUL then
         a := a * b;
      end if;

      push (a, s);
   end Math_Operator;

   ----------------------------------------------------------
   -- Purpose: Performs multiplication by repeated addition
   -- Parameters: x, y: values to multiply
   -- Precondition: x <= y
   -- Postcondition: Returns product of x and y
   ----------------------------------------------------------
   procedure Apply_Operator (op : in Operator; s : in out Stack) is
   begin
      case op is
         when SUB | ADD | MUL =>
            Math_Operator (op, s);
         when PRINT =>
            Put (top (s));
            New_Line;
         when POP =>
            pop (s);
         when EMPTY =>
            Print_Stack (s);
      end case;

   exception
      when Stack_Empty =>
         Put_Line (Error_Message);
   end Apply_Operator;

   ----------------------------------------------------------
   -- Purpose: Performs multiplication by repeated addition
   -- Parameters: x, y: values to multiply
   -- Precondition: x <= y
   -- Postcondition: Returns product of x and y
   ----------------------------------------------------------
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
         when 'e' =>
            op := EMPTY;
         when others =>
            raise Operator_Exception;
      end case;
   end Get;

   ----------------------------------------------------------
   -- Purpose: Check if a Character should be treated as an operator
   -- Parameters: c: Character to parse
   -- Returns: True if c in "+-*pPq"
   ----------------------------------------------------------
   function Is_Operator
     (c : in Character) return Boolean is
     (Index (OP_Chars, "" & c) > 0);

   ----------------------------------------------------------
   -- Purpose: Check if a Character should be treated as a number
   -- Parameters: c: Character to parse
   -- Returns: True if c is in '0'..'9' or '_'
   ----------------------------------------------------------
   function Is_Number
     (c : in Character) return Boolean is
     (c in '0' .. '9' or c = '_');

   ----------------------------------------------------------
   -- Purpose: Performs multiplication by repeated addition
   -- Parameters: x, y: values to multiply
   -- Precondition: x <= y
   -- Postcondition: Returns product of x and y
   ----------------------------------------------------------
   procedure Handle_Other is
      letter : Character;
   begin
      Get (letter);

      -- Got something other than a space, log error.
      if letter /= ' ' then
         Put ("Unknown input <");
         Put (letter);
         Put_Line ("> skipping...");
      end if;
   end Handle_Other;

   ----------------------------------------------------------
   -- Purpose: Performs multiplication by repeated addition
   -- Parameters: x, y: values to multiply
   -- Precondition: x <= y
   -- Postcondition: Returns product of x and y
   ----------------------------------------------------------
   procedure Handle_Number (s : out Stack) is
      bn : Signed_BigNum;
   begin
      Get (bn);
      push (bn, s);
   end Handle_Number;

   ----------------------------------------------------------
   -- Purpose: Performs multiplication by repeated addition
   -- Parameters: x, y: values to multiply
   -- Precondition: x <= y
   -- Postcondition: Returns product of x and y
   ----------------------------------------------------------
   procedure Handle_Operator (s : out Stack) is
      oper : Operator;
   begin
      Get (oper);
      Apply_Operator (oper, s);
   end Handle_Operator;

   ----------------------------------------------------------
   -- Purpose: Parse input Character and route execution according to it's
   --    Type.
   -- Parameters: c: Character to parse for execution flow.
   --             s: Stack to preform operations on.
   ----------------------------------------------------------
   procedure Parse_Char (c : in Character; s : in out Stack) is
   begin
      if Is_Number (c) then
         Handle_Number (s);
      elsif Is_Operator (c) then
         Handle_Operator (s);
      else
         Handle_Other;
      end if;
   end Parse_Char;

   ----------------------------------------------------------
   -- Purpose: Continue taking user input until 'q' is recieved.
   -- Parameters: s: values to multiply
   ----------------------------------------------------------
   procedure Input_Loop (s : in out Stack) is
      letter : Character;    -- Next Character of input
      nl     : Boolean;      -- Was a newline recieved
   begin
      loop
         Look_Ahead (letter, nl);  -- Grab the next Character
         exit when letter = 'q';   -- If it's q, just Quit

         if nl then
            Skip_Line;
         else
            Parse_Char (letter, s);
         end if;

      end loop;
   end Input_Loop;

   -- Entry point
   numStack : Stack;
begin
   Input_Loop (numStack);
   Print_Stack (numStack);
end bigcalc;
