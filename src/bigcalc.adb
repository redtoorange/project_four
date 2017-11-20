with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Ada.Exceptions;      use Ada.Exceptions;
with BigNumPkg.Signed;    use BigNumPkg.Signed;
with StackPkg;

procedure bigcalc is
   Stack_Size    : constant        := 100;
   Error_Message : constant String := "Error: Stack is empty";
   OP_Chars      : constant String := "+-*pPqe";

   type Operator is (ADD, SUB, MUL, PRINT, POP, EMPTY);
   Operator_Exception : exception;

   package Signed_BigNum_Stack is new StackPkg (Stack_Size, Signed_BigNum);
   use Signed_BigNum_Stack;

   ----------------------------------------------------------
   -- Purpose: Empty the contents of a Stack, printing each item
   -- Parameters: s: Stack to print and clear
   ----------------------------------------------------------
   procedure Print_Clear_Stack (s : in out Stack) is
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
   end Print_Clear_Stack;

   ----------------------------------------------------------
   -- Purpose: Pop two operands off the Stack and store them in A
   --    and B, if the pop fails, the Stack remains unchanged.
   -- Parameters: a, b: Operand A and B from the Stack
   --                s: Stack to pull operands from
   ----------------------------------------------------------
   procedure Get_Operands (A, B : out Signed_BigNum; S : in out Stack) is

   begin
      -- Try to grab Operand A
      if not isEmpty (S)then
         A := top (S);
         pop (S);
      else
         raise Stack_Empty with "Error: Stack is empty";
      end if;

      -- Try to grab Operand B
      if not isEmpty (S) then
         B := top (S);
         pop (S);
      else
         -- No second Operand, push A back on the stack
		 push (A, S);    
		 raise Stack_Empty with "Error: Stack is empty";
      end if;
   end Get_Operands;

   ----------------------------------------------------------
   -- Purpose: Performs multiplication by repeated addition
   -- Parameters: x, y: values to multiply
   -- Precondition: x <= y
   -- Postcondition: Returns product of x and y
   ----------------------------------------------------------
   procedure Math_Operation (op : in Operator; S : in out Stack) is
      A, B : Signed_BigNum;
   begin
      Get_Operands (A, B, S);

      if op = ADD then
         A := A + B;
      elsif op = SUB then
         A := B - A;
      elsif op = MUL then
         A := A * B;
      end if;

      push (A, S);
   end Math_Operation;

   ----------------------------------------------------------
   -- Purpose: Performs multiplication by repeated addition
   -- Parameters: x, y: values to multiply
   -- Precondition: x <= y
   -- Postcondition: Returns product of x and y
   ----------------------------------------------------------
   procedure Perform_Operation (op : in Operator; S : in out Stack) is
   begin
      case op is
         when SUB | ADD | MUL =>
            Math_Operation (op, S);
         when PRINT =>
            Put (top (S));
            New_Line;
         when POP =>
            pop (S);
         when EMPTY =>
            Print_Clear_Stack (S);
      end case;
   end Perform_Operation;

   ----------------------------------------------------------
   -- Purpose: Performs multiplication by repeated addition
   -- Parameters: x, y: values to multiply
   -- Precondition: x <= y
   -- Postcondition: Returns product of x and y
   ----------------------------------------------------------
   procedure Get (op : out Operator) is
      C : Character;
   begin
      Get (C);

      case C is
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
            raise Operator_Exception with "Unknown Operator <" & C & ">";
      end case;
   end Get;

   ----------------------------------------------------------
   -- Purpose: Check if a Character should be treated as an operator
   -- Parameters: c: Character to parse
   -- Returns: True if c in "+-*pPq"
   ----------------------------------------------------------
   function Is_Operator
     (C : in Character) return Boolean is
     (Index (OP_Chars, "" & C) > 0);

   ----------------------------------------------------------
   -- Purpose: Check if a Character should be treated as a number
   -- Parameters: c: Character to parse
   -- Returns: True if c is in '0'..'9' or '_'
   ----------------------------------------------------------
   function Is_Number
     (C : in Character) return Boolean is
     (C in '0' .. '9' or C = '_');

   ----------------------------------------------------------
   -- Purpose: Get the next Character, if it's not a space, log an
   --    error to the user and toss the Character.
   ----------------------------------------------------------
   procedure Handle_Other is
      letter : Character;
   begin
      Get (letter);

      -- Got something other than a space, log error.
      if letter /= ' ' then
         raise Operator_Exception with "Unknown input <" & letter & ">";
      end if;
   end Handle_Other;

   ----------------------------------------------------------
   -- Purpose: Read in the next Signed_BigNum and push it on the Stack
   -- Parameters: s: Stack to push input onto.
   ----------------------------------------------------------
   procedure Handle_Number (S : out Stack) is
      bn : Signed_BigNum;
   begin
      Get (bn);
      push (bn, S);
   end Handle_Number;

   ----------------------------------------------------------
   -- Purpose: Read in the next Operator and perform it's action.
   -- Parameters: s: Stack to perform operations on.
   ----------------------------------------------------------
   procedure Handle_Operator (S : out Stack) is
      op : Operator;
   begin
      Get (op);
      Perform_Operation (op, S);
   end Handle_Operator;

   ----------------------------------------------------------
   -- Purpose: Process input Character and route execution accordingly.
   -- Parameters: c: Character to parse for execution flow.
   --             s: Stack to preform operations on.
   ----------------------------------------------------------
   procedure Process_Input (C : in Character; S : in out Stack) is
   begin
      if Is_Number (C) then
         Handle_Number (S);
      elsif Is_Operator (C) then
         Handle_Operator (S);
      else
         Handle_Other;
      end if;

   exception
      when E : Stack_Empty | Signed_BigNumOverFlow | Operator_Exception =>
         Put_Line (Exception_Message(E));
   end Process_Input;

   ----------------------------------------------------------
   -- Purpose: Continue taking user input until 'q' is recieved.
   -- Parameters: s: values to multiply
   ----------------------------------------------------------
   procedure Input_Loop (S : in out Stack) is
      letter : Character;    -- Next Character of input
      nl     : Boolean;      -- Was a newline recieved
   begin
      loop
         -- Grab the next Character
         Look_Ahead (letter, nl);
         exit when letter = 'q';

         if nl or letter = '#' then
            Skip_Line;
         else
            -- Send the character for processing
            Process_Input (letter, S);
         end if;

      end loop;
   end Input_Loop;

   -- Entry point
   numStack : Stack;
begin
   Input_Loop (numStack);
end bigcalc;
