with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with StackPkg;
with BigNumPkg; use BigNumPkg;


procedure bigcalc is
   type Input_Type is (NUMBER, OPERATOR, SPACE, OTHER);

   package BigNumStack is new StackPkg(100, BigNum);
   use BigNumStack;

   function Is_Operator(c : in Character) return Boolean
   is
   begin
      if c = '+' or else c = '-' or else c ='*' then
         return True;
      else
         return False;
      end if;

   end Is_Operator;

   function Is_Number(c : in Character) return Boolean
   is
   begin
      return Character'Pos(c) >= Character'Pos('0')
        and then
          Character'Pos(c) <= Character'Pos('9');
   end Is_Number;


   function Parse_Char(c : in Character) return Input_Type
   is
      result : Input_Type := OTHER;
   begin
      if Is_Number( c ) then
         result := NUMBER;
      elsif Is_Operator( c ) then
         result := OPERATOR;
      elsif c = ' ' then
         result := SPACE;
      end if;

      return result;
   end Parse_Char;

   --
   --     num      : BigNum;
   --     numStack : BigNumStack.Stack;
begin
   declare
      --line : String := Get_Line;
      ch : Character;
      nl : Boolean := False;
      bn : BigNum;
      op : Character;
      trash : Character;
   begin
      Main_Loop:
      loop
         Look_Ahead(Item        => ch,
                    End_Of_Line => nl);

         if not nl then
            if ch = 'q' then
               Put_Line("Quitting");
               exit Main_Loop;
            elsif Is_Operator(ch) then
               Get(op);
               Put("Found an Operator <"); Put(op); Put_Line(">");
            elsif Is_Number(ch) then
               Get(bn);
               Put("Found a Number <"); Put(bn); Put_Line(">");
            else
               Get(trash);
               Put("Found something else <"); Put(trash); Put_Line(">");
            end if;
         elsif nl then
            Skip_Line;
         end if;
      end loop Main_Loop;


   end;

   --     Put("Enter a BigNum: ");
   --     Get( num );
   --
   --     New_Line;
   --     Put("Pushing <"); Put(num); Put_Line("> on to stack.");
   --     BigNumStack.push( num, numStack);
   --     New_Line;
   --     Put("Top of stack: ");
   --     Put(top( numStack) );

end bigcalc;
