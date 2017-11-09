with StackPkg;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure bigcalc is
   package IntStack is new StackPkg (10, Integer);
   use IntStack;

   myStack : IntStack.Stack;
   temp    : Integer;

begin
   Put_Line ("Pushing 10");
   push (10, myStack);

   Put_Line ("Pushing 11");
   push (11, myStack);

   Put ("Top: ");
   temp := top (myStack);
   Put_Line (temp'Image);

   Put_Line ("Popping...");
   pop (myStack);

   Put ("Top: ");
   temp := top (myStack);
   Put_Line (temp'Image);
   New_Line;

   Put_Line ("Popping...");
   pop (myStack);

   begin
      Put ("Top: ");
      temp := top (myStack);
      Put_Line (temp'Image);
      New_Line;
   exception
      when Stack_Empty =>
         Put_Line ("Caught Stack Empty correctly...");
   end;

   begin
      Put_Line ("Popping...");
      pop (myStack);
   exception
      when Stack_Empty =>
         Put_Line ("Caught Stack Empty correctly...");
   end;

   begin
      for i in 1 .. 11 loop
         Put_Line ("Pushing: " & i'Image);
         push (i, myStack);
         Put_Line ("Pushed: " & i'Image);
      end loop;
   exception
      when Stack_Full =>
         Put_Line ("Caught Stack Full correctly...");

   end;

end bigcalc;
