package body StackPkg is
   ----------------------------------------------------------
   -- Purpose: Determine if stack is empty
   ----------------------------------------------------------
   function isEmpty (S : Stack) return Boolean is (S.Top = 0);

   ----------------------------------------------------------
   -- Purpose: Determine if stack is full
   ----------------------------------------------------------
   function isFull (S : Stack) return Boolean is (S.Top = Size);

   ----------------------------------------------------------
   -- Purpose: Return top element from Stack s
   ----------------------------------------------------------
   function top (S : Stack) return ItemType is
     (if isEmpty (S) then raise Stack_Empty else s.Elements (S.Top));
   
   ----------------------------------------------------------
   -- Purpose: Put element Item onto Stack s
   ----------------------------------------------------------
   procedure push (item : ItemType; S : in out Stack) is
   begin
      if isFull (S) then
         raise Stack_Full;
      else
         S.Top              := S.Top + 1;
         S.Elements (S.Top) := item;
      end if;
   end push;
  
   ----------------------------------------------------------
   -- Purpose: Remove an element from Stack s
   ----------------------------------------------------------
   procedure pop (S : in out Stack) is
   begin
      if isEmpty (S) then
         raise Stack_Empty;
      else
         S.Top := S.Top - 1;
      end if;
   end pop;
end StackPkg;
