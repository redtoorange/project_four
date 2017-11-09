package body StackPkg is

   -- Determine if stack is empty or full
   function isEmpty (s : Stack) return Boolean is
   begin
      return s.Top = 0;
   end isEmpty;

   function isFull (s : Stack) return Boolean is
   begin
      return s.Top = Size;
   end isFull;

   -- Put element Item onto Stack s
   procedure push (item : ItemType; s : in out Stack) is
   begin
      if isFull (s) then
         raise Stack_Full;
      end if;

      s.Top              := s.Top + 1;
      s.Elements (s.Top) := item;
   end push;

   -- Remove an element from Stack s
   procedure pop (s : in out Stack) is
   begin
      if isEmpty (s) then
         raise Stack_Empty;
      end if;

      s.Top := s.Top - 1;
   end pop;

   -- Return top element from Stack s
   function top (s : Stack) return ItemType is
   begin
      if isEmpty (s) then
         raise Stack_Empty;
      end if;

      return s.Elements (s.Top);
   end top;
end StackPkg;
