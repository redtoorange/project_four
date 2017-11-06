generic  -- Generic parameters are declared here

   Size : Positive;            -- Size of stack to create

   type ItemType is private;   -- Type of elements that stack will contain

package body StackPkg is

-- Determine if stack is empty or full
   function isEmpty (s : Stack) return Boolean
   is
   begin
      null;
   end isEmpty;

   function isFull  (s : Stack) return Boolean
   is
   begin
      null;
   end isFull;

   -- Put element Item onto Stack s
   procedure push (item : ItemType; s : in out Stack)
   is
   begin
      null;
   end push;


   -- Remove an element from Stack s
   procedure pop  (s : in out Stack)
   is
   begin
      null;
   end pop;

   -- Return top element from Stack s
   function  top   (s : Stack) return ItemType
   is
   begin
      null;
   end top;
end StackPkg;
