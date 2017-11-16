with Ada.Text_IO;
package body BigNumPkg.Signed is

   function Is_Negative
     (X : Signed_BigNum) return Boolean is
     (BigNum (X) >= BigNum (first));

   --  Removes leading zeros
   function toString (X : Signed_BigNum) return String is
      output : String := "Hello";
   begin
      return output;
   end toString;

   function "<" (X, Y : Signed_BigNum) return Boolean is
   begin
      if Is_Negative (X) and not Is_Negative (Y) then
         return True;
      elsif not Is_Negative (X) and Is_Negative (Y) then
         return False;
      elsif Is_Negative (X) and Is_Negative (Y) then
         return BigNum (X) > BigNum (Y);
      else
         return BigNum (X) < BigNum (Y);
      end if;
   end "<";

   function ">"
     (X, Y : Signed_BigNum) return Boolean is
     (not (X < Y or X = Y));

   function "<=" (X, Y : Signed_BigNum) return Boolean is (X < Y or X = Y);

   function ">=" (X, Y : Signed_BigNum) return Boolean is (X > Y or X = Y);

   --  Returns the negative of X
   --  N.B. the smallest negative cannot be negated
   function negate (X : Signed_BigNum) return Signed_BigNum is
      Y : Signed_BigNum;
   begin
      if X = first then
         raise Signed_BigNumOverFlow;
      end if;

      for I in reverse 0 .. Size - 1 loop
         Y (I) := 9 - X (I);
      end loop;

      Y := Y + One;

      return Y;
   end negate;

   --  Return absolute value of X
   --  N.B. absolute value of the smallest negative cannot be represented
   function abs_val (X : Signed_BigNum) return Signed_BigNum is
      Y : Signed_BigNum;
   begin
      if X = first then
         raise Signed_BigNumOverFlow;
      elsif Is_Negative (X) then
         Y := negate (X);
      end if;

      return Y;
   end abs_val;

   function "+" (X, Y : Signed_BigNum) return Signed_BigNum is
      overflow : Boolean;
      result   : BigNum;
   begin
      plus_ov (BigNum (X), BigNum (Y), result, overflow);

      if X > first and Y > first and overflow then
         raise Signed_BigNumOverFlow;
      end if;

      return Signed_BigNum (result);
   end "+";

   function "-" (X, Y : Signed_BigNum) return Signed_BigNum is
   begin
      return X;
   end "-";

   function "*" (X, Y : Signed_BigNum) return Signed_BigNum is
   begin
      return X;
   end "*";

   procedure Put (Item : Signed_BigNum; Width : Natural := 1) is
   begin
      if Is_Negative (Item) then
         Ada.Text_IO.Put ('-');
         Put (BigNum (negate (Item)), Width);
      else
         Put (BigNum (Item), Width);
      end if;
   end Put;

   --  Get reads positive and negative numbers
   --  Negative numbers are preceded by a minus sign (ie '_')
   procedure Get (Item : out Signed_BigNum) is
   begin
      null;
   end Get;

end BigNumPkg.Signed;
