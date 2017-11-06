package body BigNumPkg.Signed is
--  Removes leading zeros
   function toString(X: Signed_BigNum) return String
   is
      output : String := "Hello";
   begin
      return output;
   end toString;

   function "<"  (X, Y : Signed_BigNum) return Boolean
   is
   begin
      null;
   end "<";


   function ">"  (X, Y : Signed_BigNum) return Boolean
   is
   begin
      null;
   end ">";


   function "<=" (X, Y : Signed_BigNum) return Boolean
   is
   begin
      null;
   end "<=";


   function ">=" (X, Y : Signed_BigNum) return Boolean
   is
   begin
      null;
   end ">=";

   --  Returns the negative of X
   --  N.B. the smallest negative cannot be negated
   function negate (X : Signed_BigNum) return Signed_BigNum
   is
   begin
      null;
   end negate;

   --  Return absolute value of X
   --  N.B. absolute value of the smallest negative cannot be represented
   function abs_val (X : Signed_BigNum) return Signed_BigNum
   is
   begin
      null;
   end abs_val;

   function "+" (X, Y : Signed_BigNum) return Signed_BigNum
   is
   begin
      null;
   end "+";


   function "-" (X, Y : Signed_BigNum) return Signed_BigNum
   is
   begin
      null;
   end "-";


   function "*" (X, Y : Signed_BigNum) return Signed_BigNum
   is
   begin
      null;
   end "*";

   procedure Put (Item : Signed_BigNum; Width : Natural := 1)
   is
   begin
      null;
   end Put;

   --  Get reads positive and negative numbers
   --  Negative numbers are preceded by a minus sign (ie '-')
   procedure Get (Item : out Signed_BigNum)
   is
   begin
      null;
   end Get;

end BigNumPkg.Signed;
