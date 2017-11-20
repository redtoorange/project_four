with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body BigNumPkg.Signed is
   ----------------------------------------------------------
   -- Purpose: Determine if the Signed_BigNum is negative.
   -- Parameters: X: Signed_BigNum to test.
   ----------------------------------------------------------
   function Is_Negative(X : Signed_BigNum) return Boolean is
     (BigNum (X) >= BigNum (First));

   ----------------------------------------------------------
   -- Purpose: Convert a Signed_BigNum into a String.
   -- Parameters: X: Signed_BigNum to convert.
   ----------------------------------------------------------
   function toString (X : Signed_BigNum) return String is
   begin
      if Is_Negative (X) then
         if X = First then
            return '-' & BigNumPkg.toString (BigNum (X));
         else
            return '-' & BigNumPkg.toString (BigNum (abs_val (X)));
         end if;
      else
         return BigNumPkg.toString (BigNum (X));
      end if;
   end toString;

   ----------------------------------------------------------
   -- Purpose: Special operator to compare Signed_BigNums
   -- Parameters: X,Y: Signed_BigNums to compare
   ----------------------------------------------------------
   function "<" (X, Y : Signed_BigNum) return Boolean is
   begin
      -- Left is not negative, right is negative
	  if Is_Negative (X) and (not Is_Negative (Y)) then
         return True;
      
	  -- Left is negative, right is not
	  elsif (not Is_Negative (X)) and Is_Negative (Y) then
         return False;
      
	  -- Both are negative, figure out which one is less negative
	  elsif Is_Negative (X) and Is_Negative (Y) then
         return BigNum (X) > BigNum (Y);
      
	  -- Both are positive, normal comparison
	  else
         return BigNum (X) < BigNum (Y);
      end if;
   end "<";

   ----------------------------------------------------------
   -- Purpose: Special operator to compare Signed_BigNums
   -- Parameters: X,Y: Signed_BigNums to compare
   ----------------------------------------------------------
   function ">" (X, Y : Signed_BigNum) return Boolean is
     (not (X < Y or X = Y));

   ----------------------------------------------------------
   -- Purpose: Special operator to compare Signed_BigNums
   -- Parameters: X,Y: Signed_BigNums to compare
   ----------------------------------------------------------
   function "<=" (X, Y : Signed_BigNum) return Boolean is 
     (X < Y or X = Y);

   ----------------------------------------------------------
   -- Purpose: Special operator to compare Signed_BigNums
   -- Parameters: X,Y: Signed_BigNums to compare
   ----------------------------------------------------------
   function ">=" (X, Y : Signed_BigNum) return Boolean is 
     (X > Y or X = Y);

   ----------------------------------------------------------
   -- Purpose: Compute the 9's complement for a Signed_BigNum.
   -- Parameters: X: Signed_BigNums to compute the 9's complement for.
   ----------------------------------------------------------
   function Nines_Comp (X : Signed_BigNum) return Signed_BigNum is
      Y : Signed_BigNum;
   begin
   	  -- Invert each digit
      for I in reverse 0 .. Size - 1 loop
         Y (I) := 9 - X (I);
      end loop;

	 -- Must add 1 to the result for 9's complement
      return Y + One;
   end Nines_Comp;

   ----------------------------------------------------------
   -- Purpose: Return negative value of a Signed_BigNum
   -- Parameters: X: Signed_BigNum to negate.
   -- Exceptions: Smallest negative value cannot be negated.
   ----------------------------------------------------------
   function negate (X : Signed_BigNum) return Signed_BigNum is
     (if X = First then raise Signed_BigNumOverFlow
      else Nines_Comp (X) );

   ----------------------------------------------------------
   -- Purpose: Return absolute value of a Signed_BigNum
   -- Parameters: X: Signed_BigNum to get the abs of.
   -- Exceptions: absolute value of the smallest negative cannot
   --    be represented.
   ----------------------------------------------------------
   function abs_val (X : Signed_BigNum) return Signed_BigNum is
     (if Is_Negative (X) then negate (X) else X);

   ----------------------------------------------------------
   -- Purpose: Add two Signed_BigNums
   -- Parameters: X,Y: Signed_BigNums to add
   ----------------------------------------------------------
   function "+" (X, Y : Signed_BigNum) return Signed_BigNum is
      overflow : Boolean;
      result   : Signed_BigNum;
   begin
      -- Do a standard addition, allowing overflow
      plus_ov (BigNum (X), BigNum (Y), BigNum (result), overflow);

	  -- Detect overflow via incorrect sign of result
      if Is_Negative (X) = Is_Negative (Y) and
         Is_Negative (Y) /= Is_Negative (result)
      then
         raise Signed_BigNumOverFlow;
      end if;

      return result;
   end "+";

   ----------------------------------------------------------
   -- Purpose: Subtract two Signed_BigNums, X-Y
   -- Parameters: X,Y: Signed_BigNums to subtract
   ----------------------------------------------------------
   function "-" (X, Y : Signed_BigNum) return Signed_BigNum is
     (X + negate (Y));

   ----------------------------------------------------------
   -- Purpose: Multiply two Signed_BigNums
   -- Parameters: X,Y: Signed_BigNums to multiply
   ----------------------------------------------------------
   function "*" (X, Y : Signed_BigNum) return Signed_BigNum is
      result   : Signed_BigNum;
      negative : Boolean := Is_Negative (X) /= Is_Negative (Y);
   begin
      -- Handle some special cases
      if X = Zero or Y = Zero then
         return Zero;
      elsif X = One then
         return Y;
      elsif Y = One then
         return X;
      end if;

      -- Do a full multiply based on positive values of both numbers
      result := Signed_BigNum (BigNum (abs_val (X)) * BigNum (abs_val (Y)));

      -- Detect overflows
      if BigNum (result) > BigNum (first) or
         (result = first and not negative)
      then
         raise Signed_BigNumOverFlow;
      end if;

      -- Return the value
      if negative and result /= first then
         return negate (result);
      else
         return result; 
      end if;


   exception
      when others =>
         raise Signed_BigNumOverFlow with
           "Result for Signed_BigNum multiply needs more digits.";
   end "*";

   ----------------------------------------------------------
   -- Purpose: Writes a Signed_BigNum to the output, padding with
   --    leading spaces if the width given is larger than the length
   --    of the number (leading zeros are not printed).
   -- Parameters: Item: Signed_BigNum to print
   --            Width: Amount of padding
   ----------------------------------------------------------
   procedure Put (Item : Signed_BigNum; Width : Natural := 1) is
   begin
      if Is_Negative (Item) then
         Ada.Text_IO.Put ('-');
         if Item = first then
            Put (BigNum (Item), Width);
         else
            Put (BigNum (abs_val (Item)), Width);
         end if;

      else
         Put (BigNum (Item), Width);
      end if;
   end Put;

   ----------------------------------------------------------
   -- Purpose: Get reads positive and negative numbers
   --    Negative numbers are preceded by a minus sign (ie '_').
   -- Parameters: Item: Signed_BigNum to store input in.
   ----------------------------------------------------------
   procedure Get (Item : out Signed_BigNum) is
      Letter   : Character;
      LineEnd  : Boolean;
      Negative : Boolean := False;
   begin
      -- Skip leading whitespace
      loop
         if End_Of_File then
            raise Data_Error;
         elsif End_Of_Line then
            Skip_Line;
         else
            Look_Ahead (Letter, LineEnd);

            -- exit if find a digit or minus symbol
            exit when Letter in '0' .. '9' or else Letter = '_';

            Get (Letter);
            if Letter /= ' ' and Letter /= ASCII.HT and Letter /= '0' then
               raise Data_Error;
            end if;
         end if;
      end loop;

      -- Check for negative symbol
      if Letter = '_' then
         Get (Letter);
         Negative := True;
      end if;

      -- Get the BigNum
      Get (BigNum (Item));

      -- Handle user putting in a number too large that it overflows
      if (Is_Negative (Item) and Item /= first) or
        (Item = First and not Negative) then
         raise Signed_BigNumOverFlow;
      end if;

      -- Negate the number is needed
      if Negative and Item /= first then
         Item := negate (Item);
      end if;
   end Get;

end BigNumPkg.Signed;
