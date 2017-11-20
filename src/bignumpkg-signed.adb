with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body BigNumPkg.Signed is

   ----------------------------------------------------------
   -- Purpose: Determine if the Signed_BigNum is negative.
   -- Parameters: X: Signed_BigNum to test.
   ----------------------------------------------------------
   function Is_Negative
     (X : Signed_BigNum) return Boolean is
     (BigNum (X) >= BigNum (first));

   ----------------------------------------------------------
   -- Purpose: Convert a Signed_BigNum into a String.
   -- Parameters: X: Signed_BigNum to convert.
   ----------------------------------------------------------
   function toString (X : Signed_BigNum) return String is
   begin
      if Is_Negative (X) then
         if X = first then
            return '-' & BigNumPkg.toString (BigNum (X));
         else
            return '-' & BigNumPkg.toString (BigNum (negate (X)));
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

   ----------------------------------------------------------
   -- Purpose: Special operator to compare Signed_BigNums
   -- Parameters: X,Y: Signed_BigNums to compare
   ----------------------------------------------------------
   function ">"
     (X, Y : Signed_BigNum) return Boolean is
     (not (X < Y or X = Y));

   ----------------------------------------------------------
   -- Purpose: Special operator to compare Signed_BigNums
   -- Parameters: X,Y: Signed_BigNums to compare
   ----------------------------------------------------------
   function "<=" (X, Y : Signed_BigNum) return Boolean is (X < Y or X = Y);

   ----------------------------------------------------------
   -- Purpose: Special operator to compare Signed_BigNums
   -- Parameters: X,Y: Signed_BigNums to compare
   ----------------------------------------------------------
   function ">=" (X, Y : Signed_BigNum) return Boolean is (X > Y or X = Y);

   function Invert_Digits (X : Signed_BigNum) return Signed_BigNum is
      Y : Signed_BigNum := X;
   begin
      for I in reverse 0 .. Size - 1 loop
         Y (I) := 9 - X (I);
      end loop;

      return Y + One;
   end Invert_Digits;

   ----------------------------------------------------------
   -- Purpose: Returns the negative of X
   --    N.B. the smallest negative cannot be negated
   -- Parameters: X: Signed_BigNum to negate
   ----------------------------------------------------------
   function negate (X : Signed_BigNum) return Signed_BigNum is
   begin
      if Is_Negative (X) then
         return X;
      else
         return Invert_Digits (X);
      end if;
   end negate;

   ----------------------------------------------------------
   -- Purpose: Return absolute value of X
   --    N.B. absolute value of the smallest negative cannot be represented
   -- Parameters: X: Number to get the abs of.
   ----------------------------------------------------------
   function abs_val (X : Signed_BigNum) return Signed_BigNum is
   begin
      if X = first then
         raise Signed_BigNumOverFlow;
      elsif Is_Negative (X) then
         return Invert_Digits (X);
      else
         return X;
      end if;
   end abs_val;

   ----------------------------------------------------------
   -- Purpose: Empty the contents of a Stack, printing each item
   -- Parameters: s: Stack to print and clear
   ----------------------------------------------------------
   function "+" (X, Y : Signed_BigNum) return Signed_BigNum is
      overflow : Boolean;
      result   : Signed_BigNum;
   begin
      plus_ov (BigNum (X), BigNum (Y), BigNum (result), overflow);

      declare
         A : Boolean := Is_Negative (X);
         B : Boolean := Is_Negative (Y);
         C : Boolean := Is_Negative (result);
      begin
         if (A = B) and (A /= C or B /= C) then
            raise Signed_BigNumOverFlow;
         end if;
      end;

      return Signed_BigNum (result);
   end "+";

   ----------------------------------------------------------
   -- Purpose: Empty the contents of a Stack, printing each item
   -- Parameters: s: Stack to print and clear
   ----------------------------------------------------------
   function "-"
     (X, Y : Signed_BigNum) return Signed_BigNum is
     (X + Invert_Digits (Y));

   ----------------------------------------------------------
   -- Purpose: Empty the contents of a Stack, printing each item
   -- Parameters: s: Stack to print and clear
   ----------------------------------------------------------
   function "*" (X, Y : Signed_BigNum) return Signed_BigNum is
      result   : Signed_BigNum;
      negative : Boolean := Is_Negative (X) /= Is_Negative (Y);
   begin
      if X = Zero or Y = Zero then
         return Zero;
      end if;

      result := Signed_BigNum (BigNum (abs_val (X)) * BigNum (abs_val (Y)));

      if Is_Negative (result) then
         if result = first and negative then
            if not negative then

            end if;
         else
            raise Signed_BigNumOverFlow;
         end if;
      else
         if negative then
            result := negate (result);
         end if;
      end if;



      return result;

   exception
      when BigNumOverFlow =>
         raise Signed_BigNumOverFlow;
   end "*";

   ----------------------------------------------------------
   -- Purpose: Empty the contents of a Stack, printing each item
   -- Parameters: s: Stack to print and clear
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
   -- Purpose: Empty the contents of a Stack, printing each item
   -- Parameters: s: Stack to print and clear
   ----------------------------------------------------------
   --  Get reads positive and negative numbers
   --  Negative numbers are preceded by a minus sign (ie '_')
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

            -- exit if find a digit
            exit when Letter in '0' .. '9' or else Letter = '_';
            -- Original version skipped leading zeros, as follows:
            -- exit when Letter in '1'..'9';

            Get (Letter);
            if Letter /= ' ' and Letter /= ASCII.HT and Letter /= '0' then
               raise Data_Error;
            end if;
         end if;
      end loop;

      Negative := Letter = '_';
      if Negative then
         Get (Letter);
      end if;

      Get (BigNum (Item));

      if Is_Negative (Item) and Item /= first then
         raise Signed_BigNumOverFlow;
      end if;

      if Negative then
         if Item /= first then
            Item := negate (Item);
         end if;
      elsif Item = first then
         raise Signed_BigNumOverFlow;
      end if;

   end Get;

end BigNumPkg.Signed;
