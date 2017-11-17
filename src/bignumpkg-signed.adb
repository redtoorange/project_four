with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body BigNumPkg.Signed is

   function Is_Negative
     (X : Signed_BigNum) return Boolean is
     (BigNum (X) >= BigNum (first));

   --  Removes leading zeros
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
      Y : Signed_BigNum := X;
   begin
      if Y = first then
         raise Signed_BigNumOverFlow;
      elsif Is_Negative (Y) then
         Y := negate (Y);
      end if;

      return Y;
   end abs_val;

   function "+" (X, Y : Signed_BigNum) return Signed_BigNum is
      overflow : Boolean;
      result   : Signed_BigNum;
   begin
      plus_ov (BigNum (X), BigNum (Y), BigNum (result), overflow);

      if (Is_Negative (X) and Is_Negative (Y) and not Is_Negative (result))
        or else
        (not Is_Negative (X) and not Is_Negative (Y) and Is_Negative (result))
      then
         raise Signed_BigNumOverFlow;
      end if;

      return Signed_BigNum (result);
   end "+";

   function "-" (X, Y : Signed_BigNum) return Signed_BigNum is
   begin
      return X + negate (Y);
   end "-";

   function "*" (X, Y : Signed_BigNum) return Signed_BigNum is
      result   : Signed_BigNum;
      negative : Boolean := Is_Negative (X) /= Is_Negative (Y);
   begin
      result := Signed_BigNum (BigNum (abs_val (X)) * BigNum (abs_val (Y)));

      if negative and result /= first then
         result := negate (result);
      elsif not negative and result = first then
         raise Signed_BigNumOverFlow;
      end if;

      return result;
   end "*";

   procedure Put (Item : Signed_BigNum; Width : Natural := 1) is
   begin
      if Is_Negative (Item) then
         Ada.Text_IO.Put ('-');
         if Item = first then
            Put (BigNum (Item), Width);
         else
            Put (BigNum (negate (Item)), Width);
         end if;

      else
         Put (BigNum (Item), Width);
      end if;
   end Put;

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

      if Negative then
         if Item /= first then
            Item := negate (Item);
         end if;
      elsif Item = first then
         raise Signed_BigNumOverFlow;
      end if;

   end Get;

end BigNumPkg.Signed;
