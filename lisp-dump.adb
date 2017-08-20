pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
procedure Lisp.Dump is
   function Left (S : String; Length : Natural) return String is
     (if S'Length <= Length then S
      else S (S'First .. S'First - 1 + Length));

   function Fill (S : String; Width : Positive) return String is
     (if S'Length >= Width then S else Fill (S & ' ', Width));

   procedure Put (E : Expr; Width : Positive);

   procedure Put (E : Expr; Width : Positive) is
      S : constant String := E'Img;
   begin
      for J in S'Length + 1 .. Width loop
         Put (' ');
      end loop;

      Put (S);
   end Put;

   P : Pair;
   N : Atomic := nil;

begin
   Put_Line ("*** NAMES ***");
   loop
      Put_Line (Fill (N'Img, 5) & " | " & Image (N));
      N := Next (N);
      exit when N = nil;
   end loop;

   Put_Line ("*** MEMORY ***");
   for L in reverse Last_Cons .. nil - 1 loop
      P := Memory (L);

      Put (nil - L, 5);
      Put (" | ");
      if P.A in Atomic then
         Put (Image (P.A));

      else
         Put (nil - P.A, 5);
      end if;

      Set_Col (25);

      if P.D in Atomic then
         Put (Image (P.D));

      else
         Put (nil - P.D, 5);
      end if;
      Set_Col (33);
      Put ("| ");
      Put (Left (Image (L), 40));
      New_Line;

   end loop;
end Lisp.Dump;
