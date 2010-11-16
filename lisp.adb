pragma Ada_2012;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO; use Ada.Text_IO;
package body Lisp is

   ----------------
   -- Dictionary --
   ----------------

   function Before (Left, Right : Atomic) return Boolean is
     (if Left = Right then False
      elsif Names (Left) = Names (Right) then Before (Left + 1, Right + 1)
      else Names (Left) < Names (Right));

   package Ordered_Atoms is new Ordered_Sets (Atomic, "<" => Before);
   use Ordered_Atoms;
   subtype Dictionary is Ordered_Atoms.Set;

   function Next (Item : Atomic) return Atomic is
     (if Names (Item) = ' ' then Item + 1 else Next (Item + 1));

   function Last (Item : Atomic) return Atomic is (Next (Item) - 2);

   function Key (Item : Atomic) return String is
     (String (Names (Item .. Last (Item))));

   package String_Lookup is new Ordered_Atoms.Generic_Keys (String, Key);
   use String_Lookup;

   Atoms_By_Name   : Dictionary;
   Last_Allocated  : List := Nil;

   ----------
   -- cons --
   ----------

   function cons (A, D : Expr) return Non_Nil_List is
   begin
      Last_Cons := Last_Cons - 1;
      Memory (Last_Cons) := (A, D);

      return Last_Cons;
   end cons;

   ----------
   -- Atom --
   ----------

   function Atom (Name : String) return Atomic is
      C : Cursor := Find (Atoms_By_Name, Name);
      R : Atomic;
   begin
      if Has_Element (C) then
         return Element (C);
      end if;

      Last_Atom := Last_Atom + 1;
      R := Last_Atom;

      for X of Name loop
         Names (Last_Atom) := X;
         Last_Atom := Last_Atom + 1;
      end loop;

      Names (Last_Atom) := ' ';

      Include (Atoms_By_Name, R);
      return R;
   end Atom;

   procedure Dump is
      function Left (S : String; Length : Natural) return String is
        (if S'Length <= Length then S
         else S (S'First .. S'First - 1 + Length));

      procedure Put (E : Expr; Width : Positive) is
         S : constant String := E'Img;
      begin
         for J in S'Length + 1 .. Width loop
            Put (' ');
         end loop;

         Put (S);
      end Put;

      P : Pair;
   begin
      Put_Line ("*** NAMES ***");
      Put (Atomic'First, 5);
      Put (" | ");

      for N in Atomic'First .. Last_Atom loop
         if Names (N) = ' ' then
            New_Line;
            Put (N + 1, 5);
            Put (" | ");

         else
            Put (Names (N));
         end if;
      end loop;
      New_Line;

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
   end Dump;
end Lisp;
