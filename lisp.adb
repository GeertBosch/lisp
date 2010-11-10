pragma Ada_2012;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO; use Ada.Text_IO;
package body Lisp is

   ----------------
   -- Dictionary --
   ----------------

   function Before (Left, Right : Atom) return Boolean is
     (if Left = Right then False
      elsif Names (Left) = Names (Right) then Before (Left + 1, Right + 1)
      else Names (Left) < Names (Right));

   package Ordered_Atoms is new Ordered_Sets (Atom, "<" => Before);
   use Ordered_Atoms;
   subtype Dictionary is Ordered_Atoms.Set;

   function Next (Item : Atom) return Atom is
     (if Names (Item) = ' ' then Item + 1 else Next (Item + 1));

   function Last (Item : Atom) return Atom is (Next (Item) - 2);

   function Key (Item : Atom) return String is
     (String (Names (Item .. Last (Item))));

   package String_Lookup is new Ordered_Atoms.Generic_Keys (String, Key);
   use String_Lookup;


   Atoms_By_Name   : Dictionary;
   Memory_Info     : Heap_Ptr := new Heap; --  Used for annotations
   Memory_Usage    : Natural := 0;
   First_Allocated : List := Nil;
   Last_Allocated  : List := Nil;

   ----------
   -- cons --
   ----------

   function cons (A, D : Expr) return Non_Nil_List is

      LCG_A : constant := 1_103_515_245; --  From GLIBC's random generator
      LCG_C : constant :=        12_345;

      type Hash_Val is mod 2**32;

      Hash_To_List : constant := Hash_Val'Modulus / Max_Lists;

      function U (E : Expr) return Hash_Val is (Hash_Val (E - Expr'First));

      function To_List (U : Hash_Val) return List is
        (List'First + List'Base (U / Hash_To_List));

      function Hash (A : Hash_Val; D : Hash_Val := 0) return Hash_Val is
        (LCG_A * ((LCG_A * A + LCG_C) xor D));

      Free_Pair_Val  : constant Pair := (nil, nil);
      Free_Pair_List : constant List := To_List (Hash (U (nil), U (nil)));

      P : constant Pair := (A, D);
      H : Hash_Val := Hash (U (A), U (D));
      L : List;
   begin
      for J in 1 .. Max_Lists loop
         L := To_List (H);

         if Memory (L) = P then
            return L;
         end if;

         exit when Memory (L) = Free_Pair_Val and then L /= Free_Pair_List;
         H := Hash (H);
      end loop;

      if Memory (L) /= Free_Pair_Val then
         raise Storage_Error;
      end if;

      Memory (L) := P;
      Memory_Info (L) := (Atom (Memory_Usage), nil);

      if First_Allocated = Nil then
         First_Allocated := L;

      else
         Memory_Info (Last_Allocated).D := L;
      end if;

      Memory_Usage := Memory_Usage + 1;
      Last_Allocated := L;

      return L;
   end cons;

   ----------
   -- Find --
   ----------

   function Find (Name : String) return Atom is
      C : Cursor := Find (Atoms_By_Name, Name);
      R : Atom;
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
   end Find;

   procedure pe (E : Expr) is
   begin
      if E in Atom then
         Put (Image (E));
      else
         declare
            L : Expr := E;
         begin
            Put ('(');

            while cdr (L) in Non_Nil_List loop
               pe (car (L));
               Put (' ');
            end loop;

            if cdr (L) /= Nil then
               Put (" . ");
               Put (Image (cdr (L)));
            end if;

            Put (')');
         end;
      end if;
   end pe;

   procedure Dump is
      procedure Put (E : Expr; Width : Positive) is
         S : constant String := E'Img;
      begin
         for J in S'Length + 1 .. Width loop
            Put (' ');
         end loop;

         Put (S);
      end Put;

      P : Pair;
      L : List := First_Allocated;
   begin
      Put_Line ("*** NAMES ***");
      Put (Atom'First, 5);
      Put (" | ");

      for N in Atom'First .. Last_Atom loop
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
      while L /= nil loop
         P := Memory (L);

         Put (Memory_Info (L).A, 5);
         Put (" | ");
         if P.A in Atom then
            Put (P.A, 4);
            Put (' ');
            Put (Image (P.A));

         else
            Put (Memory_Info (P.A).A, 5);
         end if;

         Set_Col (25);

         if P.D in Atom then
            Put (Image (P.D));

         else
            Put (Memory_Info (P.D).A, 5);
         end if;
         Set_Col (33);
         Put ("| ");
         New_Line;

         L := Memory_Info (L).D;
      end loop;
   end Dump;

begin
   Names (0) := ' ';
   for P in Memory'Range loop
      Memory (P) := (0, 0);
      Memory_Info (P) := (0, 0);
   end loop;
end Lisp;
