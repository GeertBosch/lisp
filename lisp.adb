pragma Ada_2012;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO; use Ada.Text_IO;
procedure Lisp is

   Atom_Bits : constant := 24;
   Ptr_Bits  : constant := 24;
   Var_Bits  : constant := 24;
   Expr_Bits : constant := 32;

   Scan_Error : exception;

   type Expr is mod 2**Expr_Bits;

   subtype Atom is Expr range 0 .. 2**Atom_Bits - 1;

   Nil       : constant Atom := 0;
   Last_Atom : Atom := Nil;

   subtype Ptr is Expr range Atom'Last + 1 .. Atom'Last + 2**Ptr_Bits;

   type Pair is record
      A : Expr;
      D : Expr;
   end record;

   type Heap is array (Ptr) of Pair;
   type Heap_Ptr is access Heap;
   Memory  : Heap_Ptr := new Heap;

   type Name_Table is array (Atom) of Character;
   type Name_Table_Ptr is access Name_Table;
   Names   : Name_Table_Ptr := new Name_Table;

   function Before (Left, Right : Atom) return Boolean is
     (if Left = Right then False
      elsif Names (Left) = Names (Right) then Before (Left + 1, Right + 1)
      else Names (Left) < Names (Right));

   function Next (Item : Atom) return Atom is
     (if Names (Item) = ' ' then Item + 1 else Next (Item + 1));

   function Last (Item : Atom) return Atom is (Next (Item) - 2);

   function Key (Item : Atom) return String is
     (String (Names (Item .. Last (Item))));

   function Inc (Item : in out Expr; By : Positive := 1) return Expr is
   begin
      Item := Item + Expr (By);
      return Item;
   end Inc;

   package Ordered_Atoms is new Ordered_Sets (Atom, "<" => Before);
   use Ordered_Atoms;
   subtype Dictionary is Ordered_Atoms.Set;

   package String_Lookup is new Ordered_Atoms.Generic_Keys (String, Key);
   use String_Lookup;

   Atoms_By_Name : Dictionary;

   LCG_A : constant := 1_103_515_245; --  From random generator used in GLIBC
   LCG_C : constant :=        12_345;

   function Hash (A : Expr; D : Expr := Nil) return Expr is
     ((LCG_A * ((LCG_A * A + LCG_C) xor D) - LCG_A * LCG_C + Atom'First));
   pragma Assert (Hash (Nil) = Nil);

   function Ptr_Hash (A : Expr; D : Expr := Nil) return Ptr is
     (Hash (A, D) / 2**(Expr_Bits - Ptr_Bits) + Ptr'First);

   function cons (A, D : Expr) return Expr is
      H : Ptr := Ptr_Hash (A, D);
   begin
      for J in 1 .. 2**Ptr_Bits loop
         if Memory (H) = (A, D) then 
            return H;
         end if;

         exit when H /= Nil and then Memory (H) = (Nil, Nil);
         H := Ptr_Hash (H);
      end loop;

      if Memory (H) /= (Nil, Nil) then
         raise Storage_Error;
      end if;

      Memory (H) := (A, D);

      return H;
   end cons;

   function car (S : Expr) return Expr is
     (if S in Atom then S else Memory (S).A);

   function cdr (S : Expr) return Expr is
     (if S in Atom then S else Memory (S).D);

   function Reverse_And_Append (S : Expr; T : Expr) return Expr is
     (if S in Atom then cons (S, T)
      else Reverse_And_Append (cdr (S), car (S)));

   function Find_Atom (Name : String) return Atom is
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
      return R;
   end Find_Atom;

   procedure Put (A : Atom);

   function Scan_Line return Expr is
      Line   : constant String := Get_Line;
      Last   : constant Natural := Line'Last;
      Ptr    : Positive := Line'First;
      Result : Expr := Nil;

      procedure Error (Msg : String) is
         Prefix : String := Line (Line'First .. Ptr - 1);
      begin
         for C of Prefix loop
            C := (if C > ' ' then ' ' else C);
         end loop;

         Put_Line (Standard_Error, ">>> " & Line);
         Put_Line (Standard_Error, "    " & Prefix & '^' & Msg);
         raise Scan_Error;
      end Error;

      procedure Scan_Atom_Part is
      begin
         while Ptr <= Last and then Line (Ptr) in 'A' .. 'Z' | '0' .. '9' loop
            Ptr := Ptr + 1;
         end loop;
      end Scan_Atom_Part;

   begin
      while Ptr <= Line'Last loop
         case Line (Ptr) is
            when ' '      =>
               Ptr := Ptr + 1;

            when 'A'..'Z' =>
               Scan_Atomic_Symbol : declare
                  First : constant Positive := Ptr;
               begin
                  Ptr := Ptr + 1;
                  Scan_Atom_Part;
                  Result := cons (Find_Atom (Line (First .. Ptr - 1)), Result);
               end Scan_Atomic_Symbol;

            when '(' | '.' | ')' =>
               Ptr := Ptr + 1;
               Result := Cons (Find_Atom (Line (Ptr - 1 .. Ptr - 1)), Result);

            when others =>
               Error ("invalid character");
         end case;
      end loop;

      return Reverse_And_Append (car (Result), cdr (Result));
   end Scan_Line;

   procedure Interpreter is
   begin
      loop
         declare
            Line : Expr := Scan_Line;
         begin
            while Line /= Nil loop
               Put (car (Line));
               Line := cdr (Line);
            end loop;
            New_Line;
         end;
      end loop;
   end Interpreter;

   procedure Put (A : Atom) is
   begin
      Put (Names (A));

      if Names (A) /= ' ' then
         Put (A + 1);
      end if;
   end Put;

   procedure pe (E : Expr) is
   begin
      if E in Atom then
         Put (E);
      else
         declare
            L : Expr := E;
         begin
            Put ('(');

            while cdr (L) in Ptr loop
               pe (car (L));
               Put (' ');
            end loop;

            if cdr (L) /= Nil then
               Put (" . ");
               Put (cdr (L));
            end if;

            Put (')');
         end;
      end if;
   end pe;

begin
   for P in Memory'Range loop
      Memory (P) := (0, 0);
   end loop;

   Interpreter;
end Lisp;
