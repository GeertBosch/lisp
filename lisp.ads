pragma Ada_2012;
package Lisp is

   Max_Atoms : constant := 2**24;
   Max_Lists : constant := 2**24;

   type Expr is range -Max_Lists .. Max_Atoms - 1;

   subtype Atom is Expr range 0 .. Expr'Last;
   subtype List is Expr range Expr'First .. 0;

   subtype Non_Nil_Atom is Atom range 1 .. Atom'Last;
   subtype Non_Nil_List is List range List'First .. -1;

   nil       : constant Atom := 0;

   function cons (A, D : Expr) return Non_Nil_List;

   function car (S : Expr) return Expr;

   function cdr (S : Expr) return Expr;

   function cadr (S : Expr) return Expr is (car (cdr (S)));
   function cddr (S : Expr) return Expr is (cdr (cdr (S)));

   function Find (Name : String) return Atom;

   function Image (A : Atom) return String;

   procedure Dump;

private

   type Pair is record
      A : Expr;
      D : Expr;
   end record;

   type Heap is array (Non_Nil_List) of Pair;
   type Heap_Ptr is access Heap;
   Memory  : Heap_Ptr := new Heap;

   type Name_Table is array (Atom) of Character;
   type Name_Table_Ptr is access Name_Table;
   Names   : Name_Table_Ptr := new Name_Table;

   Last_Atom : Atom := Nil;

   function car (S : Expr) return Expr is
     (if S in Atom then S else Memory (S).A);

   function cdr (S : Expr) return Expr is
     (if S in Atom then S else Memory (S).D);

   function Image (A : Atom) return String is
     (if Names (A) = ' ' then "" else Names (A) & Image (A + 1));

end Lisp;
