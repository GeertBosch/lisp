pragma Ada_2012;
package Lisp is

   Max_Atoms : constant := 2**24;
   Max_Lists : constant := 2**24;

   type Expr is range -Max_Lists .. Max_Atoms - 1;

   subtype Atomic is Expr range 0 .. Expr'Last;

   nil       : constant Atomic := 0; --  Also empty list or falsity
   T         : constant Atomic := 1; --  Truth

   subtype Predicate is Atomic range nil .. T; -- Note any non-nil expr is true
   subtype List is Expr range Expr'First .. 0;

   subtype Non_Nil_Atom is Atomic range 1 .. Atomic'Last;
   subtype Non_Nil_List is List range List'First .. -1;

   --  Conversion functions

   function Atom  (Name : String) return Atomic;
   function Image (E : Expr) return String;
   function False (E : Expr) return Boolean is (E = nil);
   function True  (E : Expr) return Boolean is (E /= nil);

   --  Five elementary functions

   function cons (A, D : Expr) return Non_Nil_List;
   function car  (S : Non_Nil_List) return Expr;
   function cdr  (S : Non_Nil_List) return Expr;
   function atom (E : Expr) return Predicate;
   function eq   (X, Y : Expr) return Predicate;

   --  Convenience functions

   function atom (E : Expr) return Boolean is (True (atom (E)));
   function eq   (X, Y : Expr) return Boolean is (True (eq (X, Y)));

   function caar (S : Non_Nil_List) return Expr is (car (car (S)));
   function cdar (S : Non_Nil_List) return Expr is (cdr (car (S)));
   function cadr (S : Non_Nil_List) return Expr is (car (cdr (S)));
   function cddr (S : Non_Nil_List) return Expr is (cdr (cdr (S)));

   function caaar (S : Non_Nil_List) return Expr is (caar (car (S)));
   function cdaar (S : Non_Nil_List) return Expr is (cdar (car (S)));
   function cadar (S : Non_Nil_List) return Expr is (cadr (car (S)));
   function cddar (S : Non_Nil_List) return Expr is (cddr (car (S)));
   function caadr (S : Non_Nil_List) return Expr is (caar (cdr (S)));
   function cdadr (S : Non_Nil_List) return Expr is (cdar (cdr (S)));
   function caddr (S : Non_Nil_List) return Expr is (cadr (cdr (S)));
   function cdddr (S : Non_Nil_List) return Expr is (cddr (cdr (S)));

   --  Debug procedure

   procedure Dump;

private

   type Pair is record
      A : Expr;
      D : Expr;
   end record;

   type Heap is array (Non_Nil_List) of Pair;
   type Heap_Ptr is access Heap;

   Memory  : Heap_Ptr := new Heap;

   function car (S : Non_Nil_List) return Expr is
     (if atom (S) then S else Memory (S).A);

   function cdr (S : Non_Nil_List) return Expr is
     (if atom (S) then S else Memory (S).D);

   function atom (E : Expr) return Predicate is (Boolean'Pos (E in Atomic));

   function eq (X, Y : Expr) return Predicate is (Boolean'Pos (X = Y));

   type Name_Table is array (Atomic) of Character;
   type Name_Table_Ptr is access Name_Table;

   Last_Atom : Atomic := T + 1;

   Names   : Name_Table_Ptr := new Name_Table'
     (nil => ' ', T => 'T', others => ' ');

   function Image_List (E : Expr) return String is
     (if E = nil then ""
      elsif E in Atomic then "." & Image (E)
      elsif cdr (E) = nil then Image (car (E))
      elsif cdr (E) in Atomic then Image (car (E)) & Image_List (cdr (E))
      else Image (car (E)) & ' ' & Image_List (cdr (E)));

   function Image (E : Expr) return String is
     (if E = nil then "()"
      elsif E in list then '(' & Image_List (E) & ')'
      elsif Names (E) = ' ' then ""
      else Names (E) & Image (E + 1));

end Lisp;
