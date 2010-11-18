pragma Ada_2012;
package body Lisp is

   type Name_Table is array (Atomic range <>) of Character;
   type Name_Table_Ptr is access Name_Table;

   Names   : Name_Table_Ptr := new Name_Table'
     (nil => ' ', T => 'T', T + 1 .. Atomic'Last => ' ');

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

   ----------
   -- Atom --
   ----------

   function Atom (Name : String) return Atomic is

      function Enter (Name : String) return Atomic is
         New_Atom : constant Atomic := Last_Atom + 1;
      begin
         Last_Atom := New_Atom + Name'Length;
         Names (New_Atom .. Last_Atom) := Name_Table (Name & ' ');
         return New_Atom;
      end Enter;

      function Find (Name : String; From : Atomic) return Atomic is
        (if From = nil or else Image (From) = Name then From
         else Find (Name, Next (From)));

      function Maybe_Enter (Name : String; Existing : Atomic) return Atomic is
        (if Existing = nil or else Name = "" then Enter (Name) else Existing);

   begin
      return Maybe_Enter (Name, Find (Name, Next (nil)));
   end Atom;

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
   -- Next --
   ----------

   function Next (A : Atomic) return Atomic is
     (if Names (A) /= ' ' then Next (A + 1)
      elsif A = Last_Atom then nil else A + 1);
end Lisp;
