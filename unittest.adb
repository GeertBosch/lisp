pragma Assertion_Policy (Check);

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Text_IO;       use Ada.Text_IO;
with System.Assertions; use System.Assertions;
with Lisp.Interpreter;
use Lisp, Lisp.Interpreter;

procedure Unittest is
   A : constant Atomic := Atom ("A");
   B : constant Atomic := Atom ("B");
   C : constant Atomic := Atom ("C");
   D : constant Atomic := Atom ("D");
   E : constant Atomic := Atom ("E");
   M : constant Atomic := Atom ("M");
   N : constant Atomic := Atom ("N");
   U : constant Atomic := Atom ("U");
   V : constant Atomic := Atom ("V");
   W : constant Atomic := Atom ("W");
   X : constant Atomic := Atom ("X");
   Y : constant Atomic := Atom ("Y");

   type Aggregate is array (Positive range <>) of Expr;
   function Make_List (L : Aggregate) return List is
     (if L'Length = 0 then nil
      else cons (L (L'First), Make_List (L (L'First + 1 .. L'Last))));
begin
   --  These unit tests are directly from the examples in the LISP 1.5 manual.

   --  equal
   pragma Assert (equal (cons (T, nil), cons (T, nil)));
   pragma Assert (not equal (cons (T, nil), cons (T, T)));

   --  append
   pragma Assert
     (equal
        (append (Make_List ((A, B, C)), Make_List ((D, E))),
         Make_List ((A, B, C, D, E))));

   --  subst
   pragma Assert
     (equal
        (subst (cons (X, A), B, cons (cons (A, B), C)),
         cons (cons (A, cons (X, A)), C)));

   --  pairlis
   pragma Assert
     (equal
        (pairlis
           (Make_List ((A, B, C)), Make_List ((U, V, W)),
            Make_List ((cons (D, X), cons (E, Y)))),
         Make_List
           ((cons (A, U), cons (B, V), cons (C, W), cons (D, X),
             cons (E, Y)))));

   --  assoc
   pragma Assert
     (equal
        (assoc
           (B,
            Make_List
              ((cons (A, Make_List ((M, N))),
                cons (B, Make_List ((Atom ("CAR"), X))),
                cons (C, Make_List ((Atom ("CDR"), X))),
                cons (D, Make_List ((Atom ("CAR"), X)))))),
         cons (B, Make_List ((Atom ("CAR"), X)))));

   --  sublis
   declare
      SHAKESPEARE : constant Atomic := Atom ("SHAKESPEAR");
      THE         : constant Atomic := Atom ("THE");
      TEMPEST     : constant Atomic := Atom ("TEMPEST");
      WROTE       : constant Atomic := Atom ("WROTE");
   begin
      pragma Assert
        (equal
           (sublis
              (Make_List
                 ((cons (X, SHAKESPEARE),
                   cons (Y, Make_List ((THE, TEMPEST))))),
               Make_List ((X, WROTE, Y))),
            Make_List ((SHAKESPEARE, WROTE, Make_List ((THE, TEMPEST))))));
   end;

   --  evalquote
   declare
      package K renames Keywords;
   begin
      pragma Assert
        (equal
           (evalquote
              (Make_List
                 ((K.LAMBDA, Make_List ((X, Y)),
                   Make_List ((K.CONS, Make_List ((K.CAR, X)), Y)))),
               Make_List ((Make_List ((A, B)), Make_List ((C, D))))),
            Make_List ((A, C, D))));
   end;

exception
   when E : Assert_Failure =>
      Put (Exception_Information (E));
      raise;
end Unittest;
