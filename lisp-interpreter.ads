pragma Ada_2012;
package Lisp.Interpreter is

   --  Operations on expressions

   function equal (X, Y : Expr) return Boolean;
   function subst (X, Y, Z : Expr) return Expr;

   --  Operations on lists

   function append  (X : List; Y : Expr) return Expr;
   function member  (X : Expr; Y : List) return Boolean;
   function pairlis (X, Y, A : Expr) return Expr;
   function assoc   (X : Expr; A : List) return Expr;
   function sublis  (A : List; Y : Expr) return Expr;

   --  Operations on functions and forms

   function eval (E : Expr; A : List) return Expr;
   function apply (Fn : Expr; X : List; A : List) return Expr;
   function evalquote (Fn, X : Expr) return Expr;

   package Keywords is
      CAR    : constant Atomic := Lisp.Atom ("CAR");
      CDR    : constant Atomic := Lisp.Atom ("CDR");
      CONS   : constant Atomic := Lisp.Atom ("CONS");
      ATOM   : constant Atomic := Lisp.Atom ("ATOM");
      EQ     : constant Atomic := Lisp.Atom ("EQ");
      COND   : constant Atomic := Lisp.Atom ("COND");
      LAMBDA : constant Atomic := Lisp.Atom ("LAMBDA");
      LABEL  : constant Atomic := Lisp.Atom ("LABEL");
      QUOTE  : constant Atomic := Lisp.Atom ("QUOTE");
   end Keywords;

private

   function equal (X, Y : Expr) return Boolean is
     (if atom (X) then atom (Y) and then eq (X, Y)
      else equal (car (X), car (Y)) and then equal (cdr (X), cdr (Y)));

   function subst (X, Y, Z : Expr) return Expr is
     (if equal (Y, Z) then X
      elsif atom (Z) then Z
      else cons (subst (X, Y, car (Z)), subst (X, Y, cdr (Z))));

   function append (X : List; Y : Expr) return Expr is
     (if X = nil then Y else cons (car (X), append (cdr (X), Y)));

   function member (X : Expr; Y : List) return Boolean is
     (if Y = nil then False
      elsif equal (X, car (Y)) then True
      else member (X, cdr (Y)));

   function pairlis (X, Y, A : Expr) return Expr is
     (if X = nil then A
      else cons (cons (car (X), car (Y)), pairlis (cdr (X), cdr (Y), A)));

   function assoc (X : Expr; A : List) return Expr is
     (if equal (caar (A), X) then car (A) else assoc (X, cdr (A)));

   function sub2 (A : List; Z : Expr) return Expr is
     (if A = nil then Z
      elsif eq (caar (A), Z) then cdar (A)
      else sub2 (cdr (A), Z));

   function sublis (A : List; Y : Expr) return Expr is
     (if atom (Y) then sub2 (A, Y)
      else cons (sublis (A, car (Y)), sublis (A, cdr (Y))));

   function evcon (C : List; A : Expr) return Expr is
     (if eval (caar (C), A) /= nil then eval (cadar (C), A)
      else evcon (cdr (C), A));

   function evlis (M : List; A : Expr) return Expr is
     (if M = nil then nil
      else cons (eval (car (M), A), evlis (cdr (M), A)));

   function eval (E : Expr; A : List) return Expr is
     (if atom (E) then cdr (assoc (E, A))
      elsif not atom (car (E)) then apply (car (E), evlis (cdr (E), A), A)
      elsif car (E) = Keywords.QUOTE then cadr (E)
      elsif car (E) = Keywords.COND then evcon (cdr (E), A)
      else apply (car (E), evlis (cdr (E), A), A));

   function apply (Fn : Expr; X : List; A : List) return Expr is
     (if atom (Fn) then
        (if Fn = Keywords.CAR then caar (X)
         elsif Fn = Keywords.CDR then cdar (X)
         elsif Fn = Keywords.CONS then cons (car (X), cadr (X))
         elsif Fn = Keywords.ATOM then atom (car (X))
         elsif Fn = Keywords.EQ then eq (car (X), cadr (X))
         else cons (Atom ("APPLY"), cons (Fn, cons (X, cons (A, nil)))))
      elsif car (Fn) = Keywords.LAMBDA then
         eval (caddr (Fn), pairlis (cadr (Fn), X, A))
      elsif car (Fn) = Keywords.LABEL then
         apply (caddr (Fn), X, cons (cons (cadr (Fn), caddr (Fn)), A))
      else cons (cons (cons (cons (Atom ("APPLY"), Fn), X), A), nil));

   function evalquote (Fn, X : Expr) return Expr is (apply (Fn, X, nil));
end Lisp.Interpreter;
