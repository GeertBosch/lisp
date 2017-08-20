pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions;

with Lisp; use Lisp;
with Lisp.Interpreter; use Lisp.Interpreter;
with Lisp.Dump;
procedure Lispcmd is

   LPAR    : constant Atomic := Atom ("LPAR");
   RPAR    : constant Atomic := Atom ("RPAR");
   PERIOD  : constant Atomic := Atom ("PERIOD");
   QUOTE   : constant Atomic := Atom ("QUOTE");
   ERROR   : constant Atomic := Atom ("ERROR");
   QUIT    : constant Atomic := Atom ("QUIT");
   DEFINE  : constant Atomic := Atom ("DEFINE");
   DUMP    : constant Atomic := Atom ("DUMP");

   Last_Predefined : constant Atomic := New_Atom;

   Defines : List := nil;

   function "&" (Left : String; Right : Expr) return String is
     (Left & Image (Right));

   function "-" (Left, Right : Character) return Integer is
     (Character'Pos (Left) - Character'Pos (Right));

   function Head (S : String) return Character is (S (S'First));

   function Upcase (C : Character) return Character is
     (if C not in 'a' .. 'z' then C
      else Character'Val (Character'Pos (C) + ('A' - 'a')));

   function Upcase (S : String) return String is
     (if S'Length = 0 then ""
      else Upcase (S (S'First)) & Upcase (S (S'First + 1 .. S'Last)));

   function Tail (S : String) return String is (S (S'First + 1 .. S'Last));

   function Needs_Quoting (A : Atomic) return Boolean is
     (A /= nil and A <= Last_Predefined);

   function Reverse_And_Append (S : List; T : Expr) return Expr is
     (if cdr (S) = nil then cons (car (S), T)
      else Reverse_And_Append (cdr (S), cons (car (S), T)));

   function Rev_List (S : List) return Expr is
     (if S = nil then nil else Reverse_And_Append (S, nil));

   ------------------------------
   -- Scanner and Preprocessor --
   ------------------------------

   type Scan_State is record
      Ptr    : Integer;
      Result : Expr;
   end record;

   function Whiteout (C : Character) return Character is
     (if C in ' ' | ASCII.HT then C else ' ');

   function Whiteout (S : String) return String is
     (if S = "" then S else Whiteout (Head (S)) & Whiteout (Tail (S)));

   function Remove_Comment (S : String) return String is
     (if S'Length > 0 and then Head (S) = '#' then "" else S);

   function Expand_Def (E : Expr; A : List) return Expr is
     (if A = nil or else E in List then E
      elsif caar (A) = E then cdar (A)
      else Expand_Def (E, cdr (A)));

   function Expand_List (E : Expr; A : List) return Expr is
     (if E = nil then E
      else cons (Expand_Def (car (E), A), Expand_List (cdr (E), A)));

   function Make_Atom (S : Scan_State; A : Atomic) return Scan_State is
     (if Needs_Quoting (A) then (S.Ptr, cons (A, cons (QUOTE, S.Result)))
      else (S.Ptr, cons (A, S.Result)));

   function Make_Atom (S : Scan_State; Text : String) return Scan_State is
     (Make_Atom ((Text'Last + 1, S.Result), Atom (Upcase (Text))));

   function Scan_Atom_Part (Line : String; P : Positive) return Positive is
     (if P > Line'Last then P
      elsif Line (P) not in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' then P
      else Scan_Atom_Part (Line, P + 1));

   function Scan_Atom (S : Scan_State; Line : String) return Scan_State is
     (Make_Atom (S, Line (S.Ptr .. Scan_Atom_Part (Line, S.Ptr) - 1)));

   function Scan_Line return Expr;
   function Scan_Line (S : Scan_State; Line : String) return Scan_State is
     (if S.Ptr < Line'First then S
      elsif S.Ptr > Line'Last then (S.Ptr, Rev_List (S.Result))
      else (case Line (S.Ptr) is
            when ' ' => Scan_Line ((S.Ptr + 1, S.Result), Line),
            when 'A' .. 'Z' |
                 'a' .. 'z' => Scan_Line (Scan_Atom (S, Line), Line),
            when '(' => Scan_Line ((S.Ptr + 1, cons (LPAR, S.Result)), Line),
            when ')' => Scan_Line ((S.Ptr + 1, cons (RPAR, S.Result)), Line),
            when '.' => Scan_Line ((S.Ptr + 1, cons (PERIOD, S.Result)), Line),
            when others => (-S.Ptr, S.Result)));

   function Scan_Line return Expr is
      Line  : constant String := Remove_Comment (Get_Line);
      State : constant Scan_State := Scan_Line ((Line'First, nil), Line);
   begin
      if State.Ptr < Line'First then
         Put_Line (Standard_Error, ">>> " & Line);
         Put_Line (Standard_Error, "    " & Whiteout (Line (1 .. -State.Ptr)));
         return nil;

      else
         return Expand_List (State.Result, Defines);
      end if;
   end Scan_Line;

   ------------
   -- Parser --
   ------------

   subtype Parse_State is List;
   --  This subtype is used for documentation purposes in communicating current
   --  parse state. The car of a Parse_State represents the resulting parse
   --  tree, while the cdr contains remaining source text. In case of a syntax
   --  error, the cdr is a non-nil atom while the car is the remaining source
   --  after the error was detected.

   function appcar (X : Non_Nil_List; E : Expr) return Non_Nil_List is
     (cons (cons (car (X), E), cdr (X))); -- (A, B) => ((A, E), B)

   function Parse (S : List) return Parse_State;

   function Parse_Error (S : Parse_State) return Boolean is
     (cdr (S) in Non_Nil_Atom);

   function Parse_Pair (S : Parse_State; L : List) return Parse_State is
     (if cdr (S) = nil then Parse_Pair (cons (car (S), Scan_Line), L)
      elsif atom (cdr (S)) then S -- Propagate earlier error
      elsif cadr (S) /= RPAR then (cons (cdr (S), ERROR))
      else cons (append (Rev_List (L), car (S)), cddr (S)));

   function Parse_Period (S : Parse_State) return Parse_State is
     (if cdr (S) = nil then Parse_Period (cons (car (S), Scan_Line))
      else Parse_Pair (Parse (cdr (S)), car (S)));

   function Parse_List (S : Parse_State) return Parse_State is
     (if cdr (S) = nil then Parse_List (cons (car (S), Scan_Line))
      elsif Parse_Error (S) then S
      elsif cadr (S) = RPAR then cons (Rev_List (car (S)), cddr (S))
      elsif car (S) /= nil and then cadr (S) = PERIOD
         then Parse_Period (cons (car (S), cddr (S)))
      else Parse_List (appcar (Parse (cdr (S)), car (S))));

   function Parse (S : List) return Parse_State is
     (if S = nil then cons (nil, nil)
      elsif car (S) = LPAR then Parse_List (cons (nil, cdr (S)))
      elsif car (S) in RPAR | PERIOD then cons (S, ERROR)
      elsif car (S) = QUOTE then cdr (S)
      else S);

   function Parse_Line (S : Parse_State) return Parse_State is
     (if cdr (S) in Non_Nil_List then
        Parse_Line (appcar (Parse (cdr (S)), car (S)))
      elsif cdr (S) = nil and then (car (S) = nil or else cdar (S) /= nil) then
         cons (Rev_List (car (S)), nil)
      else cons (caar (S), cdr (S)));

begin -- Processing for Lispcmd
   REPL : loop
      Put ("> ");
      declare
         E : constant Expr := Scan_Line;
         S : List;
      begin
         S := Parse_Line (cons (nil, E));
         Put_Line (if S = nil then "" else " => " & car (S));

         if S = nil then
            null;

         elsif cdr (S) /= nil then
            Put_Line ("??? " & cdr (S));

         elsif car (S) in Non_Nil_List and then caar (S) = DEFINE then
            Defines := pairlis (cons (cadar (S), nil), cddar (S), Defines);

         elsif car (S) in Non_Nil_List and then not atom (cdar (S)) then
            begin
               Put_Line ("= " & evalquote (caar (S), cdar (S)));
            exception
               when E : Constraint_Error =>
                  Put_Line (("Error in (evalquote "
                              & caar (S) & " " & cdar (S) & ")"));
                  pragma Debug (Put_Line (Exception_Message (E)));
            end;

         elsif car (S) = DUMP then
            Lisp.Dump;

         elsif car (S) = QUIT then
            exit REPL;
         end if;
      end;
   end loop REPL;

exception
   when Ada.IO_Exceptions.End_Error => Put_Line ("End of input. Goodbye.");
   when E : others =>
      Put (Exception_Information (E));
      Lisp.Dump;
end Lispcmd;
