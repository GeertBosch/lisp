pragma Ada_2012;
with Lisp; use Lisp;
with Lisp.Interpreter; use Lisp.Interpreter;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions;
procedure Lispcmd is

   Scan_Error : exception;

   LPAR      : constant Atomic := Atom ("LPAR");
   RPAR      : constant Atomic := Atom ("RPAR");
   PERIOD    : constant Atomic := Atom ("PERIOD");
   QUOTE     : constant Atomic := Atom ("QUOTE");
   ERROR     : constant Atomic := Atom ("ERROR");

   Last_Predefined : constant Atomic :=  ERROR;

   function Link (Left : List; Right : Expr) return List is
     (if Left not in Atomic then cons (car (Left), Link (cdr (Left), Right))
      elsif Left = nil then Right
      else cons (Left, Right));

   function "-" (Left, Right : Character) return Integer is
     (Character'Pos (Left) - Character'Pos (Right));

   function Upcase (C : Character) return Character is
     (if C not in 'a' .. 'z' then C
      else Character'Val (Character'Pos (C) + ('A' - 'a')));

   function Upcase (S : String) return String is
     (if S'Length = 0 then ""
      else Upcase (S (S'First)) & Upcase (S (S'First + 1 .. S'Last)));

   function Needs_Quoting (A : Atomic) return Boolean is
     (A /= nil and A <= Last_Predefined);

   function Reverse_And_Append (S : List; T : Expr) return Expr is
     (if cdr (S) = nil then cons (car (S), T)
      else Reverse_And_Append (cdr (S), cons (car (S), T)));

   function Rev_List (S : List) return Expr is 
     (if S = nil then nil else Reverse_And_Append (S, nil));

   function Get_Line (Prompt : String) return String is
   begin
      Put (Prompt);
      return Get_Line;
   end Get_Line;

   function Scan_Line (Prompt : String := "> ") return Expr is
      Line   : constant String := Get_Line (Prompt);
      Last   : constant Natural := Line'Last;
      Ptr    : Positive := Line'First;
      Result : Expr := Nil;

      procedure Error (Msg : String; Loc : Positive);

      procedure Scan_Atom_Part;

      procedure Error (Msg : String; Loc : Positive) is
         Prefix : String := Line (Line'First .. Loc - 1);
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
         while Ptr < Last 
           and then Line (Ptr + 1) in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'
         loop
            Ptr := Ptr + 1;
         end loop;
      end Scan_Atom_Part;

   begin
      --  Build the result in reversed order, and correct order at end
      while Ptr <= Line'Last loop
         case Line (Ptr) is
            when ' '      => null;
            when 'A'..'Z' | 'a' .. 'z' =>
               Scan_Atomic_Symbol : declare
                  First : constant Positive := Ptr;
                  A     : Atomic;
               begin
                  Scan_Atom_Part;
                  A := Atom (Upcase (Line (First .. Ptr)));

                  Result := cons (A,
                    (if Needs_Quoting (A)
                     then cons (QUOTE, Result)
                     else Result));
               end Scan_Atomic_Symbol;

            when '('    => Result := cons (LPAR, Result);
            when ')'    => Result := cons (RPAR, Result);
            when '.'    => Result := cons (PERIOD, Result);
            when others => Error ("invalid character", Ptr);
         end case;

         Ptr := Ptr + 1;
      end loop;

      return Rev_List (Result);
   end Scan_Line;

   subtype Parse_State is List;
   --  This subtype is used for documentation purposes in communicating current
   --  parse state. The car of a Parse_State represents the resulting parse
   --  tree, while the cdr contains remaining source text. In case of a syntax
   --  error, the cdr is a non-nil atom

   function appcar (X : Non_Nil_List; E : Expr) return Non_Nil_List is
     (cons (cons (car (X), E), cdr (X))); -- (A, B) => ((A, E), B)

   function Parse (S : List) return Parse_State;

   function Parse_Error (S : Parse_State) return Boolean is
     (cdr (S) in Non_Nil_Atom);

   function Parse_Pair (S : Parse_State; L : List) return Parse_State is
     (if cdr (S) = nil then Parse_Pair (cons (car (S), Scan_Line), L)
      elsif atom (cdr (S)) then S -- Propagate earlier error
      elsif cadr (S) /= RPAR then (cons (cdr (S), ERROR))
      else cons (Link (Rev_List (L), car (S)), cddr (S)));

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

begin
   REPL : loop
      declare
         E : Expr := Scan_Line ("> ");
         S : List;
      begin
         S := Parse (E);
         Put_Line (" => " & Image (car (S)));

         if cdr (S) /= nil then
            Put_Line ("??? " & Image (cdr (S)));

         elsif False and then not Atom (car (S)) then
            Put_Line ("= " & Image (evalquote (caar (S), cdar (S))));
         end if;
      end;
   end loop REPL;

exception
   when Ada.IO_Exceptions.End_Error => Put_Line ("Goodbye.");
   when E : others =>
      Put (Exception_Information (E));
      Dump;
end Lispcmd;
