pragma Ada_2012;
with Lisp; use Lisp;
with Lisp.Interpreter; use Lisp.Interpreter;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
procedure Lispcmd is

   Scan_Error : exception;

   LPAR      : constant Atomic := Atom ("LPAR");
   RPAR      : constant Atomic := Atom ("RPAR");
   PERIOD    : constant Atomic := Atom ("PERIOD");
   QUOTE     : constant Atomic := Atom ("QUOTE");
   ERROR     : constant Atomic := Atom ("ERROR");

   Last_Predefined : constant Atomic :=  QUOTE;

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

   type Parse_State is record
      Input  : List;
      Output : Expr;
   end record;

   function "&" (S : Parse_State; L : List) return Parse_State is
     (S.Input, cons (S.Output, L));

   function Parse (S : List) return Parse_State;

   function Parse_Pair (S : Parse_State; L : List) return Parse_State is
     (if S.Input = nil then Parse_Pair ((Scan_Line ("PERIOD> "), S.Output), L)
      elsif car (S.Input) /= RPAR then
        (cons (S.Output, S.Input), Link (Rev_List (L), ERROR))
      else (cdr (S.Input), Link (Rev_List (L), S.Output)));

   function Parse_Period (S : Parse_State) return Parse_State is
     (if S.Input = nil then Parse_Period ((Scan_Line ("PERIOD> "), S.Output))
      else Parse_Pair (Parse (S.Input), S.Output));

   function Parse_List (S : Parse_State) return Parse_State is
     (if S.Input = nil then Parse_List ((Scan_Line ("LPAR> "), S.Output))
      elsif car (S.Input) = RPAR then (cdr (S.Input), Rev_List (S.Output))
      elsif car (S.Input) = PERIOD then Parse_Period ((cdr (S.Input), S.Output))
      else Parse_List (Parse (S.Input) & S.Output));

   function Parse (S : List) return Parse_State is
     (if S = nil then (nil, nil)
      elsif car (S) = LPAR then Parse_List ((cdr (S), nil))
      elsif car (S) = QUOTE then (cddr (S), cadr (S))
      else (cdr (S), car (S)));

begin
   REPL : loop
      declare
         E : Expr := Scan_Line ("> ");
         S : Parse_State;
      begin
         S := Parse (E);
         Put_Line (" => " & Image (S.Output));

         if S.Input /= nil then
            Put_Line ("??? " & Image (S.Input));

         elsif False and then not Atom (S.Output) then
            Put_Line ("= "
              & Image (evalquote (car (S.Output), cdr (S.output))));
         end if;
      end;
   end loop REPL;

exception
   when E : others =>
      Put (Exception_Information (E));
      Dump;
end Lispcmd;
