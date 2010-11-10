pragma Ada_2012;
with Lisp; use Lisp;
with Ada.Text_IO; use Ada.Text_IO;
procedure Lispcmd is

   Scan_Error : exception;

   LPAR      : constant Atom := Find ("LPAR");
   RPAR      : constant Atom := Find ("RPAR");
   PERIOD    : constant Atom := Find ("PERIOD");
   ERROR     : constant Atom := Find ("ERROR");
   QUOTE     : constant Atom := Find ("QUOTE");

   Last_Predefined : constant Atom :=  QUOTE;

   procedure Put (A : Atom);

   function Img (E : Expr) return String;

   function Img_List (E : Expr) return String is
     (if E = nil then ""
      elsif E in Atom then "." & Image (E)
      elsif cdr (E) = nil then Img (car (E))
      else Img (car (E)) & ' ' & Img_List (cdr (E)));

   function Img (E : Expr) return String is
     (if E = nil then "()"
      elsif E in Atom then Image (E)
      else '(' & Img_List (E) & ')');

   function Needs_Quoting (A : Atom) return Boolean is
     (A /= nil and A <= Last_Predefined);

   function Reverse_And_Append (S : Expr; T : Expr) return Expr is
     (if cdr (S) = nil then cons (car (S), T)
      else Reverse_And_Append (cdr (S), cons (car (S), T)));

   function Rev_List (S : Expr) return Expr is (Reverse_And_Append (S, nil));

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
         while Ptr < Last and then Line (Ptr + 1) in 'A' .. 'Z' | '0' .. '9'
         loop
            Ptr := Ptr + 1;
         end loop;
      end Scan_Atom_Part;

   begin
      --  Build the result in reversed order, and correct order at end
      while Ptr <= Line'Last loop
         case Line (Ptr) is
            when ' '      => null;
            when 'A'..'Z' =>
               Scan_Atomic_Symbol : declare
                  First : constant Positive := Ptr;
                  A     : Atom;
               begin
                  Scan_Atom_Part;
                  A := Find (Line (First .. Ptr));

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

   function Parse (S : Expr) return Parse_State;

   function Parse_List (S : Parse_State) return Parse_State is
     (if S.Input = nil then Parse_List ((Scan_Line, S.Output))
      elsif car (S.Input) = RPAR then (cdr (S.Input), Rev_List (S.Output))
      elsif car (S.Input) = LPAR then
          Parse_List (Parse_List ((cdr (S.Input), nil)) & S.Output)
      else Parse_List ((cdr (S.Input), cons (car (S.Input), S.Output))));

   function Parse (S : Expr) return Parse_State is
     (if S = nil then (nil, nil)
      elsif car (S) = LPAR then Parse_List ((cdr (S), nil))
      else (cdr (S), car (S)));

   procedure Interpreter is
   begin
      loop
         declare
            E : Expr := Scan_Line ("? ");
            S : Parse_State;
         begin
            Put_Line (" Scan_Line => " & Img (E));

            S := Parse (E);
            Put_Line (" Parse output => " & Img (S.Output));

            Put_Line (" Leftover input => " & Img (S.Input));
         end;
      end loop;
   end Interpreter;

   procedure Put (A : Atom) is
   begin
      Put (Image (A));
   end Put;

begin
   Interpreter;
exception
   when others =>
      Dump;
end Lispcmd;
