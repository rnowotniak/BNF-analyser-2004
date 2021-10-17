--
-- Copyright (C) 2004   Robert Nowotniak <robercik@toya.net.pl>
--

With Ada.Text_Io;
Use  Ada.Text_Io;
With Ada.Strings.Unbounded;
Use  Ada.Strings.Unbounded;
With Graf;
Use  Graf;
With Unchecked_Deallocation;


package body Determinizm is


   Spelniona    : Boolean := TRUE;
   NIEROZLACZNE : exception;

   --
   -- Procedura do sprawdzania, czy gramatyka
   -- spelnia Regule1 (rozlacznosc zbiorow pierwszych symboli)
   --
   procedure Sprawdz_Regule1 is

      procedure Regula1(
         Wez     : in Wezel_Ptr;
         Zbior1  : in out Zbior_Ptr_t;  -- Zbior, do ktorego dopisywac
         W_Glab  : Boolean := TRUE)     -- Czy sprawdzac rekurencyjnie Nast
      is

         NowyZbior : Zbior_Ptr_t;

      begin
         Wez.Licznik := Wez.Licznik + 1;

         if Wez.Typ /= Zwykly then
            null;

            if Wez.Nast /= Null then
               case Wez.Typ is
                  when Opcjonalny | Jawnie_Pusty | Po_Iteracyjny =>
                     Regula1(Wez.Nast, Zbior1, W_Glab);
                  when Others =>
                     null;
               end case;
            end if;
            if Wez.CzlAlt /= Null
               and then Wez.CzlAlt.Licznik < Wez.Licznik then
               Regula1(Wez.CzlAlt, Zbior1, W_Glab);

            end if;
         elsif Wez.RefSym = null then
            Dopisz_Do_Zbioru(Zbior1, Wez.KSym, Terminalny);
            if Wez.CzlAlt /= Null
               and then Wez.CzlAlt.Licznik < Wez.Licznik then
               Regula1(Wez.CzlAlt, Zbior1, W_Glab);
            end if;
            if W_Glab and then Wez.Nast /= Null
               and then Wez.Nast.Licznik < Wez.Licznik then

               NowyZbior := null;
               Regula1( Wez.Nast, NowyZbior, TRUE);
               Usun_Zbior(NowyZbior);

            end if;
         else -- Wezel ze wskaznikiem RefSym
            Dopisz_Do_Zbioru(Zbior1, Wez.RefSym.PSym, NieTerminalny);

            --
            -- Nalezy sprawdzic, jakie pierwsze symbole moze
            -- produkowac ten podcel, ale nie trzeba (i nie mozna)
            -- sprawdzac go calego (nie sprawdzac jego Nast).
            --
            Regula1(Wez.RefSym.Wyj, Zbior1, FALSE);
            if Wez.CzlAlt /= null
               and then Wez.CzlAlt.Licznik < Wez.Licznik then
               Regula1(Wez.CzlAlt, Zbior1, W_Glab);
            end if;
            if W_Glab and then Wez.Nast /= null
               and then Wez.Nast.Licznik < Wez.Licznik then

               NowyZbior := null;
               Regula1(Wez.Nast, NowyZbior, TRUE);
               Usun_Zbior(NowyZbior);
            end if;
         end if;

      end Regula1;


      Zbior : Zbior_Ptr_t := null;

      p   : Wezel_Pocz_Ptr := LISTA_Wezlow;
   begin

      Put_Line("--- Sprawdzanie reguly 1 ---");

      while p /= null loop
         Put("Produkcja dla symbolu " & To_String(p.PSym) & "...  ");

         Resetuj_Liczniki;

         begin
            Dopisz_Do_Zbioru(Zbior, p.PSym, NieTerminalny);
            Regula1( p.Wyj, Zbior );
            Usun_Zbior(Zbior);
            Put_Line("Spelnia!");
         exception
            when NIEROZLACZNE =>
               Put_Line("Nie spelniona!");
               Usun_Zbior(Zbior);
         end;

         p := p.Next;
      end loop;

   end Sprawdz_Regule1;


   procedure Sprawdz_Regule2 is

      procedure Regula2(
         Wez     : in Wezel_Ptr;
         Zbior1  : in out Zbior_Ptr_t;
         W_Glab  : Boolean := TRUE
      )is

      NowyZbior : Zbior_Ptr_t;

      begin
         Wez.Licznik := Wez.Licznik + 1;

         if Wez.Typ = Zwykly then
            if Wez.RefSym = null then
               Dopisz_Do_Zbioru(Zbior1, Wez.KSym, Terminalny);
               if Wez.CzlAlt /= null
                  and then Wez.CzlAlt.Licznik < Wez.Licznik then
                  Regula2(Wez.CzlAlt, Zbior1, W_Glab);
               end if;
               if W_Glab and then Wez.Nast /= null
                  and then Wez.Nast.Licznik < Wez.Licznik then

                  NowyZbior := null;
                  Regula2(Wez.Nast, NowyZbior, TRUE);
                  Usun_Zbior(NowyZbior);

               end if;
            else -- Wezel ze wskaznikiem RefSym
               Dopisz_Do_Zbioru(Zbior1, Wez.RefSym.PSym, NieTerminalny);

               Regula2(Wez.RefSym.Wyj, Zbior1, FALSE);
               if Wez.CzlAlt /= null
                  and then Wez.CzlAlt.Licznik < Wez.Licznik then
                  Regula2(Wez.CzlAlt, Zbior1, W_Glab);
               end if;
               if W_Glab and then Wez.Nast /= null
                  and then Wez.Nast.Licznik < Wez.Licznik then

                  NowyZbior := null;
                  Regula2(Wez.Nast, NowyZbior, TRUE);
                  Usun_Zbior(NowyZbior);
               end if;
            end if;
         else -- Wezel pusty

            if Wez.Nast /= null
               and then Wez.Nast.Licznik < Wez.Licznik then

               Regula2(Wez.Nast, Zbior1, W_Glab);

            end if;
         end if;

      end Regula2;



      Zbior  : Zbior_Ptr_t    := null;
      p      : Wezel_Pocz_Ptr := LISTA_Wezlow;
   begin

      Put_Line("--- Sprawdzanie reguly 2 ---");

      while p /= null loop
         Put("Produkcja dla symbolu " & To_String(p.PSym) & "... ");
         Resetuj_Liczniki;

         begin
            Dopisz_Do_Zbioru(Zbior, p.PSym, NieTerminalny);
            Regula2( p.Wyj, Zbior );
            Usun_Zbior(Zbior);
            Put_Line("Spelniona");
         exception
            when NIEROZLACZNE =>
               Put_Line("Nie spelniona!");
               Usun_Zbior(Zbior);
         end;

         p := p.Next;
      end loop;

   end Sprawdz_Regule2;


   --
   -- Procedura dodaje do zbioru nowy element,
   -- symbol terminalny lub nieterminalny
   --
   procedure Dopisz_Do_Zbioru(
      Zbior    : in out Zbior_Ptr_t;
      Symbol   : in Symb_Term;
      Typ      : in Typ_Elementu_t := Terminalny) is

      p        : Zbior_Ptr_t;
   begin

      if Zbior = null then
         --
         -- Przypadek listy pustej
         --
         Zbior        := new Zbior_t;
         Zbior.Symbol := Symbol;
         Zbior.Typ    := Typ;
         Zbior.Next   := null;
      else
         p := Zbior;

         loop
            if p.Typ = Typ and p.Symbol = Symbol then
               Spelniona := FALSE;
               --
               -- Taki symbol jest juz w zbiorze.
               -- Regula 1 nie jest spelniona.
               --
               Put_Line("Element wspolny: " & To_String(Symbol) );
               raise NIEROZLACZNE;
            end if;
            exit when p.Next = null;
            p := p.Next;
         end loop;

         if p.Typ = Typ and p.Symbol = Symbol then
            Spelniona := FALSE;
            --
            -- Jest taki symbol w zbiorze
            --
            Put_Line("Element wspolny: " & To_String(Symbol) );
            raise NIEROZLACZNE;
         else
            p.Next   := new Zbior_t;
            p        := p.Next;
            p.Symbol := Symbol;
            p.Typ    := Typ;
            p.Next   := null;
         end if;

      end if;

   end Dopisz_Do_Zbioru;


   procedure Usun_Zbior(Zbior: in out Zbior_Ptr_t) is
      procedure Free is
         new Unchecked_Deallocation( Zbior_t, Zbior_Ptr_t );
      p     : Zbior_Ptr_t;
      Next  : Zbior_Ptr_t;
   begin

      p := Zbior;

      while p /= null loop
         Next := p.Next;
         Free(p);
         p := Next;
      end loop;

      Zbior := null;

   end Usun_Zbior;


end Determinizm;



