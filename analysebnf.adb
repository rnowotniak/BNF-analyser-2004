------------------------------------------------------------------
--                                                              --
-- Copyright 2004 (C)   Robert Nowotniak <rnowotniak@gmail.com> --
--                                                              --
-- $Id: analysebnf.adb,v 1.10 2003/12/12 18:39:41 rob Exp $     --
--                                                              --
-- Analizator skladniowy sterowany dynamiczna struktura grafu,  --
-- tworzona na podstawie gramatyki, podanej w postaci BNF.      --
--                                                              --
--                                                              --
------------------------------------------------------------------

With Ada.Text_IO;
Use  Ada.Text_IO;
With Ada.Integer_Text_IO;
Use  Ada.Integer_Text_IO;
With Ada.Strings;
Use  Ada.Strings;
With Ada.Strings.Fixed;
Use  Ada.Strings.Fixed;
With Ada.Strings.Unbounded;
Use  Ada.Strings.Unbounded;
With Ada.Characters.Handling;
Use  Ada.Characters.Handling;
With Ada.Sequential_IO;
With Unchecked_Deallocation;
With GNAT.OS_Lib;
With Graf;
Use  Graf;
With Determinizm;
With Ada.Command_Line;

procedure analyseBNF is


   --
   -- Analizator jest sterowany opisem skladni w BNF, wiec jednostki
   -- leksykalne musza byc zawsze pojedynczymi znakami.
   --
   package Symbole_IO is
      new Ada.Sequential_IO( Element_Type => Character );
   use Symbole_IO;


   --
   -- Puste slowo
   --
   EPSILON           : constant Unbounded_String := Null_Unbounded_String;


   -- Czy wyswietlac informacje diagnostyczne
   DEBUG_MODE        : constant Boolean := FALSE;



   Ilosc_Wezlow     : Natural  := 0;

   -- maksymalna dlugosc sciezki dostepu do pliku
   PATH_MAXLEN      : constant := 256;

   type Wejscie_t is (Plik, Klawiatura);

   Zrodlo_Gramatyki   : Wejscie_t;
   Zrodlo_Zdania      : Wejscie_t;

   PLIK_GRAMATYKA     : Symbole_IO.File_Type;


   --
   -- Globalny, pojedynczy, aktualnie wczytany symbol terminalny
   --
   SYMBOL             : Character;

   NAPIS_NIE_NALEZY   : exception;




   procedure DEBUG(Msg: in String) is
   begin
      if DEBUG_MODE then
         Put_Line(Msg);
      end if;
   end DEBUG;


   -------------------------------------------------------------------
   -- Procedury wspomagajace obsluge dynamicznie alokowanej pamieci --
   -------------------------------------------------------------------


   --------------------------------------------------------------
   -- Procedura zwalniajaca dynamicznie alokowana pamiec
   -- dla wezla grafu (uzywane przy optymalizacji).
   --
   -- Te procedury musza tak wygladac, bo dealokacja wymaga
   -- mozliwosci ustawienia wskaznika na NULL natomiast
   -- zwalnianie odbywa sie w funkcjach, ktore wskaznik
   -- dostaja jako argument (wiec musi byc staly).
   --
   --------------------------------------------------------------
   procedure Zwolnij_Wezel( Arg : in Wezel_Ptr ) is
      procedure Zwolnij is
         new Unchecked_Deallocation( Wezel_t, Wezel_Ptr );
      Ptr : Wezel_Ptr := Arg;
   begin
      Zwolnij(Ptr);
      Ilosc_Wezlow := Ilosc_Wezlow - 1;
   end Zwolnij_Wezel;

   procedure Zwolnij_Wezel( Arg : in Wezel_Pocz_Ptr ) is
      procedure Zwolnij is
         new Unchecked_Deallocation( Wezel_Pocz_t, Wezel_Pocz_Ptr );
      Ptr : Wezel_Pocz_Ptr := Arg;
   begin
      Zwolnij(Ptr);
      Ilosc_Wezlow := Ilosc_Wezlow - 1;
   end Zwolnij_Wezel;
   --------------------------------------------------------------


   --
   -- Funkcja do dynamicznego dodawania nowego wezla
   --
   function NowyWezel(
      KSym     :  Symb_Term      := EPSILON;
      RefSym   :  Wezel_Pocz_Ptr := null;
      CzlAlt   :  Wezel_Ptr      := null;
      Nast     :  Wezel_Ptr      := null;
      Typ      :  Typ_Wezla_t    := Zwykly;
      FC       :  Natural        := 1)
      return Wezel_Ptr is

      Ret : Wezel_Ptr;
      p   : wsk_w_ptr;
   begin

      --
      -- Utworzenie nowego wezla
      -- i wypelnienie jego pol
      --

      Ret            := new Wezel_t;
      Ret.all.KSym   := Ksym;
      Ret.all.RefSym := RefSym;
      Ret.all.CzlAlt := CzlAlt;
      Ret.all.Nast   := Nast;
      Ret.all.Typ    := Typ;
      Ret.all.FC     := FC;
      Ret.all.Id     := Ilosc_Wezlow;

      Ilosc_Wezlow := Ilosc_Wezlow + 1;

      --
      -- Dodanie wskaznika na wezel do listy
      -- wskaznikow na wszystkie wezly
      --
      case Typ is
         when Zwykly | Jawnie_Pusty | Po_Iteracyjny | Opcjonalny =>
            if Wszystkie_Wezly = null then
               Wszystkie_Wezly           := new wsk_w;
               Wszystkie_Wezly.all.Wezel := Ret;
            else
               p := Wszystkie_Wezly;
               while p.Next /= null loop
                  p := p.Next;
               end loop;
               p.Next           := new wsk_w;
               p.Next.all.Wezel := Ret;
            end if;
         when Po_Alternatywny | Przed_Iteracyjny | Konkat =>
            --
            -- Te wezly sa usuwane w fazie optymalizacji
            --
            null;
      end case;

      return Ret;
   end NowyWezel;



   ----------------------------------------------------------
   -- Procedura do zwalniania obszarow pamieci, dynamicznie
   -- alokowanych na potrzeby wszystkich produkcji,
   -- definiujacych cala gramatyke
   ----------------------------------------------------------
   procedure Zwolnij_Gramatyke( Gram: in Wezel_Pocz_Ptr ) is

      PodCel_Ptr : Wezel_Pocz_Ptr := Gram;
      Tmp        : Wezel_Pocz_Ptr;
      p, p2      : wsk_w_ptr;

      procedure Zwolnij is
         new Unchecked_Deallocation(wsk_w, wsk_w_ptr);

   begin

      DEBUG("Usuwanie grafu gramatyki...");

      while PodCel_Ptr /= null loop
         Tmp := PodCel_Ptr.Next;
         Zwolnij_Wezel(PodCel_Ptr);
         PodCel_Ptr := Tmp;
      end loop;

      p := Wszystkie_Wezly;

      while p /= null loop
         p2 := p.Next;
         Zwolnij_Wezel(p.Wezel);
         Zwolnij(p);
         p := p2;
      end loop;

      LISTA_Wezlow    := null;
      Wszystkie_Wezly := null;
      Ilosc_Wezlow    := 0;

   end Zwolnij_Gramatyke;




   ---------------------------------------------------------------
   -- Funkcja znajduje wezel z symbolem nieterminalnym PSym
   -- na liscie i zwraca wskaznik do niego.
   --
   -- Jesli taki wezel nie wystepuje funkcja go dodaje do listy.
   ---------------------------------------------------------------
   function Znajdz_Symb_Poczatkowy( PSym : Symb_NieTerm )
      return Wezel_Pocz_Ptr is

      Ptr   :  Wezel_Pocz_Ptr;

   begin
      if DEBUG_MODE then
         Put("Szukam symbolu poczatkowego ");
         Put(To_String(Psym));
         Put("...  ");
      end if;

      Ptr := LISTA_Wezlow;

      if Ptr = null then
         --
         -- Przypadek listy pustej
         --

         LISTA_Wezlow := new Wezel_Pocz_t;
         LISTA_Wezlow.all.PSym := PSym;
         Ilosc_Wezlow := Ilosc_Wezlow + 1;

         DEBUG("dodalem.");
         return LISTA_Wezlow;
      end if;


      PETLA_SZUKANIE:
      while Ptr.next /= null and Ptr.Psym /= Psym loop
         Ptr := Ptr.next;
      end loop PETLA_SZUKANIE;

      if Ptr.Psym /= Psym then
         --
         -- Taki symbol nie wystepuje
         --

         DEBUG("dodalem.");

         Ptr.next := new Wezel_Pocz_t;
         Ptr := Ptr.next;
         Ptr.PSym := Psym;
         Ilosc_Wezlow := Ilosc_Wezlow + 1;
      end if;

      DEBUG("OK.");

      return Ptr;
   end Znajdz_Symb_Poczatkowy;



   ----------------------------------------------
   -- Procedura do wyswietlania sciagi z BNF'u --
   ----------------------------------------------
   procedure Pokaz_Pomoc is
   begin

      Put_Line(76 * "-");
      Put_Line("<Sigma>        ::= <Gramatyka> [ '.' <NowaLinia> ]");
      Put_Line("<Gramatyka>    ::= <Prod> <Reszta>");
      Put_Line("<Reszta>       ::= [ <NowaLinia> ( <Gramatyka> | PUSTE ) ]");
      Put_Line("<Prod>         ::=  <W> ""<"" <Ident> "">"" <W> ""::="" <W> <Wyrazenie>");
      Put_Line("<Wyrazenie>    ::=  <Skladnik> { ""|"" <W> <Skladnik> }");
      Put_Line("<Skladnik>     ::=  <Czynnik> { <Czynnik> }");
      Put_Line("<Czynnik>      ::=  ""<"" <Ident> "">"" <W> | <Lancuch> <W> |");
      Put_Line("                                        <JedenZnak> <W> |");
      Put_Line("                        ""("" <W> <Wyrazenie> <W> "")"" <W> |");
      Put_Line("                        ""["" <W> <Wyrazenie> <W> ""]"" <W> |");
      Put_Line("                        ""{"" <W> <Wyrazenie> <W> ""}"" <W> |");
      Put_Line("                                            ""PUSTE"" <W>");
      Put_Line("<Ident>        ::=  <Literka> { <Literka> | <Cyfra> }");
      Put_Line("<Lancuch>      ::= '""' { <NieCudzyslow> } '""'");
      Put_Line("<JedenZnak>    ::= ''' <DowolnyZnak> '''");
      Put_Line("<NieCudzyslow> ::= znak o kodzie z przedzialu 1 - 33 lub 35 - 255");
      Put_Line("<Literka>      ::= ""a"" | ..... | ""z"" | ""A"" | ..... | ""Z"" ");
      Put_Line("<NowaLinia>    ::= Znak nowej linii");
      Put_Line("<Cyfra>        ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ");
      Put_Line("<DowolnyZnak>  ::= znak o kodzie z przedzialu 1 - 255");
      Put_Line("<SPC>          ::= ' '");
      Put_Line("<W>            ::= { <SPC> }");
      Put_Line(76 * "-");

   end Pokaz_Pomoc;



   --
   -- Zmienne do zwracania informacji o blednej skladni
   --
   Oczekiwany_Znak   : Unbounded_String := Null_Unbounded_String;
   Znaleziony_Znak   : Character;


   --
   -- Polozenie w pliku
   -- (uzywane do wskazywania, gdzie wystepuje blad)
   --
   Linia_Pliku       : Positive := 1;
   Kolumna_Pliku     : Natural  := 0;



   --
   -- Procedura pobierajaca pojedyncza jednoste leksykalna.
   -- Zawsze jest to tutaj pojedynczy znak.
   --
   procedure Pobierz_Symbol is
      znak : character;
   begin

      if Zrodlo_Gramatyki = Plik then
         if End_Of_File(PLIK_GRAMATYKA) then
            znak := Character'Val(0);
         else
            Read(PLIK_GRAMATYKA, znak);
         end if;
      elsif Zrodlo_Gramatyki = Klawiatura then
         if End_Of_Line then
            skip_line;
            Znak := Character'Val(10);
         else
            Get(znak);
         end if;
      end if;

      Kolumna_Pliku := Kolumna_Pliku + 1;

      if DEBUG_MODE then
         Put("-" & znak & "-   ");
         Put("Kod: ");
         Put(Character'Pos(Znak),1);
         New_Line;
      end if;

      SYMBOL := znak;

   end Pobierz_Symbol;



   procedure SetBlad(Oczekiwany: in String) is
   begin
      Znaleziony_Znak := SYMBOL;
      Oczekiwany_Znak := To_Unbounded_String(Oczekiwany);
   end SetBlad;


   ---------------------------------------------------------------
   -- Deklaracje zapowiadajace funkcji i procedur, uzywanych do --
   -- analizy syntaktycznej zadanej gramatyki - gramatki BNF'u. --
   --                                                           --
   -- Zapowiedzi sa konieczne, bo w trakcie analizy te funkcje  --
   -- wywoluja sie nawzajem.                                    --
   --                                                           --
   ---------------------------------------------------------------
   procedure Nieterminalny_Wyrazenie(
      G    : in Wezel_P2;
      Nast : in Wezel_Ptr := null;
      Alt  : in Wezel_Ptr := null );
   procedure Nieterminalny_Czynnik(
      G    : in Wezel_P2;
      Nast : in Wezel_Ptr := null;
      Alt  : in Wezel_Ptr := null);
   procedure Nieterminalny_Skladnik(
      G    : in Wezel_P2;
      Nast : in Wezel_Ptr := null;
      Alt  : in Wezel_Ptr := null);
   function  Nieterminalny_Literka return Character;
   function  Nieterminalny_Cyfra return Character;
   function  Nieterminalny_Ident return Symb_NieTerm;
   procedure Nieterminalny_NowaLinia;
   procedure Nieterminalny_Spc;
   procedure Nieterminalny_Whitespace;
   function  Nieterminalny_DowolnyZnak return Character;
   function  Nieterminalny_NieCudzyslow return Character;
   function  Nieterminalny_Lancuch return Unbounded_String;
   function  Nieterminalny_JedenZnak return Unbounded_String;
   procedure Nieterminalny_Prod;
   procedure Nieterminalny_Gramatyka;
   procedure Nieterminalny_Reszta;
   procedure Sigma;
   ---------------------------------------------------------------




   -------------------------------------------------------------------
   -------------------------------------------------------------------
   -- FUNKCJE I PROCEDURY ANALIZATORA SYNTAKTYCZNEGO DLA METAJEZYKA --
   -------------------------------------------------------------------
   -------------------------------------------------------------------


   --
   -- <Literka>     ::= "a" | .. | "b" | "z" | "A" | "B" | .. | "Z"
   --
   function Nieterminalny_Literka return Character is
      Ret   : Character;
   begin

      DEBUG("--- Nieterminalny_Literka ---");

      if SYMBOL not in 'a' .. 'z' and SYMBOL not in 'A' .. 'Z' then
         SetBlad("duza lub mala litera");
         raise NAPIS_NIE_NALEZY;
      end if;
      Ret := Character(SYMBOL);
      Pobierz_Symbol;
      return Ret;
   end Nieterminalny_Literka;


   --
   -- <Cyfra>      ::= '0' | '1' | '2' | ..... | '9'
   --
   function Nieterminalny_Cyfra return Character is
      Ret   : Character;
   begin
      DEBUG("--- Nieterminalny_Cyfra ---");
      if SYMBOL not in '0' .. '9' then
         SetBlad("dowolna cyfra");
         raise NAPIS_NIE_NALEZY;
      end if;
      Ret := Character(SYMBOL);
      Pobierz_Symbol;
      return Ret;
   end Nieterminalny_Cyfra;


   --
   -- <Ident>      ::=  <Literka> { <Literka> | <Cyfra> }
   --
   function Nieterminalny_Ident return Symb_NieTerm is
      Str   : Unbounded_String;
      Znak  : Character;
   begin
      DEBUG("--- Nieterminalny_Ident ---");
      Znak := Nieterminalny_Literka;
      Append(Str, Znak);

      while SYMBOL in 'a'..'z' or SYMBOL in 'A'..'Z' or SYMBOL in '0'..'9' loop
         if SYMBOL in 'a' .. 'z' or SYMBOL in 'A' .. 'Z' then
            Znak := Nieterminalny_Literka;
         elsif SYMBOL in '0' .. '9' then
            Znak := Nieterminalny_Cyfra;
         end if;
         Append(Str, Znak);

      end loop;

      return Str;

   end Nieterminalny_Ident;


   --
   -- <NowaLinia>  ::= x0D \x0A | \x0A
   --
   procedure Nieterminalny_NowaLinia is
   begin
      DEBUG("--- Nieterminalny_NowaLinia ---");
      if SYMBOL = Character'Val(13) then
         Pobierz_Symbol;
         if SYMBOL /= Character'Val(10) then
            SetBlad("nowej linii (NL)   [Kod: 10]");
            raise NAPIS_NIE_NALEZY;
         end if;
      elsif SYMBOL = Character'Val(10) then
         null;
      else
         SetBlad("nowej linii (NL)   [Kod: 10]");
         raise NAPIS_NIE_NALEZY;
      end if;

      Kolumna_Pliku := 0;
      Linia_Pliku   := Linia_Pliku + 1;

      Pobierz_Symbol;

   end Nieterminalny_NowaLinia;


   --
   -- <SPC>        ::= \x09 | ' '
   --
   procedure Nieterminalny_Spc is
   begin
      DEBUG("--- Nieterminalny_Spc ---");
      if SYMBOL /= ' ' and SYMBOL /= ASCII.HT then
         SetBlad("bialego znaku (spacja lub tabulator)");
         raise NAPIS_NIE_NALEZY;
      end if;
      Pobierz_Symbol;
   end Nieterminalny_Spc;


   --
   -- <W>          ::= { <SPC> }
   --
   procedure Nieterminalny_Whitespace is
   begin
      DEBUG("--- Nieterminalny_Whitespace ---");
      while SYMBOL = ' ' or SYMBOL = ASCII.HT loop
         Nieterminalny_Spc;
      end loop;
   end Nieterminalny_Whitespace;


   --
   -- <DowolnyZnak> -> dowolny znak z ASCII lub Latin1
   --
   function Nieterminalny_DowolnyZnak return Character is
      Ret   : Character;
   begin
      DEBUG("--- Nieterminalny_DowolnyZnak ---");

      Ret := SYMBOL;

      Pobierz_Symbol;
      return Ret;
   end Nieterminalny_DowolnyZnak;


   --
   -- <NieCudzyslow> ::= \x00 .... \x21 | \x23 .... \xFF
   --
   function Nieterminalny_NieCudzyslow return Character is
      Ret   : Character;
   begin
      DEBUG("--- Nieterminalny_NieCudzyslow ---");
      if SYMBOL = '"' then
         SetBlad("dowolnego oprocz cudzyslowu ("")");
         raise NAPIS_NIE_NALEZY;
      end if;
      Ret := SYMBOL;
      Pobierz_Symbol;
      return Ret;
   end Nieterminalny_Niecudzyslow;


   --
   -- <Lancuch>    ::= '"' { <NieCudzyslow> } '"'
   --
   function Nieterminalny_Lancuch return Unbounded_String is
      Lancuch  : Unbounded_String;
   begin
      DEBUG("--- Nieterminalny_Lancuch ---");
      if SYMBOL /= '"' then
         SetBlad("""");
         raise NAPIS_NIE_NALEZY;
      end if;
      Pobierz_Symbol;

      Lancuch := To_Unbounded_String(0);

      while SYMBOL /= '"' and SYMBOL /= Character'Val(0) loop
         Append(Lancuch, Nieterminalny_NieCudzyslow);
      end loop;


      if Ada.Strings.Unbounded.Index(Lancuch, String'(1=>ASCII.LF)) > 0 then
         Linia_Pliku := Linia_Pliku + Ada.Strings.Unbounded.Count(Lancuch, String'(1 => ASCII.LF));
         Kolumna_Pliku :=
            1 + Length(Lancuch) - Ada.Strings.Unbounded.Index(Lancuch, String'(1=>ASCII.LF), Backward);
      end if;


      if SYMBOL /= '"' then
         SetBlad("""");
         raise NAPIS_NIE_NALEZY;
      end if;
      Pobierz_Symbol;

      return Lancuch;
   end Nieterminalny_Lancuch;

   --
   -- <JedenZnak>  ::= ''' <DowolnyZnak> '''
   --
   function Nieterminalny_JedenZnak return Unbounded_String is
      Ret   : Unbounded_String;
   begin
      DEBUG("--- Nieterminalny_JedenZnak ---");
      if SYMBOL /= ''' then
         SetBlad("'");
         raise NAPIS_NIE_NALEZY;
      end if;
      Pobierz_Symbol;

      Ret := To_Unbounded_String(
         String'( 1=> Nieterminalny_DowolnyZnak));

      if Element(Ret,1) = ASCII.LF then
         Linia_Pliku := Linia_Pliku + 1;
      end if;

      if SYMBOL /= ''' then
         SetBlad("'");
         raise NAPIS_NIE_NALEZY;
      end if;
      Pobierz_Symbol;

      return Ret;
   end Nieterminalny_JedenZnak;


   --
   -- <Czynnik>    ::=  "<" <Ident> ">" <W> |
   --                         <Lancuch> <W> |
   --                       <JedenZnak> <W> |
   --       "(" <W> <Wyrazenie> <W> ")" <W> |
   --       "[" <W> <Wyrazenie> <W> "]" <W> |
   --       "{" <W> <Wyrazenie> <W> "}  <W>"
   --
   procedure Nieterminalny_Czynnik(
      G    : in Wezel_P2;
      Nast : in Wezel_Ptr := null;
      Alt  : in Wezel_Ptr := null)
   is
      SymN  : Symb_NieTerm;
      Ptr   : Wezel_Ptr;
   begin
      DEBUG("--- Nieterminalny_Czynnik ---");

      case SYMBOL is
         when '<' =>
            Pobierz_Symbol;

            --
            -- Wstawienie nowego wezla, takiego z polem RefSym, ktory
            -- bedzie wskazywal na wezel poczatkowy jakiejs produkcji.
            --

            SymN := Nieterminalny_Ident;

            G.all := NowyWezel(
               RefSym => Znajdz_Symb_Poczatkowy( SymN ),
               Nast   => Nast,
               CzlAlt => Alt);

            if Nast /= null then
               Nast.FC := Nast.FC + 1;
            end if;
            if Alt /= null then
               Alt.FC := Alt.FC + 1;
            end if;

            if SYMBOL /= '>' then
               SetBlad(">");
               raise NAPIS_NIE_NALEZY;
            end if;
            Pobierz_Symbol;
            Nieterminalny_Whitespace;

         when '"' =>

            G.all := NowyWezel(
               KSym   => Nieterminalny_Lancuch,
               Nast   => Nast,
               CzlAlt => Alt);

            --
            -- To mo¿na by dopisaæ do zbioru pierwszych symboli, ktore mozna
            -- wyprowadzic z symbolu, bêd±cego aktualnym podcelem analizatora
            --

            if Nast /= null then
               Nast.FC := Nast.FC + 1;
            end if;
            if Alt /= null then
               Alt.FC := Alt.FC + 1;
            end if;

            Nieterminalny_Whitespace;
         when ''' =>

            G.all := NowyWezel(
               KSym   => Nieterminalny_JedenZnak,
               Nast   => Nast,
               CzlAlt => Alt);

            if Nast /= null then
               Nast.FC := Nast.FC + 1;
            end if;
            if Alt /= null then
               Alt.FC := Alt.FC + 1;
            end if;

            Nieterminalny_Whitespace;

         when '(' =>
            -- Nawiasy zmieniaja lacznosc/priorytet,
            -- na przyklad w produkcji takiej jak ta:
            -- <S> ::= ( 'a' | 'b' ) 'c'

            Pobierz_Symbol;
            Nieterminalny_Whitespace;

            Nieterminalny_Wyrazenie(
               G,
               Nast=>Nast,
               Alt=>Alt);

            if SYMBOL /= ')' then
               SetBlad(")");
               raise NAPIS_NIE_NALEZY;
            end if;


            Pobierz_Symbol;
            Nieterminalny_Whitespace;
         when '[' =>
            --
            -- Tutaj moze byc produkowany:
            --    ciag symboli, ktory wystapi w nawiasach
            --    lub pusty ciag symboli
            --
            Pobierz_Symbol;
            Nieterminalny_Whitespace;

            Ptr := NowyWezel(
               KSym   => To_Unbounded_String("Pusty (Opt)"),
               Nast   => Nast,
               CzlAlt => Alt,       -- patrz poni¿ej
               Typ    => Opcjonalny,
               FC     => 0);


            ----------------------------------------------------------------
            -- Czy tutaj pole CzlAlt pustego wezla powinno byc ustawiane?!
            -- Czy to ma sens, skoro analizator ogolny, nigdy nie wchodzi
            -- w czlon alternatywny wezlow pustych?
            --
            -- Warto tak zrobic, aby graf odpowiadal podanej gramatyce nawet
            -- wtedy, gdy jest ona niebardzo madra. Wezmy taka produkcje:
            -- <S> ::= ( 'a' | [ "b" ] | 'c' ) 'd'
            --
            -- ["b"] moze produkowac pusty ciag symboli, a taki zawsze
            -- przypasuje, wiec analiza nigdy nie wejdzie do galezi,
            -- zawierajacej symbol 'c' takiej produkcji alternatywnej.


            -- Ptr.all.Czlalt := Alt;


            -- Jezeli Alt wskazuje na taki wezel, ktorego typ to
            -- Po_Alternatywny, to taka gramatyka bedzie obarczona jakimis
            -- wadami. Moze warto byloby wyswietlac ostrzezenie?
            ----------------------------------------------------------------

            if Ptr.Nast /= null then
               Ptr.Nast.FC := Ptr.Nast.FC + 1;
            end if;
            if Ptr.CzlAlt /= null then
               Ptr.CzlAlt.FC := Ptr.CzlAlt.FC + 1;
            end if;

            Nieterminalny_Wyrazenie(G, Nast=>Nast, Alt=>Ptr);

            if SYMBOL /= ']' then
               SetBlad("]");
               raise NAPIS_NIE_NALEZY;
            end if;
            Pobierz_Symbol;
            Nieterminalny_Whitespace;

         when '{' =>
            Pobierz_Symbol;
            Nieterminalny_Whitespace;

            -- Wstawienie pomocniczego wezla pustego, poprzedzajacego
            -- produkcje iteracyjna.
            G.all := NowyWezel(
               KSym   => To_Unbounded_String("Przed Iteracyjny"),
               Typ    => Przed_Iteracyjny);


            -------------------------------------------------------
            --                    WAZNA UWAGA                    --
            -------------------------------------------------------
            -- Tutaj nastepuje wstawienie pustego wezla, ale takiego
            -- typowego dla produkcji iteracyjnej. Ten pusty wezel
            -- konczy iteracje.
            -- Ten wezel NIE bedzie usuwany przy zadnej pozniejszej
            -- optymalizacji grafu.
            --

            Ptr := NowyWezel(
               KSym   => To_Unbounded_String("PUSTE (Po It)"),
               Nast   => Nast,
                                        -- Je¿eli to jest ustawiane,
               CzlAlt => Alt,           -- oznacza to jak±¶ dziwn±/niedobr±
                                        -- produkcjê, ale odpowiadaj±c± gramatyce.
               Typ    => Po_Iteracyjny,
               FC     => 0);

            if Nast /= null then
               Nast.FC := Nast.FC + 1;
            end if;
            if Alt /= null then
               Alt.FC := Alt.FC + 1;
            end if;

            Nieterminalny_Wyrazenie(
               G.all.Nast'Access,
               Nast=>G.all,
               Alt=>Ptr );

            if SYMBOL /= '}' then
               SetBlad("}");
               raise NAPIS_NIE_NALEZY;
            end if;
            Pobierz_Symbol;
            Nieterminalny_Whitespace;

         when 'P' =>
            Pobierz_Symbol;
            if SYMBOL /= 'U' then
               SetBlad("identyfikatora PUSTE");
               raise NAPIS_NIE_NALEZY;
            end if;

            Pobierz_Symbol;
            if SYMBOL /= 'S' then
               SetBlad("identyfikatora PUSTE");
               raise NAPIS_NIE_NALEZY;
            end if;

            Pobierz_Symbol;
            if SYMBOL /= 'T' then
               SetBlad("identyfikatora PUSTE");
               raise NAPIS_NIE_NALEZY;
            end if;

            Pobierz_Symbol;
            if SYMBOL /= 'E'  and SYMBOL /= 'Y' then
               SetBlad("identyfikatora PUSTE");
               raise NAPIS_NIE_NALEZY;
            end if;

            --
            -- Wstawienie jawnego wezla pustego slowa
            -- (epsilon figuruje w specyfikacji gramatyki)
            --

            G.all := NowyWezel(
               KSym   => To_Unbounded_String("Epsilon"),
               Nast   => Nast,
               CzlAlt => Alt,
               Typ    => Jawnie_Pusty);

            if G.all.Nast /= null then
               G.all.Nast.all.FC := G.all.Nast.all.FC + 1;
            end if;
            if G.all.CzlAlt /= null then
               G.all.CzlAlt.all.FC := G.all.CzlAlt.all.FC + 1;
            end if;

            Pobierz_Symbol;
            Nieterminalny_Whitespace;

         when others =>
            SetBlad("< lub "" lub ' lub ( lub { lub [");
            raise NAPIS_NIE_NALEZY;
      end case;

   end Nieterminalny_Czynnik;


   --
   -- <Skladnik>   ::=  <Czynnik> { <Czynnik> }
   --
   procedure Nieterminalny_Skladnik(
      G    : in Wezel_P2;
      Nast : in Wezel_Ptr := null;
      Alt  : in Wezel_Ptr := null)
   is
      Ptr       : Wezel_Ptr;
      Ptr2      : Wezel_Ptr;
   begin
      DEBUG("--- Nieterminalny_Skladnik ---");

      --
      -- W tej procedurze robimy konkatenacjê.
      -- Gdzie¶ bêdzie trzeba ustawiæ CzlAlt na przes³an± warto¶æ Alt.
      -- Gdzie? W wê¼le pierwszego symbolu, który na pewno wystêpuje
      -- w produkcji. Trzeba go bêdzie znale¼æ.
      --


      Ptr := NowyWezel(
         KSym   => To_Unbounded_String("PUSTY (Konk)"),
         Nast   => Nast,
         FC     => 0,
         Typ    => Konkat);

      if Nast /= null then
         Nast.FC := Nast.FC + 1;
      end if;

      --
      -- Ostatni z wezlow symboli konkatenacji tej procedury
      -- powinien miec ustawiony wskaznik Nast na taka wartosc,
      -- jaka zostala przeslana tej procedurze w drugim argumencie.
      --

      Nieterminalny_Czynnik( G, Nast=>Ptr);

      --
      -- Kolejne symbole tej produkcji sa podczepiane do pola
      -- Nast, uprzednio utworzonego, wezla pomocniczego.
      --

      KONKATENACJA:
      loop
         case SYMBOL is
            when '<' |'"' | '(' | '[' | '{' | ''' | 'P' =>

               Ptr2 := NowyWezel(
                  KSym   => To_Unbounded_String("PUSTY2 (Konk)"),
                  Nast   => Nast,
                  FC     => 0,
                  Typ    => Konkat);

               --
               -- Nie, tutaj _NIE_ powinien byc zwiekszany
               -- licznik ,,Frequency Counter'' wezlow pomocniczych!
               --

               Nieterminalny_Czynnik( Ptr.all.Nast'Access, Nast=>Ptr2 );
               Ptr := Ptr2;
            when others =>
               exit KONKATENACJA;
         end case;
      end loop KONKATENACJA;


      --
      -- Znalezienie w tej konkatenacji wezla takiego symbolu,
      -- ktory na pewno wystapi. Jemu trzeba ustawic CzlAlt.
      --

      Ptr := Pierwszy_Pewny(G.all);

      if DEBUG_MODE then
         Put_Line("Pierwszy pewny: " & To_String(Ptr.all.KSym));
      -- Pokaz_Graf(Standard_Output, Znajdz_Symb_Poczatkowy(To_Unbounded_String("Sigma")));
      end if;


      if Ptr.Typ /= Zwykly then
      -- Put_Line("Uwaga: Jakas dziwna produkcja!");
         null;
         --
         -- Jakas dziwna/niedobra produkcja,
         -- bo jest ustawiany CzlAlt wêz³owi pustemu.
         --
      end if;
      Ptr.CzlAlt := Alt;

   end Nieterminalny_Skladnik;


   --
   -- <Wyrazenie>  ::=  <Skladnik> { "|" <W> <Skladnik> }
   --
   procedure Nieterminalny_Wyrazenie(
      G    : in Wezel_P2;
      Nast : in Wezel_Ptr := null;  -- na ten adres ustawiane sa pola
                                    -- Nast wszystkich ostatnich symboli,
                                    -- ktore mozna wyprowadzic z tego
                                    -- wyrazenia
      Alt  : in Wezel_Ptr := null )
   is
      Ptr   : Wezel_Ptr;
      Ptr2  : Wezel_Ptr;
   begin
      DEBUG("--- Nieterminalny_Wyrazenie ---");

      Ptr := NowyWezel(
         KSym   => To_Unbounded_String("PUSTY (Po Alt)"),
         Typ    => Po_Alternatywny,
         FC     => 0);

      Nieterminalny_Skladnik( G, Nast=>Nast, Alt=>Ptr );

      while SYMBOL = '|' loop

         Ptr2 := NowyWezel(
            KSym   => To_Unbounded_String("PUSTY (Po Alt)"),
            FC     => 0,
            Typ    => Po_Alternatywny);

         Pobierz_Symbol;
         Nieterminalny_Whitespace;
         Nieterminalny_Skladnik(
            Ptr.all.Nast'Access,
            Nast=>Nast,
            Alt=>Ptr2);

         Ptr := Ptr2;

      end loop;

      Ptr.all.Nast := Alt;

      if Ptr.all.Nast /= null then
         Ptr.all.Nast.FC := Ptr.all.Nast.FC + 1;
      end if;

   end Nieterminalny_Wyrazenie;


   --
   -- <Prod>       ::=  <W> "<" <Ident> ">" <W> "::=" <W> <Wyrazenie>
   --
   procedure Nieterminalny_Prod is
      P             : Wezel_Pocz_Ptr;
   begin
      DEBUG("--- Nieterminalny_Prod ---");

      Nieterminalny_Whitespace;

      if SYMBOL /= '<' then
         SetBlad("<");
         raise NAPIS_NIE_NALEZY;
      end if;
      Pobierz_Symbol;


      p := Znajdz_Symb_Poczatkowy( Nieterminalny_Ident );

      if SYMBOL /= '>' then
         SetBlad(">");
         raise NAPIS_NIE_NALEZY;
      end if;
      Pobierz_Symbol;

      Nieterminalny_Whitespace;

      --
      -- Symbol ,,jest okreslone jako''
      --

      if SYMBOL /= ':' then
         SetBlad(":");
         raise NAPIS_NIE_NALEZY;
      end if;
      Pobierz_Symbol;
      if SYMBOL /= ':' then
         SetBlad(":");
         raise NAPIS_NIE_NALEZY;
      end if;
      Pobierz_Symbol;
      if SYMBOL /= '=' then
         SetBlad("=");
         raise NAPIS_NIE_NALEZY;
      end if;
      Pobierz_Symbol;

      Nieterminalny_Whitespace;

      Nieterminalny_Wyrazenie( p.Wyj'Access );

      Nieterminalny_Whitespace;

   end Nieterminalny_Prod;


   --
   -- <Gramatyka>    ::= <Prod> <Reszta>
   --
   procedure Nieterminalny_Gramatyka is
   begin

      Nieterminalny_Prod;
      Nieterminalny_Reszta;

   end Nieterminalny_Gramatyka;


   --
   -- <Reszta>       ::= [ <NowaLinia> ( <Gramatyka> | PUSTE ) ]
   --
   procedure Nieterminalny_Reszta is
   begin

      if SYMBOL = Character'Val(10) or SYMBOL = Character'Val(13) then

         Nieterminalny_NowaLinia;

         if SYMBOL = '<' or SYMBOL = ' ' or SYMBOL = ASCII.HT then
            Nieterminalny_Gramatyka;
         end if;

      end if;

   end Nieterminalny_Reszta;



   --
   -- <Sigma>      ::=  <Gramatyka> [ '.' <NowaLinia> ]
   --
   procedure Sigma is
   begin

      Nieterminalny_Gramatyka;

      if SYMBOL /= Character'Val(0) then
         if SYMBOL = '.' then
            Pobierz_Symbol;
            Nieterminalny_NowaLinia;
         else
            SetBlad(".");
            raise NAPIS_NIE_NALEZY;
         end if;
      end if;

   end Sigma;



   -------------------------------------------------------------------
   -------------------------------------------------------------------



   --
   -- Procedura do sprawdzania, czy wszystkie symbole poczatkowe,
   -- wystepujace w tej gramatyce, zostaly zdefiniowane.
   --
   function Wszystkie_Pomocnicze_Zdefiniowane return Boolean is
      Ptr   : Wezel_Pocz_Ptr;
   begin
      Ptr := LISTA_Wezlow;

      while Ptr /= null loop
         if Ptr.Wyj = null then
            Put_Line("B³±d w podanej specyfikacji BNF:");
            Put("Nie zosta³ zdefiniowany symbol nieterminalny:");
            Put(ASCII.HT);
            Put_Line(To_String(Ptr.PSym));
            return FALSE;
         end if;
         Ptr := Ptr.Next;
      end loop;

      return TRUE;

   end Wszystkie_Pomocnicze_Zdefiniowane;



   -------------------------------------------------
   -- Procedura do wyswietlania grafu dla symbolu --
   -- pomocniczego S                              --
   -------------------------------------------------
   procedure Pokaz_Graf(
      Wyjscie   : in Ada.Text_Io.File_Type;   -- miejsce, w ktore wyswietlac
      S         : in Wezel_Pocz_Ptr)          -- wskaznik na wezel poczatkowy
   is


      --
      -- Pomocnicza procedura do ladnego wyswietlania podanej liczby
      -- w systemie szesnastkowym na odpowiedniej ilosci cyfr
      --
      procedure PutHex(
         Wyjscie   : Ada.Text_Io.File_Type;
         Num       : in Natural)
      is
         Len   : Positive;   -- Dlugosc lancucha
         Pad   : Natural;    -- Ilosc poczatkowych zer
      begin
         if Num < 4096 then
            -- Wyswietlac na trzech cyfrach
            Len := 3 + 4;
         elsif Float(Num) < 16.0E6 then
            -- Wyswietlac na szesciu cyfrach
            Len := 6 + 4;
         else
            -- Wyswietlac na dziesieciu cyfrach
            Len := 10 + 4;
         end if;

         declare
            Buf : String(1 .. Len);
         begin
            Put(Buf, Num, 16);
            Put(Wyjscie, "0x");

            Pad := Natural(Len - 4 - (Buf'Last - (Index(Buf, "#")+ 1)));

            Put(Wyjscie, Pad * "0" );

            Put(Wyjscie, Buf( Index(Buf, "#")+1 ..  Buf'Last-1 ) );
         exception
            when LAYOUT_ERROR =>
               --
               -- Bufor byl za krotki na taka ilosc cyfr (?!)
               -- W takim razie wyswietlmy jakos standardowo (brzydko)
               --
               Put(Wyjscie, Num, 1, 16);

               DEBUG("[PutHex]: Layout error.");
         end;
      end PutHex;


      --
      -- Rekurencyjna procedura wyswietlajaca
      -- pojedynczy wezel grafu
      --
      procedure Pokaz_Wezel(
         W   : in Wezel_Ptr;   -- wskaznik na wezel do wyswietlenia
         Ind : Natural := 3)   -- Wciecie (indent) od brzegu ekranu
      is
         FullLen, StrLen, Pad : Integer;
         NewInd               : Integer := Ind;  -- Nowa wartosc wciecia
      begin

         W.Licznik := W.Licznik + 1;

         --
         -- Przygotowanie informacji o tym, co bedzie
         -- trzeba wyswietlic (dlugosci lancuchow, wciec, ..)
         --
         if W.RefSym = null then

            -- Jest to zwykly wezel symbolu koncowego
            if W.Ksym = EPSILON then
               W.Ksym := To_Unbounded_String("PUSTY");
            end if;
            StrLen := Length(W.KSym) + 5;

         else
            StrLen := Length(W.RefSym.PSym) + 5;
         end if;
         FullLen := 9;
         if StrLen > 9 then
            FullLen := StrLen;
         end if;
         Pad := ( FullLen - StrLen ) / 2;


         -- Identyfikator wezla w pierwszej kolumnie
         PutHex(Wyjscie, W.Id);


         --
         -- Wyswietlenie pierwszego wiersza wezla grafu
         -- (co to za symbol, itp).
         --
         Put(Wyjscie, (Ind-5) * " ");
         Put(Wyjscie, "|");
         Put(Wyjscie, Pad * " ");
         if W.RefSym = null then
            Put(Wyjscie, "[T]: " & To_String(W.Ksym) );
         else
            Put(Wyjscie, "[R]: " & To_String(W.RefSym.Psym) );
         end if;
         Pad := FullLen - StrLen - Pad;
         if Pad < 0 then
            Pad := 0;
         end if;
         Put(Wyjscie, Pad * " ");
         Put(Wyjscie, "|");

         if DEBUG_MODE then
            -- Wyswietlenie przy kazdym wezle wartosci
            -- Frequency Counter (diagnostyka)
            Put(Wyjscie, W.FC,1);
         end if;

         New_Line(Wyjscie);


         --
         -- Przygotowanie do wyswietlenia drugiego wiersza wezla.
         -- Ustalenie wciec, dlugosci lancuchow, odstepow.
         --
         Put(Wyjscie, Ind * " ");
         Put(Wyjscie, "|");
         StrLen := FullLen / 2;
         Pad := ( StrLen - 3 ) / 2;
         NewInd := NewInd + 1 + Pad + 3;
         Put(Wyjscie, Pad * "_");
         Put(Wyjscie, "Alt");
         Pad := StrLen - Pad - 3;
         Put(Wyjscie, Pad * "_");
         Put(Wyjscie, "|");
         NewInd := NewInd + Pad + 1;
         StrLen := FullLen - StrLen - 1;
         Pad := ( StrLen - 4 ) / 2;
         Put(Wyjscie, Pad * "_");
         NewInd := NewInd + Pad + 2;
         Put(Wyjscie, "Nast");
         Pad := StrLen - Pad - 4;
         Put(Wyjscie, Pad * "_");
         Put(Wyjscie, "|");
         New_Line(Wyjscie);

         Put(Wyjscie, (NewInd+2) * " ");
         Put(Wyjscie, "|");
         New_Line(Wyjscie);

         --
         -- Pokazanie informacji o podwezlach
         --

         if W.Nast = null then
            -- Ten wezel nie ma nastepnika
            Put(Wyjscie, (NewInd) * " ");
            Put_Line(Wyjscie, "NULL");
            New_Line(Wyjscie);
         else
            -- rekurencyjne wejscie w nastepny wezel grafu

            if W.Nast.Licznik >= W.Licznik then
               --
               -- Ten wezel byl juz wyswietlony, wiec pora wracac!
               -- (inaczej moznaby sie zapetlic)
               --

               -- Wyswietlenie informacji o identyfikatorze
               -- wezla wskazywanego przez parametr procedury
               Put(Wyjscie, (NewInd-1) * " ");
               Put(Wyjscie, "(");
               PutHex(Wyjscie, W.Nast.Id);
               Put(Wyjscie, ")");
               New_Line(Wyjscie);
               New_Line(Wyjscie);

            else

               Pokaz_Wezel(W.Nast, NewInd - 3);
            end if;

         end if;

         if W.CzlAlt /= null then

            if W.CzlAlt.Licznik >= W.Licznik then
               Put(Wyjscie, Ind * " ");
               Put(Wyjscie, "(");
               PutHex(Wyjscie, W.CzlAlt.Id);
               Put(Wyjscie, ")");
               New_Line(Wyjscie);
               New_Line(Wyjscie);
            else
               -- rekurencyjne wejscie w wezel czlonu alternatywnego
               Pokaz_Wezel(W.CzlAlt, Ind);
            end if;

         end if;

      end Pokaz_Wezel;

   begin

      Put_Line(Wyjscie, "        [[N]: " & To_String(S.PSym) & "]");
      Put_Line(Wyjscie, "            |");

      Pokaz_Wezel( S.Wyj, 7 );

   end Pokaz_Graf;


   --------------------------------------------------------
   --           Procedura optymalizujaca graf            --
   --------------------------------------------------------
   procedure Optymalizuj_Graf(Lista : in Wezel_Pocz_Ptr) is

      --
      -- Optymalizacja pojedynczej produkcji:
      -- Usuwanie tymczasowych wezlow pomocniczych,
      -- skracanie polaczen miedzy wezlami grafu
      --
      procedure Optymalizuj_Produkcje(Prod : in Wezel_Pocz_Ptr) is

         function Przejdz( Ptr : in Wezel_Ptr ) return Wezel_Ptr is
            Tmp : Wezel_Ptr;
         begin

            if not Ptr.all.Optymal then

               Ptr.all.Optymal := TRUE;

               if Ptr.Nast /= null then
                  Ptr.Nast := Przejdz( Ptr.all.Nast );
               end if;
               if Ptr.CzlAlt /= null then
                  Ptr.CzlAlt := Przejdz( Ptr.all.Czlalt );
               end if;

            end if;

            if DEBUG_MODE then
               Put(Ptr.all.Id,1);
               New_Line;
            end if;

            --
            -- Usuniecie tymczasowych wezlow pomocniczych
            -- dla produkcji - konkatenacji
            --
            if Ptr.Typ = Konkat and Ptr.FC <= 1 then
               Tmp := Ptr.Nast;
               Zwolnij_Wezel( Ptr );
               return Tmp;
            end if;

            -- Odpowiednie zajecie sie wezlami, na ktore wskazuje
            -- duza liczba innych wezlow
            if Ptr.Typ = Konkat and Ptr.FC > 1 then
               Ptr.FC := Ptr.FC - 1;
               return Ptr.Nast;
            end if;


            --
            -- Pozbycie sie wezlow pomocniczych
            -- dla produkcji alternatywnych
            --
            if Ptr.Typ = Po_Alternatywny and Ptr.FC <= 1 then
               Tmp := Ptr.Nast;
               Zwolnij_Wezel( Ptr );
               return Tmp;
            end if;

            --
            -- Optymalizacja produkcji iteracyjnych
            --
            if Ptr.Typ = Przed_Iteracyjny and Ptr.FC > 1 then
               Ptr.FC := Ptr.FC - 1;
               return Ptr.Nast;
            end if;

            if Ptr.typ = Przed_Iteracyjny and Ptr.FC <= 1 then
               Tmp := Ptr.Nast;
               Zwolnij_Wezel( Ptr );
               return Tmp;
            end if;

            return Ptr;
         end Przejdz;

      begin -- (procedura Optymalizuj_Produkcje)
         if DEBUG_MODE then
            Put("Optymalizacja grafu symbolu nieterminalnego ");
            Put(To_String(Prod.Psym) & ".." & ASCII.HT);
         else
            Put(".");
         end if;

         Prod.Wyj := Przejdz( Prod.Wyj );

         DEBUG("OK!");
      end Optymalizuj_Produkcje;

      Ptr : Wezel_Pocz_Ptr;

   begin -- (procedura Optymalizuj_Graf)
      Put("Optymalizacja Grafu.");

      if DEBUG_MODE then
         New_Line;
      end if;

      Ptr := Lista;
      while Ptr /= null loop
         Optymalizuj_Produkcje(Ptr);
         Ptr := Ptr.Next;
      end loop;

      if not DEBUG_MODE then
         Put_Line(" OK!");
      end if;

   end Optymalizuj_Graf;



   --------------------------------------------------------------------
   -- Procedura do sprawdzania (na podstawie grafu), czy jakis napis --
   -- nalezy do jezyka generowanego przez wczytana gramatyke         --
   --------------------------------------------------------------------
   procedure Sprawdz_Zdanie(
      Batch  : in Boolean := FALSE;
      NazwaP : in String  := "") is

      SYMBOL        : Character;

      PLIK_NAPIS    : Ada.Text_IO.File_Type;

      --
      -- Wczytywanie pojedynczych jednostek leksykalnych,
      -- ktore beda poddawane analizie syntaktycznej
      --
      --------------------------------------------------------------------
      -- Przy wyszukanej analizie leksykalnej modul tokenizera powinien --
      -- definiowac tu swoja, odpowiednia wersje tej procedury          --
      --------------------------------------------------------------------
      --
      procedure Pobierz_Symbol is
         Znak : Character;
         EOL  : Boolean;
      begin
         if Zrodlo_Zdania = Plik then
            if End_Of_File(PLIK_NAPIS) then
               Znak := Character'Val(0);
            else
               Get_Immediate(PLIK_NAPIS, Znak);

            end if;
         elsif Zrodlo_Zdania = Klawiatura then
            Look_Ahead(Znak, EOL);
            if EOL then
               Znak := Character'Val(0);
            else
               Get(Znak);
            end if;
         end if;

         if Znak = ASCII.LF then
            Kolumna_Pliku := Kolumna_Pliku + 1;
         else
            Kolumna_Pliku := Kolumna_Pliku + 1;
         end if;

         if DEBUG_MODE then
            Put("Pobralem symbol: ");
            Put(Znak);
            New_Line;
         end if;

         SYMBOL := Znak;

      exception
         when Ada.Text_IO.USE_ERROR =>
            Put_Line("Blad dostepu do pliku.");
         when Ada.Text_IO.DEVICE_ERROR =>
            Put_Line("Blad (device error) podczas proby odczytania z pliku.");
         when Ada.Text_IO.END_ERROR =>
            Put("No nie wierze..... Wyjatek END_ERROR. ");
            Put_Line("To nie moglo nastapic... (?!?).");
            Put_Line("Niespodziewany koniec pliku.");
            New_Line;
      end Pobierz_Symbol;



      ------------------------------------------------------------------
      ------------------------------------------------------------------
      -- Podprogram:                                                  --
      --  procedura analizatora ogolnego, sterowana struktura grafowa --
      ------------------------------------------------------------------
      ------------------------------------------------------------------

      procedure Analizuj(
         Cel    :   in Wezel_Pocz_Ptr;
         Jest   :   in out Boolean)
      is

         Ptr    : Wezel_Ptr;

         --
         -- Przeladowana funkcja operatorowa do porownywania symboli
         -- terminalnych z aktualnie wczytanym symbolem
         --
         -- XXX
         -- Jednostkami leksykalnymi s± w zasadzie pojedyncze znaki,
         -- jednak z drugiej strony dobrze jest móc trzymaæ w wê¼le
         -- jaki¶ ³añcuch i móc porównaæ, czy pasuje.
         -- To ma zady i walety.
         -- Mo¿na sprawdziæ pasowanie symboli-³añcuchów, ale wtedy nie
         -- zadzia³a sprawdzanie determinizmu i ustalanie miejsca, od
         -- którego napis nie nale¿y do jêzyka.
         --
         function "="(
            Symb1 : Unbounded_String;      -- Symbol terminalny
            Symb2 : Character              -- Wczytany, pojedynczy znak
            ) return Boolean is

            Znak  : Character;
            EOL   : Boolean;
         begin

            if DEBUG_MODE then
               Put("Sprawdzam, czy pasuje znak ");
               Put(Symb2);
               Put(" do symbolu ");
               Put(To_String(Symb1));
               New_Line;
            end if;

            if Length(Symb1) = 0 then
               return FALSE;
            -- Lancuch o dlugosc 1 mozna porownac tak po prostu...
            elsif Length(Symb1) = 1 then
               if Element(Symb1,1) = Symb2 then
                  return TRUE;
               else
                  return FALSE;
               end if;
            end if;

            if Element(Symb1,1) /= Symb2 then
               return FALSE;
            end if;

            for N in 2 .. Length(Symb1) loop

               if Zrodlo_Zdania = Plik then
                  Look_Ahead(PLIK_NAPIS, Znak, EOL);
               elsif Zrodlo_Zdania = Klawiatura then
                  Look_Ahead(Znak, EOL);
                  if EOL then
                     Znak := ASCII.LF;
                  end if;
               end if;

               if EOL or else Element(Symb1,N) /= Znak then
                  Kolumna_Pliku := Kolumna_Pliku + 1;
                  SYMBOL        := Znak;
                  return FALSE;
               end if;
               Pobierz_Symbol;
            end loop;

            return TRUE;
         end "=";


      begin -- (procedura Analizuj)

         Ptr := Cel.Wyj;

         loop
            if Ptr.RefSym = null then
               if Ptr.Typ = Zwykly and then Ptr.Ksym = Symbol then

                  DEBUG("Symbol przypasowal.");
                  if Symbol = ASCII.LF then
                     Linia_Pliku := Linia_Pliku + 1;
                     Kolumna_Pliku := 0;
                  end if;

                  Jest := TRUE;
                  Pobierz_Symbol;
               else

                  DEBUG("Symbol NIE pasowal.");

                  if Ptr.Typ /= Zwykly then
                     Jest := TRUE;
                  else
                     Jest := FALSE;
                  end if;
               end if;
            else
               Analizuj(Ptr.RefSym, Jest);
            end if;
            if Jest then
               Ptr := Ptr.Nast;
            else
               Ptr := Ptr.Czlalt;
            end if;
            exit when Ptr = null;
         end loop;

      end Analizuj;


      Buf          : String(1 .. PATH_MAXLEN);  -- bufor dla nazwy pliku
      Ost          : Natural;

      Znak         : Character;
      Czy_Nalezy   : Boolean;

   begin -- (procedura Sprawdz_Zdanie)
      if not Batch then
         New_Line;
         Put_Line("Sprawdzanie przynale¿no¶ci zdania do jêzyka, generowanego przez");
         Put("gramatykê, ");
         if Zrodlo_Gramatyki = Klawiatura then
            Put_Line("ktora podales z klawiatury...");
         elsif Zrodlo_Gramatyki = Plik then
            Put_Line("ktora wczytalem z pliku...");
         end if;
      end if;
      New_Line;

      --
      -- Ustalenie, skad wczytywac zdanie do analizy
      --

      if Batch then
         Zrodlo_Zdania := Plik;
      else
         Put_Line("Skad wczytac zdanie, ktore bede sprawdzal dla tej gramatyki?");
         Put_Line("  [K]  - klawiatura");
         Put_Line("  [P]  - plik na dysku");
         loop
            Get_Immediate(Znak);
            New_Line;
            Znak := To_Lower(Znak);
            if Znak = 'k' then
               Zrodlo_Zdania := Klawiatura;
               Put_Line("Wprowadz zdanie...");
               exit;
            elsif Znak = 'p' then
               Zrodlo_Zdania := Plik;
               exit;
            end if;
            Put("Skad wczytac? [K/P]");
         end loop;
      end if;

      if Zrodlo_Zdania = Plik then

         if not Batch then
            loop
               begin
                  Put_Line("Nazwa pliku, zawierajacego napis do analizy:");
                  Get_Line(Buf, Ost);

                  if Ost /= 0 then
                     Open( PLIK_NAPIS, In_File, Buf(1 .. Ost) );
                     exit;
                  end if;

               exception
                  when Ada.Text_IO.NAME_ERROR =>
                     Put_Line("Blad: Taki plik nie istnieje.");
                     New_Line;
                  when Ada.Text_IO.USE_ERROR | Ada.Text_IO.DEVICE_ERROR =>
                     Put_Line("Blad przy probie dostepu do tego pliku!");
                     New_Line;
                  when Others =>
                     Put_Line("Nierozpoznany blad przy probie dostepu.");
                     New_Line;
               end;
            end loop;

         else
            -- Tryb nieinteraktywny
            begin
               Open( PLIK_NAPIS, In_File, NazwaP );
            exception
               when Ada.Text_IO.NAME_ERROR =>
                  Put_Line("Blad: Taki plik jaki podales po -s nie istnieje.");
                  GNAT.OS_Lib.OS_Exit(1);
               when Ada.Text_IO.USE_ERROR | Ada.Text_IO.DEVICE_ERROR =>
                  Put_Line("Blad przy probie dostepu do tego pliku po -s !");
                  GNAT.OS_Lib.OS_Exit(1);
               when Others =>
                  Put_Line("Nierozpoznany blad przy probie dostepu do pliku");
                  GNAT.OS_Lib.OS_Exit(1);
            end;
         end if;

      end if;

      --
      -- Zresetowanie licznikow znakow
      --
      Linia_Pliku   := 1;
      Kolumna_Pliku := 0;


      Pobierz_Symbol;

      Analizuj(
         Znajdz_Symb_Poczatkowy(To_Unbounded_String("Sigma")),
         Czy_Nalezy);


      if Czy_Nalezy then
         --
         -- Analizator ogolny znalazl ciag symboli o pewnej
         -- dlugosci, ktore naleza do jezyka, ale trzeba jeszcze
         -- sprawdzic, co wystepuje po tych symbolach.
         --

         if SYMBOL /= Character'Val(0) then
            Czy_Nalezy := FALSE;
            DEBUG("po napisie cos nastepuje, wiec nie nalezy");
         end if;

         if Czy_Nalezy then
            Put_Line("TAK. NAPIS NALEZY do jezyka.");
         end if;

      end if;
      -- Tu nie moze byc ELSE.
      if not Czy_Nalezy then

         --
         -- Napis nie nalezy do jezyka
         --

         New_Line;
         Put_Line("Ten napis NIE nale¿y do jêzyka, generowanego przez gramatykê.");
         Put_Line("Ustali³em to, analizuj±c znak w po³o¿eniu:");
         Put("Linia:    ");
         Put(Linia_Pliku, 1);
         New_Line;
         Put("Kolumna:  ");
         Put(Kolumna_Pliku, 1);
         New_Line;

         Put("Znak, który siê tam znajduje, to:   ");

         case SYMBOL is
            when ' ' =>
               Put("Spacja  [Kod:  32]");
            when ASCII.HT =>
               Put("Tabulator  [Kod:   9]");
            when ASCII.LF =>
               Put("Nowa linia [Kod:  10]");
            when ASCII.CR =>
               Put("Cariage return (nowa linia) [Kod:  13]");
            when Character'Val(0) =>
               Put("Koniec napisu");
            when Others =>
               Put(SYMBOL);
               Put("       [Kod: ");
               Put(Character'Pos(SYMBOL),3);
               Put("]");
         end case;

         New_Line;
         if Batch then
            if Is_Open(PLIK_NAPIS) then
               close(PLIK_NAPIS);
            end if;
            GNAT.OS_Lib.OS_Exit(3);
         end if;
      end if;


      if Zrodlo_Zdania = Klawiatura then
         Skip_Line;
      elsif Zrodlo_Zdania = Plik  then
         Close(PLIK_NAPIS);
      end if;

      if not Batch then
         Put_Line("---- Nacisnij dowolny klawisz, aby kontynuowac ----");
         Get_Immediate(Znak); New_Line;
      end if;

   end Sprawdz_Zdanie;


   -----------------------------------------------------------
   -- Procedura do zmieniania aktualnie wczytanej gramatyki --
   -----------------------------------------------------------
   procedure Zmiana_Gramatyki is
      Znak        : Character;
      P           : Wezel_Pocz_Ptr;
      Jest_Sigma  : Boolean := FALSE;
   begin

      POBRANIE_GRAMATYKI:
      loop

         if LISTA_Wezlow /= null then
            --
            -- Dealokacja pamieci, przydzielonej na graf,
            -- wczytanej uprzednio gramatyki
            --
            Zwolnij_Gramatyke( LISTA_Wezlow );
         end if;

         New_Line;
         Put_Line("Musze poznac zbior regul produkcji dla gramatyki.");
         Put_Line("Skad mam go wczytac?");
         Put_Line("  [K]  - klawiatura");
         Put_Line("  [P]  - plik na dysku");
         loop
            Get_Immediate(Znak);
            New_Line;
            Znak := To_Lower(Znak);
            if Znak = 'k' then
               New_Line;
               Zrodlo_Gramatyki := Klawiatura;
               exit;
            elsif Znak = 'p' then
               Zrodlo_Gramatyki := Plik;
               exit;
            end if;
            Put("Skad mam wczytac? [K/P] ");
         end loop;

         Linia_Pliku   := 1;
         Kolumna_Pliku := 0;

         --
         -- Przygotowanie pliku
         --
         if Zrodlo_Gramatyki = Plik then

            declare
               Buf   : String(1 .. PATH_MAXLEN);
               Ost   : Natural;
            begin

               USTALANIE_NAZWY:
               loop
                  begin
                     Put_Line("Nazwa pliku zawierajacego gramatyke:");
                     Get_Line(Buf, Ost);

                     if Ost > 0 then
                        Open( PLIK_GRAMATYKA, In_File, Buf(1 .. Ost) );
                        exit USTALANIE_NAZWY;
                     end if;
                  exception
                     when Ada.Text_Io.STATUS_ERROR =>
                        Put_Line("status error");
                        New_Line;
                     when Ada.Text_Io.NAME_ERROR =>
                        Put_Line("Blad: Plik o takiej nazwie nie istnieje.");
                        New_Line;
                     when Ada.Text_Io.DEVICE_ERROR | Ada.Text_Io.USE_ERROR =>
                        Put_Line("Blad dostepu do podanego pliku.");
                        New_Line;
                  end;
               end loop USTALANIE_NAZWY;

            end;

         elsif Zrodlo_Gramatyki = Klawiatura then
            Put_Line("OK. Wpisz zbior regul produkcji...");
         end if;

         begin

            --
            -- Analiza syntaktyczna podanego zbioru regul produkcji
            -- i jednoczesna translacja na struktury grafowe
            --
            Pobierz_Symbol;
            Sigma;

            if Zrodlo_Gramatyki = Plik then
               Close(PLIK_GRAMATYKA);
            end if;

            Put_Line("Udalo sie pomyslnie wytworzyc graf.");

            --
            -- Sprawdzenie, czy zdefiniowane wszystkie symbole pomocnicze
            --
            if Wszystkie_Pomocnicze_Zdefiniowane then

               --
               -- Optymalizacja grafu
               --
               Optymalizuj_Graf(LISTA_Wezlow);

               --
               -- Sprawdzenie, czy w zbiorze produkcji
               -- jest wyznaczony aksjomat gramatyki.
               --
               P := LISTA_Wezlow;
               while P /= null loop
                  if P.PSym = "Sigma" then
                     Jest_Sigma := TRUE;
                     exit;
                  end if;
                  P := P.Next;
               end loop;

               if not Jest_Sigma then
                  Put("B³±d: ");
                  Put_Line("W podanym zbiorze produkcji nie figuruje aksjomat gramatyki!");
                  Put_Line("Nie zosta³a okre¶lona produkcja dla symbolu <Sigma>.");
                  New_Line;
               else
                  exit POBRANIE_GRAMATYKI;
               end if;

            end if;

         exception

            --
            -- Obsluga sytuacji wyjatkowych
            --

            when Ada.Text_Io.DEVICE_ERROR |
                    Ada.Text_Io.USE_ERROR |
                 Ada.Text_Io.STATUS_ERROR =>

               Put_Line("Blad podczas proby wczytywania z pliku.");

               if Zrodlo_Gramatyki = Plik and Is_Open(PLIK_GRAMATYKA) then
                  Close(PLIK_GRAMATYKA);
               end if;
               New_Line;

            when NAPIS_NIE_NALEZY =>

               --
               -- W trakcie analizy skladniowej okazalo sie, iz podany
               -- zbior regul produkcji nie jest napisem, nalezacym
               -- oczekiwanej notacji BNF
               --

               if Zrodlo_Gramatyki = Plik and Is_Open(PLIK_GRAMATYKA) then
                  Close(PLIK_GRAMATYKA);
               elsif Zrodlo_Gramatyki = Klawiatura then
                  Skip_Line;
               end if;

               Put("Gramatyka, któr± poda³e¶, nie nale¿y do metajêzyka BNF, ");
               Put_Line("jakiego oczekujê!");
               Put("Problem wystêpuje w linii ");
               Put(Linia_Pliku, 1);
               Put(", w kolumnie ");
               Put(Kolumna_Pliku, 1);
               Put_Line(".");

               --
               -- Wypisanie informacji diagnostycznych
               --

               if Length(Oczekiwany_Znak) = 1 then
                  Put("Spodziewano sie znaku: ");
                  Put(Element(Oczekiwany_Znak,1));
                  Put("       [Kod: ");
                  Put(Character'Pos(Element(Oczekiwany_Znak,1)),3);
                  Put("]");
                  New_Line;
               else
                  Put("Spodziewano sie:       ");
                  Put( To_String(Oczekiwany_Znak) );
                  New_Line;
               end if;

               Put("Znaleziono znak:       ");
               case Znaleziony_Znak is
                  when ' ' =>
                     Put("Spacja  [Kod:  32]");
                  when ASCII.HT =>
                     Put("Tabulator  [Kod:   9]");
                  when ASCII.LF =>
                     Put("Nowa linia [Kod:  10]");
                  when ASCII.CR =>
                     Put("Cariage return (nowa linia) [Kod:  13]");
                  when Character'Val(0) =>
                     Put("Koniec napisu");
                  when Others =>
                     Put(Znaleziony_Znak);
                     Put("       [Kod: ");
                     Put(Character'Pos(Znaleziony_Znak),3);
                     Put("]");
               end case;

               New_Line;
               New_Line;

         end;

      end loop POBRANIE_GRAMATYKI;

      --
      -- Gramatyka wczytana OK, graf zrobiony!
      --

      Put_Line("--- Gramatyka zostala poprawnie wczytana ---");
      Put("Liczba wszystkich wezlow w grafie: " );
      Put(Ilosc_Wezlow, 1);
      New_Line;
      Put_Line("--------------------------------------------");
      New_Line;

   end Zmiana_Gramatyki;


   ---------------------------------------
   -- Procedura do zapisywania schematu --
   -- grafu do zbioru na dysku          --
   ---------------------------------------
   procedure Zapisz_Graf is

      Buf            : String(1 .. PATH_MAXLEN);  -- bufor dla nazwy pliku
      Ost            : Natural;

      Ptr            : Wezel_Pocz_Ptr;
      PLIK_GRAFU     : Ada.Text_IO.File_Type;
      Jest_Taki_Plik : Boolean;

   begin

      USTALENIE_PLIKU:
      loop
         begin
            Put_Line("Podaj nazwe pliku, w ktorym zapisac graf:");
            Get_Line(Buf, Ost);

            if Ost > 0 then
               begin
                  Jest_Taki_Plik := TRUE;
                  Open(PLIK_GRAFU, In_File, Buf(1 .. Ost));

                  --------------------------------------------------
                  -- Program NIE bedzie nadpisywal zadnych plikow --
                  --------------------------------------------------

                  Put_Line("Plik o podanej nazwie istnieje.");
                  Put_Line("Nie bede nadpisywal!");
                  New_Line;
                  Close(PLIK_GRAFU);
               exception
                  when Ada.Text_IO.NAME_ERROR =>
                     Jest_Taki_Plik := FALSE;
               end;

               if not Jest_Taki_Plik then
                  Create(PLIK_GRAFU, Out_File, Buf(1 .. Ost) );
                  exit USTALENIE_PLIKU;
               end if;
            end if;

         exception
            when Ada.Text_IO.DEVICE_ERROR | Ada.Text_IO.USE_ERROR =>
               Put_Line("Blad dostepu do podanego pliku!");
               New_Line;
         end;
      end loop USTALENIE_PLIKU;

      --
      -- Wpisanie poszczegolnych informacji
      --

      Put_Line(PLIK_GRAFU, "Zbior symboli nieterminalnych:");
      Ptr := LISTA_Wezlow;
      while Ptr /= null loop
         Put(PLIK_GRAFU, To_String(Ptr.all.PSym));
         Put(PLIK_GRAFU, " ");
         Ptr := Ptr.Next;
      end loop;

      New_Line(PLIK_GRAFU);
      New_Line(PLIK_GRAFU);

      Put(PLIK_GRAFU, "Ilosc wszystkich wezlow w grafie: ");
      Put(PLIK_GRAFU, Ilosc_Wezlow, 1);
      New_Line(PLIK_GRAFU);
      New_Line(PLIK_GRAFU);


      Ptr := LISTA_Wezlow;
      while Ptr /= null loop
         Put(PLIK_GRAFU, "-------------------" & Length(Ptr.PSym) * "-" );
         New_Line(PLIK_GRAFU);
         Put(PLIK_GRAFU, "Symbol poczatkowy: ");
         Put(PLIK_GRAFU, To_String(Ptr.PSym) );
         New_Line(PLIK_GRAFU);
         Put(PLIK_GRAFU, "-------------------" & Length(Ptr.PSym) * "-" );
         New_Line(PLIK_GRAFU);
         Pokaz_Graf( PLIK_GRAFU, Ptr );
         Ptr := Ptr.Next;
      end loop;

      Close(PLIK_GRAFU);

      Put_Line("Graf zostal pomyslnie zapisany!");

   exception
      when Ada.Text_IO.DEVICE_ERROR | Ada.Text_IO.USE_ERROR =>
         Put_Line("Blad dostepu do pliku podczas proby zapisu.");
         New_Line;
         return;
      when others =>
         Put_Line("Nie rozpoznany blad podczas proby zapisu do pliku.");
         New_Line;
         return;
   end Zapisz_Graf;


   ------------------------------
   --   Menu glowne programu   --
   ------------------------------
   procedure Menu1 is
      Znak : Character;
   begin

      MENU_GLOWNE:
      loop
         Put_Line("------------------------------------------------------------");
         Put_Line("      Translator metajezyka BNF na struktury grafowe,       ");
         Put_Line("        sterujace skladniowym analizatorem ogolnym.         ");
         Put_Line("------------------------------------------------------------");
         New_Line;
         Put_Line("  [S] - sprawdzenie przynaleznosci zdania do jezyka");
         Put_Line("  [G] - wyswietlenie wygenerowanego grafu");
         Put_Line("  [P] - zapisanie schematu grafu do pliku");
         Put_Line("  [Z] - zmiana zbioru regul produkcji");
         Put_Line("  [H] - wyswietlenie pomocy o uzywanym BNF'ie");
         Put_Line("  [K] - zakonczenie programu");
         Put_Line("  [1] - Sprawdzenie reguly1");
         Put_Line("  [2] - Sprawdzenie reguly2");
         New_Line;

         Put("Co wybierasz? ");

         PYTANIE:
         loop
            Get_Immediate(Znak);
            New_Line;
            Znak := To_Lower(Znak);
            case Znak is
               --
               -- Wykonanie dzialania w zaleznosci od wcisnietego klawisza
               --

               when 's' =>
                  Sprawdz_Zdanie(Batch => FALSE);
                  exit PYTANIE;
               when 'g' =>
                  declare
                     Ptr : Wezel_Pocz_Ptr;
                  begin
                     Put("Ilosc wszystkich wezlow w grafie: ");
                     Put(Ilosc_Wezlow, 1);
                     New_Line;
                     New_Line;
                     Ptr := LISTA_Wezlow;
                     while Ptr /= null loop
                        Put("Symbol nieterminalny: ");
                        Put_Line(To_String(Ptr.PSym));
                        New_Line;
                        Resetuj_Liczniki;
                        Pokaz_Graf(Standard_Output, Ptr);
                        Put_Line("---");
                        Ptr := Ptr.Next;
                     end loop;
                  end;

                  Put_Line("---- Nacisnij dowolny klawisz, aby kontynuowac ----");
                  Get_Immediate(Znak); New_Line;
                  exit PYTANIE;

               when 'z' =>
                  Zmiana_Gramatyki;
                  exit PYTANIE;
               when '?' =>
                  exit PYTANIE;
               when 'p' =>
                  Zapisz_Graf;
                  exit PYTANIE;
               when 'k' =>
                  GNAT.OS_Lib.OS_Exit(0);
               when '1' =>

                  Determinizm.Sprawdz_Regule1;

                  exit PYTANIE;
               when '2' =>

                  Determinizm.Sprawdz_Regule2;

                  exit PYTANIE;
               when 'h' =>
                  for N in 1 .. 10 loop
                     New_Line;
                  end loop;
                  Pokaz_Pomoc;
                  Put_Line("---- Nacisnij dowolny klawisz, aby kontynuowac ----");
                  Get_Immediate(Znak); New_Line;
                  exit PYTANIE;
               when others =>
                  null;
            end case;
            New_Line;
            Put("Co wybierasz? ");
         end loop PYTANIE;

      end loop MENU_GLOWNE;

   end Menu1;

begin -- (procedure analyseBNF)

   -- Cialo glownej jednostki programowej


   declare
      Use Ada.Command_Line;

      p              : Wezel_Pocz_Ptr;
      N              : Integer;
      Jest_Sigma     : Boolean := FALSE;
      Jest_Gramatyka : Boolean := FALSE;
   begin
      if Argument_Count > 0 then

         --
         -- Dzia³anie w trybie nieinteraktywnym.
         -- Sprawdzanie argumentów linii poleceñ.
         --
         -- To mog³oby byæ znacznie lepiej robione,
         -- a najlepiej jako¶ w stylu getopt(3).
         --

         N := 1;
         while N <= Argument_Count loop
            if Argument(N) = "-p" then
               if N = Argument_Count then
                  Put_Line("brak nazwy pliku po -p");
                  GNAT.OS_Lib.OS_Exit(1);
               else
                  Zrodlo_Gramatyki := Plik;
                  Jest_Gramatyka   := TRUE;
                  Linia_Pliku      := 1;
                  Kolumna_Pliku    := 0;
                  begin
                     Open( PLIK_GRAMATYKA, In_File, Argument(N+1) );
                  exception
                     when Ada.Text_IO.NAME_ERROR =>
                        Put_Line("Blad: Taki plik jaki podales po -p nie istnieje.");
                        GNAT.OS_Lib.OS_Exit(1);
                     when Ada.Text_IO.USE_ERROR | Ada.Text_IO.DEVICE_ERROR =>
                        Put_Line("Blad przy probie dostepu do tego pliku po -p !");
                        GNAT.OS_Lib.OS_Exit(1);
                     when Others =>
                        Put_Line("Nierozpoznany blad przy probie dostepu do pliku");
                        GNAT.OS_Lib.OS_Exit(1);
                  end;

                  N := N + 1;

                  begin
                     Pobierz_Symbol;
                     Sigma;
                     Close(PLIK_GRAMATYKA);
                     if not Wszystkie_Pomocnicze_Zdefiniowane then
                        New_Line;
                        Put_Line("Nie zdefiniowano wszystkich symboli pomocnicznych.");
                        GNAT.OS_Lib.OS_Exit(4);
                     end if;
                     Optymalizuj_Graf(LISTA_Wezlow);

                     P := LISTA_Wezlow;
                     while P /= null loop
                        if P.PSym = "Sigma" then
                           Jest_Sigma := TRUE;
                           exit;
                        end if;
                        P := P.Next;
                     end loop;

                     if not Jest_Sigma then
                        Put("B³±d: ");
                        Put_Line("W podanym zbiorze produkcji nie figuruje aksjomat gramatyki!");
                        Put_Line("Nie zosta³a okre¶lona produkcja dla symbolu <Sigma>.");
                        New_Line;
                     end if;

                  exception
                     when NAPIS_NIE_NALEZY =>

                        Put("Gramatyka, któr± poda³e¶, nie nale¿y do metajezyka BNF, ");
                        Put_Line("jakiego oczekujê!");
                        Put("Problem wystepuje w linii ");
                        Put(Linia_Pliku, 1);
                        Put(", w kolumnie ");
                        Put(Kolumna_Pliku, 1);
                        Put_Line(".");

                        if Length(Oczekiwany_Znak) = 1 then
                           Put("Spodziewano sie znaku: ");
                           Put(Element(Oczekiwany_Znak,1));
                           Put("       [Kod: ");
                           Put(Character'Pos(Element(Oczekiwany_Znak,1)),3);
                           Put("]");
                           New_Line;
                        else
                           Put("Spodziewano sie:       ");
                           Put( To_String(Oczekiwany_Znak) );
                           New_Line;
                        end if;

                        Put("Znaleziono znak:       ");
                        case Znaleziony_Znak is
                           when ' ' =>
                              Put("Spacja  [Kod:  32]");
                           when ASCII.HT =>
                              Put("Tabulator  [Kod:   9]");
                           when ASCII.LF =>
                              Put("Nowa linia [Kod:  10]");
                           when ASCII.CR =>
                              Put("Cariage return (nowa linia) [Kod:  13]");
                           when Character'Val(0) =>
                              Put("Koniec napisu");
                           when Others =>
                              Put(Znaleziony_Znak);
                              Put("       [Kod: ");
                              Put(Character'Pos(Znaleziony_Znak),3);
                              Put("]");
                        end case;

                        New_Line;

                        GNAT.OS_Lib.OS_Exit(1);
                     when Others =>
                        Put_Line("B³±d podczas wczytywania z pliku.");
                  end;
               end if;

            elsif Argument(N) = "-s" then
               if N = Argument_Count then
                  Put_Line("Brak argumentu po -s");
                  GNAT.OS_Lib.OS_Exit(1);
               elsif not Jest_Gramatyka then
                  Put_Line("Nie wczytano zbioru regu³ produkcji.");
                  GNAT.OS_Lib.OS_Exit(1);
               elsif not Jest_Sigma then
                  GNAT.OS_Lib.OS_Exit(2);
               else
                  Sprawdz_Zdanie(Batch => TRUE, NazwaP => Argument(N+1));
               end if;
            elsif Argument(N) = "-g" then
               if not Jest_Gramatyka then
                  Put_Line("Nie wczytano zbioru regu³ produkcji.");
                  GNAT.OS_Lib.OS_Exit(1);
               end if;
               declare
                  Ptr : Wezel_Pocz_Ptr;
               begin
                  Put("Ilosc wszystkich wezlow w grafie: ");
                  Put(Ilosc_Wezlow, 1);
                  New_Line;
                  New_Line;
                  Ptr := LISTA_Wezlow;
                  while Ptr /= null loop
                     Put("Symbol nieterminalny: ");
                     Put_Line(To_String(Ptr.PSym));
                     New_Line;
                     Resetuj_Liczniki;
                     Pokaz_Graf(Standard_Output, Ptr);
                     Put_Line("---");
                     Ptr := Ptr.Next;
                  end loop;
               end;


            elsif Argument(N) = "-d" then
               if not Jest_Gramatyka then
                  Put_Line("Nie wczytano zbioru regul produkcji.");
                  GNAT.OS_Lib.OS_Exit(1);
               end if;
               Determinizm.Sprawdz_Regule1;
               Determinizm.Sprawdz_Regule2;
               GNAT.OS_Lib.OS_Exit(0);
            end if;

            N := N + 1;

         end loop;

         GNAT.OS_Lib.OS_Exit(0);
      end if;
   end;


   Pokaz_Pomoc;

   Zmiana_Gramatyki;

   Menu1;

   ------------
   -- Koniec --
   ------------

end analyseBNF;


