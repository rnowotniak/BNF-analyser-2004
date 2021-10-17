--
-- Copyright (C) 2004   Robert Nowotniak <robercik@toya.net.pl>
--

With Ada.Strings.Unbounded;
Use  Ada.Strings.Unbounded;

package Graf is

   subtype Symb_Term    is Unbounded_String;
   subtype Symb_Nieterm is Unbounded_String;

   ------------------------------
   --       Wezly grafu        --
   ------------------------------

   type Wezel_t;
   type Wezel_Ptr is access Wezel_t;

   type Wezel_Pocz_t;
   type Wezel_Pocz_Ptr is access Wezel_Pocz_t;

   --
   -- Typ rekordowy jako wezel grafu symbolu poczatkowego
   --
   type Wezel_Pocz_t is
      record
         -- Nazwa symbolu pomocniczego
         PSym   :  Symb_NieTerm       := To_Unbounded_String(0);
         -- Wskaznik na pierwszy wezel tej produkcji
         Wyj    :  aliased Wezel_Ptr  := null;

         -- Pola czlonu alternatywnego i nastepnego
         CzlAlt :  aliased Wezel_Ptr  := null;
         Nast   :  aliased Wezel_Ptr  := null;

         --
         -- Wskaznik na kolejny symbol ze zbioru
         -- symboli pomocniczych dla tej gramatyki
         --
         Next   :  Wezel_Pocz_Ptr := null;
      end record;

   --
   -- Do konstrukcji struktury grafu sa uzywane m.in. tymczasowe
   -- wezly pomocnicze, ktore ostatecznie moga byc usuniete
   -- w fazie optymalizacji grafu.
   --
   --------------------------------------------------------------------
   -- Czy daloby sie w ogole nie uzywac _zadnych_ tymczasowych wezlow
   -- pomocniczych?
   -- Moim zdaniem - NIE, jesli analizator/translator ma dzialac bez
   -- powtorzen z wyprzedzeniem o jeden symbol
   --------------------------------------------------------------------
   type Typ_Wezla_t is (

      Zwykly,             -- standardowy wezel symbolu koncowego
      Jawnie_Pusty,       -- wezel pusty, narzucony przez gramatyke

                          -- Wezly pomocnicze:
      Po_Alternatywny,    --    wezel ,,scalajacy'' alternatywe
      Przed_Iteracyjny,   --    wezel pomocniczy przed iteracja
      Po_Iteracyjny,      --    obowiazkowy wezel pusty po iteracji
      Opcjonalny,         --    produkcja opcjonalna
      Konkat              --    produkcja konkatenacji

      );


   -- Typ licznika odwolan do wezla
   type Licznik_t is mod Integer'Last;


   --
   -- Definicja rekordu wezla grafu dla symbolu terminalnego
   --
   type Wezel_t is
      record
         -- Symbol
         Ksym    :  Symb_Term;
         -- Pole ze wskaznikiem na ewentualny symbol poczatkowy
         RefSym  :  Wezel_Pocz_Ptr      := null;
         -- Wskaznik na czlon alternatywny
         CzlAlt  :  aliased Wezel_Ptr   := null;
         -- Wskaznik na wezel nastepny
         Nast    :  aliased Wezel_Ptr   := null;

         --
         -- Unikalny identyfikator wezla
         -- Jest to liczba heksadecymalna, co najmniej 3-znakowa
         --
         Id       :  Natural            := 0;

         -- Typ wezla pomocniczego (pustego)
         Typ      :  Typ_Wezla_t        := Zwykly;

         --
         -- Frequency Counter
         --   (licznik wezlow, ktore wskazuja na ten wezel)
         --
         FC       :  Natural            := 1;

         --
         -- Czy aktualny wezel byl poddawany optymalizacji w grafie
         --
         Optymal  :  Boolean            := FALSE;


         --
         -- Licznik czynnosci wykonywanych na wezle
         --
         Licznik  :  Licznik_t            := 0;

      end record;


   --
   -- Definicja typu wskaznika na wskazanie na wezel/rekord grafu.
   --
   type Wezel_P2 is access all Wezel_Ptr;


   type wsk_w;
   type wsk_w_ptr is access wsk_w;

   type wsk_w is
      record
         Wezel      : Wezel_Ptr := null;
         Next       : wsk_w_ptr := null;
      end record;

   LISTA_Wezlow     : Wezel_Pocz_Ptr := null;
   Wszystkie_Wezly  : wsk_w_ptr      := null;

   procedure Resetuj_Liczniki;

   function Pierwszy_Pewny(W  : Wezel_Ptr)return Wezel_Ptr;

end Graf;

