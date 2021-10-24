--
-- Copyright (C) 2004   Robert Nowotniak <rnowotniak@gmail.com>
--

With Graf;
Use  Graf;
With Unchecked_Deallocation;


package Determinizm is

   type Zbior_t;
   type Zbior_Ptr_t is access Zbior_t;

   type Typ_Elementu_t is ( Terminalny, NieTerminalny );

   type Zbior_t is
      record
         Symbol    : Symb_Term;
         Typ       : Typ_Elementu_t := Terminalny;
         Next      : Zbior_Ptr_t    := null;
      end record;

   procedure Sprawdz_Regule1;
   procedure Sprawdz_Regule2;

   procedure Dopisz_Do_Zbioru(
      Zbior    : in out Zbior_Ptr_t;
      Symbol   : in Symb_Term;
      Typ      : in Typ_Elementu_t := Terminalny);

   procedure Usun_Zbior(Zbior: in out Zbior_Ptr_t);

end Determinizm;

