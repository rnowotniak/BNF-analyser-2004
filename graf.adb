--
-- Copyright (C) 2004   Robert Nowotniak <robercik@toya.net.pl>
--

With Ada.Text_IO;
Use  Ada.Text_IO;

package body Graf is


   --
   -- Funkcja znajduje wêze³ tego symbolu danej produkcji,
   -- który jest pierwszym, który na pewno _musi_ wyst±piæ.
   --
   function Pierwszy_Pewny(
      W  : Wezel_Ptr
      )return Wezel_Ptr is

      p : Wezel_Ptr;

   begin

      ----------------------------------------------------------------------------
      --                                                                        --
      -- XXX To jest zdecydowanie najbardziej niepewny fragment w tym programie --
      --                                                                        --
      ----------------------------------------------------------------------------

      if W.Typ = Zwykly or W.Typ = Opcjonalny then

         if W.CzlAlt = null then
         -- Put_Line("Zwrócê1: " & To_String(W.KSym) );
            return W;
         else -- jest CzlAlt
            p := Pierwszy_Pewny(W.CzlAlt);
            if p.Typ = Zwykly or p.Typ = Opcjonalny or p.Typ = Po_Iteracyjny then
            -- Put_Line("Zwrócê2: " & To_String(p.KSym) );
               return p;
            else
            -- Put_Line("Zwrócê3: " & To_String(W.KSym) );
               return W;
            end if;
         end if;

      else -- Nie jest typu Zwykly

         if W.Nast = null then
         -- Put_Line("Zwrócê4: " & To_String(W.KSym) );
            return W;
         else
            -- jest Nast
            p := Pierwszy_Pewny(W.Nast);
            if p.Typ /= Konkat then
               return p;
            else
               return W;
            end if;
         end if;

      end if;

   end Pierwszy_Pewny;

   procedure Resetuj_Liczniki is
      p   : wsk_w_ptr := Wszystkie_Wezly;
   begin
      while p /= null loop
         p.Wezel.Licznik := 0;
         p := p.Next;
      end loop;
   end Resetuj_Liczniki;

end Graf;

