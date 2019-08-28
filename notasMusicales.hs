module CMusica where

data Nota = Do | Dos | Re | Res | Mi | Fa | Fas | Sol | SolS | La | Las | Si
    deriving Show;

reB::Nota 
reB = Dos

ord::Nota->Int
ord x = case x of {
    Do->1;
    Dos->2;
    Re->3;
    Mi->4;
    Fa->5;
    Fas->6;
    Sol->7;
    SolS->8;
    La->9;
    Las->10;
    Si->11;
}

type Octava = Int
type Sonido=(Nota,Octava)

data Duracion = R | B | N | C | SC | F | SF
    deriving Show;

data Musica = Silencio Duracion | Son Sonido Duracion | Seq Musica Musica | Par Musica Musica

a1::Musica
a1 = Silencio N
a2::Musica
a2 = Son(Mi,3) R
a3::Musica
a3 = Seq a1 a2
a4::Musica
a4 = Son(Mi,2) R
a5::Musica
a5 = Par a3 a4


--transpieza::Int->Musica->Musica
--transpieza n (Silencio d) = Silencio d
--transpieza n (Son s d) = Sen (trans s n) d
--transpieza n (Seq m1 m2) = Seq (transpieza n m1) (transpieza n m2)
--transpieza n (Par m1 m2) = Seq (transpieza n m1) (transpieza n m2)
