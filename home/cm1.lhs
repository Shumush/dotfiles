Les types en Haskell --

On appelle type l'ensemble de
tous les elements de ce type.

Exemple : le type Entier est l'ensemble
des elements des Entiers

En Haskell, on peut creer des valeurs
de certains types :

5 est la valeur, cinq est un nom

> cinq :: Int
> cinq = 5

Dans cet exemple, cinq est le nom
donne a la valeur 5, de type Int

On peut aussi nommer des valeurs sans
definir leur type

> bonjour = "hello"

Ici, bonjour est le nom donne a la
valeur "hello" qui est de type String

Il existe plusieurs types de base en plus
des nombres :

Les valeurs Vrai/Faux (bouleens)

> vrai = True
> faux = False

> vrai_ou_faux = True || False

Les listes

> un_a_cinq :: [Int]
> un_a_cinq = [1,2,3,4,5]

Les chaines de caracteres

> bonjour :: String
> bonjour  = "hello"

qui sont en fait des listes de caracteres

> bonjour' :: String
> bonjour' = ['h', 'e', 'l', 'l', 'o']

Les tuples

> point :: (Float, Float)
> point = (1.5, 0.8)

les tuples peuvent contenir des valeurs
de types differents

> nomAge :: (String, Int)
> nomAge = ("Arthur", 21)

Les Fonctions --

Les fonctions sont aussi des types !
Le type d'une fonction qui prend
un Int et renvoie un Int s'ecrit

> f :: Int -> Int

> plus1 :: Int -> Int
> plus1 x = x + 1

de maniere generale, pour tout
types 'a' et 'b', une fonction
de 'a' vers 'b' a pour type (a -> b)

Typage Statique --

Un langage Type statiquement est un
langage qui ne permet pas d'utiliser une
valeur d'un type 'b' quand on demande un
type 'a'.

Exemple : la fonction plus1 demande un Int.
Par consequent, on ne peut pas lui donner en
parametre une valeur de type Float.

Erreur !

> a = plus1 1.2

Ok

> a' = plus1 2

Polymorphisme --

Une fonction peut exister en plusieurs exemplaires
par exemple, la fonction identite existe pour tout
type.

> idInt :: Int -> Int
> idInt x = x

> idFloat :: Float -> Float
> idFloat x = x


Pour tout type a, id est la fonction identite

> id :: a -> a
> id x = x

utilise la version Int -> Int

> cinq' = id 5

utilise la version Float -> Float

> monfloat = id 1.5

Fonction d'ordre superieur --

Une fonction d'ordre superieur
est une fonction qui prend en parametre
une fonction

etant donne que Haskell est un langage fonctionnel
une fonction se traite exactement comme une valeur
elle peut donc etre passee en parametre d'une
autre fonction

Cette fonctionnalite, et le polyomrphisme nous
permet de definir la fonction de composition

> f :: B -> C
> g :: A -> B

> f . g :: A -> C

> compose :: (b -> c) -> (a -> b) -> a -> c
> compose f g x = f (g x)


Curry Howard --

Attention ! Le pic technique arrive !!!

Tout a l'heure, on a vu le cas de la fonction
[compose] qui prenait 3 parametres :
- deux fonctions, f et g
- et une valeur, x

Mais en realite :

Toutes les fonctions du monde de Haskell
ne peuvent avoir qu'un parametre. Une fonction
a pour type (a -> b) et c'est tout !!

Comment faire pour creer une "fonction a plusieurs
parametres" comme la fonction [compose] du coup ?

C'est ici que la Correspondance de Curry Howard  arrive.

Tout a l'heure on a vu que les fonctions etaient des
valeurs comme les autres, et qu'elles pouvaient
etres envoyees en parametre. Et bien elles peuvent
aussi etre en valeur de retour d'une fonction !

On peut voir la fleche qui lie deux types comme un
operateur associatif a droite :

> a -> b -> c -> d = a -> (b -> (c -> d))

Ce qui veut dire que la fonction [compose]

dont le type est :

> (b -> c) ->  (a -> b) ->  a -> c

a en fait pour type :

> (b -> c) -> ((a -> b) -> (a -> c))

notez le parenthesage supplementaire par rapport a avant

ca veut dire que [compose] est une fonction qui :

prend un objet de type (b -> c) en parametre
et renvoie un objet de type (a -> b) -> (a -> c)

Ce meme objet de retour, est une fonction qui :

prend un objet de type (a -> b) en parametre
et renvoie un objet de type (a -> c)

qui lui meme est une fonction qui :

prend un objet de type a
et renvoie un objet de type c


Exos:

Donnez le type de cette fonction :

> add3var x y z = x + y + z

Reponse:
> add3var :: Int -> (Int -> (Int -> Int))

Application de fonction --

Pour appliquer une fonction a une valeur,
on ecrit le nom de la fonction, puis la valeur

> id 5
> plus1 9

Pour les fonctions a plusieurs parametres
on ecrit chaque parametre les uns apres les autres
separes par des espaces.

          f  g     x
> compose id plus1 10


Que fait le langage quand on applique une fonction ?

Il ne fait que re-ecrire la valeur du parametre partout
ou il doit, puis, quand la dite fonction ne demande plus
de parametres, il evalue la fonction.

Exemple: 

[plus1] est definie comme ceci : plus1 x = x + 1

Appliquer [plus1] a 5 revient a remplacer [x] par 5

> plus1 5 = 5 + 1


Pour une fonction a plusieurs parametres, on va
effectuer un remplacement parametre par parametre

[compose] est definie comme ceci :
> compose f g x = f (g x)

Si on applique [id], [plus1], puis 5, on substituera d'abord
[f] en [id], puis [g] en [plus1], puis [x] en 5 :


>   compose id           = id (g x)
>  (compose id) plus1    = id (plus1 x)
> ((compose id) plus1) 5 = id (plus1 5)


Une fois qu'une fonction ne demande plus de parametres,
elle est evaluee. Dans le cas de [plus1 5], elle donnera
le resultat 6

Dans le cas de [compose id plus1 5], la fonction donnera

> id (plus1 5)

Cette expression est ensuite elle meme evaluee en:

> id 6

puis en

> 6

En tout, le langage a fait ca :

> compose id plus1 5
> = id (plus1 5)
> = id 6
> = 6


Exos:

Developpez comme dans l'exemple de la fonction [compose]
la fonction [add3var], en sachant que (+) est associatif
a gauche.

Rappel:

> add3var x y z = x + y + z

> (((add3var 1) 2) 3)
> = (((1 + y + z) 2) 3)
> =  ((1 + 2 + z) 3)
> =    (1 + 2) + 3
> =       3    + 3
> =          6

Conditions --

La structure if :

quand on veut choisir selon une condition C, une valeur
entre A ou B, on ecrit ceci :

> if C then A else B

Exemple:

la fonction "valeur absolue" :

notation mathematique :
| x | = -x si x < 0
         x sinon

notation Haskell :
   abs x = if x < 0 then -x else x

Typage d'un if :

si C est la condition, C doit avoir le type Bool (Vrai/Faux)

> if True  then A else B (ok, True est un Bool)
> if 5 < 2 then A else B (ok, 5<2 est une comparaison)
> if "lol" then A else B (pas ok, "lol" a pour type String)

si A et B sont les deux valeurs a choisir,
le type de A doit etre le meme que celui de B

> if C then 1 else 2      (ok, 1 :: Int, 2 :: Int)
> if C then "lol" else [] (ok, "lol" :: [Char], [] :: [Char])
> ( [] est la liste vide )

if C then 0 else 1.2 (ok, Haskell est gentil et prend 0 comme un
nombre entier ou a virgule et decide
a la fin)

if C then 'a' else 5   (pas ok, 'a' :: Char, 5 :: Int)
if C then (1,2) else 3 (pas ok, (1,2) :: (Int, Int), 3 :: Int)

Exos :
Marquez V ou F devant les if Corrects, ou Incorrects

> f : if 0 then True else False
> v : if False then [] else [1]
> v : if [] == "" then plus1 else id
> f : if (id True) then "a" else 'b'


Recursivite --

Dans un langage fonctionnel, les boucles n'existent pas
Pas de while, ni de for !

Pour definir des boucles, on utilise la recursivite.
C'est le fait de faire reference a sois meme.

Exemple :

> infini = infini + 1

Cette ligne ne veut pas dire "mettre la valeur 'infini+1' dans
la variable 'infini' " mais veut dire "infini est lui meme +1"

Biensur, evaluer infini revient a faire :

>   infini
> = infini+1
> = infini+1+1
> = infini+1+1+1
    ...

On boucle indefiniment, car infini fait reference a lui meme
sans s'arreter, on n'a pas defini de condition d'arret !

Exemple :

> sum_to n = if   n == 0
>            then 0
>            else n + sum_to (n-1)

 
ici, [sum_to n] fait la somme de tous les entiers de 1 a n

Deroulez [sum_to 3]

>   sum_to 3
> = 3 + sum_to (2)
> = 3 + 2 + sum_to (1)
> = 3 + 2 +1 + sum_to(0)
> = 3 + 2 + 1 +0
> = 6

Ecrivez dans GHCi la definition de [sum_to] et testez la


Recursivite terminale --

Quand un programme dans n'importe quel langage est compile
puis lance, un espace de taille fixe appele la pile d'appel
lui est donne pour pouvoir appeler des fonctions.

Quand on appelle une fonction, on empile les parametres de
la fonction sur la pile, et on les depile quand la fonction
a finit son travail.

Dans le cas de la fonction [sum_to], quand on l'evalue avec
n = 3 (sum_to 3), voici ce qu'il se passe

> sum_to 3                 => empile 3 dans la pile
> 3 + sum_to 2             => empile 2 dans la pile
> 3 + 2 + sum_to 1         => empile 1
> 3 + 2 + 1 + sum_to 0     => empile 0

> 3 + 2 + 1 + 0            => depile 0
> 3 + 2 +   1              => calcule 1+0 et depile 1 
> 3 +    3                 => calcule 2+1 et depile 2
>    6                     => calcule 3+3 et depile 3

on voit bien que pour calculer [sum_to n] on va empiler
[n] fois. Or la pile d'appel a une taille fixe, si [n]
est trop grand, on risque de la faire deborder !!

Tenter de simuler un debordement de pile avec une fonction
comme celle ci est impossible, car Haskell optimise le code
de maniere a eviter ce bug.
 
Un autre exemple, que vous pouvez tester, est la fonction [fix]
 
Dans GHCi, tapez : 

> Prelude> let fix f = f (fix f)
> Prelude> fix (2+)

et attendez un peu, jusqu'a avoir le message d'erreur:
*** Exception: stack overflow

"stack overflow" est l'anglais pour "debordement de pile"

deroulons [fix (2+)]

>   fix (2+)
> = 2 + (fix (2+))
> = 2 + (2 + (fix (2+)))
> = 2 + (2 + (2 + (fix (2+))))
  .....

on voit ici que la pile d'appel va grossir sans fin, jusqu'au
debordement, car pour evaluer (2+fix (2+)) il faut evaluer 
(fix (2+)), donc appeler fix, donc remplir la pile, et ca,
a l'infini.

 

Haskell peut optimiser le code pour des fonctions simples
en transformant l'appel recursif en une boucle (oui, une
vraie boucle) dans le code final,
mais quand il s'agit de fonctions complexes, une telle
optimisation est impossible.

Pour aider Haskell, il faut que la fonction respecte une regle
simple, mais difficile a appliquer, la "recursivite terminale" :

L'appel recursif doit etre la toute derniere action de la
fonction.

Exemple :

> sum_to n = if n==0 then 0 else n+sum_to (n-1)

Ici, la derniere action est l'addition "n+", car on evalue
d'abord le terme "sum_to (n-1)", puis l'addition "n+..."
cette fonction n'est donc pas recursive terminale.

Mais [sum_to] peut s'ecrire differemment !!

> sum_to n = sum_to' 0 n
> sum_to' acc n = if n==0 then acc else (sum_to' (acc+n) (n-1))

 
Exos:
 
1) Ecrire une fonction [compte n] qui renvoie [n]
   mais en faisant +1 n fois

> compte n = if n==0 then 0 else 1+compte (n-1)

   puis sous une forme recursive-terminale.

> compte_tr n = compte_tr' 0 n
> compte_tr' acc n = if n==0 then acc else (compte_tr' (acc+1) (n-1))

2) Rendre la puissance recursive-terminale :
> pow x n = if n==0
>           then 1
>           else x * pow x (n-1)

> pow_tr x n = pow_tr' 1 x n
> pow_tr' acc x n = if n==0 then acc else pow_tr' (acc*x) x (n-1)



3) Rendre la factorielle recursive-terminale :
> fac n = if n==0
>         then 1
>         else n * fac (n-1)

> fac_tr n = fac_tr' 1 n
> fac_tr' acc n = if n==0 then acc else fac_tr' (acc*n) (n-1)


4) Rendre fibonacci recursive-terminale (bonus) :
> fibo n = if n==0
>          then 1
>          else if n==1
>               then 1
>               else fibo (n-1) + fibo (n-2)


(Indice : ici, la fonction auxiliaire devra avoir 2 accumulateurs)
> fibo_tr n=fibo_tr' 1 1 n
> fibo_tr' acc1 acc2 n = if n==0 then acc2
>                        else if n==1 then acc2
>                        else fibo_tr' (acc2) (acc2+acc1) (n-1)

-- Creer ses propres types --

Utiliser des types de base (nombres, listes, tuples ...)
ne permet pas tout le temps d'ecrire du code lisible et efficace !

Il est quelque-fois beaucoup plus confortable d'ecrire ses propres
types de donnees.

Un type se definit comme ceci :

> data MonType = C1 | C2 | ... | Cn

MonType est le nom du type.
C1 a Cn sont les constructeurs du type.

> data CouleurFeu = Rouge | Orange | Vert

Ici, le type CouleurFeu definit les couleurs d'un feu de signalisation
il est soit Rouge, soit Orange, soit Vert (ce sont ses constructeurs)

En realite, CouleurFeu est un constructeur de Type, et Rouge/Orange/Vert
sont des constructeurs de valeurs.

Un type peut etre parametrique, c'est a dire prendre en parametre un ou
plusieurs types pour se construire.


> data Pair a b = Pair a b

Ici, Pair est un constructeur de type a deux parametres.
On peut creer des (Pair Int Int), mais aussi des (Pair String Bool)

> pair_point :: Pair Int Int
> pair_point = Pair 10 50
> pair_point = Pair "bonjour" False
> pair_bizarre :: Pair ( Pair Int Int) CouleurFeu
> pair_bizarre = Pair pair_point Orange

-- Quelques types tres utiles --

En Haskell, certains types de etendus ont ete crees pour
repondre a certains besoins. Ici, on va parler de quelques
uns de ces types.

-- Maybe --
 
Le Type Maybe est un type qui permet la representation de
l'absence de valeur :

> data Maybe a = Nothing | Just a

Ce type a deux constructeurs, un qui ne contient rien,
et un deuxieme qui contient une seule valeur de type [a]


ici, juste_1 contient la valeur [1] empaquetee dans le
constructeur [Just]
> juste_1 :: Maybe Int
> juste_1 = Just 1

ici, [pas_de_valeur] est juste le constructeur [Nothing]
ce constructeur n'a aucune valeur donc represente
l'absence de valeur
> pas_de_valeur :: Maybe Int
> pas_de_valeur = Nothing

-- Either --

Either represente un choix entre deux types de valeurs :

> data Either a b = Left a | Right b

> une_chaine :: Either String Int
> une_chaine = Left "hello"

> un_entier :: Either String Int
> un_entier = Right 10

Ici, [une_chaine] et [un_entier] sont deux valeurs du
meme type : [Either String Int] !! Pourtant,
elles contiennent des valeurs de types differents.

[une_chaine] contient une String
[un_entier] contient un Int

Il faut bien comprendre la difference entre
"avoir un type" et "contenir une valeur d'un type"

ici, les deux variables ont le meme type mais
contiennent des valeurs de types differents

Exercices:

Ecrivez un type qui definit les 4 ponts cardinaux:
> data PointCardinal = Est | Ouest | Sud | Nord

Ecrivez un type 'Arbre' qui definit un arbre binaire
d'entiers definit par ce principe:

Un Arbre est soit un  Noeud contenant une valeur et ayant deux enfants
             soit une Feuille contenant simplement une valeur
> data Arbre = Noeud Int Arbre Arbre | Feuille Int

Ecrivez le meme type, mais dont les valeurs des noeuds peuvent etre
de n'importe quel type. (un type Arbre paramétrique)
> data Arbre mt= Noeud mt (Arbre mt) (Arbre mt) | Feuille Int

-- Pattern Matching --

Maintenant que l'on connaît des types plus complexes, et
notemment des types qui encapsulent des valeurs, on voudrait
travailler sur ces jolis types.

On va tenter d'écrire une fonction qui calcule la profondeur d'un
arbre :

> depth :: Arbre mt -> Int

On peut la définir comme ceci:

- La profondeur d'une feuille est 1
- La profondeur d'un noeud est 1+le max de la profondeur de ses enfants

Tentons une première définition :

> depth arbre = if arbre == Feuille
>               then 1
>               else 1 + max (depth ...) (depth ...)

Hmmm ... Un probème se pose ... plusieurs en fait, mais concentrons nous sur
celui ci : comment récupérer les enfants d'un arbre dont on ne connaît que le nom ?

Et bien on est obligé d'utiliser un concept qui s'appelle le pattern matching !
Cela consiste, plutôt que de donner un nom à une valeur, à tenter de lui faire
correspondre un motif (pattern matching == correspondance de motif).

Par exemple, le motif [Feuille x] définit un [Arbre] créé avec le constructeur
[Feuille], et qui encapsule une valeur nommée [x]. Cette valeur peut être
n'importe quel nombre, on s'en fiche ! L'important c'est qu'elle ait un nom, qu'elle
soit connue !

Si on écrit :

> depth (Feuille x) = 1

On vient de définir par pattern matching une partie de la fonction [depth] ! En effet,
ici, [depth] n'est définie que pour le motif [Feuille x].
Quelle différence avec un IF ?
Et bien la structure [if c then a else b] demande que la condition [c] soit une valeur
calculable pour pouvoir au final donner le résultat [a] ou [b]. Or il est impossible
sans pattern matching de calculer le fait de ""correspondre à un motif"". Autrement dit
impossible d'écrire quelque chose comme ceci :

> if une_valeur 'est comme' (Feuille x) then ... else ...

On pourrait écrire ceci :

> if une_valeur == (Feuille 0) || une_valeur == (Feuille 1) || .. || une_valeur == (Feuille VAL_MAX)
> then ...
> else ...

Mais énumérer TOUTES les valeurs possibles d'un [Int] ... n'est pas une solution !
Le pattern matching permet de remplacer ce ['est comme'] imaginaire dans notre if.

Bon, c'est bien beau mais ... et qu'en est-il de l'autre partie de la fonction ?
[depth] d'un noeud ?

Et bien il nous suffit d'écrire juste en dessous de notre première ligne, la définition
pour un noeud !

> depth (Feuille x) = 1
> depth (Noeud x a b) = 1 + max (depth a) (depth b)

On peut voir qu'en plus de définir un motif, on a pu nommer les paramètres des constructeurs
[Feuille] et [Noeud] !

[Feuille] prend un [Int] en paramètre, et dans le motif, il est nommé [x].
[Noeud] prend un [Int], et deux [Arbre] en paramètre (l'enfant de gauche et de droite),
qu'on a nommé respectivement : [x] [a] et [b] ! Ceci nous permet d'utiliser [a] et [b]
pour calculer le [max] de leur profondeur !

-- Notation "case of" --

On peut re-écrire la fonction [depth] en utilisant une notation appelée
la notation "case of" qu'on expliquera plus tard. Voici comment on l'écrit :

> depth arbre = case arbre of
>                 Feuille x   -> 1
>                 Noeud x a b -> 1 + max (depth a) (depth b)

Ici, on voit plusieurs changements :
  - la fonction n'est définie qu'une fois
  - le paramètre est juste nommé et ne correspond à aucun motif
  - le pattern matching se fait à l'intérieur de la fonction
    et opère sur le nom [arbre]

De manière générale, la notation [case x of ...] se définit comme ceci :

> case mon_expression of
>   pattern1 -> resultat1
>   pattern2 -> resultat2
>   ...

c'est du pattern matching sur l'expression [mon_expression] !

L'avantage de cette notation c'est qu'à la place d'un paramètre ou d'une simple variable,
on peut mettre n'importe quelle expression !

-- Exemple plus compliqué et farfelu --

Imaginons qu'on veuille créer une "calculatrice" qui permet d'évaluer
des expressions mathématiques simples (+, -, *, /), et aussi d'enregistrer
des variables !

On peut créer un type de donnée pour représenter une expression :
> data Expression
>   = Add Expression Expression -- (e1 + e2)
>   = Sub Expression Expression -- (e1 - e2)
>   = Mul Expression Expression -- (e1 * e2)
>   = Div Expression Expression -- (e1 / e2)
>   = Val Double -- nombre
>   = Var String -- variable

On va aussi définir un type [Env] qui définit l'ensemble des
noms de variables associés à leurs valeurs.

> type Env = [(String, Double)]

Appelons "eval" la fonction évalue une [Expression]
selon un [Env] donné :

> eval :: Expression -> Env -> Double

Il existe deux cas où le programme devrait crasher :
  - division par 0
  - variable inexistante

Pour eviter les crash, on va tenter d'ecrire une fonction "div"
qui divise de maniere "safe" un nombre. Pour ce faire, on va
encapsuler le resultat dans le type "Maybe". Si jamais on veut
diviser par 0, au lieu de "crash" on va creer une boite sans
valeur (a/0 n'a pas de sens), et sinon, on encapsule le resultat.

> div :: Double -> Double -> Maybe Double
> div a 0 = Nothing
> div a b = Just (a/b)

Voila !! On peut jouer avec :

> Prelude> div 2 0 => Nothing
> Prelude> div 1 2 => Just 0.5

Maintenant, ecrivons la partie de la fonction [eval] qui
correspond a la division :

> eval (Div a b) env = case div (eval a env) (eval b env) of
>                    Nothing -> 0
>                    Just x  -> x

Ici, on a utilisé deux fois le pattern matching :
  - dans le paramètre pour définir [eval] d'une [Div]ision
  - dans le corps de la fonction pour différencier
    le cas où [div] rate, et le cas où [div] réussi

Voilà pour le cas des [Div]. Maintenant, que fait-on quand
une variable n'existe pas ?

On va définir une fonction qui va "essayer" d'extraire la
valeur d'une variable si elle existe, et va renvoyer [Nothing]
si la variable n'existe pas.

Voici son type :
> lookup
>   :: Env          -- L'environnement
>   -> String       -- Le nom de la variable
>   -> Maybe Double -- La valeur de la variable

Et sa définition :
> lookup [] s = Nothing
> lookup ((nom, val):suite) s = if   nom==s
>                               then Just val
>                               else lookup suite s

On remarque qu'ici, on utilise vraiment tous les concepts vu précédemment
  - if then else
  - types complexes
  - récursivité
  - pattern matching

Etudions cette fonction :

> lookup [] s = Nothing

"Si l'environnement est vide, il ne contient aucune variable, donc la variable
 [s] n'a pas de valeur" -> on renvoie donc [Nothing]

> lookup ((nom, val):suite) s = ...

"Si l'environnement a AU MOINS un élément qui ressemble à un couple, alors on
 nomme [nom] et [val] les éléments du couple, [suite] la suite de la liste et
 [s] la variable dont on cherche la valeur"

> lookup ((nom, val):suite) s = if   nom==s
>                               then Just val
>                               else lookup suite s

"Si [nom==s] alors on a trouvé la variable, et sa valeur est [val]. On l'encapsule
   dans le constructeur [Just] et on envoie le résultat.
 Sinon on cherche dans la suite de la liste pour voir si la variable ne s'y cache pas."


Utilisons maintenant cette fonction :

Testez la :
> Prelude> lookup [] "ma_var"
> Prelude> lookup [("ma_var", 10)] "ma_var"
> Prelude> lookup [("bonjour", 8)] "au revoir"
> Prelude> lookup [("arthur", 22), ("marine", 21)] "marine"

Maintenant utilisons la dans notre code :

> eval (Var nom) env = case lookup env nom of
>                        Nothing -> 0
>                        Just x  -> x

Pouf ! Magie ! si la variable n'existe pas, alors on ne crash pas, mais on renvoie
simplement 0 !

Au final, la fonction [eval] va ressembler à ça :

> eval e env =
>   case e of
>     Add a b -> eval a env + eval b env   
>     Sub a b -> eval a env - eval b env   
>     Mul a b -> eval a env * eval b env   
>     Div a b -> case div (eval a env) (eval b env) of
>                  Nothing -> 0
>                  Just x  -> x
>     Val v   -> v
>     Var s   -> case lookup env s of
>                  Nothing -> 0
>                  Just x  -> x

Ouf ! Compliqué non ?
C'est à cause du type [Expression] qui a beaucoup de constructeurs
et des [case of] imbriqués. Prenez du temps pour digérer.

-- Le sucre syntaxique (miam) --

Bon ... Tout ça c'est beau mais très verbeux non ?
heureusement on va voir des éléments de syntaxe qui vont nous
faciliter la vie plus tard !

-- let --

Si vous tombez sur une preuve mathématique écrite en anglais,
vous verrez beaucoup le mot [let] dans des phrases du style:
"let A be ..." c'est notre "Soit A un ..." français !

En Haskell, il est possible de donner des noms à des valeurs à
la volée :

> let a = 5 in a*a

Cette expression veut dire "soit [a] l'entier [5] dans l'expression [a*a]",
ce qui nous donne au final 5*5 = 25.

-- let et le pattern matching --

Une chose amusante avec la notation [let], c'est qu'on peut écrire un motif à
gauche qui va match avec la valeur à droite :

> let liste = [ (5, 6), (8, 7) ] in -- premier let qui définit une liste
>   let (a, b) = liste !! 0 in      -- deuxième let qui définit les éléments du premier tuple de la liste
>     a * b                         -- liste !! 0 == (5, 6) donc -> a = 5 et b = 6 -> a*b = 30

Ici, on a fait correspondre le motif (a, b) avec le premier élément de la liste.
si on s'amuse à résoudre cette correspondance on obtien ceci :

(a, b) = (5, 6)
a = 5 ET b = 6

Si on va plus loin, on peut créer des expressions qui peuvent crash (oh les vilains) !

> let Just x = lookup [] "" in x

Ici, on veut retourner la valeur encapsulée potentiellement dans un [Just] retournée par la
fonction [lookup] ... mais attendez ...

> lookup [] s = Nothing

Ca ne match pas ! Tentons de résoudre ça :

Just x = Nothing ... c'est impossible ! Le programme va donc crasher car il ne peut pas faire correspondre
les deux motifs.


-- where --

[where] est "un peu" comme un [let], dans le sens où il permet de donner un nom à de longues expressions.
Mais [where] fonctionne différemment et par conséquent donne accès à plus de fonctionnalités.

En fait, [where] permet de définir un espace de nommage local à une fonction :

> x1 = 8
>
> f = f' 2
>   where x1 = 5
>         x2 = x1*x1   -- ici on utilise le [x1] du where
>         f' x = x*x2 
>
> valeur = f + x1      -- ici on utilise le [x1] du debut

Ici, on a définit plusieurs variables ( x1, x2 et f' ) après le [where].
Ces variables n'existent que dans le contexte de la fonction [f] et si
jamais elle existaient déjà, comme [x1] dans notre exemple, leur valeur
masque la valeur précédente.

Au final, [valeur] vaut :

> f + x1
> (f' 2) + x1
> (2 * (5*5)) + 8
> 58 -- et non pas 55 ou pire encore, 136

On a vu un exemple de la puissance de [where] : il nous a permi de créer une fonction [f']
locale au contexte de [f]. Cette fonctionnalité nous permet d'écrire un code plus robuste
dès qu'on cré des fonctions récursives. Rappelez-vous de la fonction puissance.

> pow x n = pow' x n 1
> pow' x n acc = if n == 0 then acc else pow' x (n-1) (acc*x)

et bien maintenant, avec nos connaissances nous pouvons écrire ceci :

> pow = pow' 1
>   where
>     pow' acc x n  if n > 0 then pow' (acc*x) x (n-1) else acc

Maintenant, [pow'] n'existe que dans le contexte de [pow] et par conséquent n'est pas
appelable

-- Haskell et les maths --

Haskell est extremement lie aux mathematiques et a la theorie
des categories. C'est un concept qu'il est utile d'apprendre
pour pouvoir comprendre comment avec seulement des fonctions,
on peut creer quelque-chose de dynamique qui discute avec
le monde reel. Nous apprendrons les categories, puis les
foncteurs, et enfin le plus interessant : les monades.

Une Categorie C est un objet mathematique contenant :

1 --
Ob(C) = une collection d'ensembles (ou Objets)

2 --
Ar(C) = la collection des fonctions appelees fleches (Arrow)
        dont le domaine de depart et d'arrivee sont des
        objets de Ob(C)

        Pour chaque object O de Ob(C), Ar(C) doit
        contenir une fleche [idO] de O dans O
        (en Haskell : idO :: O -> O)
         
3 --
(.) = L'operation de composition entre les fonctions de Ar(C)


Pour etre une Categorie, C doit suivre 2 axiomes :

1 -- la loi de composition de l'identite
Si [f :: A -> B] alors f . idA = idB . f = f
 
2 -- l'associativite de la composition
Si [f :: A -> B] et [g :: B -> C] et [h :: C -> D]
alors (h . g) . f = h . (g . f)
 
Ainsi, on distingue plusieurs categories :

La categorie des ensembles Ens, avec :
  Ob(Ens) = l'ensemble de tous les ensembles
  Ar(Ens) = l'ensemble des applications d'un ensemble
            A vers un ensemble B avec A et B dans Ob(Ens)
  (.) = la composition de fonction usuelle

La categorie des groupes.
La categorie Ord, des ensembles ordonnes ...

Ainsi que la categorie ... des categories 

-- Foncteur --

Un Foncteur est un objet mathematique reliant deux
categories ensemble.

Soient deux categories C et D,
alors le Foncteur F : C -> D fait correspondre :

- Tout objet A dans Ob(C) a un objet
   F(A) dans Ob(D)

- Tout morphisme (f : A -> B) dans Ar(C)
   en (F(f) : F(A) -> F(B)) dans Ar(D)

Un foncteur repond a deux lois :

1 -- la conservation de l'identite
Le foncteur applique a la fonction identite
F(idA) est identitque a l'identite dans F(A) : id(F(A))

2 -- la distributivite de la composition
La composition de morphismes doit se distribuer
avec le foncteur : F(A . B) = F(A) . F(B)


-- Quelques exemples de foncteurs en Haskell --

Fini la theorie, et les lois/formules indigestes
passons a la pratique !

Un Foncteur en Haskell a une fonction : [fmap]
de type :

> fmap :: (Functor f) => (a -> b) -> f a -> f b

ce qui veut dire "pour tout foncteur f, fmap
prend un morphisme (a -> b) et applique le
foncteur dessus (f a -> f b)"

Tout type qui implemente [fmap] est un foncteur

Par exemple, [Maybe a] est un foncteur !!
et sa definition de [fmap] est :

> fmap f Nothing = Nothing
> fmap f (Just a) = Just (f a)

Que se veut dire ? Cela veut dire que quand
on n'a aucune valeur (Nothing), on ne fait rien
(Nothing), et quand on a une valeur (Just a)
on applique la fonction [f] a [a] (Just (f a))

testons :

ici, (+1) est une fonction qui prend un
Int et lui ajoute 1.

si on applique fmap dessus, on obtient une
fonction qui prend un Maybe Int et ajoute 1
a la valeur de ce Maybe Int, si elle existe
> maybe_plus :: Maybe Int -> Maybe Int
> maybe_plus = fmap (+1)

Nothing signifie l'absence de valeur,
on ne peut donc pas ajouter 1 au neant
on renvoie donc le neant (Nothing)
> rien = fmap (+1) Nothing

ici, juste_1 = Just 1
du coup, juste_2 = (Just (1+1)) = Just 2
> juste_2 = maybe_plus juste_1 


-- Monades --
--
--
