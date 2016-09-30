module AncientLanguage.Test where

open import Agda.Builtin.Bool
open import Agda.Builtin.Char
open import Agda.Builtin.Nat
open import Agda.Builtin.String
open import AncientLanguage.Source
open import AncientLanguage.Greek.Source.Sblgnt.Matthew

length : {A : Set} → List A → Nat
length [] = 0
length (_ ∷ xs) = suc (length xs)

maybeCons : {A : Set} → Bool → A → List A → List A
maybeCons true x xs = x ∷ xs
maybeCons false _ xs = xs

filter : {A : Set} → (A → Bool) → List A → List A
filter f [] = []
filter f (x ∷ xs) = maybeCons (f x) x (filter f xs)

isBeta : Char → Bool
isBeta 'β' = true
isBeta _ = false

betas : Content → List Char
betas (word (word _ t _)) = filter isBeta (primStringToList t)
betas _ = []

map : {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

_++_ : {A : Set} → (xs ys : List A) → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ xs ++ ys
infixr 6 _++_

join : {A : Set} → List (List A) → List A
join [] = []
join (x ∷ xs) = x ++ join xs

betaCount = length (join (map betas (Source.getContents matthew)))
