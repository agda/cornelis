module Hello where
open import Agda.Builtin.Nat
data Bool : Set where
  true : Bool
  false : Bool

data Unit : Set where
  one : Unit

unit : Unit
unit = ?

test : Bool → Bool
test x = ?

unicodeTest₁ : Bool → Bool
unicodeTest₁ x = ?

slap : Bool → Bool
slap = λ { x → ? }

module _ where
  testIndent : Bool → Bool
  testIndent b = ?

isEven∘ : Nat → Set
isEven∘ zero = Nat
isEven∘ (suc n) = {! isEven∘ !}

copattern : Bool → Bool
copattern = ?

foo? : Bool → Bool → Set
foo? ?f = {! !}

give : Bool
give = {! !}

elaborate : Nat
elaborate = {! 3 !}

sub₀and-super⁹ : Nat
sub₀and-super⁹ = 15

