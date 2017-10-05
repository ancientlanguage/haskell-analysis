{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Smyth.Prepositions where

import Grammar.Greek.Morph.QuasiQuoters

-- Smyth 1675
withAccusative = [accentedWords| ἀνά εἰς |]
withDative = [accentedWords| ἐν σύν |]
withGenitive = [accentedWords| ἀντί ἀπό ἐξ ἐκ πρό |]
withAccusativeGenitive = [accentedWords| ἀμφί διά κατά μετά ὑπέρ |]
withAccusativeGenitiveDative = [accentedWords| ἐπί παρά περί πρός ὑπό |]

-- Smyth 1700-1702
improperWithGenitive = [accentedWords|
ἄνευ
ἄτερ
ἄχρι
μέχρι
δίκην
δίχα
εἴσω ἔσω
ἑκάς
ἑκατέρωθεν
ἐκτός
ἔμπροσθεν
ἕνεκα ἕνεκεν εἵνεκα εἵνεκεν
οὕνεκα
ἔνερθε
ἐντός
ἔξω
εὐθύ
καταντικρύ
κρύφα λάθρᾳ
μεταξύ
μέχρι
νόσφι
ὄπισθεν
πάρος
πέρᾱ
πέρᾱν
πλήν
πόρρω πρόσω
πρίν
σχεδόν
τῆλε
χάριν
χωρίς
  |]

improperWithGenitiveDative = [accentedWords|
ἀγχοῦ
ἀντία ἀντίον
ἐγγύς
ἐναντίον
πέλας
πλησίον
  |]

improperWithDative = [accentedWords| ἅμα ὁμοῦ |]

improperWithAccusative = [accentedWords| ὡς |]
