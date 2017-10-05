{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Grammar.Greek.Morph.Smyth.Verbs.Verbs where

import Grammar.Greek.Morph.QuasiQuoters

--  i. (a) vowel verbs:  not contracted verbs
-- Smyth 383
luo =
  -- indicative present
  [ [verbParadigm|
λύ̄ω
λύ̄εις
λύ̄ει
λύ̄ετον
λύ̄ετον
λύ̄ομεν
λύ̄ετε
λύ̄ουσι
    |]

  -- indicative imperfect
  , [verbParadigm|
ἔλῡον
ἔλῡες
ἔλῡε
ἐλύ̄ετον
ἐλῡέτην
ἐλύ̄ομεν
ἐλύ̄ετε
ἔλῡον
    |]

  -- indicative Future
  , [verbParadigm|
λύ̄σω
λύ̄σεις
λύ̄σει
λύ̄σετον
λύ̄σετον
λύ̄σομεν
λύ̄σετε
λύ̄σουσι
    |]

-- subjunctive Present
  , [verbParadigm|
      λύ̄ω
      λύ̄ῃς
      λύ̄ῃ
      λύ̄ητον
      λύ̄ητον
      λύ̄ωμεν
      λύ̄ητε
      λύ̄ωσι
    |]

-- optative Present
  , [verbParadigm|
      λύ̄οιμι
      λύ̄οις
      λύ̄οι
      λύ̄οιτον
      λῡοίτην
      λύ̄οιμεν
      λύ̄οιτε
      λύ̄οιεν
    |]

-- optative Imperfect
  , [verbParadigm|
      λύ̄σοιμι
      λύ̄σοις
      λύ̄σοι
      λύ̄σοιτον
      λῡσοίτην
      λύ̄σοιμεν
      λύ̄σοιτε
      λύ̄σοιεν
    |]

-- imperative Present
  , [verbParadigm|
      *
      λῦε
      λῡέτω
      λύ̄ετον
      λῡέτων
      *
      λύ̄ετε
      λῡόντων
    |]

-- indicative first aorist
  , [verbParadigm|
      ἔλῡσα
      ἔλῡσας
      ἔλῡσε
      ἐλύ̄σατον
      ἐλῡσάτην
      ἐλύ̄σαμεν
      ἐλύ̄σατε
      ἔλῡσαν
    |]

  -- indicative first perfect
  , [verbParadigm|
      λέλυκα
      λέλυκας
      λέλυκε
      λελύκατον
      λελύκατον
      λελύκαμεν
      λελύκατε
      λελύκᾱσι
    |]

  -- indicative first pluperfect
  , [verbParadigm|
      ἐλελύκη
      ἐλελύκης
      ἐλελύκει(ν)
      ἐλελύκετον
      ἐλελυκέτην
      ἐλελύκεμεν
      ἐλελύκετε
      ἐλελύκεσαν
    |]

  -- subjunctive first aorist
  , [verbParadigm|
      λύ̄σω
      λύ̄σῃς
      λύ̄σῃ
      λύ̄σητον
      λύ̄σητον
      λύ̄σωμεν
      λύ̄σητε
      λύ̄σωσι
    |]

  -- subjunctive first pluperfect
  , [verbParadigm|
      λελύκω
      λελύκῃς
      λελύκῃ
      λελύκητον
      λελύκητον
      λελύκωμεν
      λελύκητε
      λελύκωσι
    |]
  
  -- optative first aorist
        -- λύ̄σειας (2nd Sing)
        -- λύ̄σειε  (3rd Sing)
        -- λύ̄σειαν (3rd Pl)
  , [verbParadigm|
      λύ̄σαιμι
      λύ̄σαις  
      λύ̄σαι   
      λύ̄σαιτον
      λῡσαίτην
      λύ̄σαιμεν
      λύ̄σαιτε
      λύ̄σαιεν  
    |]

  -- optative first pluperfect
      -- -οίην (1st Sing)
      -- -οίης (2nd Sing)
      -- -οίη  (3rd Sing)
  , [verbParadigm|
      λελύκοιμι
      λελύκοις
      λελύκοι
      λελύκοιτον
      λελυκοίτην
      λελύκοιμεν
      λελύκοιτε
      λελύκοιεν
    |]

    -- imperative aorist
  , [verbParadigm|
      *
      λῦσον
      λῡσάτω
      λύ̄σατον
      λῡσάτων
      *
      λύ̄σατε
      λῡσάντων
    |]
  ]