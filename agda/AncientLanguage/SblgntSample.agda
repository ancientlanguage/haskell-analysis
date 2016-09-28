module AncientLanguage.SblgntSample where

open import AncientLanguage.Sblgnt

tmSblgnt : Sblgnt
tmSblgnt = sblgnt
  ( p (text "The Greek New Testament: SBL Edition" ∷ [])
  ∷ p (text "Michael W. Holmes, General Editor" ∷ [])
  ∷ p (text "Copyright 2010 Logos Bible Software and the Society of Biblical Literature" ∷ [])
  ∷ [])
  ( p (text "See " ∷ link (a "http://SBLGNT.com" "SBLGNT.com") ∷ text " for license details." ∷ []) ∷ [])
  ( book "Mt" "ΚΑΤΑ ΜΑΘΘΑΙΟΝ"
    ( p
      ( v "Matthew 1:1" "1:1"
      ∷ ws "Βίβλος"
      ∷ ws "γενέσεως"
      ∷ [])
    ∷ []) none
  ∷ book "Mk" "ΚΑΤΑ ΜΑΡΚΟΝ"
    ( p
      ( v "Mark 1:1" "1:1"
      ∷ ws "Ἀρχὴ"
      ∷ ws "τοῦ"
      ∷ ws "εὐαγγελίου"
      ∷ ws "Ἰησοῦ"
      ∷ wp " ⸀" "χριστοῦ" ".  "
      ∷ [])
    ∷ []) none
  ∷ [])
