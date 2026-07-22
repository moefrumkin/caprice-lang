
type t =
  { stem : Stem.t
  ; answer : Answer.t }

let catch {stem; answer} =
  {stem; answer = Answer.catch_err answer}