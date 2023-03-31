

inductive MdElement
  | heading : Nat → String → MdElement
  | paragraph : String → MdElement
  | bold : String → MdElement
  | italic : String → MdElement
  | link : String → String → MdElement
  | image : String → String → MdElement

-- 1. implement a function named `markdown`
-- that support all syntax of `MdElement`
-- and output html string