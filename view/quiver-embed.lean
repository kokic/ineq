

import Lean

import ProofWidgets.Component.HtmlDisplay

open Lean Widget


def Quiver.Point := Nat × Nat × String
def Quiver.Arrow := Nat × Nat

structure Quiver := 
  points : List Quiver.Point
  arrows : List Quiver.Arrow

def Quiver.empty : Quiver := ⟨[], []⟩

def Quiver.point (diagram : Quiver) (point : Quiver.Point) : Quiver :=
  ⟨diagram.points ++ [point], diagram.arrows⟩

def Quiver.arrow (diagram : Quiver) (arrow : Arrow) : Quiver :=
  ⟨diagram.points, diagram.arrows ++ [arrow]⟩

-- def Quiver.ArrowProperty : Type

-- def Quiver.add (quiver : Quiver) (element : Element) : Quiver :=
    -- ⟨quiver.elements ++ [ element ]⟩


def Quiver.defaultVersionNumber := 0

def Quiver.toString (quiver : Quiver) : String :=     
  let version : Json := defaultVersionNumber
  let count := quiver.points.length
  let data := quiver.points.map (λ ⟨x, y, s⟩ => Json.arr ⟨[x, y, s]⟩)
           ++ quiver.arrows.map (λ ⟨x, y⟩ => Json.arr ⟨[x, y]⟩)
  Json.arr (version :: count :: data |>.toArray) |>.compress

instance : ToString Quiver := ⟨Quiver.toString⟩

-- def Quiver.props := Json.mk

@[widget]
def quiverWidget : UserWidgetDefinition where
  name := "Quiver Diagram"
  javascript := include_str "quiver-provider.js"


def Quiver.toJson (diagram : Quiver) := Json.mkObj [("data", diagram.toString)]

def diagram1 : Quiver where 
  points := [
      (0, 0, "\\R")
    , (1, 0, "1_{\\R}")
  ]
  arrows := []

def diagram2 := diagram1
  |>.point (0, 1, "X")
  |>.point (1, 1, "Π_Y")
  |>.arrow (0, 1)
  |>.arrow (0, 3)

open scoped ProofWidgets.Jsx



-- #widget quiverWidget diagram1.toJson
-- #widget quiverWidget diagram2.toJson









