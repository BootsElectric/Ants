module Messages exposing ( Msg(..) )

import Model exposing ( State )
import Window exposing ( Size )
import Game.Resources as Resources exposing ( Resources )
import Keyboard.Extra
import ElementRelativeMouseEvents exposing ( Point )

type Msg
  = NoMessageYet
  | Resize Size
  | MouseMove Point
  | MouseDown Point
  | Resources Resources.Msg
  | Keys Keyboard.Extra.Msg
  | Tick Float
  | Collect
  | UpdateState State
  | Dig
  | Generate
  | Generated State ( List Float ) 
