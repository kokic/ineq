import Lake
open Lake DSL

package IUT {
  -- add any package configuration options here
}

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"    

-- @[ defaultTarget ]
-- lean_lib «iutt» {
--   -- add any library configuration options here
-- }

-- lean_lib ineq

-- lean_lib algorithm where
--   roots := #[ `algorithm ]

-- lean_lib IUT where 
--   roots := #[ `IUT ]
