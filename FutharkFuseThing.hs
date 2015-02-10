module FutharkFuseThing where

import Vidar
import Futhark.Representation.Basic

thing :: Element Exp
thing = Block (ExactName "main")
              (UnorderedBlock
                    [Binding AnyName
                             (Block (ExactName "mapT")
                                    (StrictBlock [Block (ExactName "<anon>")
                                                        (UnorderedBlock [])
                                                 ,Anything
                                                 ]
                                    )
                             )
                    ]
              )
