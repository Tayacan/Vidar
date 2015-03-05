import Vidar
import Vidar.Match

foo :: [Element]
foo = [
    Block (ExactName "main") $ StrictBlock [
        (SubBlock $ StrictBlock [
            Name (ExactName "b2")
        ]),
        (SubBlock $ StrictBlock [
            Binding (ExactName "literal4") (Block (ExactName "Array") $ StrictBlock [Name $ ExactName "1"
                                                                                    ,Name $ ExactName "2"
                                                                                    ,Name $ ExactName "3"]),
            Binding (ExactName "res5") (Block (ExactName "If") $ StrictBlock [Name $ ExactName "b2"
                                                                             ,SubBlock $ StrictBlock [Name $ ExactName "0"]
                                                                             ,SubBlock $ StrictBlock [Name $ ExactName "1"]
                                                                             ]),
            Binding (ExactName "x6") (Block (ExactName "Leq") $ StrictBlock [Name $ ExactName "0"
                                                                            ,Name $ ExactName "res5"]),
            Binding (ExactName "y7") (Block (ExactName "Less") $ StrictBlock [Name $ ExactName "res5"
                                                                             ,Name $ ExactName "3"]),
            Binding (ExactName "assert_arg8") (Block (ExactName "Log_and") $ StrictBlock [Name $ ExactName "x6"
                                                                                         ,Name $ ExactName "y7"]),
            Binding (ExactName "bounds_check9") (Block (ExactName "Assert") $ StrictBlock [Name $ ExactName "assert_arg8"]),
            Binding (ExactName "res10") (Block (ExactName "Index") $ StrictBlock [SubBlock $ StrictBlock [Name $ ExactName "bounds_check9"]
                                                                                 ,Name $ ExactName "literal4"
                                                                                 ,SubBlock $ StrictBlock [Name $ ExactName "res5"]]),
            SubBlock $ StrictBlock [Name $ ExactName "res10"]

        ])
    ]]

target :: [Element]
target = [Block (ExactName "main") $
             StrictBlock [SubBlock $ UnorderedBlock []
                         ,SubBlock $ UnorderedBlock [Not $ Binding AnyName (Block (ExactName "Assert") $ UnorderedBlock [])]]]

{-

-}

{-
Prog {
    progFunctions = [
        FunDec {
            funDecName = Name "main",
            funDecRetType = ExtRetType [Basic Int],
            funDecParams = [
                Bindee {
                    bindeeIdent = Ident {identName = ID (Name "b",2),
                    identType = Basic Bool},
                    bindeeLore = ()
                }
            ],
            funDecBody = Body {
                bodyLore = (),
                bodyBindings = [
                    Let {
                        bindingPattern = Pattern {
                            patternBindees = [
                                Bindee {
                                    bindeeIdent = Ident {
                                        identName = ID (Name "literal",4),
                                        identType = Array Int (Shape {shapeDims = [Constant (IntVal 3)]}) Nonunique
                                    },
                                    bindeeLore = ()
                                }
                            ]
                        },
                        bindingLore = (),
                        bindingExp = PrimOp (ArrayLit [
                            Constant (IntVal 1),
                            Constant (IntVal 2),
                            Constant (IntVal 3)
                        ] (Basic Int))
                    },
                    Let {
                        bindingPattern = Pattern {
                            patternBindees = [
                                Bindee {
                                    bindeeIdent = Ident {
                                        identName = ID (Name "res",5),
                                        identType = Basic Int
                                    },
                                    bindeeLore = ()
                                }
                            ]
                        },
                        bindingLore = (),
                        bindingExp = If (Var (Ident {identName = ID (Name "b",2), identType = Basic Bool}))
                                        (Body {bodyLore = (), bodyBindings = [], bodyResult = Result {resultSubExps = [Constant (IntVal 0)]}})
                                        (Body {bodyLore = (), bodyBindings = [], bodyResult = Result {resultSubExps = [Constant (IntVal 1)]}})
                                        [Basic Int]
                    },
                    Let {
                        bindingPattern = Pattern {
                            patternBindees = [
                                Bindee {
                                    bindeeIdent = Ident {
                                        identName = ID (Name "x",6),
                                        identType = Basic Bool
                                    },
                                    bindeeLore = ()
                                }
                            ]
                        },
                        bindingLore = (),
                        bindingExp = PrimOp (BinOp Leq (Constant (IntVal 0))
                                                       (Var (Ident {identName = ID (Name "res",5), identType = Basic Int}))
                                                       (Basic Bool))
                    },
                    Let {
                        bindingPattern = Pattern {
                            patternBindees = [
                                Bindee {
                                    bindeeIdent = Ident {
                                        identName = ID (Name "y",7),
                                        identType = Basic Bool
                                    },
                                    bindeeLore = ()
                                }
                            ]
                        },
                        bindingLore = (),
                        bindingExp = PrimOp (BinOp Less (Var (Ident {identName = ID (Name "res",5), identType = Basic Int}))
                                                        (Constant (IntVal 3))
                                                        (Basic Bool))
                    },
                    Let {
                        bindingPattern = Pattern {
                            patternBindees = [
                                Bindee {
                                    bindeeIdent = Ident {
                                        identName = ID (Name "assert_arg",8),
                                        identType = Basic Bool
                                    },
                                    bindeeLore = ()
                                }
                            ]
                        },
                        bindingLore = (),
                        bindingExp = PrimOp (BinOp LogAnd (Var (Ident {identName = ID (Name "x",6), identType = Basic Bool}))
                                                          (Var (Ident {identName = ID (Name "y",7), identType = Basic Bool}))
                                                          (Basic Bool))
                    },
                    Let {
                        bindingPattern = Pattern {
                            patternBindees = [
                                Bindee {
                                    bindeeIdent = Ident {
                                        identName = ID (Name "bounds_check",9),
                                        identType = Basic Cert
                                    },
                                    bindeeLore = ()
                                }
                            ]
                        },
                        bindingLore = (),
                        bindingExp = PrimOp (Assert (Var (Ident {identName = ID (Name "assert_arg",8), identType = Basic Bool})) noLoc)
                    },
                    Let {
                        bindingPattern = Pattern {
                            patternBindees = [
                                Bindee {
                                    bindeeIdent = Ident {
                                        identName = ID (Name "res",10),
                                        identType = Basic Int
                                    },
                                    bindeeLore = ()
                                }
                            ]
                        },
                        bindingLore = (),
                        bindingExp = PrimOp (Index [Ident {identName = ID (Name "bounds_check",9), identType = Basic Cert}]
                                                   (Ident {
                                                        identName = ID (Name "literal",4),
                                                        identType = Array Int (Shape {shapeDims = [Constant (IntVal 3)]}) Nonunique
                                                    })
                                                   [Var (Ident {identName = ID (Name "res",5), identType = Basic Int})])
                    }
                ],
                bodyResult = Result {resultSubExps = [Var (Ident {identName = ID (Name "res",10), identType = Basic Int})]}
            }
        }    
    ]
}
-}
