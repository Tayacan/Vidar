import Vidar
import Vidar.Match

t = SubBlock (StrictBlock [
        Block (ExactName "main") (StrictBlock [
            SubBlock (UnorderedBlock []),
            SubBlock (UnorderedBlock [
                Binding (SomeName "r") (Block (ExactName "Map") (StrictBlock [
                    Block (ExactName "FN") (StrictBlock [
                        SubBlock (UnorderedBlock [Name (SomeName "row")]),
                        SubBlock (UnorderedBlock [
                            Binding AnyName (Block (ExactName "Map") (StrictBlock [
                                Block (ExactName "FN") (StrictBlock [
                                    SubBlock (UnorderedBlock []),
                                    SubBlock (UnorderedBlock [
                                        Binding AnyName (Block (ExactName "Plus") (UnorderedBlock [
                                            Name (ExactName "1")
                                        ])),
                                        Binding AnyName (Block (ExactName "Times") (UnorderedBlock [
                                            Name (ExactName "2")
                                        ]))
                                    ])
                                ]),
                                SubBlock (StrictBlock [Name (SomeName "row")])
                            ]))
                        ])
                    ]),
                    SubBlock (StrictBlock [Name AnyName])
                ]))
            ])
        ])
    ])

v = SubBlock (StrictBlock [
        Block (ExactName "main") (StrictBlock [
            SubBlock (StrictBlock []),
            SubBlock (StrictBlock [
                Binding (ExactName "res32") (Block (ExactName "Map") (StrictBlock [
                    Block (ExactName "FN") (StrictBlock [
                        SubBlock (StrictBlock [Name (ExactName "pullReshape_param11")]),
                        SubBlock (StrictBlock [
                            Binding (ExactName "res24") (Block (ExactName "Map") (StrictBlock [
                                Block (ExactName "FN") (StrictBlock [
                                    SubBlock (StrictBlock [Name (ExactName "y20")]),
                                    SubBlock (StrictBlock [
                                        Binding (ExactName "res21") (Block (ExactName "Plus") (StrictBlock [
                                            Name (ExactName "1"),
                                            Name (ExactName "y20")
                                        ])),
                                        Binding (ExactName "res23") (Block (ExactName "Times") (StrictBlock [
                                            Name (ExactName "res21"),
                                            Name (ExactName "2")
                                        ])),
                                        SubBlock (StrictBlock [Name (ExactName "res23")])
                                    ])
                                ]),
                                SubBlock (StrictBlock [Name (ExactName "pullReshape_param11")])
                            ]))
                        ])
                    ]),
                    SubBlock (StrictBlock [Name (ExactName "reshape_outer10")])
                ]))
            ])
        ])
    ])
