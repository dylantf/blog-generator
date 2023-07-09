module TestDoc (myDocument) where

import HSBlog.Markup

myDocument :: Document
myDocument =
  [ Heading 1 "Compiling programs with GHC",
    Paragraph "Running `ghc` invokes the Glasgow Haskell Compiler (GHC)",
    Paragraph "Create a new Haskell source file named hello.hs, and write the following:",
    CodeBlock ["main = putStrLn \"Hello, Haskell!\""],
    Paragraph "Now, we can compile the program by invoking ghc:",
    CodeBlock
      [ "$ ghc hello.hs",
        "[1 of 1] Compiling Main (hello.hs, hello.o)",
        "Linking hello ..."
      ],
    Paragraph "GHC created the following files:",
    UnorderedList
      [ "hello.hi - Haskell interface file",
        "hello.o - Object file, the output of the compiler before linking",
        "hello (or hello.exe on Windows) - A native runnable executable"
      ],
    Paragraph "GHC will produce an executable when the source file satisfies both conditions:",
    OrderedList
      [ "Defines the main function in the source file",
        "Defines the module name to be Main or does not have a module declaration"
      ],
    Paragraph "Otherwise, it will only produce the .o and .hi files."
  ]
