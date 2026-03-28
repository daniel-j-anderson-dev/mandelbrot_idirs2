module PGM

public export
data FileFormat = Ascii | Binary

public export
magicNumber : FileFormat -> String
magicNumber Ascii  = "P2"
magicNumber Binary = "P5"
