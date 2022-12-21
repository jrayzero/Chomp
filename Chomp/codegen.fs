module Chomp.codegen

type CodegenCPP() =
    inherit visitor.ConstVisitor()
    
    let mutable s = ""
    
    