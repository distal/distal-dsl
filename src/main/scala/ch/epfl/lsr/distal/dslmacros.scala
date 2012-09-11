package ch.epfl.lsr.distal

import scala.reflect.macros.Context

object DSLMacros { 
  def RECEIVING(c :Context)(caseObject :c.Expr[Any]) :c.Expr[DSL.ReceivingBranch] = { 
    new DSLMacros[c.type](c).RECEICING(caseObject)
  }

  // unused
  //def DO(c :Context)(block :c.Expr[Any]) :c.Expr[Unit] = { 
  //new DSLMacros[c.type](c).DO(block)
  //}
}


class DSLMacros[C <: Context](val context :C) { 
  import context.universe._
  

  def RECEICING(expr :Expr[Any]) = { 
    val enclosingDSL = context.prefix.tree.children.head
    val typedReceiveBranchSymbol = context.typeOf[ch.epfl.lsr.distal.DSL.TypedReceivingBranch[_]].typeSymbol
    
    val typeArgumentSymbol = expr.tree.symbol match { 
      case m : ModuleSymbol => m.companionSymbol.asInstanceOf[ClassSymbol]
      case _ =>       context.abort(expr.tree.pos, "expected companion object")
    }

    // new DSL.TypedReceivingBranch[T]()
    val tree = Apply(
      Select(New(
	AppliedTypeTree(
	  Ident(typedReceiveBranchSymbol), List(Ident(typeArgumentSymbol)))),
	     nme.CONSTRUCTOR),
      List(Select(enclosingDSL,"__dslRuntime")))
    
    //println("expanding receiving")
    context.Expr[DSL.ReceivingBranch](tree)
  }


  //  class FooMessage extends Message

  def DO(block: Expr[Any]) = { 
    val compositeMarkerType = context.typeOf[ch.epfl.lsr.distal.DSL.CompositeMarker]

    val typeArgumentSymbolTrees :List[Tree] = context.prefix.tree.tpe match { 
      case TypeRef(_,_, args) => args.map { _.typeSymbol } map{ Ident(_)}
    }
    
    //val Expr(x) = reify(new DSL.CompositeDOBranch[FooMessage](null).apply(null))

    val Expr(blockTree) = block

    val branchTypeSymbol = 
      if(context.prefix.tree.tpe <:< compositeMarkerType) 
	context.typeOf[ch.epfl.lsr.distal.DSL.CompositeDOBranch[_]].typeSymbol
      else
	context.typeOf[ch.epfl.lsr.distal.DSL.SimpleDOBranch[_]].typeSymbol
    
    val DOBRANCHtree = Apply(Select(New(AppliedTypeTree(Ident(branchTypeSymbol), typeArgumentSymbolTrees)),
				    nme.CONSTRUCTOR),
			     List(context.prefix.tree))

    val rv = Apply(Select(DOBRANCHtree, "DO"), List(blockTree))
    
    
    println("expanding DO")
    context.Expr(rv)
  }
  
  
  
}



