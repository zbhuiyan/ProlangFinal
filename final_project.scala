//
// language FUNC with (simple) static types
//
// changes annotated with TYPE:


//
//  Values
//


abstract class Value {


   // TYPE: removed predicates for checking types (all done statically now)

   // TYPE: doesn't matter -- type system will ensure these never happens...
   def getInt () : Int = 0
   def getBool () : Boolean = false
   def getString () : String = ""
   def getList () : List[Value] = List()
   def apply (args: List[Value]) : Value = new VInteger(0)

   def isInteger () : Boolean = false
   def isBoolean () : Boolean = false
   def isString () : Boolean = false
   def isVector () : Boolean = false
   def isFunction () : Boolean = false
   def isRefCell () : Boolean = false
   def isNone () : Boolean = false
   def isObject () : Boolean = false

   def error (msg : String) : Nothing = {
      throw new Exception("Value error: "+ msg + "\n   in value " + this)
   }

  //  def getInt () : Int = {
  //     error("Value not of type INTEGER")
  //  }
   //
  //  def getBool () : Boolean = {
  //     error("Value not of type BOOLEAN")
  //  }
   //
  //  def getList () : List[Value] = {
  //     error("Value not of type VECTOR")
  //  }
   //
  //  def apply (args: List[Value]) : Value =  {
  //     error("Value not of type FUNCTION")
  //  }

   def getRefContent () : Value = {
      error("Value not of type REFCELL")
   }

   def setRefContent (v:Value) : Unit = {
      error("Value not of type REFCELL")
   }

   def lookupField (s:String) : Value = {
      error("Value not of type OBJECT")
   }

   def lookupMethod (s:String) : Value = {
      error("Value not of type OBJECT")
   }

   def checkInteger () : Unit = {
     if (!isInteger()) {
        error("Value not of type INTEGER")
     }
   }

   def checkBoolean () : Unit = {
     if (!isBoolean()) {
        error("Value not of type BOOLEAN")
     }
   }

   def checkVector () : Unit = {
     if (!isVector()) {
        error("Value not of type VECTOR")
     }
   }

   def checkFunction () : Unit = {
     if (!isFunction()) {
        error("Value not of type FUNCTION")
     }
   }

   def checkRefCell () : Unit = {
     if (!isRefCell()) {
        error("Value not of type REFCELL")
     }
   }

   def checkUnit () : Unit = {
     if (!isNone()) {
        error("Value not of type NONE")
     }
   }

   def checkObject () : Unit = {
     if (!isObject()) {
        error("Value not of type OBJECT")
     }
   }
}


class VInteger (val i:Int) extends Value {

  override def toString () : String = i.toString()
  override def getInt () : Int = i
}


class VBoolean (val b:Boolean) extends Value {

  override def toString () : String = b.toString()
  override def getBool () : Boolean = b
}

class VString (val s:String) extends Value {

  override def getString () : String = s
}

class VVector (val l:List[Value]) extends Value {

  override def toString () : String =
     return l.addString(new StringBuilder(), "[ ", " ", " ]").toString()

  override def getList () : List[Value] = l
}


class VPrimOp (val oper : (List[Value]) => Value) extends Value {

  override def toString () : String = "primop(" + oper + ")"

  override def apply (args: List[Value]) : Value =
     oper(args)
}


class VRecClosure (val self: String, val params: List[String], val body:Exp, val env:Env[Value], val classt : Env[(List[(String, Exp)], List[(String, Exp)])]) extends Value {

  override def toString () : String = params + " | " + self + " => " + body

  override def apply (args: List[Value]) : Value = {
     // TYPE: type system will ensure that right # args is passed
     var new_env = env
     for ((p,v) <- params.zip(args)) {
        new_env = new_env.push(p,v)
     }

     // push the current closure as the value bound to identifier self
     new_env = new_env.push(self,this)
     return body.eval(new_env, classt)
  }
}





//
//  Primitive operations
//

object Ops {

   def runtimeError (msg: String) : Nothing = {
       throw new Exception("Runtime error: "+msg)
   }

   // TYPE: All operations simplify because:
   //       - we don't need to check the types of values
   //       - we don't have overloading of operations
   //         over multiple types of values -- we need
   //         to choose one :/

   def operPlus (vs:List[Value]) : Value = {

      val v1 = vs(0)
      val v2 = vs(1)

      return new VInteger(v1.getInt() + v2.getInt())
   }


   def operTimes (vs: List[Value]):Value = {

      val v1 = vs(0)
      val v2 = vs(1)

      return new VInteger(v1.getInt() * v2.getInt())
   }


   def operEqual (vs: List[Value]) : Value = {

      val v1 = vs(0)
      val v2 = vs(1)

      return new VBoolean(v1.getInt() == v2.getInt())
   }


   def operLess (vs: List[Value]) : Value = {

       val v1 = vs(0)
       val v2 = vs(1)

       return new VBoolean(v1.getBool() < v2.getBool())
   }


   def operEmpty (vs : List[Value]) : Value = {

     val v = vs(0)
     return new VBoolean(v.getList().length == 0)
   }


   def operFirst (vs : List[Value]) : Value = {
     val v = vs(0)
     val l = v.getList()
     if (l.length == 0) {
       runtimeError("Taking first of an empty vector")
     }
     return l(0)
   }


   def operRest (vs : List[Value]) : Value = {
     val v = vs(0)
     val l = v.getList()
     if (l.length == 0) {
       runtimeError("Taking rest of an empty vector")
     }
     return new VVector(l.tail)
   }


   def operCons (vs : List[Value]) : Value = {
     val item = vs(0)
     val vec = vs(1)
     return new VVector(item::vec.getList())
   }

}




//
//  Types
//

// TYPE: new class for types

abstract class Type {

   def isSame (t:Type) : Boolean

   def isInteger () : Boolean = return false
   def isBoolean () : Boolean = return false
   def isString () : Boolean = return false
   def isIntVector () : Boolean = return false
   def isFunction () : Boolean = return false
   def isObject () : Boolean = return false

   def funParams () : List[Type] = {
      throw new Exception("Type error: type is not a function\n   "+this)
   }

   def funResult () : Type = {
      throw new Exception("Type error: type is not a function\n   "+this)
   }

}


object TInteger extends Type {

   override def toString () : String = "int"

   def isSame (t:Type):Boolean = return t.isInteger()
   override def isInteger () : Boolean = true
}

object TBoolean extends Type {

   override def toString () : String = "bool"

   def isSame (t:Type):Boolean = return t.isBoolean()
   override def isBoolean () : Boolean = true
}

object TString extends Type {

   override def toString () : String = "string"

   def isSame (t:Type):Boolean = return t.isString()
   override def isString () : Boolean = true
}

object TIntVector extends Type {

   override def toString () : String = "ivector"

   def isSame (t:Type):Boolean = return t.isIntVector()
   override def isIntVector () : Boolean = true
}

class TObject (val class_name:String) extends Type {

   override def toString () : String = "(object " + class_name + ")"
   def isSame (t:Type):Boolean = return t.isObject()
   override def isObject () : Boolean = true
  //  override def getClass () : String = return class_name

}

class TFunction (val params:List[Type], val result:Type) extends Type {

   override def toString () : String =
     "(fun "+params.addString(new StringBuilder(),"("," ",")").toString() + " " + result + ")"

   def isSame (t:Type):Boolean = {

     if (!t.isFunction()) {
        return false
     }

     if (t.funParams().length != params.length) {
        return false
     }
     for ((t1,t2) <- t.funParams().zip(params)) {
        if (!t1.isSame(t2)) {
	   return false
	}
     }
     return t.funResult().isSame(result)
   }

   override def isFunction () : Boolean = return true
   override def funParams () : List[Type] = return params
   override def funResult () : Type = return result
}


//
//  Expressions
//


// TYPE: Env now parameterized by the type of things associated
//       with identifiers :
//         - values for environments (during evaluation),
//         - types for symbol tables (during type checking)

class Env[A] (val content: List[(String, A)]) {

      override def toString () : String = {
          var result = ""
      	  for (entry <- content) {
      	     result = result + "(" + entry._1 + " <- " + entry._2 + ") "
      	  }
      	  return result
        }


      // push a single binding (id,v) on top of the environment

      def push (id : String, v : A) : Env[A] =
          new Env[A]((id,v)::content)


      // lookup value for an identifier in the environment

      def lookup (id : String) : A = {
      	  for (entry <- content) {
  	      if (entry._1 == id) {
  	      	 return entry._2
  	      }
	       }
	        throw new Exception("Environment error: unbound identifier "+id)
      }

      def contains(id: String) : Boolean = {
        for (entry <- content) {
          if (entry._1 == id) {
             return true
          }
        }
        return false
      }
}



abstract class Exp {

    def eval (env : Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value

    def error (msg : String) : Nothing = {
       throw new Exception("Eval error: "+ msg + "\n   in expression " + this)
    }

    def terror (msg : String) : Nothing = {
       throw new Exception("Type error: "+ msg + "\n   in expression " + this)
    }

    // TYPE: new methods for type checking
    //       implemented in every subclass of Exp

    def typeOf (symt:Env[Type]) : Type

    def typeCheck (t:Type, symt:Env[Type]) : Boolean = {
    	val t2 = this.typeOf(symt)
      return t.isSame(t2)
    }

    // def classOf(classt:Env[]) :

}


class EInteger (val i:Integer) extends Exp {
    // integer literal

    override def toString () : String =
        "EInteger(" + i + ")"

    def eval (env:Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value =
        new VInteger(i)

    def typeOf (symt:Env[Type]) : Type =
        TInteger
}


class EBoolean (val b:Boolean) extends Exp {
    // boolean literal

    override def toString () : String =
        "EBoolean(" + b + ")"

    def eval (env:Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value =
        new VBoolean(b)

    def typeOf (symt:Env[Type]) : Type =
        TBoolean
}

class EString (val s:String) extends Exp {
    // string literal

    override def toString () : String =
        "EString(" + s + ")"

    def eval (env:Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value =
        new VString(s)

    def typeOf (symt:Env[Type]) : Type =
        TString
}


class EVector (val es: List[Exp]) extends Exp {
    // Vectors

   override def toString () : String =
      "EVector" + es.addString(new StringBuilder(),"(", " ", ")").toString()

   def eval (env : Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value = {
      val vs = es.map((e:Exp) => e.eval(env, classt))
      return new VVector(vs)
   }

    def typeOf (symt:Env[Type]) : Type = {
       for (e <- es) {
         if (!e.typeCheck(TInteger,symt)) {
	   terror("Vector component not an integer")
	 }
       }
       return TIntVector
    }
}


class EIf (val ec : Exp, val et : Exp, val ee : Exp) extends Exp {
    // Conditional expression

    override def toString () : String =
        "EIf(" + ec + "," + et + "," + ee +")"

    def eval (env:Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value = {
        val ev = ec.eval(env, classt)
      	if (!ev.getBool()) {
        	  return ee.eval(env, classt)
      	} else {
      	  return et.eval(env,classt)
      	}
    }

    def typeOf (symt:Env[Type]) : Type = {
      if (ec.typeCheck(TBoolean,symt)) {
        val t = et.typeOf(symt)
    	if (ee.typeCheck(t,symt)) {
    	  return t
    	} else {
    	  terror("Branches of conditional have different types")
    	}
      } else {
        terror("Condition should be Boolean")
      }
    }

}


class EId (val id : String) extends Exp {

    override def toString () : String =
        "EId(" + id + ")"

    def eval (env : Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value = env.lookup(id)

    def typeOf (symt:Env[Type]) : Type =  symt.lookup(id)
}


class EApply (val f: Exp, val args: List[Exp]) extends Exp {
   override def toString () : String =
      "EApply(" + f + "," + args + ")"

   def eval (env : Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value = {
      val vf = f.eval(env,classt)
      val vargs = args.map((e:Exp) => e.eval(env,classt))
      return vf.apply(vargs)
   }

    def typeOf (symt:Env[Type]) : Type = {
      val t = f.typeOf(symt)
      if (t.isFunction()) {
        val params = t.funParams()
        if (params.length != args.length) {
	   terror("Wrong number of arguments")
	} else {
	   // check the argument types
	   for ((pt,a) <- params.zip(args)) {
	     if (!a.typeCheck(pt,symt)) {
	        terror("Argument "+a+" not of expected type")
	     }
	   }
	   return t.funResult()
	}
      } else {
        terror("Applied expression not of function type")
      }
   }
}


class EFunction (val params : List[String], val typ : Type, val body : Exp) extends Exp {

   override def toString () : String =
     "EFunction(" + params + "," + body + ")"

   def eval (env : Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value =
      new VRecClosure("",params,body,env,classt)

    def typeOf (symt:Env[Type]) : Type = {
      if (!typ.isFunction()) {
        terror("Function not defined with function type")
      }
      var tparams = typ.funParams()
      var tresult = typ.funResult()
      if (params.length != tparams.length) {
        terror("Wrong number of types supplied")
      }
      var new_symt = symt
      for ((p,pt) <- params.zip(tparams)) {
        new_symt = new_symt.push(p,pt)
      }
      if (body.typeCheck(tresult,new_symt)) {
        return typ
      } else {
        terror("Return type of function not same as declared")
      }
    }
}

class ERecFunction (val self: String, val params: List[String], val typ: Type, val body : Exp) extends Exp {

   override def toString () : String =
     "ERecFunction(" + self + "," + params + "," + body + ")"

   def eval (env : Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value =
      new VRecClosure(self,params,body,env,classt)

   def typeOf (symt:Env[Type]) : Type = {
      if (!typ.isFunction()) {
        terror("Function not defined with function type")
      }
      var tparams = typ.funParams()
      var tresult = typ.funResult()
      if (params.length != tparams.length) {
        terror("Wrong number of types supplied")
      }
      var new_symt = symt
      for ((p,pt) <- params.zip(tparams)) {
        new_symt = new_symt.push(p,pt)
      }
      // assume self has the declared function type
      new_symt = new_symt.push(self,typ)
      if (body.typeCheck(tresult,new_symt)) {
        return typ
      } else {
        terror("Return type of function not same as declared")
      }
    }

}


class ELet (val bindings : List[(String,Exp)], val ebody : Exp) extends Exp {

    override def toString () : String =
        "ELet(" + bindings + "," + ebody + ")"

    def eval (env : Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value = {
        var new_env = env
        for ((n,e) <- bindings) {
          val v = e.eval(env,classt)
      	  new_env = new_env.push(n,v)
      	}
	      return ebody.eval(new_env,classt)
    }

    def typeOf (symt:Env[Type]) : Type = {
       var new_symt = symt
       for ((n,e) <- bindings) {
         val t = e.typeOf(symt)
	        new_symt = new_symt.push(n,t)
       }
       return ebody.typeOf(new_symt)
    }
}




/*
 * OBJECTS
 *
 */

class VObject (val fields: List[(String,Value)], val methods:List[(String,Value)]) extends Value {

  override def toString () : String = "object(" + fields + "," + methods + ")"
  override def isObject () : Boolean = true

  override def lookupField (s:String) : Value = {
     for ((n,v) <- fields) {
       if (n==s) {
       	  return v
       }
     }
     error("No field "+s+" in object")
  }

  override def lookupMethod (s:String) : Value = {
     for ((n,v) <- methods) {
       if (n==s) {
       	  return v
       }
     }
     error("No method "+s+" in object")
  }
}

class EObject (val class_name: String) extends Exp {

   override def toString () : String =
     "EObject(" + class_name + ")"

   def eval (env : Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value = {
     val values = classt.lookup(class_name)
     val fields_val = values._1.map((p) => (p._1,p._2.eval(env, classt)))
     // wrap every method with as function expecting "this" as an argument
     val meths_val = values._2.map((p) => (p._1,new VRecClosure("",List("this"),p._2,env,classt)))
     return new VObject(fields_val, meths_val)
   }

   def typeOf (symt:Env[Type]) : Type =
     new TObject(class_name)

}


// def lookup_class(name:String, classt:Env[(List(String, Exp),List(String, Exp))]) :

class EField (val r:Exp, val f:String) extends Exp {

  override def toString () : String =
     "EField(" + r + "," + f + ")"

  def eval (env : Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value = {
    val vr = r.eval(env, classt)
    return vr.lookupField(f)
  }

  def typeOf (symt:Env[Type]) : Type = {
    return r.typeOf(symt)
  }

}


class EMethod (val r:Exp, val f:String) extends Exp {

  override def toString () : String =
     "EMethod(" + r + "," + f + ")"

  def eval (env : Env[Value], classt : Env[(List[(String, Exp)], List[(String, Exp)])]) : Value = {
    val vr = r.eval(env, classt)
    val m = vr.lookupMethod(f)
    return m.apply(List(vr))
  }

  def typeOf(symt:Env[Type]) : Type = {
    return r.typeOf(symt)
  }
}



//
// SURFACE SYNTAX (S-expressions)
//


import scala.util.parsing.combinator._


class SExpParser extends RegexParsers {

   // tokens

   def LP : Parser[Unit] = "(" ^^ { s => () }
   def RP : Parser[Unit] = ")" ^^ { s => () }
   def LB : Parser[Unit] = "[" ^^ { s => () }
   def RB : Parser[Unit] = "]" ^^ { s => () }
   def PLUS : Parser[Unit] = "+" ^^ { s => () }
   def TIMES : Parser[Unit] = "*" ^^ { s => () }
   def INT : Parser[Int] = """[0-9]+""".r ^^ { s => s.toInt }
   def IF : Parser[Unit] = "if" ^^ { s => () }
   def ID : Parser[String] = """[a-zA-Z_+*\-:.?=<>!|][a-zA-Z0-9_+\-*:.?=<>!|]*""".r ^^ { s => s }

   def FUN : Parser[Unit] = "fun" ^^ { s => () }
   def LET : Parser[Unit] = "let" ^^ { s => () }
   def NEW : Parser[Unit] = "new" ^^ { s => () }

   def CLASS : Parser[Unit] = "class" ^^ { s => () }
   def FIELDS : Parser[Unit] = "fields" ^^ { s => () }
   def METHODS : Parser[Unit] = "methods" ^^ { s => () }
   def FIELD : Parser[Unit] = "field" ^^ { s => () }
   def METHOD : Parser[Unit] = "method" ^^ { s => () }
   def DOT : Parser[Unit] = "." ^^ { s => () }

   // TYPE: new keywords for writing types

   def TINT : Parser[Unit] = "int" ^^ { s => () }
   def TBOOL : Parser[Unit] = "bool" ^^ { s => () }
   def TSTRING : Parser[Unit] = "string" ^^ { s => () }
   def TINTV : Parser[Unit] = "ivector" ^^ { s => () }
   def TFUN : Parser[Unit] = "tfun" ^^ { s => () }

   // grammar

   def atomic_int : Parser[Exp] = INT ^^ { i => new EInteger(i) }

   def atomic_id : Parser[Exp] =
      ID ^^ { s => new EId(s) }

   def atomic : Parser[Exp] =
      ( atomic_int | atomic_id ) ^^ { e => e}

   def expr_if : Parser[Exp] =
      LP ~ IF ~ expr ~ expr ~ expr ~ RP ^^
        { case _ ~ _ ~ e1 ~ e2 ~ e3 ~ _ => new EIf(e1,e2,e3) }

   def binding : Parser[(String,Exp)] =
      LP ~ ID ~ expr ~ RP ^^ { case _ ~ n ~ e ~ _ => (n,e) }

   def expr_let : Parser[Exp] =
      LP ~ LET ~ LP ~ rep(binding) ~ RP ~ expr ~ RP ^^
           { case _ ~ _ ~ _ ~ bindings ~ _ ~ e2 ~ _ => new ELet(bindings,e2) }

   def expr_vec : Parser[Exp] =
      LB ~ rep(expr) ~ RB ^^ { case _ ~ es ~ _ => new EVector(es) }

  //  def mkClass (name:String, params:List[String], ty: Type) : Exp = {
  //       return new ERecFunction(name,params, ty, new EObject(name))
  //     }

  //  def meth_binding : Parser[(String,Exp)] =
  //        LP ~ ID ~ LP ~ rep(ID) ~ RP ~ typ ~ expr ~ RP ^^ { case _ ~ name ~ _ ~ params ~ _ ~ typ  ~ body ~ _ => (name, new EFunction(params, typ, body)) }
   //
  //     def expr_class : Parser[Exp] =
  //       LP ~ CLASS ~ ID ~ LP ~ rep(ID) ~ RP ~ LP ~ FIELDS ~ rep(binding) ~ RP ~ typ ~ LP ~ METHODS ~ rep(meth_binding) ~ RP ~ RP ^^
  //          { case _ ~ _ ~ rec ~ _ ~ params ~ _ ~ _ ~ _ ~ fields ~ _ ~ typ ~ _ ~ _ ~ meths ~ _ ~ _ => mkClass(rec,params,typ,fields,meths) }
   //
  //     def expr_field : Parser[Exp] =
  //       LP ~ FIELD ~ expr ~ ID ~ RP ^^ { case _ ~ _ ~ e ~ id ~ _ => new EField(e,id) }
   //
  //     def expr_meth : Parser[Exp] =
  //       LP ~ METHOD ~ expr ~ ID ~ RP ^^ { case _ ~ _ ~ e ~ id ~ _ => new EMethod(e,id) }

   // TYPE: changed surface syntax of functions (and recursive functions)
   //       to add a type annotation for functions
   //  e.g. (fun (a b c) (tfun (int int int) bool) (if (= a b) (= a c) false))

   def expr_fun : Parser[Exp] =
      LP ~ FUN ~ LP ~ rep(ID) ~ RP ~ typ ~ expr ~ RP ^^
        { case _ ~ _ ~ _ ~ params ~ _ ~ typ ~ e ~ _ => new EFunction(params,typ,e) }

   def expr_funr : Parser[Exp] =
      LP ~ FUN ~ ID ~ LP ~ rep(ID) ~ RP ~ typ ~ expr ~ RP ^^
        { case _ ~ _ ~ self ~ _ ~ params ~ _ ~ typ ~ e ~ _ => new ERecFunction(self,params,typ,e) }

   def expr_app : Parser[Exp] =
      LP ~ expr ~ rep(expr) ~ RP ^^ { case _ ~ ef ~ eargs ~ _ => new EApply(ef,eargs) }

   def expr_class: Parser[Exp] =
      LP ~ NEW ~ ID ~ RP ^^ {case _ ~ _ ~ id ~ _  => new EObject(id)}

   def expr : Parser[Exp] =
      ( atomic | expr_if | expr_class | expr_vec | expr_fun | expr_funr | expr_let | expr_app ) ^^
           { e => e }

   def typ_int : Parser[Type] =
      TINT ^^ { _ => TInteger }

   def typ_bool : Parser[Type] =
      TBOOL ^^ { _ => TBoolean }

   def typ_intvec : Parser[Type] =
      TINTV ^^ { _ => TIntVector }

   def typ_fun : Parser[Type] =
      LP ~ TFUN ~ LP ~ rep(typ) ~ RP ~ typ ~ RP ^^
         { case _ ~ _ ~ _ ~ tparams ~ _ ~ tresult ~ _ => new TFunction(tparams,tresult) }

   def typ : Parser[Type] =
      ( typ_int | typ_bool | typ_intvec | typ_fun ) ^^ { e => e }

   def shell_entry : Parser[ShellEntry] =
      (LP ~ "define" ~ ID ~ expr ~ RP  ^^ { case _ ~ _ ~ n ~ e ~ _  => new SEdefine(n,e) }) |
      (LP ~ "class" ~ ID ~ rep(binding) ~ "," ~ rep(binding) ~ RP ^^ { case _ ~ _ ~ id ~ fields ~ _ ~ methods ~ _ => new SEClass(id, fields, methods)}) |
      (LP ~ "class" ~ ID ~ "inherits" ~ ID ~ rep(binding) ~ "," ~ rep(binding) ~ RP ^^ { case _ ~ _ ~ id ~ _ ~ id2 ~ fields ~ _ ~ methods ~ _ => new SEClassInherit(id, id2, fields, methods)}) |
      (LP ~ ID ~ DOT ~ ID ~ LP ~ RP ~ RP ^^ {case _ ~ className ~ _ ~ method ~ _ ~ _ ~ _ => new SEmethod(className, method)}) | // call method
      (LP ~ ID ~ DOT ~ ID ~ RP ^^ {case _ ~ className ~ _ ~ field ~ _ => new SEfield(className, field)}) | // return field body
      (expr ^^ { e => new SEexpr(e) }) |
      ("#quit" ^^ { s => new SEquit() })

}


// try these two examples
// (class A (S 1) (K 2), (D 3))
// (class B inherits A (S hello) (B hi), (L what) (D bye))
// seems if B inherits from A, the fields and methods have to be the same type. EInteger and EId don't work
// (class B inherits A (S 10) (B 10), (L 10) (D 10))
// (class C inherits B (B A), (U U) (L R))

// (class A (String1 2) (String2 3), (Method 4))
// (class B (String1 a) (String2 b), (Method c))
// (class C (String1 (+ 10 20)) (String2 (+ 20 20)), (Method (+ 30 20)))
// (class D inherits A (String1 hello) (String2 (*2 2)), (Method (*4 4)))

//  

//
//  Shell
//

abstract class ShellEntry {

   // abstract class for shell entries
   // (representing the various entries you
   //  can type at the shell)

   def processEntry (env:Env[Value],symt:Env[Type],classt:Env[(List[(String,Exp)], List[(String,Exp)])]) : (Env[Value],Env[Type],Env[(List[(String,Exp)], List[(String,Exp)])])
}


// TYPE: entering an expression in the shell first type-checks it
//       and the resulting type is printed alongside the
//       result of the evaluation

class SEexpr (e:Exp) extends ShellEntry {

   def processEntry (env:Env[Value],symt:Env[Type],classt:Env[(List[(String,Exp)], List[(String,Exp)])]) : (Env[Value],Env[Type],Env[(List[(String,Exp)], List[(String,Exp)])]) = {
      val t = e.typeOf(symt)
      val v = e.eval(env, classt)
      println(v+" : "+t)
      return (env,symt,classt)
   }
}

// TYPE: entering an definition in the shell first type-checks it
//       and the resulting type is added to the top-level symbol
//       table while the resulting value is added to the top-level
//       environment

class SEdefine (n:String, e:Exp) extends ShellEntry {

   def processEntry (env:Env[Value],symt:Env[Type],classt:Env[(List[(String,Exp)], List[(String,Exp)])]) : (Env[Value],Env[Type],Env[(List[(String,Exp)], List[(String,Exp)])]) = {
      val t = e.typeOf(symt)
      val v = e.eval(env, classt)
      println(n + " definitionned with type " + t)
      return (env.push(n,v),symt.push(n,t),classt)
   }

}

class SEClass (name:String, fields:List[(String, Exp)], methods:List[(String, Exp)]) extends ShellEntry {

   def processEntry (env:Env[Value], symt:Env[Type], classt:Env[(List[(String,Exp)], List[(String,Exp)])]) : (Env[Value], Env[Type], Env[(List[(String,Exp)], List[(String,Exp)])]) = {
      var newClasst = classt
      if (classt.contains(name)) {
        throw new Exception("class " + name + " already exists.")
      } else {
        newClasst = newClasst.push(name, (fields, methods))
      }
      return (env,symt,newClasst)
   }

}


class SEmethod (name:String, method:String) extends ShellEntry {
   def processEntry (env:Env[Value], symt:Env[Type], classt:Env[(List[(String,Exp)], List[(String,Exp)])]) : (Env[Value], Env[Type], Env[(List[(String,Exp)], List[(String,Exp)])]) = {
      println("hi")
      var value = (List[(String,Exp)](), List[(String,Exp)]())
      if (classt.contains(name)) {
        value = classt.lookup(name)
        val methods = value._2
        for ((s, e) <- methods) {
          if (s == method) {
            println(method + ": " + e)
          }
        }
      } else {
        throw new Exception("class " + name + " doesn't exist.")
      }
      return (env,symt,classt)
   }
}


class SEfield (name:String, field:String) extends ShellEntry {
   def processEntry (env:Env[Value], symt:Env[Type], classt:Env[(List[(String,Exp)], List[(String,Exp)])]) : (Env[Value], Env[Type], Env[(List[(String,Exp)], List[(String,Exp)])]) = {
      
      println("hi")
      var value = (List[(String,Exp)](), List[(String,Exp)]())
      if (classt.contains(name)) {
        value = classt.lookup(name)
        val fields = value._1
        for ((s, e) <- fields) {
          if (s == field) {
            println(field + ": " + e)
          }
        }
      } else {
        throw new Exception("class " + name + " doesn't exist.")
      }
      return (env,symt,classt)
   }
}

class SEClassInherit (name:String, name_parent:String, fields:List[(String, Exp)], methods:List[(String, Exp)]) extends ShellEntry {

   def processEntry (env:Env[Value], symt:Env[Type], classt:Env[(List[(String,Exp)], List[(String,Exp)])]) : (Env[Value], Env[Type], Env[(List[(String,Exp)], List[(String,Exp)])]) = {
      var newClasst = classt
      var value = (List[(String,Exp)](), List[(String,Exp)]())
      try {
        value = classt.lookup(name_parent)
      } catch {
        case e: Exception => throw new Exception("Parent class doesn't exist.")
      }

      val fields_parent = value._1
      val methods_parent = value._2
      var new_fields = List[(String, Exp)]()
      var new_methods = List[(String, Exp)]()

      for ((s_parent, e_parent) <- fields_parent) {
        var flag = 0
        for ((s_child, e_child) <- fields) {
          if (s_child == s_parent) {
            flag = 1
          }
        }
        if (flag!=1 && !new_fields.contains((s_parent, e_parent))) {
            println("pushing " + (s_parent, e_parent))
            new_fields = (s_parent, e_parent) :: new_fields
        }
      }

      for ((s_child, e_child) <- fields) {
          new_fields = (s_child, e_child) :: new_fields
      }

      for ((s_parent, e_parent) <- methods_parent) {
        var flag = 0
        for ((s_child, e_child) <- methods) {
          if (s_child == s_parent) {
            flag = 1
          }
        }
        if (flag!=1 && !new_methods.contains((s_parent, e_parent))) {
            println("pushing " + (s_parent, e_parent))
            new_methods = (s_parent, e_parent) :: new_methods
        }
      }

      for ((s_child, e_child) <- methods) {
          new_methods = (s_child, e_child) :: new_methods
      }

      newClasst = newClasst.push(name, (new_fields, new_methods))
      return (env,symt, newClasst)
   }

}


class SEquit extends ShellEntry {

   def processEntry (env:Env[Value],symt:Env[Type],classt:Env[(List[(String,Exp)], List[(String,Exp)])]) : (Env[Value],Env[Type],Env[(List[(String,Exp)], List[(String,Exp)])]) = {

      System.exit(0)
      return (env,symt,classt)
   }
}


object Shell {

   val parser = new SExpParser

   def parse (input:String) : ShellEntry = {

      parser.parseAll(parser.shell_entry, input) match {
         case parser.Success(result,_) => result
         case failure : parser.NoSuccess => throw new Exception("Cannot parse "+input+": "+failure.msg)
      }
   }


   val nullEnv = new Env[Value](List())
   val nullClasst = new Env[(List[(String,Exp)], List[(String,Exp)])](List())

   //
   // Standard environment
   //

   val stdEnv = new Env[Value](List(
     ("true",new VBoolean(true)),
     ("false",new VBoolean(false)),
//     ("not", new VRecClosure("",List("a"), new EIf(new EId("a"), new ELiteral(new VBoolean(false)), new ELiteral(new VBoolean(true))),nullEnv)),
     ("not", new VRecClosure("",List("a"), new EIf(new EId("a"), new EBoolean(false), new EBoolean(true)),nullEnv, nullClasst)),
     ("+", new VPrimOp(Ops.operPlus)),
     ("*", new VPrimOp(Ops.operTimes)),
     ("=", new VPrimOp(Ops.operEqual)),
     ("<", new VPrimOp(Ops.operLess)),
     ("empty?",new VPrimOp(Ops.operEmpty)),
     ("first",new VPrimOp(Ops.operFirst)),
     ("rest",new VPrimOp(Ops.operRest)),
     ("empty",new VVector(List())),
     ("cons",new VPrimOp(Ops.operCons))
   ))


   // TYPE: the initial symbol table, recording the types
   //       of all top-level functions to allow for
   //       type-checking their use.
   //
   // Keep in sync with stdEnv above!

   val stdSymt = new Env[Type](List(
     ("true",TBoolean),
     ("false",TBoolean),
     ("not", new TFunction(List(TBoolean),TBoolean)),
     ("+", new TFunction(List(TInteger,TInteger),TInteger)),
     ("*", new TFunction(List(TInteger,TInteger),TInteger)),
     ("=", new TFunction(List(TInteger,TInteger),TBoolean)),
     ("<", new TFunction(List(TInteger,TInteger),TBoolean)),
     ("empty?",new TFunction(List(TIntVector),TBoolean)),
     ("first",new TFunction(List(TIntVector),TInteger)),
     ("rest",new TFunction(List(TIntVector),TIntVector)),
     ("empty", TIntVector),
     ("cons",new TFunction(List(TInteger,TIntVector),TIntVector))
   ))

  val stdClasst = new Env[(List[(String,Exp)], List[(String,Exp)])](List())

   def shell () : Unit = {

       var env = stdEnv
       var symt = stdSymt
       var classt = stdClasst

       while (true) {
          print("TFUNC> ")
          try {
             val input = scala.io.StdIn.readLine()
             val se = parse(input)
	     val result = se.processEntry(env,symt,classt)
	     env = result._1
	     symt = result._2
       classt = result._3
       println("Classes: " + classt)
          } catch {
             case e : Exception => println(e.getMessage)
          }
       }
   }

   def main (argv:Array[String]) : Unit = {
       shell()
   }

}
