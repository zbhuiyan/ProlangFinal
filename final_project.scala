// Final Project - Class and objects

// Team members: Mojia Shen and Zarin Bhuiyan
// Emails: mshen2@wellesley.edu, zarin.bhuiyan@students.olin.edu

// adapted fundational code from lecture

//
//  Values
//


abstract class Value {

   def getInt () : Int = 0
   def getBool () : Boolean = false
   def getString () : String = ""
   def getList () : List[Value] = List()
   def apply (args: List[Value],env: Env[Value]) : Value = new VInteger(0)

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

   def getRefContent () : Value = {
      error("Value not of type REFCELL")
   }

   def setRefContent (v:Value) : Unit = {
      error("Value not of type REFCELL")
   }

   def lookupField (s:String) : Value = {
      error("Value not of type OBJECT")
   }

// changed this to take a list of parameters
   def lookupMethod (s:String) : (List[String],Value) = {
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

   def checkString () : Unit = {
     if (!isString()) {
        error("Value not of type STRING")
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
  override def toString () : String = s.toString()
  override def getString () : String = s
}

class VVector (val l:List[Value]) extends Value {

  override def toString () : String =
     return l.addString(new StringBuilder(), "[ ", " ", " ]").toString()

  override def getList () : List[Value] = l
}


class VPrimOp (val oper : (List[Value]) => Value) extends Value {

  override def toString () : String = "primop(" + oper + ")"

  override def apply (args: List[Value],env: Env[Value]) : Value =
     oper(args)
}


class VRecClosure (val self: String, val params: List[String], val body:Exp, val env:Env[Value], val classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) extends Value {

  override def toString () : String = params + " | " + self + " => " + body

  override def apply (args: List[Value], env_1: Env[Value]) : Value = {
     // TYPE: type system will ensure that right # args is passed
     var new_env = env_1
     for ((p,v) <- params.zip(args)) {
        new_env = new_env.push(p,v)
     }

     // push the current closure as the value bound to identifier self
     new_env = new_env.push(self,this)
     val result = body.eval(new_env, classt)
     return result
  }
}


//
//  Primitive operations
//

object Ops {

   def runtimeError (msg: String) : Nothing = {
       throw new Exception("Runtime error: "+msg)
   }

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

abstract class Type {

   def isSame (t:Type) : Boolean

   def isInteger () : Boolean = return false
   def isBoolean () : Boolean = return false
  //  added isString
   def isString () : Boolean = return false
   def isIntVector () : Boolean = return false
   def isFunction () : Boolean = return false
  //  added isObject
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

// added TString
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

// added TObject
class TObject (val class_name:String) extends Type {

   override def toString () : String = "(class " + class_name + ")"
   def isSame (t:Type):Boolean = return t.isObject()
   override def isObject () : Boolean = true

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

      // contains returns boolean to check if the string is in the environment
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

    def eval (env : Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value

    def error (msg : String) : Nothing = {
       throw new Exception("Eval error: "+ msg + "\n   in expression " + this)
    }

    def terror (msg : String) : Nothing = {
       throw new Exception("Type error: "+ msg + "\n   in expression " + this)
    }

    def typeOf (symt:Env[Type]) : Type

    def typeCheck (t:Type, symt:Env[Type]) : Boolean = {
    	val t2 = this.typeOf(symt)
      return t.isSame(t2)
    }

}


class EInteger (val i:Integer) extends Exp {
    // integer literal

    override def toString () : String =
        "EInteger(" + i + ")"

    def eval (env:Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value =
        new VInteger(i)

    def typeOf (symt:Env[Type]) : Type =
        TInteger
}


class EBoolean (val b:Boolean) extends Exp {
    // boolean literal

    override def toString () : String =
        "EBoolean(" + b + ")"

    def eval (env:Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value =
        new VBoolean(b)

    def typeOf (symt:Env[Type]) : Type =
        TBoolean
}

// created String representation
class EString (val s:String) extends Exp {
    // string literal

    override def toString () : String =
        "EString(" + s + ")"

    def eval (env:Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value =
        new VString(s)

    def typeOf (symt:Env[Type]) : Type =
        TString
}


class EVector (val es: List[Exp]) extends Exp {

   override def toString () : String =
      "EVector" + es.addString(new StringBuilder(),"(", " ", ")").toString()

   def eval (env : Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value = {
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

    def eval (env:Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value = {
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

    def eval (env : Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value = env.lookup(id)

    def typeOf (symt:Env[Type]) : Type =  symt.lookup(id)
}


class EApply (val f: Exp, val args: List[Exp]) extends Exp {
   override def toString () : String =
      "EApply(" + f + "," + args + ")"

   def eval (env : Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value = {
      val vf = f.eval(env,classt)
      val vargs = args.map((e:Exp) => e.eval(env,classt))
      return vf.apply(vargs, env)
   }

   def lookup(): (Exp,List[Exp]) =
     return (f,args)

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

   def eval (env : Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value =
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

   def eval (env : Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value =
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

    def eval (env : Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value = {
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
// VObject takes a list of fields and methods
class VObject (val fields: List[(String,Value)], val methods:List[(String, List[String], Value)]) extends Value {

  override def toString () : String = "object(" + fields + "," + methods + ")"
  override def isObject () : Boolean = true

  override def lookupField (s:String) : Value = {
     for ((n,v) <- fields) {
       if (n == s) {
       	  return v
       }
     }
     error("No field "+s+" in object")
  }

// return a list of arguments and values of the method
  override def lookupMethod (s:String) : (List[String],Value) = {
     for ((n,a,v) <- methods) {
       if (n == s) {
       	  return (a,v)
       }
     }
     error("No method "+ s +" in object")
  }
}

// EObject takes a class name and a list of arguments
class EObject (val class_name: String, val args: List[Exp]) extends Exp {

   override def toString () : String =
     "EObject(" + class_name + ", " + args + ")"

// takes environment and class table, and return VObject
   def eval (env : Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value = {
    //  look up through class name in class table
     val values = classt.lookup(class_name)
     val arguments = values._1 // a list of parameters of the class
     var fields_val = List[(String, Value)]()
     var meths_val = List[(String, List[String], Value)]()

    // add all the parameters with their values to the environment before eval
     var new_env = env
     for (index <- 0 to args.length-1) {
       new_env = new_env.push(arguments(index), args(index).eval(env, classt))
     }

     //go through the field and evaluate values based on the parameters
     for (field <- values._2) {
       val field_val = field._2.eval(new_env, classt)
       fields_val = (field._1, field._2.eval(new_env, classt)) :: fields_val
      // this part is actually not necessary
      //  if (arguments.contains(field_val)) {
      //    //find the index of the argument to replace
      //    var index = arguments.indexOf(field_val)
      //    fields_val = (field._1, args(index).eval(new_env, classt)) :: fields_val
      //  } else {
      //    fields_val = (field._1, field._2.eval(new_env, classt)) :: fields_val
      //  }
     }
     //update for method as well using a similar method
     for (method <- values._3) {
       for (index <- 0 to method._2.length-1) {
         new_env = new_env.push(method._2(index), null)
       }
       meths_val = (method._1, method._2, new VRecClosure("",List("this"),method._3,env,classt)) :: meths_val
     }
    //  create new VObject with a list of evaluated fields and methods
     return new VObject(fields_val, meths_val)
   }
  //  created type TObject based on class name. we only have type for objects, not class
   def typeOf (symt:Env[Type]) : Type =
     new TObject(class_name)
}

// created EField that takes name of the object and name of the field
class EField (val name:String, val field:String) extends Exp {

   override def toString () : String =
      "EField(" + name + "," + field + ")"

   def eval (env : Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value = {
    //  find object in the environment
     if (env.contains(name)) {
       val value = env.lookup(name)
       // find field based on name
       val result = value.lookupField(field)
       return result
     } else {
       error("No " + field + " found")
     }
   }

   def typeOf (symt:Env[Type]) : Type = {
     return TString
   }

 }

// EMethod takes name of the object, name of the method and a list of arguments for the method
 class EMethod (val name:String, val method_name:String, val args:List[Exp]) extends Exp {

   override def toString () : String =
      "EMethod(" + name + "," + method_name + ")"

   def eval (env : Env[Value], classt : Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) : Value = {
    //  look up the object in env
     if (env.contains(name)) {
        val objects = env.lookup(name)
        // look up method in the object
        val method = objects.lookupMethod(method_name)

        // push the parameter and their value pairs into the environment before evaluating
        var new_env = env
        for (index <- 0 to args.length-1) {
          new_env = new_env.push(method._1(index), args(index).eval(new_env, classt))
        }
        val new_args = args.map(p => p.eval(new_env, classt))
        // evaluate the method to a value
        return method._2.apply(new_args,new_env)
      } else {
        error("No method" + method_name + " found")
      }

   }

// we didnt create new type of the method, so simply return TString. We might consider changing this to the value of the method function
   def typeOf(symt:Env[Type]) : Type = {
     return TString
    //  return name.typeOf(symt)
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
   def STRING : Parser[String] = """\".*\"""".r ^^ { s => s }
   def FUN : Parser[Unit] = "fun" ^^ { s => () }
   def LET : Parser[Unit] = "let" ^^ { s => () }

  //  created new keywords
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

   def atomic_string : Parser[Exp] =
      STRING ^^ { s => new EString(s) }

   def atomic : Parser[Exp] =
      ( atomic_int | atomic_string | atomic_id ) ^^ { e => e}

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

   def expr_fun : Parser[Exp] =
      LP ~ FUN ~ LP ~ rep(ID) ~ RP ~ typ ~ expr ~ RP ^^
        { case _ ~ _ ~ _ ~ params ~ _ ~ typ ~ e ~ _ => new EFunction(params,typ,e) }

   def expr_funr : Parser[Exp] =
      LP ~ FUN ~ ID ~ LP ~ rep(ID) ~ RP ~ typ ~ expr ~ RP ^^
        { case _ ~ _ ~ self ~ _ ~ params ~ _ ~ typ ~ e ~ _ => new ERecFunction(self,params,typ,e) }

   def expr_app : Parser[Exp] =
      LP ~ expr ~ rep(expr) ~ RP ^^ { case _ ~ ef ~ eargs ~ _ => new EApply(ef,eargs) }

   def expr_object: Parser[Exp] =
      LP ~ NEW ~ ID ~ rep(expr) ~ RP ^^ {case _ ~ _ ~ id ~ args ~ _  => new EObject(id, args)}

   /* This parser allows us to create a new EMethod with the method's associated class name, the method body itself, and any arguments. */
   def expr_method: Parser[Exp] =
      LP ~ ID ~ DOT ~ ID ~ rep(expr) ~ RP ^^ {case _ ~ class_name ~ _ ~ method ~ arguments ~ _ => new EMethod(class_name, method, arguments)}

   /* This parser allows us to create a new EField with the field's associated class name, and the field itself. */
   def expr_field: Parser[Exp] =
      LP ~ ID ~ DOT ~ ID ~ RP ^^ {case _ ~ class_name ~ _ ~ field ~ _ => new EField(class_name, field)}

   /* This parser allows us to have more than one method, with this structure of method name, arguments, and body. */
   def method_helper: Parser[(String, List[String], Exp)] =
      LP ~ ID ~ LP ~ rep(ID) ~ RP ~ expr ~ RP ^^ {case _ ~ methodName ~ _ ~ input ~ _ ~ body ~ _ => (methodName, input, body)}

   def expr : Parser[Exp] =
      ( atomic | expr_if | expr_object | expr_field | expr_method | expr_vec | expr_fun | expr_funr | expr_let | expr_app ) ^^
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


   /* We added parsing for class definitions, object creation, and class inheritance. */
   def shell_entry : Parser[ShellEntry] =
      (LP ~ "define" ~ ID ~ expr ~ RP  ^^ { case _ ~ _ ~ n ~ e ~ _  => new SEdefine(n,e) }) |
      (LP ~ "class" ~ ID ~ LP ~ rep(ID) ~ RP ~ "," ~ rep(binding) ~ "," ~ rep(method_helper) ~ RP ^^ { case _ ~ _ ~ id ~ _ ~ args ~ _ ~ _ ~ fields ~ _ ~ methods ~ _ => new SEClass(id, args, fields, methods)}) |
      (LP ~ "class" ~ ID ~ "inherits" ~ ID ~ LP ~ rep(ID) ~ RP ~ "," ~ rep(binding) ~ "," ~ rep(method_helper) ~ RP ^^ { case _ ~ _ ~ id ~ _ ~ id2 ~ _ ~ args ~ _ ~ _ ~ fields ~ _ ~ methods ~ _ => new SEClassInherit(id, id2, args, fields, methods)}) |
      (expr ^^ { e => new SEexpr(e) }) |
      ("#quit" ^^ { s => new SEquit() })

}

//
//  Shell
//

abstract class ShellEntry {

   // abstract class for shell entries
   // (representing the various entries you
   def processEntry (env:Env[Value],symt:Env[Type],classt:Env[(List[String], List[(String,Exp)], List[(String, List[String], Exp)])]) : (Env[Value],Env[Type],Env[(List[String], List[(String,Exp)], List[(String, List[String], Exp)])])
}


// TYPE: entering an expression in the shell first type-checks it
//       and the resulting type is printed alongside the
//       result of the evaluation

class SEexpr (e:Exp) extends ShellEntry {

   def processEntry (env:Env[Value],symt:Env[Type],classt:Env[(List[String], List[(String,Exp)], List[(String, List[String], Exp)])]) : (Env[Value],Env[Type],Env[(List[String], List[(String,Exp)], List[(String, List[String], Exp)])]) = {
      val t = e.typeOf(symt)
      val v = e.eval(env, classt)
      // println(v+" : "+t)
      println(v)
      return (env,symt,classt)
   }
}

// TYPE: entering an definition in the shell first type-checks it
//       and the resulting type is added to the top-level symbol
//       table while the resulting value is added to the top-level
//       environment

/* This function allows us to create a new object of a class and it pushes the evaluated new class into the environment, and it also returns the classtable. */
class SEdefine (n:String, e:Exp) extends ShellEntry {

   def processEntry (env:Env[Value],symt:Env[Type],classt:Env[(List[String], List[(String,Exp)], List[(String, List[String], Exp)])]) : (Env[Value], Env[Type], Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) = {
      val t = e.typeOf(symt)
      val v = e.eval(env, classt)
      println(n + " defined with type " + t)
      return (env.push(n,v),symt.push(n,t),classt)
   }

}

/* This function allows us to create a new class, taking in the class name, arguments, list of fields, and list of methods. It pushes all these things into the classtable. */
class SEClass (name:String, args: List[String], fields:List[(String, Exp)], methods:List[(String, List[String], Exp)]) extends ShellEntry {

   def processEntry (env:Env[Value], symt:Env[Type], classt:Env[(List[String], List[(String,Exp)], List[(String, List[String], Exp)])]) : (Env[Value], Env[Type], Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) = {
      var newClasst = classt
      if (classt.contains(name)) {
        throw new Exception("class " + name + " already exists.")
      } else {
        newClasst = newClasst.push(name, (args, fields, methods))
        println("New Class " + name + " created: " + args + fields + methods)
      }
      return (env,symt,newClasst)
   }

}


/* This function allows a class to inherit from another class, taking in all the parent class's fields and methods and updating (or overriding) any fields with the same name. */
class SEClassInherit (name:String, name_parent:String, args: List[String], fields:List[(String, Exp)], methods:List[(String, List[String], Exp)]) extends ShellEntry {

   def processEntry (env:Env[Value], symt:Env[Type], classt:Env[(List[String], List[(String,Exp)], List[(String, List[String], Exp)])]) : (Env[Value], Env[Type], Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])])	 = {
      var newClasst = classt
      var value = (List[String](), List[(String,Exp)](), List[(String, List[String], Exp)]())

      try {
        value = classt.lookup(name_parent)
      } catch {
        case e: Exception => throw new Exception("Parent class doesn't exist.")
      }

      val fields_parent = value._2
      val methods_parent = value._3
      var new_fields = List[(String, Exp)]()
      var new_methods = List[(String, List[String], Exp)]()

      // check duplicate in fields in child and parents
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

      // check duplicate in methods in child and parents
      for ((s_parent, arg_parent, e_parent) <- methods_parent) {
        var flag = 0
        for ((s_child, arg_child, e_child) <- methods) {
          if (s_child == s_parent) {
            flag = 1
          }
        }
        if (flag!=1 && !new_methods.contains((s_parent, arg_parent, e_parent))) {
            println("pushing " + (s_parent, arg_parent, e_parent))
            new_methods = (s_parent, arg_parent, e_parent) :: new_methods
        }
      }

      for ((s_child, arg_child, e_child) <- methods) {
          new_methods = (s_child, arg_child, e_child) :: new_methods
      }

      newClasst = newClasst.push(name, (args, new_fields, new_methods))
      println("New Class " + name + " inherited from " + name_parent + ": " + args + new_fields + new_methods)
      return (env,symt, newClasst)
   }

}


class SEquit extends ShellEntry {

   def processEntry (env:Env[Value],symt:Env[Type],classt:Env[(List[String], List[(String,Exp)], List[(String, List[String], Exp)])]) : (Env[Value], Env[Type], Env[(List[String], List[(String, Exp)], List[(String, List[String], Exp)])]) = {

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
   val nullClasst = new Env[(List[String], List[(String,Exp)], List[(String, List[String], Exp)])](List())

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

  //  create class table that uses env. takes a list of arguments, a list of fields and a list of methods
   val stdClasst = new Env[(List[String], List[(String,Exp)], List[(String, List[String], Exp)])](List())


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
          } catch {
             case e : Exception => println(e.getMessage)
          }
       }
   }

   def main (argv:Array[String]) : Unit = {
       shell()
   }

}


/* Here are the examples we used during the demo! */


// Example 1:
// (class Adder (x y) , (field x) , (method (z) (+ z 1)))
// (define a (new Adder 1 2))
// (a . field)
// (a . method 2)

// Example 2:
// (class Adder2 (x y) , (field1 x) (field2 y) (field3 100), (method1 (z) (+ z 1)) (method2 (q) (* q 2)))
// (define b (new Adder2 1 2))
// (b . field1)
// (b . field2)
// (b . method1 2)
// (b . method2 2)

// Example 3:
// (class Adder1Child inherits Adder (a b) , (field a) , (method2 (r) (+ r 1)))
// (define c (new Adder1Child 3 4))
// (c . field)
// (c . method2 3)


// Example 4:
// (class Adder2Child inherits Adder2 (x y) , (field1 77) (field3 y), (method1 (r) (+ r 5)))
// (define d (new Adder2Child 10 20))
// (d . field3)
// (d . field2)
// (d . method1 5)
// (d . method2 50)
