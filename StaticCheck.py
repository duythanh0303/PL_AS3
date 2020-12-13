
"""
 * @author nhphung
"""
from abc import ABC, abstractmethod, ABCMeta
from dataclasses import dataclass
from typing import List, Tuple
from AST import * 
from Visitor import *
from StaticError import *
from functools import *

class Type(ABC):
    __metaclass__ = ABCMeta
    pass
class Prim(Type):
    __metaclass__ = ABCMeta
    pass
class IntType(Prim):
    pass
class FloatType(Prim):
    pass
class StringType(Prim):
    pass
class BoolType(Prim):
    pass
class VoidType(Type):
    pass
class Unknown(Type):
    pass

@dataclass
class ArrayType(Type):
    dimen:List[int]
    eletype: Type

@dataclass
class MType:
    intype:List[Type]
    restype:Type

@dataclass
class Symbol:
    name: str
    mtype:Type

class StaticChecker(BaseVisitor):
    def __init__(self,ast):
        self.ast = ast
        self.global_envi = [
Symbol("int_of_float",MType([FloatType()],IntType())),
Symbol("float_of_int",MType([IntType()],FloatType())),
Symbol("int_of_string",MType([StringType()],IntType())),
Symbol("string_of_int",MType([IntType()],StringType())),
Symbol("float_of_string",MType([StringType()],FloatType())),
Symbol("string_of_float",MType([FloatType()],StringType())),
Symbol("bool_of_string",MType([StringType()],BoolType())),
Symbol("string_of_bool",MType([BoolType()],StringType())),
Symbol("read",MType([],StringType())),
Symbol("printLn",MType([],VoidType())),
Symbol("printStr",MType([StringType()],VoidType())),
Symbol("printStrLn",MType([StringType()],VoidType()))]                           
   
    def check(self):
        return self.visit(self.ast,self.global_envi)

    def visitProgram(self,ast, c):
        #ListGlobalVar = []
        ListGlobalVar = {}
        CheckMain = False
        for x in ast.decl:
            if isinstance(x, FuncDecl) :
                # listnameVar = []
                # for a in ListGlobalVar:
                #     listnameVar.append(a.name)
                #
                # #check RedeclareFunc
                # if(x.name.name in listnameVar):
                #     raise Redeclared(Function(), x.name.name)
                # intype = []
                # if(x.name.name == 'main'):
                #     CheckMain = True
                # for y in range(len(x.param)):
                #     intype.append(Unknown())
                # ListGlobalVar.append(Symbol(x.name.name,MType(intype,Unknown())))
                if(x.name.name in ListGlobalVar.keys()):
                    raise Redeclared(Function(), x.name.name)
                intype = []
                if(x.name.name == 'main'):
                    CheckMain = True
                for y in range(len(x.param)):
                    intype.append(Unknown())
                ListGlobalVar[x.name.name]=Symbol(x.name.name,MType(intype,Unknown()))

            if isinstance(x, VarDecl):
                #checkRedeclareVar
                self.visit(x,ListGlobalVar)

        if(CheckMain == False):
            raise NoEntryPoint()

        for x in ast.decl:
            if isinstance(x, FuncDecl):
                self.visit(x,ListGlobalVar)



    def visitVarDecl(self,ast, c):
        # listnameVar = []
        # for x in c:
        #     listnameVar.append(x.name)
        #
        # if(ast.variable.name in listnameVar):
        #     raise Redeclared(Variable(),ast.variable.name)
        #
        # if (ast.varDimen == []):
        #
        #     if ast.varInit == None:
        #         c.append(Symbol(ast.variable.name, Unknown()))
        #     else:
        #         c.append(Symbol(ast.variable.name,self.visit(ast.varInit,c)))
        if ast.variable.name in c.keys():
            raise Redeclared(Variable(), ast.variable.name)
        if (ast.varDimen == []):
            if ast.varInit == None:
                c[ast.variable.name] = Symbol(ast.variable.name, Unknown())
            else:
                c[ast.variable.name] = Symbol(ast.variable.name, self.visit(ast.varInit,c))
        #chua lam array

    def visitFuncDecl(self,ast,c):
        listparam = {} #check Redeclare for param and local variable

        for x in ast.param:
            if x.variable.name in listparam.keys():
                raise Redeclared(Parameter(),x.variable.name)
            else:
                self.visit(x,listparam)

        for x in ast.body[0]:
            self.visit(x, listparam)

        list_name_var = list(listparam.keys())
        for x in list_name_var:
            if x not in listparam.keys(): #add Global variable to Local Variable
                listparam[x]=c[x]

        for x in ast.body[1]:
            self.visit(x,listparam)

    def visitAssign(self,ast,c):

        if isinstance(ast.lhs,Id):
            if ast.lhs.name not in c.keys():
                raise Undeclared(Variable(),ast.lhs.name)
            x = c[ast.lhs.name]
            y = self.visit(ast.rhs,c)
            if isinstance(y,Symbol):
                if isinstance(y.mtype,MType) == False: # rhs not kind function
                    if isinstance(x.mtype,Unknown):  # lhs not infer
                        if isinstance(y.mtype,Unknown):
                            raise TypeCannotBeInferred(ast)
                        else:
                            c[x.name].mtype = y.mtype
                    else:
                        if(type(y.mtype) == Unknown):
                            y.mtype = x.mtype
                        if(type(x.mtype) != type(y.mtype)):
                            raise TypeMismatchInStatement(ast)
            else:
                if isinstance(x.mtype, Unknown):  # lhs not infer
                        c[x.name].mtype = y
                else:
                    if (type(x.mtype) != type(y)):
                        raise TypeMismatchInStatement(ast)
        #chua xu li bien kieu ham

    def visitBinaryOp(self,ast,c):
        left = self.visit(ast.left,c)
        right = self.visit(ast.right,c)

        if ast.op in ['+','-','*','\\','%']:
            if type(left) == IntType and type(right) == IntType:
                return IntType()
            elif isinstance(left,Symbol) or isinstance(right,Symbol):

                if isinstance(left, Symbol) and type(left.mtype) == Unknown:
                    left.mtype = IntType()
                if isinstance(right,Symbol) and type(right.mtype) == Unknown:
                    right.mtype = IntType()
                if isinstance(left, Symbol) and isinstance(right, Symbol) and type(left.mtype) == IntType and type(right.mtype) == IntType:
                    return IntType()
                elif isinstance(left, Symbol) and isinstance(right, Symbol) == False:
                    if type(left.mtype) == IntType and type(right) == IntType:
                        return IntType()
                    else:
                        raise TypeMismatchInExpression(ast)
                elif isinstance(left, Symbol) == False and isinstance(right, Symbol):
                    if type(left) == IntType and type(right.mtype) == IntType:
                        return IntType()
                    else:
                        raise TypeMismatchInExpression(ast)
                else:
                    raise TypeMismatchInExpression(ast)
            else:
                raise TypeMismatchInExpression(ast)

        if ast.op in ['+.','-.','*.','\\.']:
            if type(left) == FloatType and type(right) == FloatType:
                return FloatType()
            elif isinstance(left,Symbol) or isinstance(right,Symbol):
                if isinstance(left, Symbol) and type(left.mtype) == Unknown:
                    left.mtype = FloatType()
                if isinstance(right,Symbol) and type(right.mtype) == Unknown:
                    right.mtype = FloatType()
                if isinstance(left, Symbol) and isinstance(right, Symbol):
                    if type(left.mtype) == FloatType and type(right.mtype) == FloatType:
                        return FloatType()
                    else:
                        raise TypeMismatchInExpression(ast)
                elif isinstance(left, Symbol) and isinstance(right, Symbol) == False:
                    if type(left.mtype) == FloatType and type(right) == FloatType:
                        return FloatType()
                    else:
                        raise TypeMismatchInExpression(ast)
                elif isinstance(left, Symbol) == False and isinstance(right, Symbol):
                    if type(left) == FloatType and type(right.mtype) == FloatType:
                        return FloatType()
                    else:
                        raise TypeMismatchInExpression(ast)
                else:
                    raise TypeMismatchInExpression(ast)
            else:
                raise TypeMismatchInExpression(ast)


        if ast.op in ['&&','||']:
            if type(left) == BoolType and type(right) == BoolType:
                return BoolType()
            elif isinstance(left,Symbol) or isinstance(right,Symbol):
                if isinstance(left, Symbol) and type(left.mtype) == Unknown:
                    left.mtype = BoolType()
                if isinstance(right,Symbol) and type(right.mtype) == Unknown:
                    right.mtype = BoolType()
                if isinstance(left, Symbol) and isinstance(right, Symbol):
                    if type(left.mtype) == BoolType and type(right.mtype) ==BoolType:
                        return BoolType()
                    else:
                        raise TypeMismatchInExpression(ast)
                elif isinstance(left, Symbol) and isinstance(right, Symbol) == False:
                    if type(left.mtype) == BoolType and type(right) ==BoolType:
                        return BoolType()
                    else:
                        raise TypeMismatchInExpression(ast)
                elif isinstance(left, Symbol) == False and isinstance(right, Symbol):
                    if type(left) == BoolType and type(right.mtype) == BoolType:
                        return BoolType()
                    else:
                        raise TypeMismatchInExpression(ast)
                else:
                    raise TypeMismatchInExpression(ast)
            else:
                raise TypeMismatchInExpression(ast)

        if ast.op in ['==','!=','>','<','>=','<=']:
            if type(left) == IntType and type(right) == IntType:
                return BoolType()
            elif isinstance(left,Symbol) or isinstance(right,Symbol):

                if isinstance(left, Symbol) and type(left.mtype) == Unknown:
                    left.mtype = IntType()
                if isinstance(right,Symbol) and type(right.mtype) == Unknown:
                    right.mtype = IntType()
                if isinstance(left, Symbol) and isinstance(right, Symbol):
                    if type(left.mtype) == IntType and type(right.mtype) == IntType:
                        return BoolType()
                    else:
                        raise TypeMismatchInExpression(ast)
                elif isinstance(left, Symbol) and isinstance(right, Symbol) == False:
                    if type(left.mtype) == IntType and type(right) == IntType:
                        return BoolType()
                    else:
                        raise TypeMismatchInExpression(ast)
                elif isinstance(left, Symbol) == False and isinstance(right, Symbol):
                    if type(left) == IntType and type(right.mtype) == IntType:
                        return BoolType()
                    else:
                        raise TypeMismatchInExpression(ast)
                else:
                    raise TypeMismatchInExpression(ast)
            else:
                raise TypeMismatchInExpression(ast)

        if ast.op in ['=/=', '>.', '<.', '>=.', '<=.']:
            if type(left) == FloatType and type(right) == FloatType:
                return BoolType()
            elif isinstance(left, Symbol) or isinstance(right, Symbol):
                if isinstance(left, Symbol) and type(left.mtype) == Unknown:
                    left.mtype = FloatType()
                if isinstance(right, Symbol) and type(right.mtype) == Unknown:
                    right.mtype = FloatType()
                if isinstance(left, Symbol) and isinstance(right, Symbol):
                    if type(left.mtype) == FloatType and type(right.mtype) == FloatType:
                        return BoolType()
                    else:
                        raise TypeMismatchInExpression(ast)
                elif isinstance(left, Symbol) and isinstance(right, Symbol) == False:
                    if type(left.mtype) == FloatType and type(right) == FloatType:
                        return BoolType()
                    else:
                        raise TypeMismatchInExpression(ast)
                elif isinstance(left, Symbol) == False and isinstance(right, Symbol):
                    if type(left) == FloatType and type(right.mtype) == FloatType:
                        return BoolType()
                    else:
                        raise TypeMismatchInExpression(ast)

                else:
                    raise TypeMismatchInExpression(ast)
            else:
                raise TypeMismatchInExpression(ast)

    def visitUnaryOp(self,ast,c):
        body = self.visit(ast.body,c)
        if ast.op == '-':
            if type(body) == IntType:
                return IntType()
            elif isinstance(body, Symbol):
                if type(body.mtype) == Unknown:
                    body.mtype = IntType()
                if type(body.mtype) == IntType:
                    return IntType()
                else:
                    TypeMismatchInExpression(ast)
            else:
                raise TypeMismatchInExpression(ast)
        if ast.op == '-.':
            if type(body) == FloatType:
                return FloatType()
            elif isinstance(body, Symbol):
                if type(body.mtype) == Unknown:
                    body.mtype = FloatType()
                if type(body.mtype) == FloatType:
                    return FloatType()
                else:
                    TypeMismatchInExpression(ast)
            else:
                raise TypeMismatchInExpression(ast)

        if ast.op == '!':
            if type(body) == BoolType:
                return BoolType()
            elif isinstance(body, Symbol):
                if type(body.mtype) == Unknown:
                    body.mtype = BoolType
                if type(body.mtype) == BoolType:
                    return BoolType
                else:
                    TypeMismatchInExpression(ast)
            else:
                raise TypeMismatchInExpression(ast)






    def visitId(self,ast,c):
        if(ast.name not in c.keys()):
            raise Undeclared(Variable(),ast.name)
        return c[ast.name]

    def visitIntLiteral(self,ast,c):
        return IntType()

    def visitFloatLiteral(self,ast,c):
        return FloatType()

    def visitStringLiteral(self,ast,c):
        return StringType()

    def visitBooleanLiteral(self,ast,c):
        return BoolType()





        
