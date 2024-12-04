#include "ast.hpp"

namespace cminusminus{

IRProgram * ProgramNode::to3AC(TypeAnalysis * ta){
	IRProgram * prog = new IRProgram(ta);
	for (auto global : *myGlobals){
		global->to3AC(prog);
	}
	return prog;
}

void FnDeclNode::to3AC(IRProgram * prog){
	//TODO(Implement me)
	Procedure * p = prog->makeProc(myID->getName());
	for(auto f : *myFormals){
		f->to3AC(p);
	}
	size_t idx = 1;
	std::list<SymOpd *> formals = p->getFormals();
	for(auto opd : formals){
		Quad * arg = new GetArgQuad(idx, opd);
		p->addQuad(arg);
		idx++;
	}
	for(auto b : *myBody){
		b->to3AC(p);
	}
}

void FnDeclNode::to3AC(Procedure * proc){
	//This never needs to be implemented,
	// the function only exists because of
	// inheritance needs (A function declaration
	// never occurs within another function)
	throw new InternalError("FnDecl at a local scope");
}

void FormalDeclNode::to3AC(IRProgram * prog){
	//This never needs to be implemented,
	// the function only exists because of
	// inheritance needs (A formal never
	// occurs at global scope)
	throw new InternalError("Formal at a global scope");
}

void FormalDeclNode::to3AC(Procedure * proc){
	//TODO
	SemSymbol * sym = ID()->getSymbol();
	if(sym == nullptr){
		throw new InternalError("FormalDec error");
	}
	proc->gatherFormal(sym);
}

Opd * ShortLitNode::flatten(Procedure * proc){
	const DataType * type = proc->getProg()->nodeType(this);
	return new LitOpd(std::to_string(myNum), 1);
}

Opd * IntLitNode::flatten(Procedure * proc){
	const DataType * type = proc->getProg()->nodeType(this);
	return new LitOpd(std::to_string(myNum), 8);
}

Opd * StrLitNode::flatten(Procedure * proc){
	Opd * res = proc->getProg()->makeString(myStr);
	return res;
}

Opd * TrueNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * tru = new LitOpd("1", 1);
	return tru;
}

Opd * FalseNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * fals = new LitOpd("0", 1);
	return fals;
}

Opd * AssignExpNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * right = mySrc->flatten(proc);
	Opd * left = myDst->flatten(proc);
	if(!left){
		throw InternalError("Invalid place");
	}
	AssignQuad* q = new AssignQuad(left, right);
	proc->addQuad(q);
	return(left);
}

Opd * LValNode::flatten(Procedure * proc){
	TODO(Implement me)
	//not to do
}

Opd * CallExpNode::flatten(Procedure * proc){
	//TODO(Implement me)
	std::list<Opd*> opdList;
	for(auto arg : *myArgs){
		opdList.push_back(arg->flatten(proc));
	}
	size_t idx = 1;
	for(auto opd : opdList){
		Quad* arg = new SetArgQuad(idx, opd);
		proc->addQuad(arg);
		idx++;
	}
	SemSymbol * fnIdentifier = myID->getSymbol();
	CallQuad * cQuad = new CallQuad(fnIdentifier);
	proc->addQuad(cQuad);

	Opd * ret = nullptr;
	const DataType * returnType = proc->getProg()->nodeType(this);
	if(!(returnType->isVoid())){
		ret = proc->makeTmp(1);
		GetRetQuad * retQ = new GetRetQuad(ret);
		proc->addQuad(retQ);
		return ret;
	}
	return ret;
}

Opd * NegNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = myExp->flatten(proc);
	Opd * op2 = proc->makeTmp(8);
	Quad * q = new UnaryOpQuad(op2, NEG64, op1);
	proc->addQuad(q);
	return op2;
}

Opd * NotNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = myExp->flatten(proc);
	Opd * op2 = proc->makeTmp(8);
	Quad * q = new UnaryOpQuad(op2, NOT8, op1);
	proc->addQuad(q);
	return op2;
}

Opd * PlusNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * left = myExp1->flatten(proc);
	Opd * right = myExp2->flatten(proc);
	auto leftSize = left->getWidth();
	auto rightSize = right->getWidth();
	if(leftSize > 1 && rightSize > 1){
		Opd * dest = proc->makeTmp(8);
		Quad * q = new BinOpQuad(dest, ADD64, left, right);
		proc->addQuad(q);
		return dest;
	} else {
		Opd * dest = proc->makeTmp(8);
		Quad * q = new BinOpQuad(dest, ADD64, left, right);
		proc->addQuad(q);
		return dest;
	}
}

Opd * MinusNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * left = myExp1->flatten(proc);
	Opd * right = myExp2->flatten(proc);
	auto leftSize = left->getWidth();
	auto rightSize = right->getWidth();
	if(leftSize > 1 && rightSize > 1){
		Opd * dest = proc->makeTmp(8);
		Quad * q = new BinOpQuad(dest, SUB64, left, right);
		proc->addQuad(q);
		return dest;
	} else {
		Opd * dest = proc->makeTmp(8);
		Quad * q = new BinOpQuad(dest, SUB64, left, right);
		proc->addQuad(q);
		return dest;
	}
}

Opd * TimesNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * left = myExp1->flatten(proc);
	Opd * right = myExp2->flatten(proc);
	auto leftSize = left->getWidth();
	auto rightSize = right->getWidth();
	if(leftSize > 1 && rightSize > 1){
		Opd * dest = proc->makeTmp(8);
		Quad * q = new BinOpQuad(dest, MULT64, left, right);
		proc->addQuad(q);
		return dest;
	} else {
		Opd * dest = proc->makeTmp(8);
		Quad * q = new BinOpQuad(dest, MULT64, left, right);
		proc->addQuad(q);
		return dest;
	}
}

Opd * DivideNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * left = myExp1->flatten(proc);
	Opd * right = myExp2->flatten(proc);
	auto leftSize = left->getWidth();
	auto rightSize = right->getWidth();
	if(leftSize > 1 && rightSize > 1){
		Opd * dest = proc->makeTmp(8);
		Quad * q = new BinOpQuad(dest, DIV64, left, right);
		proc->addQuad(q);
		return dest;
	} else {
		Opd * dest = proc->makeTmp(8);
		Quad * q = new BinOpQuad(dest, DIV64, left, right);
		proc->addQuad(q);
		return dest;
	}
}

Opd * AndNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = myExp1->flatten(proc);
	Opd * op2 = myExp2->flatten(proc);
	Opd * op3 = proc->makeTmp(8);
	Quad * q = new BinOpQuad(op3, AND64, op1, op2);
	proc->addQuad(q);
	return op3;
}

Opd * OrNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = myExp1->flatten(proc);
	Opd * op2 = myExp2->flatten(proc);
	Opd * op3 = proc->makeTmp(8);
	Quad * q = new BinOpQuad(op3, OR64, op1, op2);
	proc->addQuad(q);
	return op3;
}

Opd * EqualsNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = myExp1->flatten(proc);
	Opd * op2 = myExp2->flatten(proc);
	Opd * op3 = proc->makeTmp(8);
	Quad * q = new BinOpQuad(op3, EQ64, op1, op2);
	proc->addQuad(q);
	return op3;
}

Opd * NotEqualsNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = myExp1->flatten(proc);
	Opd * op2 = myExp2->flatten(proc);
	Opd * op3 = proc->makeTmp(8);
	Quad * q = new BinOpQuad(op3, NEQ64, op1, op2);
	proc->addQuad(q);
	return op3;}

Opd * LessNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = myExp1->flatten(proc);
	Opd * op2 = myExp2->flatten(proc);
	Opd * op3 = proc->makeTmp(8);
	Quad * q = new BinOpQuad(op3, LT64, op1, op2);
	proc->addQuad(q);
	return op3;}

Opd * GreaterNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = myExp1->flatten(proc);
	Opd * op2 = myExp2->flatten(proc);
	Opd * op3 = proc->makeTmp(8);
	Quad * q = new BinOpQuad(op3, GT64, op1, op2);
	proc->addQuad(q);
	return op3;}

Opd * LessEqNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = myExp1->flatten(proc);
	Opd * op2 = myExp2->flatten(proc);
	Opd * op3 = proc->makeTmp(8);
	Quad * q = new BinOpQuad(op3, LTE64, op1, op2);
	proc->addQuad(q);
	return op3;}

Opd * GreaterEqNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = myExp1->flatten(proc);
	Opd * op2 = myExp2->flatten(proc);
	Opd * op3 = proc->makeTmp(8);
	Quad * q = new BinOpQuad(op3, GTE64, op1, op2);
	proc->addQuad(q);
	return op3;
}

Opd * ShortToIntNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = myExp->flatten(proc);
	Opd * tmp = proc->makeTmp(8);
	Quad * q = new AssignQuad(tmp, op1);
	proc->addQuad(q);
	return tmp;
}

Opd * RefNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd * op1 = proc->getSymOpd(myID->getSymbol());
	Opd * src = proc->makeTmp(8);
	proc->addQuad(new LocQuad(op1, src, true, false));
	return src;
}

Opd * DerefNode::flatten(Procedure * proc){
	Opd * op1 = proc->getSymOpd(myID->getSymbol());
	Opd * src = proc->makeTmp(8);
	proc->addQuad(new LocQuad(op1, src, false, false));
	return src;
}

void AssignStmtNode::to3AC(Procedure * proc){
	//TODO(Implement me)
	Opd * assign = myExp->flatten(proc);
}

void PostIncStmtNode::to3AC(Procedure * proc){
	//TODO(Implement me)
	Opd * inc = myLVal->flatten(proc);
	LitOpd * literal = new LitOpd("1", 8);
	BinOpQuad * binQuad = new BinOpQuad(inc, ADD64, inc, literal);
	proc->addQuad(binQuad);
}

void PostDecStmtNode::to3AC(Procedure * proc){
	//TODO(Implement me)
	Opd * inc = myLVal->flatten(proc);
	LitOpd * literal = new LitOpd("1", 8);
	BinOpQuad * binQuad = new BinOpQuad(inc, SUB64, inc, literal);
	proc->addQuad(binQuad);
}

void ReadStmtNode::to3AC(Procedure * proc){
	//TODO(Implement me)
	Opd * dest = myDst->flatten(proc);
	auto type = proc->getProg()->nodeType(myDst);
	ReceiveQuad * rQuad = new ReceiveQuad(dest, type);
	proc->addQuad(rQuad);
}

void WriteStmtNode::to3AC(Procedure * proc){
	//TODO(Implement me)
	Opd * src = mySrc->flatten(proc);
	auto type = proc->getProg()->nodeType(mySrc);
	ReportQuad * wQ = new ReportQuad(src, type);
	proc->addQuad(wQ);
}

void IfStmtNode::to3AC(Procedure * proc){
	//TODO(Implement me)
	Opd * cond = myCond->flatten(proc);
	Label * exitIf = proc->makeLabel();
	Quad * jumpIf = new IfzQuad(cond, exitIf);
	proc->addQuad(jumpIf);
	for(auto stmt: *myBody){
		stmt->to3AC(proc);
	}
	Quad * exit = new NopQuad();
	exit->addLabel(exitIf);
	proc->addQuad(exit);
}

void IfElseStmtNode::to3AC(Procedure * proc){
	//TODO(Implement me)
	Opd * cond = myCond->flatten(proc);
	Label * elseLbl = proc->makeLabel();
	Quad * jumpElse = new IfzQuad(cond, elseLbl);
	proc->addQuad(jumpElse);

	for(auto stmt: *myBodyTrue){
		stmt->to3AC(proc);
	}

	Label* exitIfElse = proc->makeLabel();
	Quad * skipElse = new GotoQuad(exitIfElse);
	proc->addQuad(skipElse);

	Quad * elseNop = new NopQuad();
	elseNop->addLabel(elseLbl);
	proc->addQuad(elseNop);

	for(auto stmt : *myBodyFalse){
		stmt->to3AC(proc);
	}

	Quad * exit = new NopQuad();
	exit->addLabel(exitIfElse);
	proc->addQuad(exit);
}

void WhileStmtNode::to3AC(Procedure * proc){
	//TODO(Implement me)
	Label * loopStart = proc->makeLabel();
	Quad * start = new NopQuad();
	start->addLabel(loopStart);
	proc->addQuad(start);

	Opd * cond = myCond->flatten(proc);
	Label * exitWhile = proc->makeLabel();
	Quad * falseCondJump = new IfzQuad(cond, exitWhile);
	proc->addQuad(falseCondJump);
	for(auto stmt: *myBody){
		stmt->to3AC(proc);
	}

	Quad * loopBack = new GotoQuad(loopStart);
	proc->addQuad(loopBack);
	Quad * exit = new NopQuad();
	exit->addLabel(exitWhile);
	proc->addQuad(exit);
}

void CallStmtNode::to3AC(Procedure * proc){
	//TODO(Implement me)
	Opd * cal = myCallExp->flatten(proc);
}

void ReturnStmtNode::to3AC(Procedure * proc){
	//TODO(Implement me)
	if(myExp != NULL){
		Opd * rtn = myExp->flatten(proc);
		Quad * setRet = new SetRetQuad(rtn);
		proc->addQuad(setRet);
	}
	Quad * jump = new GotoQuad(proc->getLeaveLabel());
	proc->addQuad(jump);
}

void VarDeclNode::to3AC(Procedure * proc){
	SemSymbol * sym = ID()->getSymbol();
	assert(sym != nullptr);
	proc->gatherLocal(sym);
}

void VarDeclNode::to3AC(IRProgram * prog){
	SemSymbol * sym = ID()->getSymbol();
	assert(sym != nullptr);
	prog->gatherGlobal(sym);
}

//We only get to this node if we are in a stmt
// context (DeclNodes protect descent)
Opd * IDNode::flatten(Procedure * proc){
	//TODO(Implement me)
	Opd* sym = proc->getSymOpd(mySymbol);
	if(sym == NULL){
		throw new InternalError("null ID sym");
	}
	return sym;
}

}
