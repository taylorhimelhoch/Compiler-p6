#include "3ac.hpp"

namespace cminusminus{


Quad::Quad() : myComment(""){
}

void Quad::addLabel(Label * label){
	if (label != nullptr){
		labels.push_back(label);
	}
}

void Quad::setComment(std::string commentIn){
	this->myComment = commentIn;
}

std::string Quad::commentStr(){
	if (myComment.length() > 0){
		return "  #" + myComment;
	}
	return "";
}

std::string Quad::toString(bool verbose){
	auto res = std::string("");

	auto first = true;

	size_t labelSpace = 12;
	for (auto label : labels){
		if (first){ first = false; }
		else { res += ","; }

		res += label->getName();
	}
	if (!first){ res += ": "; }
	else { res += "  "; }
	size_t spaces;
	if (res.length() > labelSpace){ spaces = 0; }
	else { spaces = labelSpace - res.length(); }
	for (size_t i = 0; i < spaces; i++){
		res += " ";
	}

	res += this->repr();
	if (verbose){
		res += commentStr();
	}

	return res;
}

CallQuad::CallQuad(SemSymbol * calleeIn) : callee(calleeIn){ }

std::string CallQuad::repr(){
	return "call " + callee->getName();
}

EnterQuad::EnterQuad(Procedure * procIn)
: Quad(), myProc(procIn) { }

std::string EnterQuad::repr(){
	return "enter " + myProc->getName();
}

LeaveQuad::LeaveQuad(Procedure * procIn)
: Quad(), myProc(procIn) { }

std::string LeaveQuad::repr(){
	return "leave " + myProc->getName();
}

std::string AssignQuad::repr(){
	return dst->valString() + " := " + src->valString();
	
}

AssignQuad::AssignQuad(Opd * dstIn, Opd * srcIn): dst(dstIn), src(srcIn){
	assert(dstIn != nullptr);
	assert(srcIn != nullptr);
}

BinOpQuad::BinOpQuad(Opd * dstIn, BinOp oprIn, Opd * src1In, Opd * src2In)
: dst(dstIn), opr(oprIn), src1(src1In), src2(src2In){
	assert(dstIn != nullptr);
	assert(src1In != nullptr);
	assert(src2In != nullptr);
}

std::string BinOpQuad::oprString(BinOp opr){
	switch(opr){
	case ADD64: return "ADD64";  
	case SUB64: return "SUB64";  
	case DIV64: return "DIV64";  
	case MULT64: return "MULT64";  
	case OR64: return "OR64";  
	case AND64: return "AND64";  
	case EQ64: return "EQ64";  
	case NEQ64: return "NEQ64";  
	case LT64: return "LT64";  
	case GT64: return "GT64";  
	case LTE64: return "LTE64";  
	case GTE64: return "GTE64";  
	} 
	throw new InternalError("Unknonw BinOp " + opr);

}

std::string BinOpQuad::repr(){
	std::string opString;
	return dst->valString()
		+ " := " 
		+ src1->valString()
		+ " " + BinOpQuad::oprString(opr) + " "
		+ src2->valString();
}

UnaryOpQuad::UnaryOpQuad(Opd * dstIn, UnaryOp opIn, Opd * srcIn)
: dst(dstIn), op(opIn), src(srcIn) { 
	assert(dstIn != nullptr);
	assert(srcIn != nullptr);
}

std::string UnaryOpQuad::repr(){
	std::string opString;
	switch (op){
	case NEG64:
		opString = "NEG64 ";
		break;
	case NOT8:
		opString = "NOT8 ";
	}
	return dst->valString() + " := " 
		+ opString
		+ src->valString();
}

ReportQuad::ReportQuad(Opd * opd, const DataType * type) 
: myArg(opd), myType(type){ }

std::string ReportQuad::repr(){
	if(!myArg->asStrAddrOpd()){
		return "REPORT" + myArg->valString();
	}
	else{
		return "REPORT" + myArg->asStrAddrOpd()->getName();
	}
}

ReceiveQuad::ReceiveQuad(Opd * opd, const DataType * type)
: myArg(opd), myType(type){ }

std::string ReceiveQuad::repr(){
	return "RECEIVE " + myArg->valString();
}

GotoQuad::GotoQuad(Label * tgtIn)
: Quad(), tgt(tgtIn){ }

std::string GotoQuad::repr(){
	std::string res = "";
	return "goto " + tgt->getName();
}

IfzQuad::IfzQuad(Opd * cndIn, Label * tgtIn) 
: Quad(), cnd(cndIn), tgt(tgtIn){ }

std::string IfzQuad::repr(){
	std::string res = "IFZ ";
	res += cnd->valString();
	res += " GOTO ";
	res += tgt->getName();
	return res;
}

NopQuad::NopQuad()
: Quad() { }

std::string NopQuad::repr(){
	return "nop";
}

GetRetQuad::GetRetQuad(Opd * opdIn)
: Quad(), opd(opdIn) { }

std::string GetRetQuad::repr(){
	std::string res = "";
	res += "getret " + opd->valString(); 
	return res;
}

SetArgQuad::SetArgQuad(size_t indexIn, Opd * opdIn) 
: index(indexIn), opd(opdIn){
}

std::string SetArgQuad::repr(){
	std::string res = "";
	res += "setarg " + std::to_string(index) + " " + opd->valString(); 
	return res;
}

GetArgQuad::GetArgQuad(size_t indexIn, Opd * opdIn) 
: index(indexIn), opd(opdIn){
}

std::string GetArgQuad::repr(){
	std::string res = "";
	res += "getarg " + std::to_string(index) + " " + opd->valString(); 
	return res;
}

SetRetQuad::SetRetQuad(Opd * opdIn) 
: opd(opdIn){
}

std::string SetRetQuad::repr(){
	std::string res = "";
	res += "setret " + opd->valString(); 
	return res;
}

std::string LocQuad::repr(){
	std::string res = "";
	if (tgtIsLoc){ res += tgt->locString(); } 
	else { res += tgt->valString(); }

	res += " := ";
	if (srcIsLoc){ res += src->locString(); }
	else { res += src->valString(); }

	return res;
}

}
