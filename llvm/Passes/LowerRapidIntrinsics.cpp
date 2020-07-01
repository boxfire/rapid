#include "llvm/Pass.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

// The next function is a part of Julia. License is MIT: https://julialang.org/license
template<typename TIterator>
static void replaceInstruction(
    Instruction *oldInstruction,
    Value *newInstruction,
    TIterator &it)
{
    if (newInstruction != oldInstruction) {
        oldInstruction->replaceAllUsesWith(newInstruction);
        it = oldInstruction->eraseFromParent();
    }
    else {
        ++it;
    }
}


namespace {
struct Hello : public FunctionPass {
  static char ID;
  Function *rapid_unboxint;
  Hello() : FunctionPass(ID) {}

  bool doInitialization(Module &M) override {
    rapid_unboxint = M.getFunction("llvm.rapid.unboxint");
    return false;
  }

  Value *lowerUnboxint(CallInst *target, Function &F) {
    assert(target->getNumArgOperands() == 1);
    auto arg0 = target->getArgOperand(0);

    auto &ctx = target->getContext();

    IRBuilder<> builder(ctx);
    builder.SetInsertPoint(target);

    auto castToAS2 = builder.CreateAddrSpaceCast(arg0, PointerType::get(arg0->getType()->getPointerElementType(), 2));
    auto castToInt = builder.CreatePtrToInt(castToAS2, Type::getInt64Ty(ctx));
    auto shifted = builder.CreateAShr(castToInt, ConstantInt::get(castToInt->getType(), 1));
    return shifted;
  }

  bool runOnFunction(Function &F) override {
    errs() << "Hello: ";
    errs().write_escaped(F.getName()) << '\n';


    for (BasicBlock &BB : F) {
      for (auto it = BB.begin(); it != BB.end();) {
        auto *CI = dyn_cast<CallInst>(&*it);
        if (!CI) {
          ++it;
          continue;
        }

        auto callee = CI->getCalledValue();

        if (callee == rapid_unboxint) {
          errs().write_escaped("found rapid.unboxint") << '\n';
          replaceInstruction(CI, lowerUnboxint(CI, F), it);
        } else {
          ++it;
        }
      }
    }

    return false;
  }
}; // end of struct Hello
}  // end of anonymous namespace


char Hello::ID = 0;
static RegisterPass<Hello> X("hello", "Hello World Pass",
                             false /* Only looks at CFG */,
                             false /* Analysis Pass */);

static RegisterStandardPasses Y(
    PassManagerBuilder::EP_EarlyAsPossible,
    [](const PassManagerBuilder &Builder,
       legacy::PassManagerBase &PM) { PM.add(new Hello()); });
