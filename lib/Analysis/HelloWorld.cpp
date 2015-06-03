#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Analysis/Passes.h"

using namespace llvm;


namespace {
  struct HelloWorld : public ModulePass {
    static char ID; // Pass identifcation, replacement for typeid

    HelloWorld() : ModulePass(ID) {
      initializeHelloWorldPass(*PassRegistry::getPassRegistry());
    }

    // The hello world pass may be wrapped in a gate
    bool mayHavePredicate() const override { return true; }

    bool runOnModule(Module &B) override {
      dbgs() << "Hello World\n";
      return false;
    }

    void print(raw_ostream &OS, const Module* = nullptr) const override {}

    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.setPreservesAll();
    }
  };
}

char HelloWorld::ID = 0;
INITIALIZE_PASS(HelloWorld, "hello-world", "Hello World", false, true)

ModulePass *llvm::createHelloWorldPass() {
  return new HelloWorld();
}

