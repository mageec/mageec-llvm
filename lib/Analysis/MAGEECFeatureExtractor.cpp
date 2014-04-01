/*  MAGEEC Feature Extraction Pass
    Copyright (C) 2013,2014 Embecosm Limited and University of Bristol

    This file is part of MAGEEC.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>. */

#include "llvm/Analysis/Passes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/MAGEECPlugin.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "mageec/vectormath.h"


using namespace llvm;

namespace {
  struct MageecFeatureExtractor : public FunctionPass {
  public:
    static char ID;
    MageecFeatureExtractor() : FunctionPass(ID) {
      initializeMageecFeatureExtractorPass(*PassRegistry::getPassRegistry());
    }

    virtual bool runOnFunction(Function &F) {
      int basicblocks = 0;
      std::vector<unsigned> insncounts;

      // Count basic blocks
      for (Function::iterator FI = F.begin(), FE = F.end();
           FI != FE; ++FI) {
        unsigned statementcount = 0;
        basicblocks++;

        // Count instructions in each basic block
        for (BasicBlock::iterator BI = FI->begin(), BE = FI->end();
             BI != BE; ++BI) {
          statementcount++;
        }
        insncounts.push_back(statementcount);
      }

      errs() << "Function: ";
      errs().write_escaped(F.getName()) << '\n';
      errs() << "  Basic Block Count:     " << basicblocks << '\n';
      errs() << "  Min Statement in BB:   " << vector_min(insncounts) << '\n';
      errs() << "  Max Statement in BB:   " << vector_max(insncounts) << '\n';
      errs() << "  Avg Statement in BB:   " << vector_sum(insncounts)/basicblocks
             << '\n';
      errs() << "  Total Statement in BB: " << vector_sum(insncounts) << '\n';
      return false;
    }

    virtual const char* getPassName() const {
      return "MAGEEC Feature Extractor";
    }
  };
}

char MageecFeatureExtractor::ID = 0;
// The following line is used if we want to load in via a module
//static RegisterPass<MageecFeatureExtractorPass>
//  X("mageec", "MAGEEC Pass", false, false);
INITIALIZE_PASS(MageecFeatureExtractor, "mageec",
                "Enables MAGEEC Feature Extraction", false, true)

FunctionPass *llvm::createMageecFeatureExtractorPass() {
  return new MageecFeatureExtractor();
}

