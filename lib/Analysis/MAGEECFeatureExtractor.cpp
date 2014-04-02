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
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "mageec/vectormath.h"

using namespace llvm;

static unsigned count_bb_predecessors(BasicBlock *bb) {
  pred_iterator PI = pred_begin(bb), E = pred_end(bb);
  unsigned preds = 0;
  while (PI != E) {
    preds++;
    ++PI;
  }
  return preds;
}

static unsigned count_bb_successors(BasicBlock *bb) {
  succ_iterator SI = succ_begin(bb), E = succ_end(bb);
  unsigned succs = 0;
  while (SI != E) {
    succs++;
    ++SI;
  }
  return succs;
}

// We specify all instruction types here to ensure we have checked.
// Additionally this function returns true for any kind of assignment.
static bool insn_is_assignment(Instruction *I) {
  switch (I->getOpcode()) {
    case Instruction::Add:
    case Instruction::FAdd:
    case Instruction::Sub:
    case Instruction::FSub:
    case Instruction::Mul:
    case Instruction::FMul:
    case Instruction::UDiv:
    case Instruction::SDiv:
    case Instruction::FDiv:
    case Instruction::URem:
    case Instruction::SRem:
    case Instruction::FRem:
    case Instruction::Shl: // Shift left  (logical)
    case Instruction::LShr: // Shift right (logical)
    case Instruction::AShr: // Shift right (arithmetic)
    case Instruction::And:
    case Instruction::Or:
    case Instruction::Xor:
    case Instruction::Load: // Memory manipulation instrs
    case Instruction::Store:
    case Instruction::PHI: // PHI node instruction
    case Instruction::Select: // select instruction
    case Instruction::VAArg: // vaarg instruction
    // Casting, Truncating, etc.
    case Instruction::Trunc: // Truncate integers
    case Instruction::ZExt: // Zero extend integers
    case Instruction::SExt: // Sign extend integers
    case Instruction::FPToUI: // floating point -> UInt
    case Instruction::FPToSI: // floating point -> SInt
    case Instruction::UIToFP: // UInt -> floating point
    case Instruction::SIToFP: // SInt -> floating point
    case Instruction::FPTrunc: // Truncate floating point
    case Instruction::FPExt: // Extend floating point
    case Instruction::PtrToInt: // Pointer -> Integer
    case Instruction::IntToPtr: // Integer -> Pointer
    case Instruction::BitCast: // Type cast
    case Instruction::AddrSpaceCast: // addrspace cast
    // Vectors and aggregates
    case Instruction::ExtractElement: // extract from vector
    case Instruction::InsertElement: // insert into vector
    case Instruction::ShuffleVector: // shuffle two vectors.
    case Instruction::ExtractValue: // extract from aggregate
    case Instruction::InsertValue: // insert into aggregate
      return true;
    case Instruction::Ret:
    case Instruction::Br:
    case Instruction::Switch:
    case Instruction::IndirectBr:
    case Instruction::Invoke:
    case Instruction::Resume:
    case Instruction::Unreachable:
    case Instruction::Alloca: // Stack management
    case Instruction::Fence:
    case Instruction::GetElementPtr:
    case Instruction::AtomicCmpXchg:
    case Instruction::AtomicRMW:
    case Instruction::Call: // Call a function
    case Instruction::UserOp1: // May be used internally in a pass
    case Instruction::UserOp2: // Internal to passes only
    case Instruction::LandingPad: // Landing pad instruction.
    case Instruction::ICmp: // Integer comparison instruction
    case Instruction::FCmp: // Floating point comparison instr.
      return false;
    default:
      llvm_unreachable("Unknown opcode!");
  }
}

namespace {
  struct MageecFeatureExtractor : public FunctionPass {
  public:
    static char ID;
    MageecFeatureExtractor() : FunctionPass(ID) {
      initializeMageecFeatureExtractorPass(*PassRegistry::getPassRegistry());
    }

    virtual bool runOnFunction(Function &F) {

      // Variables for holding feature information
      std::vector<unsigned> insncounts;
      unsigned basicblocks = 0;           // ft1
      unsigned bb_single_successor = 0;   // ft2
      unsigned bb_two_successors = 0;     // ft3
      unsigned bb_gt2_successors = 0;     // ft4
      unsigned bb_single_predecessor = 0; // ft5
      unsigned bb_two_predecessors = 0;   // ft6
      unsigned bb_gt2_predecessors = 0;   // ft7
      unsigned bb_1pred_1succ = 0;        // ft8
      unsigned bb_1pred_2succ = 0;        // ft9
      unsigned bb_2pred_1succ = 0;        //ft10
      unsigned bb_2pred_2succ = 0;        //ft11
      unsigned bb_gt2pred_gt2succ = 0;    //ft12
      unsigned insn_count_lt15 = 0;       //ft13
      unsigned insn_count_15_to_500 = 0;  //ft14
      unsigned insn_count_gt500 = 0;      //ft15
      unsigned method_assignments = 0;    //ft21
      unsigned bb_phi_count_0 = 0;        //ft28
      unsigned bb_phi_count_0_to_3 = 0;   //ft29
      unsigned bb_phi_count_gt3 = 0;      //ft30

      // Temporaries
      unsigned total_phi_nodes = 0;       //divisor for ft27

      // Count basic blocks
      for (Function::iterator FI = F.begin(), FE = F.end();
           FI != FE; ++FI) {
        BasicBlock *BB = FI;
        unsigned statementcount = 0;
        unsigned phi_nodes = 0;  // phis in current block

        // Count instructions in each basic block
        basicblocks++;
        for (BasicBlock::iterator BI = BB->begin(), BE = BB->end();
             BI != BE; ++BI) {
          Instruction *I = BI;
          statementcount++;

          if (insn_is_assignment(I))
            method_assignments++;

          if (I->getOpcode() == Instruction::PHI) {
            phi_nodes++;
            total_phi_nodes++;
          }
        }

        // Successor/predecessor information
        unsigned preds = count_bb_predecessors(BB);
        unsigned succs = count_bb_successors(BB);
        if (succs == 1)
          bb_single_successor++;
        else if (succs == 2)
          bb_two_successors++;
        else if (succs > 2)
          bb_gt2_successors++;
        if (preds == 1)
          bb_single_predecessor++;
        else if (preds == 2)
          bb_two_predecessors++;
        else if (preds > 2)
          bb_gt2_predecessors++;
        if ((preds == 1) && (succs == 1))
          bb_1pred_1succ++;
        if ((preds == 1) && (succs == 2))
          bb_1pred_2succ++;
        if ((preds == 2) && (succs == 1))
          bb_2pred_1succ++;
        if ((preds == 2) && (succs == 2))
          bb_2pred_2succ++;
        if ((preds > 2) && (succs > 2))
          bb_gt2pred_gt2succ++;

        // Store processed data about this block
        insncounts.push_back(statementcount);
        if (statementcount < 15)
          insn_count_lt15++;
        else if (statementcount > 500)
          insn_count_gt500++;
        else
          insn_count_15_to_500++;

        if (phi_nodes == 0)
          bb_phi_count_0++;
        if (phi_nodes <= 3)
          bb_phi_count_0_to_3++;
        else
          bb_phi_count_gt3++;
      }

      errs() << "Current Function: ";
      errs().write_escaped(F.getName()) << '\n';
      errs() << "  (ft1)  Basic Block Count:       " << basicblocks << '\n';
      errs() << "  (ft2)  BB with 1 successor:     " << bb_single_successor << '\n';
      errs() << "  (ft3)  BB with 2 successor:     " << bb_two_successors << '\n';
      errs() << "  (ft4)  BB with > 2 successor:   " << bb_gt2_successors << '\n';
      errs() << "  (ft5)  BB with 1 predecessor:   " << bb_single_predecessor << '\n';
      errs() << "  (ft6)  BB with 2 predecessor:   " << bb_two_predecessors << '\n';
      errs() << "  (ft7)  BB with > 2 predecessor: " << bb_gt2_predecessors << '\n';
      errs() << "  (ft8)  BB with 1 pred 1 succ:   " << bb_1pred_1succ << '\n';
      errs() << "  (ft9)  BB with 1 pred 2 succ:   " << bb_1pred_2succ << '\n';
      errs() << "  (ft10) BB with 2 pred 1 succ:   " << bb_2pred_1succ << '\n';
      errs() << "  (ft11) BB with 2 pred 2 succ:   " << bb_2pred_2succ << '\n';
      errs() << "  (ft12) BB with >2 pred >2 succ: " << bb_gt2pred_gt2succ << '\n';
      errs() << "  (ft13) BB with insn < 15:       " << insn_count_lt15 << '\n';
      errs() << "  (ft14) BB with insn [15, 500]:  " << insn_count_15_to_500 << '\n';
      errs() << "  (ft15) BB with insn > 500:      " << insn_count_gt500 << '\n';
      errs() << "  (ft21) Assignments in method:   " << method_assignments << '\n';
      errs() << "  (ft24) Total Statement in BB:   " << vector_sum(insncounts) << '\n';
      errs() << "  (ft25) Avg Statement in BB:     " << vector_sum(insncounts)
                /basicblocks << '\n';
      errs() << "  (ft28) BB with 0 phis:          " << bb_phi_count_0 << '\n';
      errs() << "  (ft29) BB with [0, 3] phis:     " << bb_phi_count_0_to_3 << '\n';
      errs() << "  (ft30) BB with > 3 phis:        " << bb_phi_count_gt3 << '\n';


      /* Build feature vector to pass to machine learner */
      std::vector<mageec::mageec_feature*> features;

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

