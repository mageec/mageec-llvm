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
#include "llvm/IR/Constants.h"
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

static bool insn_is_binary_int(Instruction *I) {
  switch (I->getOpcode()) {
    case Instruction::Add:
    case Instruction::Sub:
    case Instruction::Mul:
    case Instruction::UDiv:
    case Instruction::SDiv:
    case Instruction::URem:
    case Instruction::SRem:
    case Instruction::Shl: // Shift left  (logical)
    case Instruction::LShr: // Shift right (logical)
    case Instruction::AShr: // Shift right (arithmetic)
    case Instruction::And:
    case Instruction::Or:
    case Instruction::Xor:
    case Instruction::PtrToInt: // Pointer -> Integer
    case Instruction::IntToPtr: // Integer -> Pointer
    case Instruction::ICmp: // Integer comparison instruction
      return true;
    default:
      return false;
  }
}

static bool insn_is_binary_float(Instruction *I) {
  switch (I->getOpcode()) {
    case Instruction::FAdd:
    case Instruction::FSub:
    case Instruction::FMul:
    case Instruction::FDiv:
    case Instruction::FRem:
    case Instruction::FCmp: // Floating point comparison instr.
      return true;
    default:
      return false;
  }
}

static inline unsigned insn_count_operands(Instruction *I) {
  unsigned ops = 0;
  for (Instruction::op_iterator OI = I->op_begin(), OE = I->op_end();
      OI != OE; ++OI)
    ops++;
  return ops;
}

static inline bool call_has_ptr_arg(Instruction *I) {
  assert((I->getOpcode() == Instruction::Call) && "Called on non-call insn.");
  llvm::Value *V;
  // The last operand of the call instruction is a pointer to the called fn.
  for (Instruction::op_iterator OI = I->op_begin(), OE = I->op_end();
      OI != OE; ++OI) {
    V = *OI;
  }
  // V is a pointer to a function. The function itself is the pointer's
  // first contained type.
  llvm::Type *FT = V->getType()->getContainedType(0);
  if (FT->getNumContainedTypes() == 1) // Function takes no arguments
    return false;
  for (int i = 1, args = FT->getNumContainedTypes(); i < args; i++)
    if (FT->getContainedType(i)->isPointerTy())
      return true;
  return false;
}

static inline bool call_returns_int(Instruction *I) {
  assert((I->getOpcode() == Instruction::Call) && "Called on non-call insn.");
  llvm::Value *V;
  // The last operand of the call instruction is a pointer to the called fn.
  for (Instruction::op_iterator OI = I->op_begin(), OE = I->op_end();
      OI != OE; ++OI) {
    V = *OI;
  }
  // V is a pointer to a function. The function itself is the pointer's
  // first contained type. The return type is the first contained type of
  // the function definition.
  llvm::Type *RT = V->getType()->getContainedType(0)->getContainedType(0);
  return RT->isIntegerTy();
}

static inline bool call_returns_ptr(Instruction *I) {
  assert((I->getOpcode() == Instruction::Call) && "Called on non-call insn.");
  llvm::Value *V;
  // The last operand of the call instruction is a pointer to the called fn.
  for (Instruction::op_iterator OI = I->op_begin(), OE = I->op_end();
      OI != OE; ++OI) {
    V = *OI;
  }
  // V is a pointer to a function. The function itself is the pointer's
  // first contained type. The return type is the first contained type of
  // the function definition.
  llvm::Type *RT = V->getType()->getContainedType(0)->getContainedType(0);
  return RT->isPointerTy();
}

static inline unsigned count_const_val(Instruction *I, unsigned Val) {
  unsigned constants = 0;
  for (Instruction::op_iterator OI = I->op_begin(), OE = I->op_end();
       OI != OE; ++OI) {
    llvm::Value *V = *OI;
    if (const ConstantInt *CI = dyn_cast<ConstantInt>(V))
      if (CI->getValue() == Val)
        constants++;
  }
  return constants;
}

static inline unsigned count_int_consts(Instruction *I, unsigned size) {
  unsigned constants = 0;
  for (Instruction::op_iterator OI = I->op_begin(), OE = I->op_end();
       OI != OE; ++OI) {
    llvm::Value *V = *OI;
    llvm::Type *T = V->getType();
    if (V->getValueID() == Value::ConstantIntVal && T->isIntegerTy(size)) {
      constants++;
    }
  }
  return constants;
}

namespace {
  struct MageecFeatureExtractor : public FunctionPass {
    MageecInterface instance;
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
      unsigned direct_calls = 0;          //ft19
      unsigned method_assignments = 0;    //ft21
      unsigned method_binary_int = 0;     //ft22
      unsigned method_binary_float = 0;   //ft23
      unsigned average_phi_node_head = 0; //ft26
      unsigned average_phi_args = 0;      //ft27
      unsigned bb_phi_count_0 = 0;        //ft28
      unsigned bb_phi_count_0_to_3 = 0;   //ft29
      unsigned bb_phi_count_gt3 = 0;      //ft30
      unsigned bb_phi_args_gt5 = 0;       //ft31
      unsigned bb_phi_args_1_to_5 = 0;    //ft32
      unsigned method_switch_stmt = 0;    //ft33
      unsigned calls_with_ptr_args = 0;   //ft42
      unsigned calls_gt4_ops = 0;         //ft43
      unsigned calls_ret_ptr = 0;         //ft44
      unsigned calls_ret_int = 0;         //ft45
      unsigned const_int_zeroes = 0;      //ft46
      unsigned const_32bit_uses = 0;      //ft47
      unsigned const_int_ones = 0;        //ft48
      unsigned const_64bit_uses = 0;      //ft49

      // Cross-BasicBlock Temporaries
      unsigned phi_header_nodes = 0;      //total for ft26
      unsigned total_phi_args = 0;        //total for ft27
      unsigned total_phi_nodes = 0;       //divisor for ft27

      // Count basic blocks
      for (Function::iterator FI = F.begin(), FE = F.end();
           FI != FE; ++FI) {
        BasicBlock *BB = FI;
        unsigned statementcount = 0;
        unsigned phi_nodes = 0;  // phis in current block
        unsigned phi_args = 0;   // phi args in current block
        bool in_phi_header = true;

        // Count instructions in each basic block
        basicblocks++;
        for (BasicBlock::iterator BI = BB->begin(), BE = BB->end();
             BI != BE; ++BI) {
          Instruction *I = BI;
          statementcount++;

          if (insn_is_assignment(I)) {
            method_assignments++;
          }
          if (I->getOpcode() == Instruction::Call) {
            direct_calls++;
            if (call_has_ptr_arg(I))
              calls_with_ptr_args++;
            if (insn_count_operands(I) > 4)
              calls_gt4_ops++;
            if (call_returns_ptr(I))
              calls_ret_ptr++;
            if (call_returns_int(I))
              calls_ret_int++;
          }

          if (insn_is_binary_int(I))
            method_binary_int++;
          if (insn_is_binary_float(I))
            method_binary_float++;
          const_32bit_uses += count_int_consts(I, 32);
          const_64bit_uses += count_int_consts(I, 64);
          const_int_zeroes += count_const_val(I, 0);
          const_int_ones += count_const_val(I, 1);

          if (I->getOpcode() == Instruction::PHI) {
            phi_nodes++;
            total_phi_nodes++;
            if (in_phi_header)
              phi_header_nodes++;
            phi_args += insn_count_operands(I);
            total_phi_args += insn_count_operands(I);
          }
          else
            in_phi_header = false;
          if (I->getOpcode() == Instruction::Switch)
            method_switch_stmt++;
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
        if (phi_args > 5)
          bb_phi_args_gt5++;
        else if ((phi_args >= 1) && (phi_args <= 5))
          bb_phi_args_1_to_5++;
      }

      // Calculate averages once totals have been collected
      if (total_phi_nodes > 0)
        average_phi_args = total_phi_args / total_phi_nodes;
      average_phi_node_head = phi_header_nodes / basicblocks;

      /* Build feature vector to pass to machine learner */
      std::vector<mageec::mageec_feature*> features;

      features.push_back(new basic_feature("ft1", "Basic Block Count:", basicblocks));
      features.push_back(new basic_feature("ft2", "BB with 1 successor:", bb_single_successor));
      features.push_back(new basic_feature("ft3", "BB with 2 successor:", bb_two_successors));
      features.push_back(new basic_feature("ft4", "BB with > 2 successor:", bb_gt2_successors));
      features.push_back(new basic_feature("ft5", "BB with 1 predecessor:", bb_single_predecessor));
      features.push_back(new basic_feature("ft6", "BB with 2 predecessor:", bb_two_predecessors));
      features.push_back(new basic_feature("ft7", "BB with > 2 predecessor:", bb_gt2_predecessors));
      features.push_back(new basic_feature("ft8", "BB with 1 pred 1 succ:", bb_1pred_1succ));
      features.push_back(new basic_feature("ft9", "BB with 1 pred 2 succ:", bb_1pred_2succ));
      features.push_back(new basic_feature("ft10", "BB with 2 pred 1 succ:", bb_2pred_1succ));
      features.push_back(new basic_feature("ft11", "BB with 2 pred 2 succ:", bb_2pred_2succ));
      features.push_back(new basic_feature("ft12", "BB with >2 pred >2 succ:", bb_gt2pred_gt2succ));
      features.push_back(new basic_feature("ft13", "BB with insn < 15:", insn_count_lt15));
      features.push_back(new basic_feature("ft14", "BB with insn [15, 500]:", insn_count_15_to_500));
      features.push_back(new basic_feature("ft15", "BB with insn > 500:", insn_count_gt500));
      features.push_back(new basic_feature("ft19", "Number of direct calls:", direct_calls));
      features.push_back(new basic_feature("ft21", "Assignments in method:", method_assignments));
      features.push_back(new basic_feature("ft22", "Binary int ops in method", method_binary_int));
      features.push_back(new basic_feature("ft23", "Binary fp ops in method:", method_binary_float));
      features.push_back(new basic_feature("ft24", "Total Statement in BB:", vector_sum(insncounts)));
      features.push_back(new basic_feature("ft25", "Avg Statement in BB:", vector_sum(insncounts)/basicblocks));
      features.push_back(new basic_feature("ft26", "Avg phis at top of BB:", average_phi_node_head));
      features.push_back(new basic_feature("ft27", "Average phi arg count:", average_phi_args));
      features.push_back(new basic_feature("ft28", "BB with 0 phis:", bb_phi_count_0));
      features.push_back(new basic_feature("ft29", "BB with [0, 3] phis:", bb_phi_count_0_to_3));
      features.push_back(new basic_feature("ft30", "BB with > 3 phis:", bb_phi_count_gt3));
      features.push_back(new basic_feature("ft31", "BB phis with > 5 args:", bb_phi_args_gt5));
      features.push_back(new basic_feature("ft32", "BB phis with [1,5] args:", bb_phi_args_1_to_5));
      features.push_back(new basic_feature("ft33", "Switch stmts in method:", method_switch_stmt));
      features.push_back(new basic_feature("ft42", "Calls with ptr arg:", calls_with_ptr_args));
      features.push_back(new basic_feature("ft43", "Calls with > 4 ops:", calls_gt4_ops));
      features.push_back(new basic_feature("ft44", "Calls that return ptr:", calls_ret_ptr));
      features.push_back(new basic_feature("ft45", "Calls that return int:", calls_ret_int));
      features.push_back(new basic_feature("ft46", "Uses of const inst 0:", const_int_zeroes));
      features.push_back(new basic_feature("ft47", "Uses of 32bit int consts", const_32bit_uses));
      features.push_back(new basic_feature("ft48", "Uses of const inst 1:", const_int_ones));
      features.push_back(new basic_feature("ft49", "Uses of 64bit int consts", const_64bit_uses));

      instance.takeFeatures(F.getName(), features);

      // Print feature vector, first as a list and then as JSON
      errs() << "Current Function: ";
      errs().write_escaped(F.getName()) << '\n';
      mageec_feature::dump_vector(features, std::cerr, false);
      mageec_feature::dump_vector(features, std::cerr, true);

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

